%%%------------------------------------------------------------------------
%% Copyright 2021, OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc This is the module providing the OpenTelemetry protocol for
%% exporting traces. It can be configured through its application
%% environment, the OS environment or directly through a map of options
%% passed when setting up the exporter in the batch processor.
%%
%% `opentelemetry_exporter' application enevironment options are:
%%
%% * `otlp_endpoint': The URL to send traces and metrics to, for traces the
%%   path `v1/traces' is appended to the path in the URL.
%% * `otlp_traces_endpoint': URL to send only traces to. This takes precedence
%%   for exporting traces and the path of the URL is kept as is, no suffix is
%%   appended.
%% * `otlp_headers': Additional headers to add to export requests.
%% * `otlp_traces_headers': Additional headers to add to only trace export requests.
%%
%% There also corresponding OS environment variables can also set those
%% configuration values:
%%
%% * `OTEL_EXPORTER_OTLP_ENDPOINT'
%% * `OTEL_EXPORTER_OTLP_TRACES_ENDPOINT'
%% * `OTEL_EXPORTER_OTLP_HEADERS'
%% * `OTEL_EXPORTER_OTLP_TRACES_HEADERS'
%%
%% @end
%%%-------------------------------------------------------------------------
-module(opentelemetry_exporter).

-export([init/1,
         export/3,
         shutdown/1]).

%% for testing
-export([to_proto_by_instrumentation_library/1,
         to_proto/1,
         endpoints/1,
         merge_with_environment/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-define(DEFAULT_ENDPOINTS, [#{host => "localhost",
                              path => [],
                              port => 4317,
                              scheme => "https"}]).
-define(DEFAULT_PORT, 4317).
-define(DEFAULT_TRACES_PATH, "v1/traces").

-record(state, {protocol :: grpc | http_protobuf | http_json,
                channel_pid :: pid() | undefined,
                headers :: [{unicode:chardata(), unicode:chardata()}],
                grpc_metadata :: map() | undefined,
                endpoints :: [uri_string:uri_map()]}).

init(Opts0) ->
    Opts1 = merge_with_environment(Opts0),
    Endpoints = endpoints(maps:get(endpoints, Opts1, ?DEFAULT_ENDPOINTS)),
    Headers = headers(maps:get(headers, Opts1, [])),
    case maps:get(protocol, Opts1, http_protobuf) of
        grpc ->
            ChannelOpts = maps:get(channel_opts, Opts1, #{}),
            case grpcbox_channel:start_link(?MODULE, grpcbox_endpoints(Endpoints), ChannelOpts) of
                {ok, ChannelPid} ->
                    {ok, #state{channel_pid=ChannelPid,
                                endpoints=Endpoints,
                                headers=Headers,
                                grpc_metadata=headers_to_grpc_metadata(Headers),
                                protocol=grpc}};
                ErrorOrIgnore ->
                    %% even if it is `ignore' we should just use `http_protobuf' because
                    %% `ignore' should never happen and means something is wrong
                    ?LOG_WARNING("unable to start grpc channel for exporting and falling back "
                                 "to http_protobuf protocol. reason=~p", [ErrorOrIgnore]),
                    {ok, #state{endpoints=Endpoints,
                                headers=Headers,
                                protocol=http_protobuf}}
            end;
        http_protobuf ->
            {ok, #state{endpoints=Endpoints,
                        headers=Headers,
                        protocol=http_protobuf}};
        http_json ->
            {ok, #state{endpoints=Endpoints,
                        headers=Headers,
                        protocol=http_json}}
    end.

export(_Tab, _Resource, #state{protocol=http_json}) ->
    {error, unimplemented};
export(Tab, Resource, #state{protocol=http_protobuf,
                             headers=Headers,
                             endpoints=[#{scheme := Scheme,
                                          host := Host,
                                          port := Port,
                                          path := Path} | _]}) ->
    Proto = opentelemetry_exporter_trace_service_pb:encode_msg(tab_to_proto(Tab, Resource),
                                                               export_trace_service_request),
    Address = uri_string:normalize(#{scheme => Scheme,
                                     host => Host,
                                     port => Port,
                                     path => Path}),
    case httpc:request(post, {Address, Headers, "application/x-protobuf", Proto}, [], []) of
        {ok, {{_, Code, _}, _, _}} when Code >= 200 andalso Code =< 202 ->
            ok;
        {ok, {{_, Code, _}, _, Message}} ->
            ?LOG_INFO("error response from service exported to status=~p ~s",
                      [Code, Message]),
            error;
        {error, Reason} ->
            ?LOG_INFO("client error exporting spans ~p", [Reason]),
            error
    end;
export(Tab, Resource, #state{protocol=grpc,
                             grpc_metadata=Metadata,
                             channel_pid=_ChannelPid}) ->
    ExportRequest = tab_to_proto(Tab, Resource),
    Ctx = grpcbox_metadata:append_to_outgoing_ctx(ctx:new(), Metadata),
    case opentelemetry_trace_service:export(Ctx, ExportRequest, #{channel => ?MODULE}) of
        {ok, _Response, _ResponseMetadata} ->
            ok;
        {error, {Status, Message}, _} ->
            ?LOG_INFO("OTLP grpc export failed with GRPC status ~s : ~s", [Status, Message]),
            error;
        {http_error, {Status, _}, _} ->
            ?LOG_INFO("OTLP grpc export failed with HTTP status code ~s", [Status]),
            error;
        {error, Reason} ->
            ?LOG_INFO("OTLP grpc export failed with error: ~p", [Reason]),
            error
    end.

shutdown(#state{channel_pid=undefined}) ->
    ok;
shutdown(#state{channel_pid=Pid}) ->
    _ = grpcbox_channel:stop(Pid),
    ok.

%%

grpcbox_endpoints(Endpoints) ->
    [{scheme(Scheme), Host, Port, maps:get(ssl_options, Endpoint, [])} || 
        #{scheme := Scheme, host := Host, port := Port} = Endpoint <- Endpoints].

headers_to_grpc_metadata(Headers) ->
    lists:foldl(fun({X, Y}, Acc) ->
                        maps:put(unicode:characters_to_binary(X), unicode:characters_to_binary(Y), Acc)
                end, #{}, Headers).

%% make all headers into list strings
headers(List) when is_list(List) ->
    [{unicode:characters_to_list(X), unicode:characters_to_list(Y)} || {X, Y} <- List];
headers(_) ->
    [].

endpoints(List) when is_list(List) ->
    Endpoints = case io_lib:printable_list(List) of
                    true ->
                        [List];
                    false ->
                        List
                end,

    lists:filtermap(fun endpoint/1, Endpoints);
endpoints(Endpoint) ->
    lists:filtermap(fun endpoint/1, [Endpoint]).

endpoint(Endpoint) ->
    case parse_endpoint(Endpoint) of
        false ->
            ?LOG_WARNING("Failed to parse and ignoring exporter endpoint ~p", [Endpoint]),
            false;
        Parsed ->
            Parsed
    end.

parse_endpoint({Scheme, Host, Port, SSLOptions}) when is_list(SSLOptions) ->
    {true, #{scheme => atom_to_list(Scheme), host => Host, port => Port, path => [], ssl_options => SSLOptions}};
parse_endpoint({Scheme, Host, Port, _}) ->
    {true, #{scheme => atom_to_list(Scheme), host => Host, port => Port, path => []}};
parse_endpoint(Endpoint=#{host := _Host, port := _Port, scheme := _Scheme, path := _Path}) ->
    {true, Endpoint};
parse_endpoint(Endpoint=#{host := _Host, scheme := _Scheme, path := _Path}) ->
    {true, Endpoint#{port => ?DEFAULT_PORT}};
parse_endpoint(String) when is_list(String) orelse is_binary(String) ->
    parse_endpoint(uri_string:parse(unicode:characters_to_list(String)));
parse_endpoint(_) ->
    false.

scheme(Scheme) when Scheme =:= "https" orelse Scheme =:= <<"https">> ->
    https;
scheme(Scheme) when Scheme =:= "http" orelse Scheme =:= <<"http">> ->
    http;
scheme(Scheme) ->
    Scheme.

merge_with_environment(Opts) ->
    %% exporters are initalized by calling their `init/1' function from `opentelemetry'.
    %% since this application depends on `opentelemetry' it will not be started during
    %% boot before its `init/1' is called. In a release this is fine since all apps
    %% are loaded first, before any are started, but in case this is run not by a
    %% release we load the application here to ensure the application environment is
    %% available to read configuration from.
    application:load(opentelemetry_exporter),

    AppEnv = application:get_all_env(opentelemetry_exporter),
    AppOpts = otel_configuration:merge_list_with_environment(config_mapping(), AppEnv),

    %% append the default path `/v1/traces` only to the path of otlp_endpoint
    Opts1 = update_opts(otlp_endpoint, endpoints, ?DEFAULT_ENDPOINTS, AppOpts, Opts, fun endpoints_append_path/1),
    Opts2 = update_opts(otlp_traces_endpoint, endpoints, ?DEFAULT_ENDPOINTS, AppOpts, Opts1, fun maybe_to_list/1),

    Opts3 = update_opts(otlp_headers, headers, [], AppOpts, Opts2),
    update_opts(otlp_traces_headers, headers, [], AppOpts, Opts3).

maybe_to_list(E) when is_list(E) ->
    case io_lib:printable_list(E) of
        true ->
            [E];
        false ->
            E
    end;
maybe_to_list(E) ->
    [E].

endpoints_append_path(E) when is_list(E) ->
    case io_lib:printable_list(E) of
        true ->
            [append_path(E)];
        false ->
            [append_path(Endpoint) || Endpoint <- E]
    end;
endpoints_append_path(E) ->
    [append_path(E)].

append_path({Scheme, Host, Port, SSLOptions}) ->
    #{scheme => atom_to_list(Scheme), host => Host, port => Port, path => "/v1/traces", ssl_options => SSLOptions};
append_path(Endpoint=#{path := Path}) ->
    Endpoint#{path => filename:join(Path, ?DEFAULT_TRACES_PATH)};
append_path(EndpointString) when is_list(EndpointString) orelse is_binary(EndpointString) ->
    Endpoint=#{path := Path} = uri_string:parse(EndpointString),
    Endpoint#{path => filename:join(Path, ?DEFAULT_TRACES_PATH)}.

%% use the value from the environment if it exists, otherwise use the value
%% passed in Opts or the default
update_opts(AppKey, OptKey, Default, AppOpts, Opts) ->
    update_opts(AppKey, OptKey, Default, AppOpts, Opts, fun id/1).

update_opts(AppKey, OptKey, Default, AppOpts, Opts, Transform) ->
    case proplists:get_value(AppKey, AppOpts) of
        undefined ->
            maps:update_with(OptKey, Transform, Transform(Default), Opts);
        EnvValue ->
            maps:put(OptKey, Transform(EnvValue), Opts)
    end.

id(X) ->
    X.

config_mapping() ->
    [
     %% endpoint the Otel protocol exporter should connect to
     {"OTEL_EXPORTER_OTLP_ENDPOINT", otlp_endpoint, undefined, url},
     {"OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", otlp_traces_endpoint, undefined, url},
     %% {"OTEL_EXPORTER_OTLP_METRICS_ENDPOINT", otlp_metrics_endpoint, "https://localhost:4317", url},

     %% headers to include in requests the exporter makes over the Otel protocol
     {"OTEL_EXPORTER_OTLP_HEADERS", otlp_headers, undefined, key_value_list},
     {"OTEL_EXPORTER_OTLP_TRACES_HEADERS", otlp_traces_headers, undefined, key_value_list}
     %% {"OTEL_EXPORTER_OTLP_METRICS_HEADERS", otlp_metrics_headers, "", key_value_list}

     %% the following are not yet defined in the spec
     %% {"OTEL_EXPORTER_OTLP_PROTOCOL", exporter_otlp_protocol, "", string},
     %% {"OTEL_EXPORTER_OTLP_TRACES_PROTOCOL", exporter_otlp_traces_protocol, "", string},
     %% {"OTEL_EXPORTER_OTLP_METRICS_PROTOCOL", exporter_otlp_metrics_protocol, "", string}
    ].

tab_to_proto(Tab, Resource) ->
    InstrumentationLibrarySpans = to_proto_by_instrumentation_library(Tab),
    Attributes = otel_resource:attributes(Resource),
    ResourceSpans = [#{resource => #{attributes => to_attributes(Attributes),
                                     dropped_attributes_count => 0},
                       instrumentation_library_spans => InstrumentationLibrarySpans}],
    #{resource_spans => ResourceSpans}.

to_proto_by_instrumentation_library(Tab) ->
    Key = ets:first(Tab),
    to_proto_by_instrumentation_library(Tab, Key).

to_proto_by_instrumentation_library(_Tab, '$end_of_table') ->
    [];
to_proto_by_instrumentation_library(Tab, Key) ->
    InstrumentationLibrarySpans = lists:foldl(fun(Span, Acc) ->
                                                      [to_proto(Span) | Acc]
                                              end, [], ets:lookup(Tab, Key)),
    [#{instrumentation_library => to_instrumentation_library(Key),
       spans => InstrumentationLibrarySpans}
     | to_proto_by_instrumentation_library(Tab, ets:next(Tab, Key))].

to_instrumentation_library(#instrumentation_library{name=Name,
                                                    version=Version}) ->
    #{name => Name,
      version => Version};
to_instrumentation_library(_) ->
    undefined.

%% TODO: figure out why this type spec fails
%% -spec to_proto(#span{}) -> opentelemetry_exporter_trace_service_pb:span().

to_proto(#span{trace_id=TraceId,
               span_id=SpanId,
               tracestate=TraceState,
               parent_span_id=MaybeParentSpanId,
               name=Name,
               kind=Kind,
               start_time=StartTime,
               end_time=EndTime,
               attributes=Attributes,
               events=TimedEvents,
               links=Links,
               status=Status,
               trace_flags=_TraceFlags,
               is_recording=_IsRecording}) ->
    ParentSpanId = case MaybeParentSpanId of undefined -> <<>>; _ -> <<MaybeParentSpanId:64>> end,
    #{name                     => to_binary(Name),
      trace_id                 => <<TraceId:128>>,
      span_id                  => <<SpanId:64>>,
      parent_span_id           => ParentSpanId,
      trace_state              => to_tracestate_string(TraceState),
      kind                     => to_otlp_kind(Kind),
      start_time_unix_nano     => to_unixnano(StartTime),
      end_time_unix_nano       => to_unixnano(EndTime),
      attributes               => to_attributes(Attributes),
      dropped_attributes_count => 0,
      events                   => to_events(TimedEvents),
      dropped_events_count     => 0,
      links                    => to_links(Links),
      dropped_links_count      => 0,
      status                   => to_status(Status)}.

-spec to_unixnano(integer()) -> non_neg_integer().
to_unixnano(Timestamp) ->
    opentelemetry:timestamp_to_nano(Timestamp).

-spec to_attributes(opentelemetry:attributes()) -> [opentelemetry_exporter_trace_service_pb:key_value()].
to_attributes(Attributes) ->
    to_attributes(Attributes, []).

%% Note: nested maps may be an issue.
to_attributes([], Acc) ->
    Acc;
to_attributes([{Key, Value} | Rest], Acc) when is_binary(Value) ->
    to_attributes(Rest, [#{key => to_binary(Key),
                           value => #{value => {string_value, Value}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_atom(Value) ->
    to_attributes(Rest, [#{key => to_binary(Key),
                           value => #{value => {string_value, to_binary(Value)}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_integer(Value) ->
    to_attributes(Rest, [#{key => to_binary(Key),
                           value => #{value => {int_value, Value}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_float(Value) ->
    to_attributes(Rest, [#{key => to_binary(Key),
                           value => #{value => {double_value, Value}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_boolean(Value) ->
    to_attributes(Rest, [#{key => to_binary(Key),
                           value => #{value => {bool_value, Value}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_map(Value) ->
    to_attributes(Rest, [#{key => to_binary(Key),
                           values => #{value => {kvlist_value, maps:to_list(Value)}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_tuple(Value) ->
    to_attributes(Rest, [#{key => to_binary(Key),
                           values => #{value => {array_value, tuple_to_list(Value)}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_list(Value) ->
    case is_proplist(Value) of
        true ->
            to_attributes(Rest, [#{key => to_binary(Key),
                                        values => #{value => {kvlist_value, Value}}} | Acc]);
        false ->
            to_attributes(Rest, [#{key => Key,
                                   values => #{value => {array_value, Value}}} | Acc])
    end;
to_attributes([_ | Rest], Acc) ->
    to_attributes(Rest, Acc).

is_proplist([]) -> true;
is_proplist([{K,_}|L]) when is_atom(K) or is_binary(K) -> is_proplist(L);
is_proplist(_) -> false.

to_binary(Term) when is_atom(Term) -> erlang:atom_to_binary(Term, unicode);
to_binary(Term) -> Term.

-spec to_status(opentelemetry:status()) -> opentelemetry_exporter_trace_service_pb:status().
to_status(#status{code=Code,
                  message=Message}) ->
    #{code => to_otlp_status(Code),
      message => Message};
to_status(_) ->
    #{}.

-spec to_events([opentelemetry:events()]) -> [opentelemetry_exporter_trace_service_pb:event()].
to_events(Events) ->
    to_events(Events, []).

to_events([], Acc)->
    Acc;
to_events([#event{system_time_nano=Timestamp,
                  name=Name,
                  attributes=Attributes} | Rest], Acc) ->
    to_events(Rest, [#{time_unix_nano => to_unixnano(Timestamp),
                       name => to_binary(Name),
                       attributes => to_attributes(Attributes)} | Acc]).

-spec to_links(opentelemetry:links()) -> [opentelemetry_exporter_trace_service_pb:link()].
to_links(Links) ->
    to_links(Links, []).

to_links([], Acc)->
    Acc;
to_links([#link{trace_id=TraceId,
                span_id=SpanId,
                attributes=Attributes,
                tracestate=TraceState} | Rest], Acc) ->
    to_links(Rest, [#{trace_id => <<TraceId:128>>,
                      span_id => <<SpanId:64>>,
                      trace_state => to_tracestate_string(TraceState),
                      attributes => to_attributes(Attributes),
                      dropped_attributes_count => 0} | Acc]).

to_tracestate_string(undefined) ->
    "";
to_tracestate_string(List) ->
    lists:join($,, [[Key, $=, Value] || {Key, Value} <- List]).

-spec to_otlp_kind(atom()) -> opentelemetry_exporter_trace_service_pb:'span.SpanKind'().
to_otlp_kind(?SPAN_KIND_INTERNAL) ->
    'SPAN_KIND_INTERNAL';
to_otlp_kind(?SPAN_KIND_SERVER) ->
    'SPAN_KIND_SERVER';
to_otlp_kind(?SPAN_KIND_CLIENT) ->
    'SPAN_KIND_CLIENT';
to_otlp_kind(?SPAN_KIND_PRODUCER) ->
    'SPAN_KIND_PRODUCER';
to_otlp_kind(?SPAN_KIND_CONSUMER) ->
    'SPAN_KIND_CONSUMER';
to_otlp_kind(_) ->
    'SPAN_KIND_UNSPECIFIED'.

-spec to_otlp_status(atom()) -> opentelemetry_exporter_trace_service_pb:'status.StatusCode'().
to_otlp_status(?OTEL_STATUS_UNSET) ->
    'STATUS_CODE_UNSET';
to_otlp_status(?OTEL_STATUS_OK) ->
    'STATUS_CODE_OK';
to_otlp_status(?OTEL_STATUS_ERROR) ->
    'STATUS_CODE_ERROR'.
