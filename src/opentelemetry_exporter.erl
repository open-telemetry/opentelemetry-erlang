-module(opentelemetry_exporter).

-export([init/1,
         export/3,
         shutdown/1]).

%% for roundtrip testing
-export([to_proto_by_instrumentation_library/1,
         to_proto/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/ot_span.hrl").

-define(DEFAULT_ENDPOINTS, [{http, "localhost", 9090, []}]).

-record(state, {protocol :: grpc | http_protobuf | http_json,
                channel_pid :: pid() | undefined,
                endpoints :: list()}).

init(Opts) ->
    Endpoints = maps:get(endpoints, Opts, ?DEFAULT_ENDPOINTS),
    case maps:get(protocol, Opts, http_protobuf) of
        grpc ->
            Endpoints = maps:get(endpoints, Opts, ?DEFAULT_ENDPOINTS),
            ChannelOpts = maps:get(channel_opts, Opts, #{}),
            {ok, ChannelPid} = grpcbox_channel:start_link(?MODULE, Endpoints, ChannelOpts),

            {ok, #state{channel_pid=ChannelPid,
                        endpoints=Endpoints,
                        protocol=grpc}};
        http_protobuf ->
            {ok, #state{endpoints=Endpoints,
                        protocol=http_protobuf}};
        http_json ->
            {ok, #state{endpoints=Endpoints,
                        protocol=http_json}}
    end.

export(_Tab, _Resource, #state{protocol=http_json}) ->
    {error, unimplemented};
export(Tab, Resource, #state{protocol=http_protobuf,
                             endpoints=[{Scheme, Host, Port, _} | _]}) ->
    Proto = opentelemetry_exporter_trace_service_pb:encode_msg(tab_to_proto(Tab, Resource),
                                                               export_trace_service_request),
    Address = uri_string:normalize(#{scheme => atom_to_list(Scheme),
                                     host => Host,
                                     port => Port,
                                     path => <<"/v1/trace">>}),
    case httpc:request(post, {Address, [], "application/x-protobuf", Proto}, [], []) of
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
                             channel_pid=_ChannelPid}) ->
    ExportRequest = tab_to_proto(Tab, Resource),
    opentelemetry_trace_service:export(ctx:new(), ExportRequest, #{channel => ?MODULE}),
    ok.

shutdown(#state{channel_pid=undefined}) ->
    ok;
shutdown(#state{channel_pid=Pid}) ->
    _ = grpcbox_channel:stop(Pid),
    ok.

%%

tab_to_proto(Tab, Resource) ->
    InstrumentationLibrarySpans = to_proto_by_instrumentation_library(Tab),
    Attributes = ot_resource:attributes(Resource),
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
               child_span_count=ChildSpanCount,
               trace_options=_TraceOptions,
               is_recording=_IsRecording}) ->
    ParentSpanId = case MaybeParentSpanId of undefined -> <<>>; _ -> <<MaybeParentSpanId:64>> end,
    #{name                     => Name,
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
      status                   => to_status(Status),
      local_child_span_count   => ChildSpanCount}.

-spec to_unixnano(integer()) -> non_neg_integer().
to_unixnano(Timestamp) ->
    opentelemetry:timestamp_to_nano(Timestamp).

-spec to_attributes(opentelemetry:attributes()) -> [opentelemetry_exporter_trace_service_pb:attribute_key_value()].
to_attributes(Attributes) ->
    to_attributes(Attributes, []).

%% Note: nested maps may be an issue.
to_attributes([], Acc) ->
    Acc;
to_attributes([{Key, Value} | Rest], Acc) when is_binary(Value) ->
    to_attributes(Rest, [#{key => Key,
                           value => #{value => {string_value, Value}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_integer(Value) ->
    to_attributes(Rest, [#{key => Key,
                           value => #{value => {int_value, Value}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_float(Value) ->
    to_attributes(Rest, [#{key => Key,
                           value => #{value => {double_value, Value}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_boolean(Value) ->
    to_attributes(Rest, [#{key => Key,
                           value => #{value => {bool_value, Value}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_map(Value) ->
    to_attributes(Rest, [#{key => Key,
                           values => #{value => {kvlist_value, maps:to_list(Value)}}} | Acc]);
to_attributes([{Key, Value} | Rest], Acc) when is_list(Value) ->
    case is_proplist(Value) of
        true ->
            to_attributes(Rest, [#{key => Key,
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
                       name => Name,
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
                      trace_state => TraceState,
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

-spec to_otlp_status(atom()) -> opentelemetry_exporter_trace_service_pb:'span.StatusCode'().
to_otlp_status(?OTEL_STATUS_OK) ->
    'STATUS_CODE_OK';
to_otlp_status(?OTEL_STATUS_CANCELLED) ->
    'STATUS_CODE_CANCELLED';
to_otlp_status(?OTEL_STATUS_UNKNOWN) ->
    'STATUS_CODE_UNKNOWN_ERROR';
to_otlp_status(?OTEL_STATUS_INVALID_ARGUMENT) ->
    'STATUS_CODE_INVALID_ARGUMENT';
to_otlp_status(?OTEL_STATUS_DEADLINE_EXCEEDED) ->
    'STATUS_CODE_DEADLINE_EXCEEDED';
to_otlp_status(?OTEL_STATUS_NOT_FOUND) ->
    'STATUS_CODE_NOT_FOUND';
to_otlp_status(?OTEL_STATUS_ALREADY_EXISTS) ->
    'STATUS_CODE_ALREADY_EXISTS';
to_otlp_status(?OTEL_STATUS_PERMISSION_DENIED) ->
    'STATUS_CODE_PERMISSION_DENIED';
to_otlp_status(?OTEL_STATUS_RESOURCE_EXHAUSTED) ->
    'STATUS_CODE_RESOURCE_EXHAUSTED';
to_otlp_status(?OTEL_STATUS_FAILED_PRECONDITION) ->
    'STATUS_CODE_FAILED_PRECONDITION';
to_otlp_status(?OTEL_STATUS_ABORTED) ->
    'STATUS_CODE_ABORTED';
to_otlp_status(?OTEL_STATUS_OUT_OF_RANGE) ->
    'STATUS_CODE_OUT_OF_RANGE';
to_otlp_status(?OTEL_STATUS_UNIMPLEMENTED) ->
    'STATUS_CODE_UNIMPLEMENTED';
to_otlp_status(?OTEL_STATUS_INTERNAL) ->
    'STATUS_CODE_INTERNAL_ERROR';
to_otlp_status(?OTEL_STATUS_UNAVAILABLE) ->
    'STATUS_CODE_UNAVAILABLE';
to_otlp_status(?OTEL_STATUS_DATA_LOSS) ->
    'STATUS_CODE_DATA_LOSS';
to_otlp_status(?OTEL_STATUS_UNAUTHENTICATED) ->
    'STATUS_CODE_UNAUTHENTICATED'.
