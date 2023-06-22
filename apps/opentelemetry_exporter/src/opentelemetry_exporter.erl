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
%% `opentelemetry_exporter' application environment options are:
%%
%% <ul>
%%   <li>
%%     `otlp_endpoint': The URL to send traces and metrics to, for traces the
%%     path `v1/traces' is appended to the path in the URL.
%%   </li>
%%   <li>
%%     `otlp_traces_endpoint': URL to send only traces to. This takes precedence
%%     for exporting traces and the path of the URL is kept as is, no suffix is
%%     appended.
%%   </li>
%%   <li>
%%     `otlp_headers': List of additional headers (`[{unicode:chardata(), unicode:chardata()}]') to add to export requests.
%%   </li>
%%   <li>
%%     `otlp_traces_headers': Additional headers (`[{unicode:chardata(), unicode:chardata()}]') to add to only trace export requests.
%%   </li>
%%   <li>
%%     `otlp_protocol': The transport protocol, supported values: `grpc' and `http_protobuf'. Defaults to `http_protobuf'.
%%   </li>
%%   <li>
%%     `otlp_traces_protocol': The transport protocol to use for exporting traces, supported values: `grpc' and `http_protobuf'. Defaults to `http_protobuf'
%%   </li>
%%   <li>
%%     `otlp_compression': Compression type to use, supported values: `gzip'. Defaults to no compression.
%%   </li>
%%   <li>
%%     `otlp_traces_compression': Compression type to use for exporting traces, supported values: `gzip'. Defaults to no compression.
%%   </li>
%% </ul>
%%
%% There also corresponding OS environment variables can also set those
%% configuration values:
%%
%% <ul>
%%   <li>`OTEL_EXPORTER_OTLP_ENDPOINT': The URL to send traces and metrics to, for traces the path `v1/traces' is appended to the path in the URL.</li>
%%   <li>`OTEL_EXPORTER_OTLP_TRACES_ENDPOINT': URL to send only traces to. This takes precedence for exporting traces and the path of the URL is kept as is, no suffix is appended.</li>
%%   <li>`OTEL_EXPORTER_OTLP_HEADERS': List of additional headers to add to export requests.</li>
%%   <li>`OTEL_EXPORTER_OTLP_TRACES_HEADERS': Additional headers to add to only trace export requests.</li>
%%   <li>`OTEL_EXPORTER_OTLP_PROTOCOL': The transport protocol to use, supported values: `grpc' and `http_protobuf'. Defaults to `http_protobuf'.</li>
%%   <li>`OTEL_EXPORTER_OTLP_TRACES_PROTOCOL': The transport protocol to use for exporting traces, supported values: `grpc' and `http_protobuf'. Defaults to `http_protobuf'.</li>
%%   <li>`OTEL_EXPORTER_OTLP_COMPRESSION': Compression to use, supported value: gzip. Defaults to no compression.</li>
%%   <li>`OTEL_EXPORTER_OTLP_TRACES_COMPRESSION': Compression to use when exporting traces, supported value: gzip. Defaults to no compression.</li>
%% </ul>
%%
%% You can also set these configuration values in the map passed to the
%% opentelemetry processor configuration.
%% <ul>
%%   <li>`endpoints': A list of endpoints to send traces to. Can take one of the forms described below. By default, exporter sends data to `http://localhost:4318'.</li>
%%   <li>`headers': List of additional headers to add to export requests.</li>
%%   <li>`protocol': The transport protocol to use, supported values: `grpc' and `http_protobuf'. Defaults to `http_protobuf'.</li>
%%   <li>`compression': Compression to use, supported value: `gzip'. Defaults to no compression.</li>
%%   <li>`ssl_options': a list of SSL options.  See Erlang's <a href='https://www.erlang.org/doc/man/ssl.html#TLS/DTLS%20OPTION%20DESCRIPTIONS%20-%20CLIENT'>SSL docs</a> for what options are available.</li>
%% </ul>
%%
%% Endpoints configuration
%%
%% You can pass your collector endpoints in three forms:
%%
%% <ul>
%%   <li> As a string, i.e `"https://localhost:4000"'.</li>
%%   <li> As a map, with the following keys:
%%     <ul>
%%       <li>`host => unicode:chardata()'</li>
%%       <li>`path => unicode:chardata()'</li>
%%       <li>`port => integer() >= 0 | undefined'</li>
%%       <li>`scheme => unicode:chardata()'</li>
%%     </ul>
%%   </li>
%%   <li> As a 4 element tuple in format `{Scheme, Host, Port, SSLOptions}'.</li>
%% </ul>
%%
%% While using `http_protobuf' protocol, currently only the first endpoint in that list is used to export traces, the rest is effectively ignored. `grpc' supports multiple endpoints.
%%
%% @end
%%%-------------------------------------------------------------------------
-module(opentelemetry_exporter).

-export([init/1,
         export/4,
         shutdown/1]).

%% for testing
-ifdef(TEST).
-export([endpoints/2,
         merge_with_environment/1]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-define(DEFAULT_HTTP_PORT, 4318).
-define(DEFAULT_HTTP_ENDPOINTS, [#{host => "localhost",
                                   path => filename:join([], ?DEFAULT_TRACES_PATH),
                                   port => ?DEFAULT_HTTP_PORT,
                                   scheme => "http"}]).

-define(DEFAULT_GRPC_PORT, 4317).
-define(DEFAULT_GRPC_ENDPOINTS, [#{host => "localhost",
                                   path => [],
                                   port => ?DEFAULT_GRPC_PORT,
                                   scheme => "http"}]).

-define(DEFAULT_TRACES_PATH, "v1/traces").

-type headers() :: [{unicode:chardata(), unicode:chardata()}].
-type scheme() :: http | https | string() | binary().
-type host() :: unicode:chardata().
-type input_endpoint_map() :: #{scheme := scheme(),
                          host := host(),
                          path => unicode:chardata(),
                          port => integer(),
                          ssl_options => []}.

-type input_endpoint() :: uri_string:uri_string() | uri_string:uri_map() | input_endpoint_map().
-type endpoint() :: uri_string:uri_string() | uri_string:uri_map().
-type endpoint_map() :: #{scheme := unicode:chardata(),
                          host := unicode:chardata(), %%host(),
                          path => unicode:chardata(),
                          port => integer(),
                          ssl_options => []}.

-type protocol() :: grpc | http_protobuf | http_json.
-type compression() :: gzip.

-type opts() :: #{endpoints => [input_endpoint()],
                  headers => headers(),
                  protocol => protocol(),
                  ssl_options => list()}.

-export_type([opts/0,
              headers/0,
              endpoint/0,
              protocol/0]).

-record(state, {channel :: term(),
                httpc_profile :: atom() | undefined,
                protocol :: protocol(),
                channel_pid :: pid() | undefined,
                headers :: headers(),
                compression :: compression() | undefined,
                grpc_metadata :: map() | undefined,
                endpoints :: [endpoint_map()]}).

-include_lib("opentelemetry_api/include/gradualizer.hrl").

%% @doc Initialize the exporter based on the provided configuration.
-spec init(opts()) -> {ok, #state{}}.
init(Opts) ->
    Opts1 = merge_with_environment(Opts),
    SSLOptions = maps:get(ssl_options, Opts1, undefined),
    Headers = headers(maps:get(headers, Opts1, [])),
    Compression = maps:get(compression, Opts1, undefined),
    case maps:get(protocol, Opts1, http_protobuf) of
        grpc ->
            Endpoints = endpoints(maps:get(endpoints, Opts1, ?DEFAULT_GRPC_ENDPOINTS), SSLOptions),
            ChannelOpts = maps:get(channel_opts, Opts1, #{}),
            UpdatedChannelOpts = case Compression of
                                   undefined -> ChannelOpts;
                                   Encoding -> maps:put(encoding, Encoding, ChannelOpts)
                                 end,

            %% Channel name can be any term. To separate Channels per
            %% the process calling the exporter (like the batch processor,
            %% metric reader and log handler) use the current pid
            Channel = self(),
            case grpcbox_channel:start_link(Channel,
                                            grpcbox_endpoints(Endpoints),
                                            UpdatedChannelOpts) of
                {ok, ChannelPid} ->
                    {ok, #state{channel=Channel,
                                channel_pid=ChannelPid,
                                endpoints=Endpoints,
                                headers=Headers,
                                compression=Compression,
                                grpc_metadata=headers_to_grpc_metadata(Headers),
                                protocol=grpc}};
                ErrorOrIgnore ->
                    %% even if it is `ignore' we should just use `http_protobuf' because
                    %% `ignore' should never happen and means something is wrong
                    ?LOG_WARNING("unable to start grpc channel for exporting and falling back "
                                 "to http_protobuf protocol. reason=~p", [ErrorOrIgnore]),
                    {ok, #state{endpoints=Endpoints,
                                headers=Headers,
                                compression=Compression,
                                protocol=http_protobuf}}
            end;
        http_protobuf ->
            HttpcProfile = start_httpc(Opts1),
            Endpoints = endpoints(maps:get(endpoints, Opts1, ?DEFAULT_HTTP_ENDPOINTS), SSLOptions),
            {ok, #state{httpc_profile=HttpcProfile,
                        endpoints=Endpoints,
                        headers=Headers,
                        compression=Compression,
                        protocol=http_protobuf}};
        http_json ->
            HttpcProfile = start_httpc(Opts1),
            Endpoints = endpoints(maps:get(endpoints, Opts1, ?DEFAULT_HTTP_ENDPOINTS), SSLOptions),
            {ok, #state{httpc_profile=HttpcProfile,
                        endpoints=Endpoints,
                        headers=Headers,
                        compression=Compression,
                        protocol=http_json}}
    end.

%% use a unique httpc profile per exporter
start_httpc(Opts) ->
    HttpcProfile = list_to_atom(lists:concat([?MODULE, "_", erlang:pid_to_list(self())])),

    case httpc:info(HttpcProfile) of
        {error, {not_started, _}} ->
            %% by default use inet6fb4 which will try ipv6 and then fallback to ipv4 if it fails
            HttpcOptions = lists:ukeymerge(1,
                                           lists:usort(maps:get(httpc_options, Opts, [])),
                                           [{ipfamily, inet6fb4}]
                                          ),
            %% can't use `stand_alone' because then `httpc:info(Profile)' would fail
            {ok, Pid} = inets:start(httpc, [{profile, HttpcProfile}]),
            ok = httpc:set_options(HttpcOptions, Pid);
        _ ->
            %% profile already started
            ok
    end,
    HttpcProfile.

%% @doc Export OTLP protocol telemery data to the configured endpoints.
export(traces, _Tab, _Resource, #state{protocol=http_json}) ->
    {error, unimplemented};
export(traces, Tab, Resource, #state{protocol=http_protobuf,
                                     httpc_profile=HttpcProfile,
                                     headers=Headers,
                                     compression=Compression,
                                     endpoints=[#{scheme := Scheme,
                                                  host := Host,
                                                  path := Path,
                                                  port := Port,
                                                  ssl_options := SSLOptions} | _]}) ->
    case otel_otlp_traces:to_proto(Tab, Resource) of
        empty ->
            ok;
        ProtoMap ->
            Proto = opentelemetry_exporter_trace_service_pb:encode_msg(ProtoMap,
                                                                       export_trace_service_request),
            {NewHeaders, NewProto} =
                case Compression of
                    gzip -> {[{"content-encoding", "gzip"} | Headers], zlib:gzip(Proto)};
                    _ -> {Headers, Proto}
                end,
            case uri_string:normalize(#{scheme => Scheme,
                                        host => Host,
                                        port => Port,
                                        path => Path}) of
                {error, Type, Error} ->
                    ?LOG_INFO("error normalizing OTLP export URI: ~p ~p",
                              [Type, Error]),
                    error;
                Address ->
                    case httpc:request(post, {Address, NewHeaders, "application/x-protobuf", NewProto},
                                       [{ssl, SSLOptions}], [], HttpcProfile) of
                        {ok, {{_, Code, _}, _, _}} when Code >= 200 andalso Code =< 202 ->
                            ok;
                        {ok, {{_, Code, _}, _, Message}} ->
                            ?LOG_INFO("error response from service exported to status=~p ~s",
                                      [Code, Message]),
                            error;
                        {error, Reason} ->
                            ?LOG_INFO("client error exporting spans ~p", [Reason]),
                            error
                    end
            end
    end;
export(traces, Tab, Resource, #state{protocol=grpc,
                                     grpc_metadata=Metadata,
                                     channel=Channel}) ->
    case otel_otlp_traces:to_proto(Tab, Resource) of
        empty ->
            ok;
        ExportRequest ->
            Ctx = grpcbox_metadata:append_to_outgoing_ctx(ctx:new(), Metadata),
            case opentelemetry_trace_service:export(Ctx, ExportRequest, #{channel => Channel}) of
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
            end
    end;
export(metrics, Tab, Resource, #state{protocol=grpc,
                                      grpc_metadata=Metadata,
                                      channel=Channel}) ->
    ExportRequest = otel_otlp_metrics:to_proto(Tab, Resource),
    Ctx = grpcbox_metadata:append_to_outgoing_ctx(ctx:new(), Metadata),
    case opentelemetry_metrics_service:export(Ctx, ExportRequest, #{channel => Channel}) of
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
    end;
export(logs, {Logs, Config}, Resource, #state{channel=Channel,
                                              protocol=grpc,
                                              grpc_metadata=Metadata}) ->
    ExportRequest = otel_otlp_logs:to_proto(Logs, Resource, Config),
    Ctx = grpcbox_metadata:append_to_outgoing_ctx(ctx:new(), Metadata),
    case opentelemetry_logs_service:export(Ctx, ExportRequest, #{channel => Channel}) of
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
    end;
export(_, _Tab, _Resource, _State) ->
    {error, unimplemented}.

%% @doc Shutdown the exporter.
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

-spec endpoints([endpoint()], list() | undefined) -> [endpoint_map()].
endpoints(List, DefaultSSLOpts) when is_list(List) ->
    Endpoints = case io_lib:printable_list(List) of
                    true ->
                        [List];
                    false ->
                        List
                end,

    lists:filtermap(fun(E) -> endpoint(E, DefaultSSLOpts) end, Endpoints);
endpoints(Endpoint, DefaultSSLOpts) ->
    lists:filtermap(fun(E) -> endpoint(E, DefaultSSLOpts) end, [Endpoint]).

endpoint(Endpoint, DefaultSSLOpts) ->
    case parse_endpoint(Endpoint, DefaultSSLOpts) of
        false ->
            ?LOG_WARNING("Failed to parse and ignoring exporter endpoint ~p", [Endpoint]),
            false;
        Parsed ->
            Parsed
    end.

parse_endpoint({Scheme, Host, Port, SSLOptions}, _DefaultSSLOpts) when is_list(SSLOptions) ->
    {true, #{scheme => atom_to_list(Scheme),
             host => unicode:characters_to_list(Host),
             port => Port,
             path => [],
             ssl_options => SSLOptions}};
parse_endpoint({Scheme, Host, Port, _}, DefaultSSLOpts) ->
    HostString = unicode:characters_to_list(Host),
    {true, #{scheme => atom_to_list(Scheme),
             host => HostString,
             port => Port,
             path => [],
             ssl_options => update_ssl_opts(HostString, DefaultSSLOpts)}};
parse_endpoint(Endpoint=#{host := Host, scheme := Scheme, path := Path}, DefaultSSLOpts) ->
    HostString = unicode:characters_to_list(Host),
    %% `merge' keeps the value in the second argument if the key is in both
    %% so to always update the scheme/host/port to charlists we set those
    %% separate from port/ssl_options which should only be added if not already
    %% found in `Endpoint'
    {true, maps:merge(#{port => scheme_port(Scheme),
                        %% we only want to run `tls_certificate_check' if absolutely
                        %% necessary, so wrapping the setup of ssl options here
                        %% in a check that it won't just be overwritten by the value
                        %% from the Endpoint anyway
                        ssl_options => case maps:is_key(ssl_options, Endpoint) of
                                           true ->
                                               [];
                                           false ->
                                               update_ssl_opts(HostString, DefaultSSLOpts)
                                       end},
                      Endpoint#{scheme => to_charlist(Scheme),
                                host => HostString,
                                path => unicode:characters_to_list(Path)})};
parse_endpoint(String, DefaultSSLOpts) when is_list(String) orelse is_binary(String) ->
    case unicode:characters_to_list(String) of
        {_, _, _} ->
            ?LOG_WARNING("error converting endpoint URI ~s to utf8", [String]),
            false;
        UnicodeList ->
            case uri_string:parse(UnicodeList) of
                {error, Reason, Message} ->
                    ?LOG_WARNING("error parsing endpoint URI: ~s : ~p", [Reason, Message]),
                    false;
                ParsedUri ->
                    ParsedUri1 = maybe_add_scheme_port(ParsedUri),
                    parse_endpoint(ParsedUri1, DefaultSSLOpts)
            end
    end;
parse_endpoint(_, _) ->
    false.

to_charlist(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_charlist(Other) ->
    unicode:characters_to_list(Other).

scheme_port(Scheme) when not is_atom(Scheme) ->
    scheme_port(scheme(Scheme));
scheme_port(http) ->
    80;
scheme_port(https) ->
    443;
scheme_port(_) ->
    %% unknown scheme
    80.

maybe_add_scheme_port(Uri=#{port := _Port}) ->
    Uri;
maybe_add_scheme_port(Uri=#{scheme := "http"}) ->
    Uri#{port => 80};
maybe_add_scheme_port(Uri=#{scheme := "https"}) ->
    Uri#{port => 443};
%% an unknown scheme
maybe_add_scheme_port(Uri) ->
    Uri.


%% if no ssl opts are defined by the user then use defaults from `tls_certificate_check'
update_ssl_opts(Host, undefined) ->
    tls_certificate_check:options(Host);
update_ssl_opts(_, SSLOptions) ->
    SSLOptions.


scheme(Scheme) when Scheme =:= "https" orelse Scheme =:= <<"https">> ->
    https;
scheme(Scheme) when Scheme =:= "http" orelse Scheme =:= <<"http">> ->
    http;
scheme(Scheme) ->
    ?LOG_WARNING("unknown scheme ~p, converting to existing atom, if possible, and using as is", [Scheme]),
    to_existing_atom(Scheme).

to_existing_atom(Term) when is_atom(Term) ->
    Term;
to_existing_atom(Scheme) when is_list(Scheme) ->
    list_to_existing_atom(Scheme);
to_existing_atom(Scheme) when is_binary(Scheme) ->
    %% TODO: switch to binary_to_existing_atom once we drop OTP-22 support
    list_to_existing_atom(binary_to_list(Scheme));
to_existing_atom(_) ->
    erlang:error(bad_exporter_scheme).

merge_with_environment(Opts) ->
    %% exporters are initialized by calling their `init/1' function from `opentelemetry'.
    %% since this application depends on `opentelemetry' it will not be started during
    %% boot before its `init/1' is called. In a release this is fine since all apps
    %% are loaded first, before any are started, but in case this is run not by a
    %% release we load the application here to ensure the application environment is
    %% available to read configuration from.
    application:load(opentelemetry_exporter),

    Config = #{otlp_endpoint => undefined,
               otlp_traces_endpoint => undefined,
               otlp_headers => undefined,
               otlp_traces_headers => undefined,
               otlp_protocol => undefined,
               otlp_traces_protocol => undefined,
               otlp_compression => undefined,
               otlp_traces_compression => undefined,
               ssl_options => undefined},

    AppEnv = application:get_all_env(opentelemetry_exporter),
    AppOpts = otel_configuration:merge_list_with_environment(config_mapping(), AppEnv, Config),

    %% check for error in app env value parsing
    case maps:get(otlp_endpoint, AppOpts) of
        {error, Reason, Message} ->
            ?LOG_WARNING("error parsing endpoint URI: ~s : ~p", [Reason, Message]),
            maps:put(endpoints, [], Opts);

        _ ->
            %% append the default path `/v1/traces` only to the path of otlp_endpoint
            Opts1 = update_opts(otlp_endpoint, endpoints, ?DEFAULT_HTTP_ENDPOINTS, AppOpts, Opts, fun endpoints_append_path/1),
            Opts2 = update_opts(otlp_traces_endpoint, endpoints, ?DEFAULT_HTTP_ENDPOINTS, AppOpts, Opts1, fun maybe_to_list/1),

            Opts3 = update_opts(otlp_headers, headers, [], AppOpts, Opts2),
            Opts4 = update_opts(otlp_traces_headers, headers, [], AppOpts, Opts3),

            Opts5 = update_opts(otlp_protocol, protocol, http_protobuf, AppOpts, Opts4),
            Opts6 = update_opts(otlp_traces_protocol, protocol, http_protobuf, AppOpts, Opts5),

            Opts7 = update_opts(otlp_compression, compression, undefined, AppOpts, Opts6),
            Opts8 = update_opts(otlp_traces_compression, compression, undefined, AppOpts, Opts7),

            update_opts(ssl_options, ssl_options, undefined, AppOpts, Opts8)
    end.

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
append_path(Endpoint=#{}) ->
    Endpoint#{path => filename:join([], ?DEFAULT_TRACES_PATH)};
append_path(EndpointString) when is_list(EndpointString) orelse is_binary(EndpointString) ->
    Endpoint=#{path := Path} = uri_string:parse(EndpointString),
    Endpoint#{path => filename:join(?assert_type(Path, string() | binary()), ?DEFAULT_TRACES_PATH)}.

%% use the value from the environment if it exists, otherwise use the value
%% passed in Opts or the default
update_opts(AppKey, OptKey, Default, AppOpts, Opts) ->
    update_opts(AppKey, OptKey, Default, AppOpts, Opts, fun id/1).

update_opts(AppKey, OptKey, Default, AppOpts, Opts, Transform) ->
    case maps:get(AppKey, AppOpts) of
        undefined ->
            %% use default unless already set, in which case just transform
            maps:update_with(OptKey, Transform, Default, Opts);
        EnvValue ->
            maps:put(OptKey, Transform(EnvValue), Opts)
    end.

id(X) ->
    X.

config_mapping() ->
    [
     %% endpoint the Otel protocol exporter should connect to
     {"OTEL_EXPORTER_OTLP_ENDPOINT", otlp_endpoint, url},
     {"OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", otlp_traces_endpoint, url},
     %% {"OTEL_EXPORTER_OTLP_METRICS_ENDPOINT", otlp_metrics_endpoint, "https://localhost:4317", url},

     %% headers to include in requests the exporter makes over the Otel protocol
     {"OTEL_EXPORTER_OTLP_HEADERS", otlp_headers, key_value_list},
     {"OTEL_EXPORTER_OTLP_TRACES_HEADERS", otlp_traces_headers, key_value_list},
     %% {"OTEL_EXPORTER_OTLP_METRICS_HEADERS", otlp_metrics_headers, "", key_value_list}

     {"OTEL_EXPORTER_OTLP_PROTOCOL", otlp_protocol, otlp_protocol},
     {"OTEL_EXPORTER_OTLP_TRACES_PROTOCOL", otlp_traces_protocol, otlp_protocol},
     %% {"OTEL_EXPORTER_OTLP_METRICS_PROTOCOL", exporter_otlp_metrics_protocol, otlp_protocol}

     {"OTEL_EXPORTER_OTLP_COMPRESSION", otlp_compression, existing_atom},
     {"OTEL_EXPORTER_OTLP_TRACES_COMPRESSION", otlp_traces_compression, existing_atom},
     %% {"OTEL_EXPORTER_OTLP_METRICS_COMPRESSION", otlp_metrics_compression, existing_atom}

     %% {"OTEL_EXPORTER_OTLP_CERTIFICATE", otlp_certificate, path},
     %% {"OTEL_EXPORTER_OTLP_TRACES_CERTIFICATE", otlp_traces_certificate, path},
     %% {"OTEL_EXPORTER_OTLP_METRICS_CERTIFICATE", otlp_metrics_certificate, path},

     %% {"OTEL_EXPORTER_OTLP_TIMEOUT", otlp_timeout, integer},
     %% {"OTEL_EXPORTER_OTLP_TRACES_TIMEOUT", otlp_traces_timeout, integer},
     %% {"OTEL_EXPORTER_OTLP_METRICS_TIMEOUT", otlp_metrics_timeout, integer}

     {"OTEL_EXPORTER_SSL_OPTIONS", ssl_options, key_value_list}
    ].
