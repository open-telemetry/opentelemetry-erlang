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
%% exporting metrics. It can be configured through its application
%% environment, the OS environment or directly through a map of options
%% passed when setting up the exporter in the batch processor.
%%
%% `opentelemetry_experimental' application environment options are:
%%
%% <ul>
%%   <li>
%%     `otlp_endpoint': The URL to send traces, metrics and logs to, for metrics the
%%     path `v1/metrics' is appended to the path in the URL.
%%   </li>
%%   <li>
%%     `otlp_metrics_endpoint': URL to send only metrics to. This takes precedence
%%     for exporting metrics and the path of the URL is kept as is, no suffix is
%%     appended.
%%   </li>
%%   <li>
%%     `otlp_headers': List of additional headers (`[{unicode:chardata(), unicode:chardata()}]') to add to export requests.
%%   </li>
%%   <li>
%%     `otlp_metrics_headers': Additional headers (`[{unicode:chardata(), unicode:chardata()}]') to add to only metric export requests.
%%   </li>
%%   <li>
%%     `otlp_protocol': The transport protocol, supported values: `grpc' and `http_protobuf'. Defaults to `http_protobuf'.
%%   </li>
%%   <li>
%%     `otlp_metrics_protocol': The transport protocol to use for exporting traces, supported values: `grpc' and `http_protobuf'. Defaults to `http_protobuf'
%%   </li>
%%   <li>
%%     `otlp_compression': Compression type to use, supported values: `gzip'. Defaults to no compression.
%%   </li>
%%   <li>
%%     `otlp_metrics_compression': Compression type to use for exporting metrics, supported values: `gzip'. Defaults to no compression.
%%   </li>
%% </ul>
%%
%% There also corresponding OS environment variables can also set those
%% configuration values:
%%
%% <ul>
%%   <li>`OTEL_EXPORTER_OTLP_ENDPOINT': The URL to send traces, metrics and logs to, for metrics the path `v1/metrics' is appended to the path in the URL.</li>
%%   <li>`OTEL_EXPORTER_OTLP_METRICS_ENDPOINT': URL to send only metrics to. This takes precedence for exporting metrics and the path of the URL is kept as is, no suffix is appended.</li>
%%   <li>`OTEL_EXPORTER_OTLP_HEADERS': List of additional headers to add to export requests.</li>
%%   <li>`OTEL_EXPORTER_OTLP_METRICS_HEADERS': Additional headers to add to only trace export requests.</li>
%%   <li>`OTEL_EXPORTER_OTLP_PROTOCOL': The transport protocol to use, supported values: `grpc' and `http_protobuf'. Defaults to `http_protobuf'.</li>
%%   <li>`OTEL_EXPORTER_OTLP_METRICS_PROTOCOL': The transport protocol to use for exporting metrics, supported values: `grpc' and `http_protobuf'. Defaults to `http_protobuf'.</li>
%%   <li>`OTEL_EXPORTER_OTLP_COMPRESSION': Compression to use, supported value: gzip. Defaults to no compression.</li>
%%   <li>`OTEL_EXPORTER_OTLP_METRICS_COMPRESSION': Compression to use when exporting metrics, supported value: gzip. Defaults to no compression.</li>
%% </ul>
%%
%% You can also set these configuration values in the map passed to the
%% opentelemetry processor configuration.
%% <ul>
%%   <li>`endpoints': A list of endpoints to send metrics to. Can take one of the forms described below. By default, exporter sends data to `http://localhost:4318'.</li>
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
%% While using `http_protobuf' protocol, currently only the first endpoint in that list is used to export metrics, the rest is effectively ignored. `grpc' supports multiple endpoints.
%%
%% @end
%%%-------------------------------------------------------------------------
-module(otel_exporter_metrics_otlp).

-behaviour(otel_exporter_metrics).

-export([init/1,
         export/3,
         shutdown/1,
         merge_with_environment/1]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_METRICS_PATH, "v1/metrics").

-record(state, {channel :: term(),
                httpc_profile :: atom() | undefined,
                protocol :: otel_exporter_otlp:protocol(),
                channel_pid :: pid() | undefined,
                headers :: otel_exporter_otlp:headers(),
                compression :: otel_exporter_otlp:compression() | undefined,
                grpc_metadata :: map() | undefined,
                endpoints :: [otel_exporter_otlp:endpoint_map()]}).

-include_lib("opentelemetry_api/include/gradualizer.hrl").

%% @doc Initialize the exporter based on the provided configuration.
-spec init(otel_exporter_otlp:opts()) -> {ok, #state{}}.
init(Opts) ->
    Opts1 = merge_with_environment(Opts),
    case otel_exporter_otlp:init(Opts1) of
        {ok, #{channel := Channel,
               channel_pid := ChannelPid,
               endpoints := Endpoints,
               headers := Headers,
               compression := Compression,
               grpc_metadata := Metadata,
               protocol := grpc}} ->
            {ok, #state{channel=Channel,
                        channel_pid=ChannelPid,
                        endpoints=Endpoints,
                        headers=Headers,
                        compression=Compression,
                        grpc_metadata=Metadata,
                        protocol=grpc}};
        {ok, #{httpc_profile := HttpcProfile,
               endpoints := Endpoints,
               headers := Headers,
               compression := Compression,
               protocol := http_protobuf}} ->
            {ok, #state{httpc_profile=HttpcProfile,
                        endpoints=Endpoints,
                        headers=Headers,
                        compression=Compression,
                        protocol=http_protobuf}};
        {ok, #{httpc_profile := HttpcProfile,
               endpoints := Endpoints,
               headers := Headers,
               compression := Compression,
               protocol := http_json}} ->
            {ok, #state{httpc_profile=HttpcProfile,
                        endpoints=Endpoints,
                        headers=Headers,
                        compression=Compression,
                        protocol=http_json}}
    end.

%% @doc Export OTLP protocol telemery data to the configured endpoints.
export(_Metrics, _Resource, #state{protocol=http_json}) ->
    {error, unimplemented};
export(Metrics, Resource, #state{protocol=http_protobuf,
                                 httpc_profile=HttpcProfile,
                                 headers=Headers,
                                 compression=Compression,
                                 endpoints=[#{scheme := Scheme,
                                              host := Host,
                                              path := Path,
                                              port := Port,
                                              ssl_options := SSLOptions} | _]}) ->
    case uri_string:normalize(#{scheme => Scheme,
                                host => Host,
                                port => Port,
                                path => Path}) of
        {error, Type, Error} ->
            ?LOG_INFO("error normalizing OTLP export URI: ~p ~p",
                      [Type, Error]),
            error;
        Address ->
            case otel_otlp_metrics:to_proto(Metrics, Resource) of
                empty ->
                    ok;
                ProtoMap ->
                    Body = opentelemetry_exporter_metrics_service_pb:encode_msg(ProtoMap,
                                                                                export_metrics_service_request),
                    otel_exporter_otlp:export_http(Address, Headers, Body, Compression, SSLOptions, HttpcProfile)
            end
    end;
export(Metrics, Resource, #state{protocol=grpc,
                                 grpc_metadata=Metadata,
                                 channel=Channel}) ->
    case otel_otlp_metrics:to_proto(Metrics, Resource) of
        empty ->
            ok;
        Request ->
            GrpcCtx = ctx:new(),
            otel_exporter_otlp:export_grpc(GrpcCtx, opentelemetry_metrics_service, Metadata, Request, Channel)
    end;
export(_Metrics, _Resource, _State) ->
    {error, unimplemented}.

%% @doc Shutdown the exporter.
shutdown(#state{channel_pid=undefined}) ->
    ok;
shutdown(#state{channel_pid=Pid}) ->
    _ = grpcbox_channel:stop(Pid),
    ok.

%%

merge_with_environment(Opts) ->
    %% exporters are initialized by calling their `init/1' function from `opentelemetry'.
    %% since this application depends on `opentelemetry' it will not be started during
    %% boot before its `init/1' is called. In a release this is fine since all apps
    %% are loaded first, before any are started, but in case this is run not by a
    %% release we load the application here to ensure the application environment is
    %% available to read configuration from.
    application:load(opentelemetry_experimental),
    AppEnv = application:get_all_env(opentelemetry_experimental),
    otel_exporter_otlp:merge_with_environment(config_mapping(),
                                              AppEnv,
                                              Opts,
                                              otlp_metrics_endpoint,
                                              otlp_metrics_headers,
                                              otlp_metrics_protocol,
                                              otlp_metrics_compression,
                                              ?DEFAULT_METRICS_PATH).

config_mapping() ->
    [
     %% endpoint the Otel protocol exporter should connect to
     {"OTEL_EXPORTER_OTLP_ENDPOINT", otlp_endpoint, url},
     {"OTEL_EXPORTER_OTLP_METRICS_ENDPOINT", otlp_metrics_endpoint, url},

     %% headers to include in requests the exporter makes over the Otel protocol
     {"OTEL_EXPORTER_OTLP_HEADERS", otlp_headers, key_value_list},
     {"OTEL_EXPORTER_OTLP_METRICS_HEADERS", otlp_metrics_headers, key_value_list},

     {"OTEL_EXPORTER_OTLP_PROTOCOL", otlp_protocol, otlp_protocol},
     {"OTEL_EXPORTER_OTLP_METRICS_PROTOCOL", exporter_otlp_metrics_protocol, otlp_protocol},

     {"OTEL_EXPORTER_OTLP_COMPRESSION", otlp_compression, existing_atom},
     {"OTEL_EXPORTER_OTLP_METRICS_COMPRESSION", otlp_metrics_compression, existing_atom},

     %% {"OTEL_EXPORTER_OTLP_CERTIFICATE", otlp_certificate, path},
     %% {"OTEL_EXPORTER_OTLP_METRICS_CERTIFICATE", otlp_metrics_certificate, path},

     %% {"OTEL_EXPORTER_OTLP_TIMEOUT", otlp_timeout, integer},
     %% {"OTEL_EXPORTER_OTLP_METRICS_TIMEOUT", otlp_metrics_timeout, integer}

     {"OTEL_EXPORTER_SSL_OPTIONS", ssl_options, key_value_list}
    ].
