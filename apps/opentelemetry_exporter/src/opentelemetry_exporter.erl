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
         export/3,
         export/4,
         shutdown/1]).

%% @doc Initialize the exporter based on the provided configuration.
init(Opts) ->
    otel_exporter_traces_otlp:init(Opts).

export(Tab, Resource, State) ->
    otel_exporter_traces_otlp:export(Tab, Resource, State).

%% @doc Export OTLP protocol telemery data to the configured endpoints.
export(traces, Tab, Resource, State) ->
    otel_exporter_traces_otlp:export(Tab, Resource, State);
export(metrics, Metrics, Resource, State) ->
    otel_exporter_metrics_otlp:export(Metrics, Resource, State);
export(logs, Logs, Resource, State) ->
    otel_exporter_logs_otlp:export(Logs, Resource, State);
export(_, _Tab, _Resource, _State) ->
    {error, unimplemented}.

%% @doc Shutdown the exporter.
shutdown(State) ->
    otel_exporter_traces_otlp:shutdown(State).

%%
