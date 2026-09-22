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
%% @doc This module exports telemetry using OTLP.
%%
%% SDK tracer configuration should select the `otlp_http' or `otlp_grpc'
%% component. Their declarative option maps use `endpoint', `headers',
%% `headers_list', `compression', and `tls'. Configuration supplied through
%% the SDK is authoritative and is not overridden by exporter application or
%% OS environment values.
%%
%% Direct callers of `init/1' use the implementation option map with
%% `endpoints', `headers', `protocol', `compression', and `ssl_options'. For
%% compatibility, direct calls without the SDK's resolved-configuration marker
%% merge the `opentelemetry_exporter' application environment and corresponding
%% `OTEL_EXPORTER_*' environment variables.
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
