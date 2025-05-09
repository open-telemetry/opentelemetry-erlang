
%%%------------------------------------------------------------------------
%% Copyright The OpenTelemetry Authors
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
%%%-------------------------------------------------------------------------
-include_lib("opentelemetry_semantic_conventions/include/attributes/otel_attributes.hrl").


%% A name uniquely identifying the instance of the OpenTelemetry component within its containing SDK instance.
%%  
-define(OTEL_COMPONENT_NAME, 'otel.component.name').


%% A name identifying the type of the OpenTelemetry component.
%%  
-define(OTEL_COMPONENT_TYPE, 'otel.component.type').

-define(OTEL_COMPONENT_TYPE_VALUES_BATCHING_SPAN_PROCESSOR, 'batching_span_processor').

-define(OTEL_COMPONENT_TYPE_VALUES_SIMPLE_SPAN_PROCESSOR, 'simple_span_processor').

-define(OTEL_COMPONENT_TYPE_VALUES_BATCHING_LOG_PROCESSOR, 'batching_log_processor').

-define(OTEL_COMPONENT_TYPE_VALUES_SIMPLE_LOG_PROCESSOR, 'simple_log_processor').

-define(OTEL_COMPONENT_TYPE_VALUES_OTLP_GRPC_SPAN_EXPORTER, 'otlp_grpc_span_exporter').

-define(OTEL_COMPONENT_TYPE_VALUES_OTLP_HTTP_SPAN_EXPORTER, 'otlp_http_span_exporter').

-define(OTEL_COMPONENT_TYPE_VALUES_OTLP_HTTP_JSON_SPAN_EXPORTER, 'otlp_http_json_span_exporter').

-define(OTEL_COMPONENT_TYPE_VALUES_OTLP_GRPC_LOG_EXPORTER, 'otlp_grpc_log_exporter').

-define(OTEL_COMPONENT_TYPE_VALUES_OTLP_HTTP_LOG_EXPORTER, 'otlp_http_log_exporter').

-define(OTEL_COMPONENT_TYPE_VALUES_OTLP_HTTP_JSON_LOG_EXPORTER, 'otlp_http_json_log_exporter').

-define(OTEL_COMPONENT_TYPE_VALUES_PERIODIC_METRIC_READER, 'periodic_metric_reader').

-define(OTEL_COMPONENT_TYPE_VALUES_OTLP_GRPC_METRIC_EXPORTER, 'otlp_grpc_metric_exporter').

-define(OTEL_COMPONENT_TYPE_VALUES_OTLP_HTTP_METRIC_EXPORTER, 'otlp_http_metric_exporter').

-define(OTEL_COMPONENT_TYPE_VALUES_OTLP_HTTP_JSON_METRIC_EXPORTER, 'otlp_http_json_metric_exporter').


%% @deprecated Use the `otel.scope.name` attribute.
%% Deprecated. Use the `otel.scope.name` attribute
-define(OTEL_LIBRARY_NAME, 'otel.library.name').

%% @deprecated Use the `otel.scope.version` attribute.
%% Deprecated. Use the `otel.scope.version` attribute.
-define(OTEL_LIBRARY_VERSION, 'otel.library.version').


%% The result value of the sampler for this span
-define(OTEL_SPAN_SAMPLING_RESULT, 'otel.span.sampling_result').

-define(OTEL_SPAN_SAMPLING_RESULT_VALUES_DROP, 'DROP').

-define(OTEL_SPAN_SAMPLING_RESULT_VALUES_RECORD_ONLY, 'RECORD_ONLY').

-define(OTEL_SPAN_SAMPLING_RESULT_VALUES_RECORD_AND_SAMPLE, 'RECORD_AND_SAMPLE').

