
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

%% The number of log records for which the export has finished, either successful or failed
-define(OTEL_SDK_EXPORTER_LOG_EXPORTED, 'otel.sdk.exporter.log.exported').


%% The number of log records which were passed to the exporter, but that have not been exported yet (neither successful, nor failed)
-define(OTEL_SDK_EXPORTER_LOG_INFLIGHT, 'otel.sdk.exporter.log.inflight').


%% The number of metric data points for which the export has finished, either successful or failed
-define(OTEL_SDK_EXPORTER_METRIC_DATA_POINT_EXPORTED, 'otel.sdk.exporter.metric_data_point.exported').


%% The number of metric data points which were passed to the exporter, but that have not been exported yet (neither successful, nor failed)
-define(OTEL_SDK_EXPORTER_METRIC_DATA_POINT_INFLIGHT, 'otel.sdk.exporter.metric_data_point.inflight').


%% The duration of exporting a batch of telemetry records.
-define(OTEL_SDK_EXPORTER_OPERATION_DURATION, 'otel.sdk.exporter.operation.duration').


%% The number of spans for which the export has finished, either successful or failed
-define(OTEL_SDK_EXPORTER_SPAN_EXPORTED, 'otel.sdk.exporter.span.exported').

%% @deprecated Renamed to `otel.sdk.exporter.span.exported`.
%% Deprecated, use `otel.sdk.exporter.span.exported` instead.
-define(OTEL_SDK_EXPORTER_SPAN_EXPORTED_COUNT, 'otel.sdk.exporter.span.exported.count').


%% The number of spans which were passed to the exporter, but that have not been exported yet (neither successful, nor failed)
-define(OTEL_SDK_EXPORTER_SPAN_INFLIGHT, 'otel.sdk.exporter.span.inflight').

%% @deprecated Renamed to `otel.sdk.exporter.span.inflight`.
%% Deprecated, use `otel.sdk.exporter.span.inflight` instead.
-define(OTEL_SDK_EXPORTER_SPAN_INFLIGHT_COUNT, 'otel.sdk.exporter.span.inflight.count').


%% The number of logs submitted to enabled SDK Loggers
-define(OTEL_SDK_LOG_CREATED, 'otel.sdk.log.created').


%% The duration of the collect operation of the metric reader.
-define(OTEL_SDK_METRIC_READER_COLLECTION_DURATION, 'otel.sdk.metric_reader.collection.duration').


%% The number of log records for which the processing has finished, either successful or failed
-define(OTEL_SDK_PROCESSOR_LOG_PROCESSED, 'otel.sdk.processor.log.processed').


%% The maximum number of log records the queue of a given instance of an SDK Log Record processor can hold
-define(OTEL_SDK_PROCESSOR_LOG_QUEUE_CAPACITY, 'otel.sdk.processor.log.queue.capacity').


%% The number of log records in the queue of a given instance of an SDK log processor
-define(OTEL_SDK_PROCESSOR_LOG_QUEUE_SIZE, 'otel.sdk.processor.log.queue.size').


%% The number of spans for which the processing has finished, either successful or failed
-define(OTEL_SDK_PROCESSOR_SPAN_PROCESSED, 'otel.sdk.processor.span.processed').

%% @deprecated Renamed to `otel.sdk.processor.span.processed`.
%% Deprecated, use `otel.sdk.processor.span.processed` instead.
-define(OTEL_SDK_PROCESSOR_SPAN_PROCESSED_COUNT, 'otel.sdk.processor.span.processed.count').


%% The maximum number of spans the queue of a given instance of an SDK span processor can hold
-define(OTEL_SDK_PROCESSOR_SPAN_QUEUE_CAPACITY, 'otel.sdk.processor.span.queue.capacity').


%% The number of spans in the queue of a given instance of an SDK span processor
-define(OTEL_SDK_PROCESSOR_SPAN_QUEUE_SIZE, 'otel.sdk.processor.span.queue.size').


%% The number of created spans for which the end operation was called
-define(OTEL_SDK_SPAN_ENDED, 'otel.sdk.span.ended').

%% @deprecated Renamed to `otel.sdk.span.ended`.
%% Deprecated, use `otel.sdk.span.ended` instead.
-define(OTEL_SDK_SPAN_ENDED_COUNT, 'otel.sdk.span.ended.count').


%% The number of created spans for which the end operation has not been called yet
-define(OTEL_SDK_SPAN_LIVE, 'otel.sdk.span.live').

%% @deprecated Renamed to `otel.sdk.span.live`.
%% Deprecated, use `otel.sdk.span.live` instead.
-define(OTEL_SDK_SPAN_LIVE_COUNT, 'otel.sdk.span.live.count').
