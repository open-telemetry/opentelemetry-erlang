defmodule OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Otel metrics.
  """
  @doc """
  The number of log records for which the export has finished, either successful or failed

  Instrument: `counter`
  Unit: `{log_record}`
  ### Notes

  For successful exports, `error.type` **MUST** **NOT** be set. For failed exports, `error.type` **MUST** contain the failure cause.
  For exporters with partial success semantics (e.g. OTLP with `rejected_log_records`), rejected log records **MUST** count as failed and only non-rejected log records count as success.
  If no rejection reason is available, `rejected` **SHOULD** be used as value for `error.type`.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_exporter_log_exported()
      :"otel.sdk.exporter.log.exported"

  ### Erlang

  ```erlang
  ?OTEL_SDK_EXPORTER_LOG_EXPORTED.
  'otel.sdk.exporter.log.exported'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_exporter_log_exported :: :"otel.sdk.exporter.log.exported"
  def otel_sdk_exporter_log_exported do
    :"otel.sdk.exporter.log.exported"
  end

  @doc """
  The number of log records which were passed to the exporter, but that have not been exported yet (neither successful, nor failed)

  Instrument: `updowncounter`
  Unit: `{log_record}`
  ### Notes

  For successful exports, `error.type` **MUST** **NOT** be set. For failed exports, `error.type` **MUST** contain the failure cause.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_exporter_log_inflight()
      :"otel.sdk.exporter.log.inflight"

  ### Erlang

  ```erlang
  ?OTEL_SDK_EXPORTER_LOG_INFLIGHT.
  'otel.sdk.exporter.log.inflight'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_exporter_log_inflight :: :"otel.sdk.exporter.log.inflight"
  def otel_sdk_exporter_log_inflight do
    :"otel.sdk.exporter.log.inflight"
  end

  @doc """
  The number of metric data points for which the export has finished, either successful or failed

  Instrument: `counter`
  Unit: `{data_point}`
  ### Notes

  For successful exports, `error.type` **MUST** **NOT** be set. For failed exports, `error.type` **MUST** contain the failure cause.
  For exporters with partial success semantics (e.g. OTLP with `rejected_data_points`), rejected data points **MUST** count as failed and only non-rejected data points count as success.
  If no rejection reason is available, `rejected` **SHOULD** be used as value for `error.type`.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_exporter_metric_data_point_exported()
      :"otel.sdk.exporter.metric_data_point.exported"

  ### Erlang

  ```erlang
  ?OTEL_SDK_EXPORTER_METRIC_DATA_POINT_EXPORTED.
  'otel.sdk.exporter.metric_data_point.exported'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_exporter_metric_data_point_exported ::
          :"otel.sdk.exporter.metric_data_point.exported"
  def otel_sdk_exporter_metric_data_point_exported do
    :"otel.sdk.exporter.metric_data_point.exported"
  end

  @doc """
  The number of metric data points which were passed to the exporter, but that have not been exported yet (neither successful, nor failed)

  Instrument: `updowncounter`
  Unit: `{data_point}`
  ### Notes

  For successful exports, `error.type` **MUST** **NOT** be set. For failed exports, `error.type` **MUST** contain the failure cause.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_exporter_metric_data_point_inflight()
      :"otel.sdk.exporter.metric_data_point.inflight"

  ### Erlang

  ```erlang
  ?OTEL_SDK_EXPORTER_METRIC_DATA_POINT_INFLIGHT.
  'otel.sdk.exporter.metric_data_point.inflight'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_exporter_metric_data_point_inflight ::
          :"otel.sdk.exporter.metric_data_point.inflight"
  def otel_sdk_exporter_metric_data_point_inflight do
    :"otel.sdk.exporter.metric_data_point.inflight"
  end

  @doc """
  The duration of exporting a batch of telemetry records.

  Instrument: `histogram`
  Unit: `s`
  ### Notes

  This metric defines successful operations using the full success definitions for [http](https://github.com/open-telemetry/opentelemetry-proto/blob/v1.5.0/docs/specification.md#full-success-1)
  and [grpc](https://github.com/open-telemetry/opentelemetry-proto/blob/v1.5.0/docs/specification.md#full-success). Anything else is defined as an unsuccessful operation. For successful
  operations, `error.type` **MUST** **NOT** be set. For unsuccessful export operations, `error.type` **MUST** contain a relevant failure cause.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_exporter_operation_duration()
      :"otel.sdk.exporter.operation.duration"

  ### Erlang

  ```erlang
  ?OTEL_SDK_EXPORTER_OPERATION_DURATION.
  'otel.sdk.exporter.operation.duration'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_exporter_operation_duration :: :"otel.sdk.exporter.operation.duration"
  def otel_sdk_exporter_operation_duration do
    :"otel.sdk.exporter.operation.duration"
  end

  @doc """
  The number of spans for which the export has finished, either successful or failed

  Instrument: `counter`
  Unit: `{span}`
  ### Notes

  For successful exports, `error.type` **MUST** **NOT** be set. For failed exports, `error.type` **MUST** contain the failure cause.
  For exporters with partial success semantics (e.g. OTLP with `rejected_spans`), rejected spans **MUST** count as failed and only non-rejected spans count as success.
  If no rejection reason is available, `rejected` **SHOULD** be used as value for `error.type`.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_exporter_span_exported()
      :"otel.sdk.exporter.span.exported"

  ### Erlang

  ```erlang
  ?OTEL_SDK_EXPORTER_SPAN_EXPORTED.
  'otel.sdk.exporter.span.exported'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_exporter_span_exported :: :"otel.sdk.exporter.span.exported"
  def otel_sdk_exporter_span_exported do
    :"otel.sdk.exporter.span.exported"
  end

  @deprecated """
  Renamed to `otel.sdk.exporter.span.exported`.
  """

  @spec otel_sdk_exporter_span_exported_count :: :"otel.sdk.exporter.span.exported.count"
  def otel_sdk_exporter_span_exported_count do
    :"otel.sdk.exporter.span.exported.count"
  end

  @doc """
  The number of spans which were passed to the exporter, but that have not been exported yet (neither successful, nor failed)

  Instrument: `updowncounter`
  Unit: `{span}`
  ### Notes

  For successful exports, `error.type` **MUST** **NOT** be set. For failed exports, `error.type` **MUST** contain the failure cause.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_exporter_span_inflight()
      :"otel.sdk.exporter.span.inflight"

  ### Erlang

  ```erlang
  ?OTEL_SDK_EXPORTER_SPAN_INFLIGHT.
  'otel.sdk.exporter.span.inflight'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_exporter_span_inflight :: :"otel.sdk.exporter.span.inflight"
  def otel_sdk_exporter_span_inflight do
    :"otel.sdk.exporter.span.inflight"
  end

  @deprecated """
  Renamed to `otel.sdk.exporter.span.inflight`.
  """

  @spec otel_sdk_exporter_span_inflight_count :: :"otel.sdk.exporter.span.inflight.count"
  def otel_sdk_exporter_span_inflight_count do
    :"otel.sdk.exporter.span.inflight.count"
  end

  @doc """
  The number of logs submitted to enabled SDK Loggers

  Instrument: `counter`
  Unit: `{log_record}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_log_created()
      :"otel.sdk.log.created"

  ### Erlang

  ```erlang
  ?OTEL_SDK_LOG_CREATED.
  'otel.sdk.log.created'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_log_created :: :"otel.sdk.log.created"
  def otel_sdk_log_created do
    :"otel.sdk.log.created"
  end

  @doc """
  The duration of the collect operation of the metric reader.

  Instrument: `histogram`
  Unit: `s`
  ### Notes

  For successful collections, `error.type` **MUST** **NOT** be set. For failed collections, `error.type` **SHOULD** contain the failure cause.
  It can happen that metrics collection is successful for some MetricProducers, while others fail. In that case `error.type` **SHOULD** be set to any of the failure causes.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_metric_reader_collection_duration()
      :"otel.sdk.metric_reader.collection.duration"

  ### Erlang

  ```erlang
  ?OTEL_SDK_METRIC_READER_COLLECTION_DURATION.
  'otel.sdk.metric_reader.collection.duration'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_metric_reader_collection_duration ::
          :"otel.sdk.metric_reader.collection.duration"
  def otel_sdk_metric_reader_collection_duration do
    :"otel.sdk.metric_reader.collection.duration"
  end

  @doc """
  The number of log records for which the processing has finished, either successful or failed

  Instrument: `counter`
  Unit: `{log_record}`
  ### Notes

  For successful processing, `error.type` **MUST** **NOT** be set. For failed processing, `error.type` **MUST** contain the failure cause.
  For the SDK Simple and Batching Log Record Processor a log record is considered to be processed already when it has been submitted to the exporter,
  not when the corresponding export call has finished.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_processor_log_processed()
      :"otel.sdk.processor.log.processed"

  ### Erlang

  ```erlang
  ?OTEL_SDK_PROCESSOR_LOG_PROCESSED.
  'otel.sdk.processor.log.processed'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_processor_log_processed :: :"otel.sdk.processor.log.processed"
  def otel_sdk_processor_log_processed do
    :"otel.sdk.processor.log.processed"
  end

  @doc """
  The maximum number of log records the queue of a given instance of an SDK Log Record processor can hold

  Instrument: `updowncounter`
  Unit: `{log_record}`
  ### Notes

  Only applies to Log Record processors which use a queue, e.g. the SDK Batching Log Record Processor.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_processor_log_queue_capacity()
      :"otel.sdk.processor.log.queue.capacity"

  ### Erlang

  ```erlang
  ?OTEL_SDK_PROCESSOR_LOG_QUEUE_CAPACITY.
  'otel.sdk.processor.log.queue.capacity'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_processor_log_queue_capacity :: :"otel.sdk.processor.log.queue.capacity"
  def otel_sdk_processor_log_queue_capacity do
    :"otel.sdk.processor.log.queue.capacity"
  end

  @doc """
  The number of log records in the queue of a given instance of an SDK log processor

  Instrument: `updowncounter`
  Unit: `{log_record}`
  ### Notes

  Only applies to log record processors which use a queue, e.g. the SDK Batching Log Record Processor.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_processor_log_queue_size()
      :"otel.sdk.processor.log.queue.size"

  ### Erlang

  ```erlang
  ?OTEL_SDK_PROCESSOR_LOG_QUEUE_SIZE.
  'otel.sdk.processor.log.queue.size'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_processor_log_queue_size :: :"otel.sdk.processor.log.queue.size"
  def otel_sdk_processor_log_queue_size do
    :"otel.sdk.processor.log.queue.size"
  end

  @doc """
  The number of spans for which the processing has finished, either successful or failed

  Instrument: `counter`
  Unit: `{span}`
  ### Notes

  For successful processing, `error.type` **MUST** **NOT** be set. For failed processing, `error.type` **MUST** contain the failure cause.
  For the SDK Simple and Batching Span Processor a span is considered to be processed already when it has been submitted to the exporter, not when the corresponding export call has finished.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_processor_span_processed()
      :"otel.sdk.processor.span.processed"

  ### Erlang

  ```erlang
  ?OTEL_SDK_PROCESSOR_SPAN_PROCESSED.
  'otel.sdk.processor.span.processed'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_processor_span_processed :: :"otel.sdk.processor.span.processed"
  def otel_sdk_processor_span_processed do
    :"otel.sdk.processor.span.processed"
  end

  @deprecated """
  Renamed to `otel.sdk.processor.span.processed`.
  """

  @spec otel_sdk_processor_span_processed_count :: :"otel.sdk.processor.span.processed.count"
  def otel_sdk_processor_span_processed_count do
    :"otel.sdk.processor.span.processed.count"
  end

  @doc """
  The maximum number of spans the queue of a given instance of an SDK span processor can hold

  Instrument: `updowncounter`
  Unit: `{span}`
  ### Notes

  Only applies to span processors which use a queue, e.g. the SDK Batching Span Processor.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_processor_span_queue_capacity()
      :"otel.sdk.processor.span.queue.capacity"

  ### Erlang

  ```erlang
  ?OTEL_SDK_PROCESSOR_SPAN_QUEUE_CAPACITY.
  'otel.sdk.processor.span.queue.capacity'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_processor_span_queue_capacity :: :"otel.sdk.processor.span.queue.capacity"
  def otel_sdk_processor_span_queue_capacity do
    :"otel.sdk.processor.span.queue.capacity"
  end

  @doc """
  The number of spans in the queue of a given instance of an SDK span processor

  Instrument: `updowncounter`
  Unit: `{span}`
  ### Notes

  Only applies to span processors which use a queue, e.g. the SDK Batching Span Processor.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_processor_span_queue_size()
      :"otel.sdk.processor.span.queue.size"

  ### Erlang

  ```erlang
  ?OTEL_SDK_PROCESSOR_SPAN_QUEUE_SIZE.
  'otel.sdk.processor.span.queue.size'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_processor_span_queue_size :: :"otel.sdk.processor.span.queue.size"
  def otel_sdk_processor_span_queue_size do
    :"otel.sdk.processor.span.queue.size"
  end

  @doc """
  The number of created spans for which the end operation was called

  Instrument: `counter`
  Unit: `{span}`
  ### Notes

  For spans with `recording=true`: Implementations **MUST** record both `otel.sdk.span.live` and `otel.sdk.span.ended`.
  For spans with `recording=false`: If implementations decide to record this metric, they **MUST** also record `otel.sdk.span.live`.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_span_ended()
      :"otel.sdk.span.ended"

  ### Erlang

  ```erlang
  ?OTEL_SDK_SPAN_ENDED.
  'otel.sdk.span.ended'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_span_ended :: :"otel.sdk.span.ended"
  def otel_sdk_span_ended do
    :"otel.sdk.span.ended"
  end

  @deprecated """
  Renamed to `otel.sdk.span.ended`.
  """

  @spec otel_sdk_span_ended_count :: :"otel.sdk.span.ended.count"
  def otel_sdk_span_ended_count do
    :"otel.sdk.span.ended.count"
  end

  @doc """
  The number of created spans for which the end operation has not been called yet

  Instrument: `updowncounter`
  Unit: `{span}`
  ### Notes

  For spans with `recording=true`: Implementations **MUST** record both `otel.sdk.span.live` and `otel.sdk.span.ended`.
  For spans with `recording=false`: If implementations decide to record this metric, they **MUST** also record `otel.sdk.span.ended`.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OtelMetrics.otel_sdk_span_live()
      :"otel.sdk.span.live"

  ### Erlang

  ```erlang
  ?OTEL_SDK_SPAN_LIVE.
  'otel.sdk.span.live'
  ```

  <!-- tabs-close -->
  """

  @spec otel_sdk_span_live :: :"otel.sdk.span.live"
  def otel_sdk_span_live do
    :"otel.sdk.span.live"
  end

  @deprecated """
  Renamed to `otel.sdk.span.live`.
  """

  @spec otel_sdk_span_live_count :: :"otel.sdk.span.live.count"
  def otel_sdk_span_live_count do
    :"otel.sdk.span.live.count"
  end
end
