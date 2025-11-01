defmodule OpenTelemetry.SemConv.Incubating.OtelAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Otel attributes.
  """
  defdelegate otel_scope_name(), to: OpenTelemetry.SemConv.OtelAttributes

  defdelegate otel_scope_version(), to: OpenTelemetry.SemConv.OtelAttributes

  defdelegate otel_status_code(), to: OpenTelemetry.SemConv.OtelAttributes

  defdelegate otel_status_code_values(), to: OpenTelemetry.SemConv.OtelAttributes

  defdelegate otel_status_description(), to: OpenTelemetry.SemConv.OtelAttributes

  @doc """
  A name uniquely identifying the instance of the OpenTelemetry component within its containing SDK instance.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Implementations **SHOULD** ensure a low cardinality for this attribute, even across application or SDK restarts.
  E.g. implementations **MUST** **NOT** use UUIDs as values for this attribute.

  Implementations **MAY** achieve these goals by following a `<otel.component.type>/<instance-counter>` pattern, e.g. `batching_span_processor/0`.
  Hereby `otel.component.type` refers to the corresponding attribute value of the component.

  The value of `instance-counter` **MAY** be automatically assigned by the component and uniqueness within the enclosing SDK instance **MUST** be guaranteed.
  For example, `<instance-counter>` **MAY** be implemented by using a monotonically increasing counter (starting with `0`), which is incremented every time an
  instance of the given component type is started.

  With this implementation, for example the first Batching Span Processor would have `batching_span_processor/0`
  as `otel.component.name`, the second one `batching_span_processor/1` and so on.
  These values will therefore be reused in the case of an application restart.

  ### Examples

  ```
  ["otlp_grpc_span_exporter/0", "custom-name"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OtelAttributes.otel_component_name()
      :"otel.component.name"

  ### Erlang

  ```erlang
  ?OTEL_COMPONENT_NAME.
  'otel.component.name'
  ```

  <!-- tabs-close -->
  """
  @spec otel_component_name :: :"otel.component.name"
  def otel_component_name do
    :"otel.component.name"
  end

  @typedoc """
  A name identifying the type of the OpenTelemetry component.


  ### Enum Values
  * `:batching_span_processor` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The builtin SDK batching span processor

  * `:simple_span_processor` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The builtin SDK simple span processor

  * `:batching_log_processor` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The builtin SDK batching log record processor

  * `:simple_log_processor` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The builtin SDK simple log record processor

  * `:otlp_grpc_span_exporter` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OTLP span exporter over gRPC with protobuf serialization

  * `:otlp_http_span_exporter` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OTLP span exporter over HTTP with protobuf serialization

  * `:otlp_http_json_span_exporter` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OTLP span exporter over HTTP with JSON serialization

  * `:otlp_grpc_log_exporter` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OTLP log record exporter over gRPC with protobuf serialization

  * `:otlp_http_log_exporter` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OTLP log record exporter over HTTP with protobuf serialization

  * `:otlp_http_json_log_exporter` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OTLP log record exporter over HTTP with JSON serialization

  * `:periodic_metric_reader` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The builtin SDK periodically exporting metric reader

  * `:otlp_grpc_metric_exporter` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OTLP metric exporter over gRPC with protobuf serialization

  * `:otlp_http_metric_exporter` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OTLP metric exporter over HTTP with protobuf serialization

  * `:otlp_http_json_metric_exporter` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OTLP metric exporter over HTTP with JSON serialization

  """
  @type otel_component_type_values() :: %{
          :batching_span_processor => :batching_span_processor,
          :simple_span_processor => :simple_span_processor,
          :batching_log_processor => :batching_log_processor,
          :simple_log_processor => :simple_log_processor,
          :otlp_grpc_span_exporter => :otlp_grpc_span_exporter,
          :otlp_http_span_exporter => :otlp_http_span_exporter,
          :otlp_http_json_span_exporter => :otlp_http_json_span_exporter,
          :otlp_grpc_log_exporter => :otlp_grpc_log_exporter,
          :otlp_http_log_exporter => :otlp_http_log_exporter,
          :otlp_http_json_log_exporter => :otlp_http_json_log_exporter,
          :periodic_metric_reader => :periodic_metric_reader,
          :otlp_grpc_metric_exporter => :otlp_grpc_metric_exporter,
          :otlp_http_metric_exporter => :otlp_http_metric_exporter,
          :otlp_http_json_metric_exporter => :otlp_http_json_metric_exporter
        }
  @doc """
  A name identifying the type of the OpenTelemetry component.


  ### Notes

  If none of the standardized values apply, implementations **SHOULD** use the language-defined name of the type.
  E.g. for Java the fully qualified classname **SHOULD** be used in this case.

  ### Examples

  ```
  ["batching_span_processor", "com.example.MySpanExporter"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OtelAttributes.otel_component_type()
      :"otel.component.type"

      iex> OpenTelemetry.SemConv.Incubating.OtelAttributes.otel_component_type_values().batching_span_processor
      :batching_span_processor

      iex> %{OpenTelemetry.SemConv.Incubating.OtelAttributes.otel_component_type() => OpenTelemetry.SemConv.Incubating.OtelAttributes.otel_component_type_values().batching_span_processor}
      %{:"otel.component.type" => :batching_span_processor}

  ### Erlang

  ```erlang
  ?OTEL_COMPONENT_TYPE.
  'otel.component.type'

  ?OTEL_COMPONENT_TYPE_VALUES_BATCHING_SPAN_PROCESSOR.
  'batching_span_processor'

  \#{?OTEL_COMPONENT_TYPE => ?OTEL_COMPONENT_TYPE_VALUES_BATCHING_SPAN_PROCESSOR}.
  \#{'otel.component.type' => 'batching_span_processor'}
  ```

  <!-- tabs-close -->
  """
  @spec otel_component_type :: :"otel.component.type"
  def otel_component_type do
    :"otel.component.type"
  end

  @spec otel_component_type_values() :: otel_component_type_values()
  def otel_component_type_values() do
    %{
      :batching_span_processor => :batching_span_processor,
      :simple_span_processor => :simple_span_processor,
      :batching_log_processor => :batching_log_processor,
      :simple_log_processor => :simple_log_processor,
      :otlp_grpc_span_exporter => :otlp_grpc_span_exporter,
      :otlp_http_span_exporter => :otlp_http_span_exporter,
      :otlp_http_json_span_exporter => :otlp_http_json_span_exporter,
      :otlp_grpc_log_exporter => :otlp_grpc_log_exporter,
      :otlp_http_log_exporter => :otlp_http_log_exporter,
      :otlp_http_json_log_exporter => :otlp_http_json_log_exporter,
      :periodic_metric_reader => :periodic_metric_reader,
      :otlp_grpc_metric_exporter => :otlp_grpc_metric_exporter,
      :otlp_http_metric_exporter => :otlp_http_metric_exporter,
      :otlp_http_json_metric_exporter => :otlp_http_json_metric_exporter
    }
  end

  @deprecated """
  Use the `otel.scope.name` attribute.
  """
  @spec otel_library_name :: :"otel.library.name"
  def otel_library_name do
    :"otel.library.name"
  end

  @deprecated """
  Use the `otel.scope.version` attribute.
  """
  @spec otel_library_version :: :"otel.library.version"
  def otel_library_version do
    :"otel.library.version"
  end

  @typedoc """
  The result value of the sampler for this span

  ### Enum Values
  * `:drop` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The span is not sampled and not recording
  * `:record_only` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The span is not sampled, but recording
  * `:record_and_sample` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The span is sampled and recording
  """
  @type otel_span_sampling_result_values() :: %{
          :drop => :DROP,
          :record_only => :RECORD_ONLY,
          :record_and_sample => :RECORD_AND_SAMPLE
        }
  @doc """
  The result value of the sampler for this span


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OtelAttributes.otel_span_sampling_result()
      :"otel.span.sampling_result"

      iex> OpenTelemetry.SemConv.Incubating.OtelAttributes.otel_span_sampling_result_values().drop
      :DROP

      iex> %{OpenTelemetry.SemConv.Incubating.OtelAttributes.otel_span_sampling_result() => OpenTelemetry.SemConv.Incubating.OtelAttributes.otel_span_sampling_result_values().drop}
      %{:"otel.span.sampling_result" => :DROP}

  ### Erlang

  ```erlang
  ?OTEL_SPAN_SAMPLING_RESULT.
  'otel.span.sampling_result'

  ?OTEL_SPAN_SAMPLING_RESULT_VALUES_DROP.
  'DROP'

  \#{?OTEL_SPAN_SAMPLING_RESULT => ?OTEL_SPAN_SAMPLING_RESULT_VALUES_DROP}.
  \#{'otel.span.sampling_result' => 'DROP'}
  ```

  <!-- tabs-close -->
  """
  @spec otel_span_sampling_result :: :"otel.span.sampling_result"
  def otel_span_sampling_result do
    :"otel.span.sampling_result"
  end

  @spec otel_span_sampling_result_values() :: otel_span_sampling_result_values()
  def otel_span_sampling_result_values() do
    %{
      :drop => :DROP,
      :record_only => :RECORD_ONLY,
      :record_and_sample => :RECORD_AND_SAMPLE
    }
  end
end
