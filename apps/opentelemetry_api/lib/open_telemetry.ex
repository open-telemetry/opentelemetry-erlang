defmodule OpenTelemetry do
  @moduledoc """
  An [OpenTelemetry](https://opentelemetry.io) Trace consists of 1 or more Spans that either have a
  parent/child relationship or are linked together through a Link. Each Span has a TraceId (`t:trace_id/0`),
  SpanId (`t:span_id/0`), and a start and end time in nanoseconds.

  This module provides declaration of the types used throughout the library, as well as functions for
  building the additional pieces of a span that are optional. Each item can be attached to individual
  Span using the functions in `OpenTelemetry.Span` module.

  ## Example

      require OpenTelemetry.Tracer
      require OpenTelemetry.Span

      OpenTelemetry.register_application_tracer(:this_otp_app)

      Tracer.start_span("some-span")
      ...
      event = "ecto.query"
      ecto_attributes = OpenTelemetry.event([{"query", query}, {"total_time", total_time}])
      OpenTelemetry.Span.add_event(event, ecto_event)
      ...
      Tracer.end_span()
  """

  @typedoc """
  A SpanContext represents the portion of a Span needed to do operations on a
  Span. Within a process it acts as a key for looking up and modifying the
  actual Span. It is also what is serialized and propagated across process
  boundaries.
  """
  @type span_ctx() :: :opentelemetry.span_ctx()

  @typedoc """
  TracerContext refers to the data kept in process by the tracer to track
  the current SpanContext and the parent.
  """
  @type tracer_ctx() :: :opentelemetry.tracer_ctx()

  @typedoc """
  Span represents a single operation within a trace. Spans can be
  nested to form a trace tree. Spans may also be linked to other spans
  from the same or different trace and form graphs. Often, a trace
  contains a root span that describes the end-to-end latency, and one
  or more subspans for its sub-operations. A trace can also contain
  multiple root spans, or none at all. Spans do not need to be
  contiguous - there may be gaps or overlaps between spans in a trace.
  """
  @type span() :: :opentelemetry.span()

  @typedoc """
  TraceId is a unique identifier for a trace. All spans from the same trace share
  the same `trace_id`. The ID is a 16-byte array. An ID with all zeroes
  is considered invalid.
  """
  @type trace_id() :: non_neg_integer()

  @typedoc """
  SpanId is a unique identifier for a span within a trace, assigned when the span
  is created. The ID is an 8-byte array. An ID with all zeroes is considered
  invalid.
  """
  @type span_id() :: non_neg_integer()

  @type attribute_key() :: String.t()
  @type attribute_value() :: String.t() | integer() | float() | boolean()

  @typedoc """
  Attributes are a collection of key/value pairs. The value can be a string,
  an integer, a double or the boolean values `true` or `false`. Note, global attributes
  like server name can be set using the resource API.

  Examples of attributes:

      [{"/http/user_agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36"}
       {"/http/server_latency", 300}
       {"abc.com/myattribute", True}
       {"abc.com/score", 10.239}]
  """
  @type attributes() :: [{attribute_key(), attribute_value()}]

  @typedoc """
  Tracestate represents tracing-system specific context in a list of key-value pairs.
  Tracestate allows different vendors propagate additional information and
  inter-operate with their legacy Id formats.

  It is a tracestate in the [w3c-trace-context format](https://www.w3.org/TR/trace-context/#tracestate-header).
  See also [https://github.com/w3c/distributed-tracing](https://github.com/w3c/distributed-tracing)
  for more details about this field.
  """
  @type tracestate() :: [{String.t(), String.t()}]

  @typedoc """
  A Link is a pointer from the current span to another span in the same trace or in a
  different trace. For example, this can be used in batching operations,
  where a single batch handler processes multiple requests from different
  traces or when the handler receives a request from a different project.
  """
  @type link() :: :opentelemetry.link()

  @typedoc """
  An Event is a time-stamped annotation of the span, consisting of user-supplied
  text description and key-value pairs.
  """
  @type event() :: :opentelemetry.event()

  @typedoc """
  An optional final status for this span. Semantically when Status
  wasn't set it means span ended without errors and assume `Ok`.
  """
  @type status() :: :opentelemetry.status()

  @doc """
  Registering a [Named Tracer](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#obtaining-a-tracer) with the name of an OTP Application enables each module in
  the Application to be mapped to the Named Tracer, named for the Application and using the
  version of the currently loaded Application by that name.

  Macros in `OpenTelemetry.Tracer` use the name of the module they are being used in in order
  to lookup the Named Tracer registered for that module and using it for trace operations.
  """
  @spec register_application_tracer(atom()) :: boolean()
  defdelegate register_application_tracer(otp_app), to: :opentelemetry

  @spec register_application_meter(atom()) :: boolean()
  defdelegate register_application_meter(name), to: :opentelemetry

  # Helpers to build OpenTelemetry structured types

  @doc """
  A monotonically increasing time provided by the Erlang runtime system in the native time unit.
  This value is the most accurate and precise timestamp available from the Erlang runtime and
  should be used for finding durations or any timestamp that can be converted to a system
  time before being sent to another system.

  Use `convert_timestamp/2` or `timestamp_to_nano/1` to convert a native monotonic time to a
  system time of either nanoseconds or another unit.

  Using these functions allows timestamps to be accurate, used for duration and be exportable
  as POSIX time when needed.
  """
  @spec timestamp() :: integer()
  defdelegate timestamp(), to: :opentelemetry

  @doc """
  Convert a native monotonic timestamp to nanosecond POSIX time. Meaning the time since Epoch.
  Epoch is defined to be 00:00:00 UTC, 1970-01-01.
  """
  @spec timestamp_to_nano(integer()) :: integer()
  defdelegate timestamp_to_nano(timestamp), to: :opentelemetry

  @doc """
  Convert a native monotonic timestamp to POSIX time of any `:erlang.time_unit/0`.
  Meaning the time since Epoch. Epoch is defined to be 00:00:00 UTC, 1970-01-01.
  """
  @spec convert_timestamp(integer(), :erlang.time_unit()) :: integer()
  defdelegate convert_timestamp(timestamp, unit), to: :opentelemetry

  # span item functions

  @doc """
  Creates a `t:link/0`.
  """
  @spec link(trace_id(), span_id(), attributes(), tracestate()) :: link()
  defdelegate link(trace_id, span_id, attributes, tracestate), to: :opentelemetry

  @doc """
  Creates a `t:link/0` from a `t:span_ctx/0`.
  """
  @spec link(span_ctx() | :undefined) :: link()
  defdelegate link(span_ctx), to: :opentelemetry

  @doc """
  Creates a `t:link/0` from a `t:span_ctx/0` and list of `t:attributes/0`.
  """
  @spec link(span_ctx() | :undefined, attributes()) :: link()
  defdelegate link(span_ctx, attributes), to: :opentelemetry

  @doc """
  Creates a list of `t:link/0` from a list of 4-tuples.
  """
  @spec links([
          {integer(), integer(), attributes(), tracestate()}
          | span_ctx()
          | {span_ctx(), attributes()}
        ]) :: [link()]
  defdelegate links(link_list), to: :opentelemetry

  @doc """
  Creates a `t:event/0`.
  """
  @spec event(String.t(), attributes()) :: event()
  defdelegate event(name, attributes), to: :opentelemetry

  @doc """
  Creates a `t:event/0`.
  """
  @spec event(integer(), String.t(), attributes()) :: event()
  defdelegate event(timestamp, name, attributes), to: :opentelemetry

  @doc """
  Creates a list of `t:event/0` items.
  """
  @spec events(list()) :: [event()]
  defdelegate events(event_list), to: :opentelemetry

  @doc """
  Creates a Status.
  """
  @spec status(atom(), String.t()) :: status()
  defdelegate status(code, message), to: :opentelemetry
end
