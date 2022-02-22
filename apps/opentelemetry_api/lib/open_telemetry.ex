defmodule OpenTelemetry do
  @moduledoc """
  An [OpenTelemetry](https://opentelemetry.io) Trace consists of 1 or more Spans that either have a
  parent/child relationship or are linked together through a Link. Each Span has a TraceId (`t:trace_id/0`),
  SpanId (`t:span_id/0`), and a start and end time in nanoseconds.

  This module provides declaration of the types used throughout the library, as well as functions for
  building the additional pieces of a span that are optional. Each item can be attached to individual
  Span using the functions in `OpenTelemetry.Span` module.

  ## Example

      require OpenTelemetry.Tracer, as: Tracer

      Tracer.with_span "some-span" do
        event = OpenTelemetry.event("ecto.query", query: query, total_time: total_time)
        Tracer.add_events([event])
      end
  """

  @typedoc """
  A SpanContext represents the portion of a Span needed to do operations on a
  Span. Within a process it acts as a key for looking up and modifying the
  actual Span. It is also what is serialized and propagated across process
  boundaries.
  """
  @type span_ctx() :: :opentelemetry.span_ctx()

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
  @type span_kind() :: :opentelemetry.span_kind()
  @type span_name() :: :opentelemetry.span_name()

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

  @type attribute_key() :: :opentelemetry.attribute_key()
  @type attribute_value() :: :opentelemetry.attribute_value()

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
  @type attributes_map() :: :opentelemetry.attributes_map()

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
  @type event_name() :: :opentelemetry.event_name()

  @typedoc """
  An optional final status for this span. Semantically when Status
  wasn't set it means span ended without errors and assume `unset`.

  Application developers may set the status as `ok` when the operation
  has been validated to have completed successfully.
  """
  @type status() :: :opentelemetry.status()

  @type status_code() :: :opentelemetry.status_code()

  defdelegate get_tracer(name), to: :opentelemetry
  defdelegate get_tracer(name, vsn, schema_url), to: :opentelemetry
  defdelegate set_default_tracer(t), to: :opentelemetry

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
  Convert a native monotonic timestamp to POSIX time of any `t::erlang.time_unit/0`.
  Meaning the time since Epoch. Epoch is defined to be 00:00:00 UTC, 1970-01-01.
  """
  @spec convert_timestamp(integer(), :erlang.time_unit()) :: integer()
  defdelegate convert_timestamp(timestamp, unit), to: :opentelemetry

  # span item functions

  @doc """
  Creates a `t:link/0`.
  """
  @spec link(trace_id(), span_id(), attributes_map(), tracestate()) :: link()
  defdelegate link(trace_id, span_id, attributes, tracestate), to: :opentelemetry

  @doc """
  Creates a `t:link/0` from a `t:span_ctx/0`.
  """
  @spec link(span_ctx() | :undefined) :: link()
  defdelegate link(span_ctx), to: :opentelemetry

  @doc """
  Creates a `t:link/0` from a `t:span_ctx/0` and list of `t:attributes_map/0`.
  """
  @spec link(span_ctx() | :undefined, attributes_map()) :: link()
  defdelegate link(span_ctx, attributes), to: :opentelemetry

  @doc """
  Creates a list of `t:link/0` from a list of 4-tuples.
  """
  @spec links([
          {integer(), integer(), attributes_map(), tracestate()}
          | span_ctx()
          | {span_ctx(), attributes_map()}
        ]) :: [link()]
  defdelegate links(link_list), to: :opentelemetry

  @doc """
  Creates a `t:event/0`.
  """
  @spec event(event_name(), attributes_map()) :: event()
  defdelegate event(name, attributes), to: :opentelemetry

  @doc """
  Creates a `t:event/0`.
  """
  @spec event(integer(), event_name(), attributes_map()) :: event()
  defdelegate event(timestamp, name, attributes), to: :opentelemetry

  @doc """
  Creates a list of `t:event/0` items.
  """
  @spec events(list()) :: [event()]
  defdelegate events(event_list), to: :opentelemetry

  @doc """
  Creates a Status with an empty description.
  """
  @spec status(:opentelemetry.status_code()) :: status()
  defdelegate status(code), to: :opentelemetry

  @doc """
  Creates a Status.
  """
  @spec status(:opentelemetry.status_code(), String.t()) :: status()
  defdelegate status(code, message), to: :opentelemetry
end
