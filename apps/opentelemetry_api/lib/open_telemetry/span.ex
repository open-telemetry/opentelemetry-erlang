defmodule OpenTelemetry.Span do
  @moduledoc """
  This module contains macros for Span operations that update the active current Span in the current process.
  An example of creating an Event and adding it to the current Span:

      require OpenTelemetry.Tracer, as: Tracer
      require OpenTelemetry.Span, as: Span

      span_ctx = Tracer.start_span("some-span")
      ...
      Span.add_event(span_ctx, "ecto.query", query: query, total_time: total_time)
      ...
      Span.end_span(span_ctx)

  A Span represents a single operation within a trace. Spans can be nested to form a trace tree.
  Each trace contains a root span, which typically describes the end-to-end latency and, optionally,
  one or more sub-spans for its sub-operations.

  Spans encapsulate:

  - The span name
  - An immutable SpanContext (`t:OpenTelemetry.span_ctx/0`) that uniquely identifies the Span
  - A parent Span in the form of a Span (`t:OpenTelemetry.span/0`), SpanContext (`t:OpenTelemetry.span_ctx/0`), or `undefined`
  - A start timestamp
  - An end timestamp
  - An ordered mapping of Attributes (`t:OpenTelemetry.attributes_map/0`)
  - A list of Links to other Spans (`t:OpenTelemetry.link/0`)
  - A list of timestamped Events (`t:OpenTelemetry.event/0`)
  - A Status (`t:OpenTelemetry.status/0`)
  """

  @type start_opts() :: :otel_span.start_opts()

  @doc """
  Get the SpanId of a Span.
  """
  @spec span_id(OpenTelemetry.span_ctx()) :: OpenTelemetry.span_id()
  defdelegate span_id(span), to: :otel_span

  @doc """
  Get the TraceId of a Span.
  """
  @spec trace_id(OpenTelemetry.span_ctx()) :: OpenTelemetry.trace_id()
  defdelegate trace_id(span), to: :otel_span

  @doc """
  Get the lowercase hex encoded span ID.
  """
  @spec hex_span_id(OpenTelemetry.span_ctx()) :: binary()
  defdelegate hex_span_id(span), to: :otel_span

  @doc """
  Get the lowercase hex encoded trace ID.
  """
  @spec hex_trace_id(OpenTelemetry.span_ctx()) :: binary()
  defdelegate hex_trace_id(span), to: :otel_span

  @doc """
  Get the Tracestate of a Span.
  """
  @spec tracestate(OpenTelemetry.span_ctx()) :: OpenTelemetry.tracestate()
  defdelegate tracestate(span), to: :otel_span

  @doc """
  End the Span. Sets the end timestamp for the currently active Span. This has no effect on any
  child Spans that may exist of this Span.

  The Span Context is returned with `is_recording` set to `false`.
  """
  defdelegate end_span(span_ctx), to: :otel_span

  @doc """
  End the Span. Sets the end timestamp for the currently active Span using the passed in timestamp if valid, current timestamp otherwise. This has no effect on any
  child Spans that may exist of this Span.

  The Span Context is returned with `is_recording` set to `false`.
  """
  defdelegate end_span(span_ctx, timestamp), to: :otel_span

  @doc """

  """
  defdelegate is_recording(span_ctx), to: :otel_span

  @doc """

  """
  defdelegate is_valid(span_ctx), to: :otel_span

  @doc """
  Set an attribute with key and value on the currently active Span.
  """
  @spec set_attribute(
          OpenTelemetry.span_ctx(),
          OpenTelemetry.attribute_key(),
          OpenTelemetry.attribute_value()
        ) :: boolean()
  defdelegate set_attribute(span_ctx, key, value), to: :otel_span

  @doc """
  Add a list of attributes to the currently active Span.
  """
  @spec set_attributes(OpenTelemetry.span_ctx(), OpenTelemetry.attributes_map()) :: boolean()
  defdelegate set_attributes(span_ctx, attributes), to: :otel_span

  @doc """
  Add an event to the currently active Span.
  """
  @spec add_event(
          OpenTelemetry.span_ctx(),
          OpenTelemetry.event_name(),
          OpenTelemetry.attributes_map()
        ) :: boolean()
  defdelegate add_event(span_ctx, event, attributes), to: :otel_span

  @doc """
  Add a list of events to the currently active Span.
  """
  @spec add_events(OpenTelemetry.span_ctx(), [OpenTelemetry.event()]) :: boolean()
  defdelegate add_events(span_ctx, events), to: :otel_span

  defguardp is_exception?(term)
            when is_map(term) and :erlang.is_map_key(:__struct__, term) and
                   is_atom(:erlang.map_get(:__struct__, term)) and
                   :erlang.is_map_key(:__exception__, term) and
                   :erlang.map_get(:__exception__, term) == true

  @doc """
  Record an exception as an event, following the semantics convetions for exceptions.

  If trace is not provided, the stacktrace is retrieved from `Process.info/2`
  """
  @spec record_exception(OpenTelemetry.span_ctx(), Exception.t()) :: boolean()
  def record_exception(span_ctx, exception, trace \\ nil, attributes \\ [])

  def record_exception(span_ctx, exception, trace, attributes) when is_exception?(exception) do
    exception_type = to_string(exception.__struct__)

    exception_attributes = [
      {"exception.type", exception_type},
      {"exception.message", Exception.message(exception)},
      {"exception.stacktrace", Exception.format_stacktrace(trace)}
    ]

    add_event(span_ctx, "exception", exception_attributes ++ attributes)
  end

  def record_exception(_, _, _, _), do: false

  @doc """
  Sets the Status of the currently active Span.

  If used, this will override the default Span Status, which is `:unset`.
  """
  @spec set_status(OpenTelemetry.span_ctx(), OpenTelemetry.status()) :: boolean()
  defdelegate set_status(span_ctx, status), to: :otel_span

  @doc """
  Updates the Span name.

  It is highly discouraged to update the name of a Span after its creation. Span name is
  often used to group, filter and identify the logical groups of spans. And often, filtering
  logic will be implemented before the Span creation for performance reasons. Thus the name
  update may interfere with this logic.

  The function name is called UpdateName to differentiate this function from the regular
  property setter. It emphasizes that this operation signifies a major change for a Span
  and may lead to re-calculation of sampling or filtering decisions made previously
  depending on the implementation.
  """
  @spec update_name(OpenTelemetry.span_ctx(), OpenTelemetry.span_name()) :: boolean()
  defdelegate update_name(span_ctx, name), to: :otel_span
end
