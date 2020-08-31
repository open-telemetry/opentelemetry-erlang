defmodule OpenTelemetry.Span do
  @moduledoc """
  This module contains macros for Span operations that update the active current Span in the current process.
  An example of creating an Event and adding it to the current Span:

      require OpenTelemetry.Span
      ...
      event = "ecto.query"
      ecto_attributes = OpenTelemetry.event([{"query", query}, {"total_time", total_time}])
      OpenTelemetry.Span.add_event(event, ecto_attributes)
      ...

  A Span represents a single operation within a trace. Spans can be nested to form a trace tree.
  Each trace contains a root span, which typically describes the end-to-end latency and, optionally,
  one or more sub-spans for its sub-operations.

  Spans encapsulate:

  - The span name
  - An immutable SpanContext (`t:OpenTelemetry.span_ctx/0`) that uniquely identifies the Span
  - A parent Span in the form of a Span (`t:OpenTelemetry.span/0`), SpanContext (`t:OpenTelemetry.span_ctx/0`), or `undefined`
  - A start timestamp
  - An end timestamp
  - An ordered mapping of Attributes (`t:OpenTelemetry.attributes/0`)
  - A list of Links to other Spans (`t:OpenTelemetry.link/0`)
  - A list of timestamped Events (`t:OpenTelemetry.event/0`)
  - A Status (`t:OpenTelemetry.status/0`)
  """

  @doc """
  Get the SpanId of a Span.
  """
  @spec span_id(OpenTelemetry.span_ctx()) :: OpenTelemetry.span_id()
  defdelegate span_id(span), to: :ot_span

  @doc """
  Get the TraceId of a Span.
  """
  @spec trace_id(OpenTelemetry.span_ctx()) :: OpenTelemetry.trace_id()
  defdelegate trace_id(span), to: :ot_span

  @doc """
  Get the Tracestate of a Span.
  """
  @spec tracestate(OpenTelemetry.span_ctx()) :: OpenTelemetry.tracestate()
  defdelegate tracestate(span), to: :ot_span

  @doc """
  Set an attribute with key and value on the currently active Span.
  """
  @spec set_attribute(OpenTelemetry.attribute_key(), OpenTelemetry.attribute_value()) :: boolean()
  defmacro set_attribute(key, value) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)

      :ot_span.set_attribute(
        tracer,
        :ot_tracer.current_span_ctx(tracer),
        unquote(key),
        unquote(value)
      )
    end
  end

  @doc """
  Add a list of attributes to the currently active Span.
  """
  @spec set_attributes(OpenTelemetry.attributes()) :: boolean()
  defmacro set_attributes(attributes) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.set_attributes(tracer, :ot_tracer.current_span_ctx(tracer), unquote(attributes))
    end
  end

  @doc """
  Add an event to the currently active Span.
  """
  @spec add_event(String.t(), OpenTelemetry.attributes()) :: boolean()
  defmacro add_event(event, attributes) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)

      :ot_span.add_event(
        tracer,
        :ot_tracer.current_span_ctx(tracer),
        unquote(event),
        unquote(attributes)
      )
    end
  end

  @doc """
  Add a list of events to the currently active Span.
  """
  @spec add_events([OpenTelemetry.event()]) :: boolean()
  defmacro add_events(events) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.add_events(tracer, :ot_tracer.current_span_ctx(tracer), unquote(events))
    end
  end

  @doc """
  Sets the Status of the currently active Span.

  If used, this will override the default Span Status, which is `Ok`.
  """
  @spec set_status(OpenTelemetry.status()) :: boolean()
  defmacro set_status(status) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.set_status(tracer, :ot_tracer.current_span_ctx(tracer), unquote(status))
    end
  end

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
  @spec update_name(String.t()) :: boolean()
  defmacro update_name(name) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.update_name(tracer, :ot_tracer.current_span_ctx(tracer), unquote(name))
    end
  end
end
