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
  defdelegate span_id(span), to: :otel_span

  @doc """
  Get the TraceId of a Span.
  """
  @spec trace_id(OpenTelemetry.span_ctx()) :: OpenTelemetry.trace_id()
  defdelegate trace_id(span), to: :otel_span

  @doc """
  Get the Tracestate of a Span.
  """
  @spec tracestate(OpenTelemetry.span_ctx()) :: OpenTelemetry.tracestate()
  defdelegate tracestate(span), to: :otel_span
end
