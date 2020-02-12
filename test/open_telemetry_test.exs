defmodule OpenTelemetryTest do
  use ExUnit.Case, async: true

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span

  test "current_span tracks nesting" do
    _ctx1 = OpenTelemetry.Tracer.start_span("span-1")
    ctx2 = OpenTelemetry.Tracer.start_span("span-2")

    assert ctx2 == OpenTelemetry.Tracer.current_span_ctx()
  end

  test "closing a span makes the parent current" do
    ctx1 = OpenTelemetry.Tracer.start_span("span-1")
    ctx2 = OpenTelemetry.Tracer.start_span("span-2")

    assert ctx2 == OpenTelemetry.Tracer.current_span_ctx()
    OpenTelemetry.Tracer.end_span()
    assert ctx1 == OpenTelemetry.Tracer.current_span_ctx()
  end

  test "macro start_span" do
    alias OpenTelemetry.Tracer, as: Tracer
    alias OpenTelemetry.Span, as: Span

    Tracer.with_span "span-1" do
      Tracer.with_span "span-2" do
        Span.set_attribute("attr-1", "value-1")

        event1 = OpenTelemetry.event("event-1", [])
        event2 = OpenTelemetry.event("event-2", [])

        Span.add_events([event1, event2])
      end
    end
  end

end
