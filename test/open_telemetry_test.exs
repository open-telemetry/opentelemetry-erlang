defmodule OpenTelemetryTest do
  use ExUnit.Case, async: true

  require OpenTelemetry.Tracer, as: Tracer
  require OpenTelemetry.Span, as: Span

  test "current_span tracks nesting" do
    _ctx1 = Tracer.start_span("span-1")
    ctx2 = Tracer.start_span("span-2")

    assert ctx2 == Tracer.current_span_ctx()
  end

  test "closing a span makes the parent current" do
    ctx1 = Tracer.start_span("span-1")
    ctx2 = Tracer.start_span("span-2")

    assert ctx2 == Tracer.current_span_ctx()
    OpenTelemetry.Tracer.end_span()
    assert ctx1 == Tracer.current_span_ctx()
  end

  test "macro start_span" do
    Tracer.with_span "span-1" do
      Tracer.with_span "span-2" do
        Span.set_attribute("attr-1", "value-1")

        event1 = OpenTelemetry.event("event-1", [])
        event2 = OpenTelemetry.event("event-2", [])

        Span.add_events([event1, event2])
      end
    end
  end

  test "can deconstruct a span context" do
    Tracer.with_span "span-1" do
      span = Tracer.current_span_ctx()

      assert nil != Span.trace_id(span)
      assert nil != Span.span_id(span)
      assert []   = Span.tracestate(span)
    end
  end
end
