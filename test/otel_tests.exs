defmodule OtelTests do
  use ExUnit.Case, async: true

  require OpenTelemetry.Tracer, as: Tracer
  require OpenTelemetry.Span, as: Span

  require Record
  @fields Record.extract(:span, from_lib: "opentelemetry/include/otel_span.hrl")
  Record.defrecordp(:span, @fields)
  @fields Record.extract(:span_ctx, from_lib: "opentelemetry_api/include/opentelemetry.hrl")
  Record.defrecordp(:span_ctx, @fields)

  test "use Tracer to set attributes" do
    :otel_batch_processor.set_exporter(:otel_exporter_pid, self())
    OpenTelemetry.register_tracer(:test_tracer, "0.1.0")

    Tracer.with_span "span-1" do
      Tracer.set_attribute("attr-1", "value-1")
      Tracer.set_attributes([{"attr-2", "value-2"}])
    end

    assert_receive {:span, span(name: "span-1", attributes: [{"attr-1", "value-1"},
                                                             {"attr-2", "value-2"}])}
  end

  test "use Span to set attributes" do
    :otel_batch_processor.set_exporter(:otel_exporter_pid, self())

    s = Tracer.start_span("span-2")
    Span.set_attribute(s, "attr-1", "value-1")
    Span.set_attributes(s, [{"attr-2", "value-2"}])

    assert span_ctx() = Span.end_span(s)

    assert_receive {:span, span(name: "span-2", attributes: [{"attr-1", "value-1"},
                                                             {"attr-2", "value-2"}])}
  end

end
