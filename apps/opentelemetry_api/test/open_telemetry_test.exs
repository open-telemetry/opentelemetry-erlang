defmodule OpenTelemetryTest do
  use ExUnit.Case, async: true

  require OpenTelemetry.Tracer, as: Tracer
  require OpenTelemetry.Span, as: Span
  require OpenTelemetry.Baggage, as: Baggage
  require OpenTelemetry.Ctx, as: Ctx

  require Record
  Record.defrecordp(:span_ctx, Record.extract(:span_ctx, from_lib: "opentelemetry_api/include/opentelemetry.hrl"))
  Record.defrecordp(:status, Record.extract(:status, from_lib: "opentelemetry_api/include/opentelemetry.hrl"))

  setup_all do
    :otel_tracer_test.set_default()
  end

  test "current_span tracks last set_span" do
    span_ctx1 = Tracer.start_span("span-1")
    assert :undefined == Tracer.current_span_ctx()
    Tracer.set_current_span(span_ctx1)
    span_ctx2 = Tracer.start_span("span-2")
    Tracer.set_current_span(span_ctx2)

    assert span_ctx2 == Tracer.current_span_ctx()
  end

  test "link creation" do
    ctx = span_ctx(trace_id: 1, span_id: 2, tracestate: {:tracestate, []})

    %{trace_id: t, span_id: s, attributes: a, tracestate: ts} = OpenTelemetry.link(ctx)

    assert 1 == t
    assert 2 == s
    assert {:tracestate, []} == ts
    assert %{} == a

    %{trace_id: t, span_id: s, attributes: a, tracestate: ts} =
      OpenTelemetry.link(ctx, [{"attr-1", "value-1"}])

    assert 1 == t
    assert 2 == s
    assert {:tracestate, []} == ts
    assert %{"attr-1" => "value-1"} == a
  end

  test "macro with_span" do
    Tracer.with_span "span-1" do
      Tracer.with_span "span-2" do
        Tracer.set_attribute("attr-1", "value-1")

        event1 = OpenTelemetry.event("event-1", [])
        event2 = OpenTelemetry.event("event-2", [])

        Tracer.add_events([event1, event2])

        error_status = OpenTelemetry.status(:error, "This is an error!")
        assert status(code: :error, message: "This is an error!") = error_status
        unset_status = OpenTelemetry.status(:unset, "This is ignored")
        assert status(code: :unset, message: "") = unset_status

        ok_status = OpenTelemetry.status(:ok, "This is ignored")
        assert status(code: :ok, message: "") = ok_status

        Tracer.set_status(error_status)
        Tracer.set_status(:error, "this is not ok")
      end
    end
  end

  test "macro with_span start_opts (map)" do
    Tracer.with_span "span-1", %{
      attributes: %{"thread.id" => inspect(self())},
      is_recording: true,
      kind: :internal,
      links: [],
      start_time: :opentelemetry.timestamp()
    } do
      :ok
    end
  end

  test "macro with_span start_opts (keyword list)" do
    Tracer.with_span "span-1",
      attributes: %{"thread.id" => inspect(self())},
      is_recording: true,
      kind: :internal,
      links: [],
      start_time: :opentelemetry.timestamp() do
      :ok
    end
  end

  test "can deconstruct a span context" do
    Tracer.with_span "span-1" do
      span = Tracer.current_span_ctx()

      assert nil != Span.trace_id(span)
      assert nil != Span.span_id(span)
      assert {:tracestate, []} = Span.tracestate(span)
    end
  end

  test "hex trace identifiers" do
    Tracer.with_span "span-1" do
      span = Tracer.current_span_ctx()

      assert Span.hex_trace_id(span) =~ ~r/[0-9a-f]{32}/
      assert Span.hex_span_id(span) =~ ~r/[0-9a-f]{16}/
    end
  end

  test "baggage api from elixir" do
    Baggage.set(%{"a" => "b"})
    assert %{"a" => {"b", []}} = Baggage.get_all()

    Baggage.set(%{"a" => "c"})
    assert %{"a" => {"c", []}} = Baggage.get_all()

    Baggage.clear()
    assert 0 = :erlang.map_size(Baggage.get_all())
  end

  test "context api from elixir" do
    ctx = Ctx.new()
    ctx = Ctx.set_value(ctx, :somekey, :somevalue)
    assert :somevalue = Ctx.get_value(ctx, :somekey, "default")

    # attach the context and get value from implicit attached context
    Ctx.attach(ctx)
    assert :somevalue = Ctx.get_value(:somekey, "default")
  end

  test "baggage across contexts" do
    ctx = Ctx.get_current()

    Baggage.set(%{"a" => "b"})
    assert %{"a" => {"b", []}} = Baggage.get_all()

    # attach the empty context
    # gets a token for the context
    token = Ctx.attach(ctx)
    assert 0 = :erlang.map_size(Baggage.get_all())

    # return to the context in the pdict before the attach
    Ctx.detach(token)
    assert %{"a" => {"b", []}} = Baggage.get_all()
  end

  test "Span.record_exception" do
    Tracer.with_span("exceptional span") do
      Tracer.record_exception(%RuntimeError{message: "too awesome"})
    end

    assert_received {:add_event, _span_ctx, :exception, attributes}
    assert %{"exception.type": "RuntimeError", "exception.stacktrace": stacktrace, "exception.message": "too awesome"} = attributes
    assert is_binary(stacktrace)
    assert String.contains?(stacktrace, "\n")
  end
end
