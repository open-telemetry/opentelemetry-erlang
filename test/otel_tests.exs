defmodule OtelTests do
  use ExUnit.Case, async: false

  require OpenTelemetry.Tracer, as: Tracer
  require OpenTelemetry.Span, as: Span
  require OpenTelemetry.Ctx, as: Ctx

  require Record
  @fields Record.extract(:span, from_lib: "opentelemetry/include/otel_span.hrl")
  Record.defrecordp(:span, @fields)

  @fields Record.extract(:tracer, from_lib: "opentelemetry/src/otel_tracer.hrl")
  Record.defrecordp(:tracer, @fields)

  @fields Record.extract(:span_ctx, from_lib: "opentelemetry_api/include/opentelemetry.hrl")
  Record.defrecordp(:span_ctx, @fields)

  setup do
    Application.load(:opentelemetry)

    Application.put_env(:opentelemetry, :processors, [
      {:otel_simple_processor, %{exporter: {:otel_exporter_pid, self()}}}
    ])

    {:ok, _} = Application.ensure_all_started(:opentelemetry)

    on_exit(fn ->
      Application.stop(:opentelemetry)
      Application.unload(:opentelemetry)
    end)
  end

  test "use Tracer to set current active Span's attributes" do
    Tracer.with_span "span-1" do
      Tracer.set_attribute("attr-1", "value-1")
      Tracer.set_attributes([{"attr-2", "value-2"}])
    end

    attributes =
      :otel_attributes.new([{"attr-1", "value-1"}, {"attr-2", "value-2"}], 128, :infinity)

    assert_receive {:span,
                    span(
                      name: "span-1",
                      attributes: ^attributes
                    )}
  end

  test "use Tracer to start a Span as currently active with an explicit parent" do
    s1 = Tracer.start_span("span-1")
    ctx = Tracer.set_current_span(Ctx.new(), s1)

    Tracer.with_span ctx, "span-2", %{} do
      Tracer.set_attribute("attr-1", "value-1")
      Tracer.set_attributes([{"attr-2", "value-2"}])
    end

    span_ctx(span_id: parent_span_id) = Span.end_span(s1)

    attributes = :otel_attributes.new([], 128, :infinity)

    assert_receive {:span,
                    span(
                      name: "span-1",
                      attributes: ^attributes
                    )}

    attributes =
      :otel_attributes.new([{"attr-1", "value-1"}, {"attr-2", "value-2"}], 128, :infinity)

    assert_receive {:span,
                    span(
                      name: "span-2",
                      parent_span_id: ^parent_span_id,
                      attributes: ^attributes
                    )}
  end

  test "use Span to set attributes" do
    s = Tracer.start_span("span-2")
    Span.set_attribute(s, "attr-1", "value-1")
    Span.set_attributes(s, [{"attr-2", "value-2"}])

    assert span_ctx() = Span.end_span(s)

    attributes =
      :otel_attributes.new([{"attr-1", "value-1"}, {"attr-2", "value-2"}], 128, :infinity)

    assert_receive {:span,
                    span(
                      name: "span-2",
                      attributes: ^attributes
                    )}
  end

  test "create child Span in Task" do
    # create the parent span
    parent = Tracer.start_span("parent")
    # make a new context with it as the active span
    ctx = Tracer.set_current_span(Ctx.new(), parent)
    # attach this context (put it in the process dictionary)
    prev_ctx = Ctx.attach(ctx)

    # start the child and set it to current in an unattached context
    child = Tracer.start_span("child")
    ctx = Tracer.set_current_span(ctx, SpanCtx)

    task =
      Task.async(fn ->
        # attach the context with the child span active to this process
        Ctx.attach(ctx)
        Span.end_span(child)
        :hello
      end)

    ret = Task.await(task)
    assert :hello = ret

    span_ctx(span_id: parent_span_id) = Span.end_span(parent)

    assert_receive {:span,
                    span(
                      name: "child",
                      parent_span_id: ^parent_span_id
                    )}

    assert span_ctx() = Span.end_span(parent)
    Ctx.detach(prev_ctx)
    assert :undefined = Tracer.current_span_ctx()

    assert_receive {:span,
                    span(
                      name: "parent",
                      parent_span_id: :undefined
                    )}
  end

  test "create Span with Link to outer Span in Task" do
    parent_ctx = Ctx.new()

    # create the parent span
    parent = Tracer.start_span("parent")
    # make a new context with it as the active span
    ctx = Tracer.set_current_span(parent_ctx, parent)
    # attach this context (put it in the process dictionary)
    prev_ctx = Ctx.attach(ctx)

    task =
      Task.async(fn ->
        # new process has a new context so this span will have no parent
        parent_link = OpenTelemetry.link(parent)
        assert parent_link != :undefined

        Tracer.with_span "child", %{links: [parent_link]} do
          :hello
        end
      end)

    ret = Task.await(task)
    assert :hello = ret

    links = :otel_links.new([OpenTelemetry.link(parent)], 128, 128, :infinity)

    assert_receive {:span,
                    span(
                      name: "child",
                      parent_span_id: :undefined,
                      links: ^links
                    )}

    assert span_ctx() = Span.end_span(parent)
    Ctx.detach(prev_ctx)
    assert :undefined = Tracer.current_span_ctx()

    assert_receive {:span,
                    span(
                      name: "parent",
                      parent_span_id: :undefined
                    )}
  end

  test "use explicit Context for parent of started Span" do
    s1 = Tracer.start_span("span-1")
    ctx = Tracer.set_current_span(Ctx.new(), s1)

    # span-2 will have s1 as the parent since s1 is the current span in `ctx`
    s2 = Tracer.start_span(ctx, "span-2", %{})

    # span-3 will have no parent because it uses the current context
    s3 = Tracer.start_span("span-3")

    Span.set_attribute(s1, "attr-1", "value-1")
    Span.set_attributes(s1, [{"attr-2", "value-2"}])

    span_ctx(span_id: s1_span_id) = Span.end_span(s1)

    assert span_ctx() = Span.end_span(s2)
    assert span_ctx() = Span.end_span(s3)

    attributes =
      :otel_attributes.new([{"attr-1", "value-1"}, {"attr-2", "value-2"}], 128, :infinity)

    assert_receive {:span,
                    span(
                      name: "span-1",
                      parent_span_id: :undefined,
                      attributes: ^attributes
                    )}

    assert_receive {:span,
                    span(
                      name: "span-2",
                      parent_span_id: ^s1_span_id
                    )}

    assert_receive {:span,
                    span(
                      name: "span-3",
                      parent_span_id: :undefined
                    )}
  end

  test "Span.record_exception/4 should return false if passed an invalid exception" do
    Tracer.with_span "span-3" do
      refute OpenTelemetry.Span.record_exception(Tracer.current_span_ctx(), :not_an_exception)
    end
  end

  test "Span.record_exception/4 should add an exception event to the span" do
    s = Tracer.start_span("span-4")

    try do
      raise RuntimeError, "my error message"
    rescue
      ex ->
        assert Span.record_exception(s, ex, __STACKTRACE__)
        assert Span.end_span(s)

        stacktrace = Exception.format_stacktrace(__STACKTRACE__)

        attributes =
          :otel_attributes.new(
            [
              {:"exception.type", "Elixir.RuntimeError"},
              {:"exception.message", "my error message"},
              {:"exception.stacktrace", stacktrace}
            ],
            128,
            :infinity
          )

        assert_receive {:span,
                        span(
                          name: "span-4",
                          events: {
                            :events,
                            128,
                            128,
                            :infinity,
                            0,
                            [
                              {:event, _, :exception, ^attributes}
                            ]
                          }
                        )}
    end
  end

  test "Custom Sampler that checks attributes" do
    name = "span-prob-sampled"
    trace_id = 120_647_249_294_066_572_380_176_333_851_662_846_319

    ctx = :otel_tracer.set_current_span(:otel_ctx.new(), :undefined)

    {sampler, _description, opts} =
      :otel_sampler.new({AttributesSampler, %{"http.target": "/healthcheck"}})

    assert {:drop, [], []} =
             sampler.should_sample(
               ctx,
               trace_id,
               [],
               name,
               :undefined,
               %{"http.target": "/healthcheck", other_attr: :other_attr_value},
               opts
             )

    assert {:record_and_sample, [], []} =
             sampler.should_sample(
               ctx,
               trace_id,
               [],
               name,
               :undefined,
               %{other_attr: :other_attr_value},
               opts
             )

    assert {:record_and_sample, [], []} =
             sampler.should_sample(
               ctx,
               trace_id,
               [],
               name,
               :undefined,
               %{},
               opts
             )
  end
end
