defmodule OpenTelemetry.Tracer do
  @moduledoc """
  This module contains macros for Tracer operations around the lifecycle of the Spans within a Trace.

  The Tracer is able to start a new Span as a child of the active Span of the current process, set
  a different Span to be the current Span by passing the Span's context, end a Span or run a code
  block within the context of a newly started span that is ended when the code block completes.

  The macros use the Tracer registered to the Application the module using the macro is included in,
  assuming `OpenTelemetry.register_application_tracer/1` has been called for the Application. If
  not then the default Tracer is used.

      require OpenTelemetry.Tracer

      OpenTelemetry.Tracer.with_span "span-1" do
        ... do something ...
      end
  """

  @type start_opts() :: %{
          optional(:parent) => OpenTelemetry.span() | OpenTelemetry.span_ctx(),
          optional(:attributes) => OpenTelemetry.attributes(),
          # TODO sampler should is an opaque type defined in the implementation
          optional(:sampler) => term(),
          optional(:links) => OpenTelemetry.links(),
          optional(:is_recording) => boolean(),
          optional(:start_time) => :opentelemetry.timestamp(),
          optional(:kind) => OpenTelemetry.span_kind()
        }

  @doc """
  Starts a new span and does not make it the current active span of the current process.

  The current active Span is used as the parent of the created Span unless a `parent` is given in the
  `t:start_opts/0` argument or there is no active Span. If there is neither a current Span or a
  `parent` option given then the Tracer checks for an extracted SpanContext to use as the parent. If
  there is also no extracted context then the created Span is a root Span.
  """
  defmacro start_span(name, opts \\ quote(do: %{})) do
    quote bind_quoted: [name: name, start_opts: opts] do
      :otel_tracer.start_span(:opentelemetry.get_tracer(__MODULE__), name, start_opts)
    end
  end

  @doc """
  Takes a `t:OpenTelemetry.span_ctx/0` and the Tracer sets it to the currently active Span.
  """
  defmacro set_current_span(span_ctx) do
    quote bind_quoted: [span_ctx: span_ctx] do
      :otel_tracer.set_current_span(span_ctx)
    end
  end

  @doc """
  Creates a new span which is set to the currently active Span in the Context of the block.
  The Span is ended automatically when the `block` completes and the Context is what it was
  before the block.

  See `start_span/2` and `end_span/0`.
  """
  defmacro with_span(name, start_opts \\ quote(do: %{}), do: block) do
    quote do
      :otel_tracer.with_span(
        :opentelemetry.get_tracer(__MODULE__),
        unquote(name),
        unquote(start_opts),
        fn _ -> unquote(block) end
      )
    end
  end

  @doc """
  Returns the currently active `t:OpenTelemetry.span_ctx/0`.
  """
  def current_span_ctx() do
    :otel_tracer.current_span_ctx()
  end

  @doc """
  End the Span. Sets the end timestamp for the currently active Span. This has no effect on any
  child Spans that may exist of this Span.

  The Context is unchanged.
  """
  def end_span() do
    :otel_span.end_span(:otel_tracer.current_span_ctx())
  end

  @doc """
  Set an attribute with key and value on the currently active Span.
  """
  @spec set_attribute(OpenTelemetry.attribute_key(), OpenTelemetry.attribute_value()) :: boolean()
  def set_attribute(key, value) do
    :otel_span.set_attribute(
      :otel_tracer.current_span_ctx(),
      key,
      value
    )
  end

  @doc """
  Add a list of attributes to the currently active Span.
  """
  @spec set_attributes(OpenTelemetry.attributes()) :: boolean()
  def set_attributes(attributes) do
    :otel_span.set_attributes(:otel_tracer.current_span_ctx(), attributes)
  end

  @doc """
  Add an event to the currently active Span.
  """
  @spec add_event(OpenTelemetry.event_name(), OpenTelemetry.attributes()) :: boolean()
  def add_event(event, attributes) do
    :otel_span.add_event(
      :otel_tracer.current_span_ctx(),
      event,
      attributes
    )
  end

  @doc """
  Add a list of events to the currently active Span.
  """
  @spec add_events([OpenTelemetry.event()]) :: boolean()
  def add_events(events) do
    :otel_span.add_events(:otel_tracer.current_span_ctx(), events)
  end

  @doc """
  Sets the Status of the currently active Span.

  If used, this will override the default Span Status, which is `Ok`.
  """
  @spec set_status(OpenTelemetry.status()) :: boolean()
  def set_status(status) do
    :otel_span.set_status(:otel_tracer.current_span_ctx(), status)
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
  def update_name(name) do
    :otel_span.update_name(:otel_tracer.current_span_ctx(), name)
  end
end
