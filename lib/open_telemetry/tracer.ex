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

      OpenTelemetry.Tracer.with_span \"span-1\" do
        ... do something ...
      end
  """

  @type start_opts() :: %{optional(:parent) => OpenTelemetry.span() | OpenTelemetry.span_ctx(),
                          optional(:attributes) => OpenTelemetry.attributes(),
                          optional(:sampler) => :ot_sampler.sampler(),
                          optional(:links) => OpenTelemetry.links(),
                          optional(:is_recording) => boolean(),
                          optional(:start_time) => :opentelemetry.timestamp(),
                          optional(:kind) => OpenTelemetry.span_kind()}

  @doc """
  Starts a new span and makes it the current active span of the current process.

  The current active Span is used as the parent of the created Span unless a `parent` is given in the
  `t:start_opts/0` argument or there is no active Span. If there is neither a current Span or a
  `parent` option given then the Tracer checks for an extracted SpanContext to use as the parent. If
  there is also no extracted context then the created Span is a root Span.
  """
  defmacro start_span(name, opts \\ quote(do: %{})) do
    quote bind_quoted: [name: name, start_opts: opts] do
      :ot_tracer.start_span(:opentelemetry.get_tracer(__MODULE__), name, start_opts)
    end
  end

  @doc """
  Starts a new span but does not make it the current active span of the current process.

  This is particularly useful when creating a child Span that is for a new process. Before spawning
  the new process start an inactive Span, which uses the current context as the parent, then
  pass this new SpanContext as an argument to the spawned function and in that function use
  `set_span/1`.

  The current active Span is used as the parent of the created Span unless a `parent` is given in the
  `t:start_opts/0` argument or there is no active Span. If there is neither a current Span or a
  `parent` option given then the Tracer checks for an extracted SpanContext to use as the parent. If
  there is also no extracted context then the created Span is a root Span.
  """
  defmacro start_inactive_span(name, opts \\ quote(do: %{})) do
    quote bind_quoted: [name: name, start_opts: opts] do
      :ot_tracer.start_inactive_span(:opentelemetry.get_tracer(__MODULE__), name, start_opts)
    end
  end

  @doc """
  Takes a `t:OpenTelemetry.span_ctx/0` and the Tracer sets it to the currently active Span.
  """
  defmacro set_span(span_ctx) do
    quote bind_quoted: [span_ctx: span_ctx] do
      :ot_tracer.set_span(:opentelemetry.get_tracer(__MODULE__), span_ctx)
    end
  end

  @doc """
  End the Span. Sets the end timestamp for the currently active Span. This has no effect on any
  child Spans that may exist of this Span.

  The default Tracer in the OpenTelemetry Erlang/Elixir SDK will then set the parent, if there
  is a local parent of the current Span, to the current active Span.
  """
  defmacro end_span() do
    quote do
      :ot_tracer.end_span(:opentelemetry.get_tracer(__MODULE__))
    end
  end

  @doc """
  Creates a new span which is ended automatically when the `block` completes.

  See `start_span/2` and `end_span/0`.
  """
  defmacro with_span(name, start_opts \\ quote(do: %{}), do: block) do
    quote do
      :ot_tracer.with_span(:opentelemetry.get_tracer(__MODULE__),
            unquote(name),
            unquote(start_opts),
            fn _ -> unquote(block) end)
    end
  end

  @doc """
  Returns the currently active `t:OpenTelemetry.tracer_ctx/0`.
  """
  defmacro current_ctx() do
    quote do
      :ot_tracer.current_ctx(:opentelemetry.get_tracer(__MODULE__))
    end
  end

  @doc """
  Returns the currently active `t:OpenTelemetry.span_ctx/0`.
  """
  defmacro current_span_ctx() do
    quote do
      :ot_tracer.current_span_ctx(:opentelemetry.get_tracer(__MODULE__))
    end
  end
end
