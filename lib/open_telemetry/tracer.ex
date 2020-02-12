defmodule OpenTelemetry.Tracer do
  @moduledoc """

      require OpenTelemetry.Tracer

      OpenTelemetry.Tracer.with_span "span-1" do
        ... do something ...
      end

  """

  defmacro start_span(name,  opts \\ quote(do: %{})) do
    quote do
      :ot_tracer.start_span(:opentelemetry.get_tracer(__MODULE__), unquote(name), unquote(opts))
    end
  end

  defmacro set_span(span_ctx) do
    quote do
      :ot_tracer.set_span(:opentelemetry.get_tracer(__MODULE__), unquote(span_ctx))
    end
  end

  defmacro end_span() do
    quote do
      :ot_tracer.end_span(:opentelemetry.get_tracer(__MODULE__))
    end
  end

  defmacro with_span(name, opts \\ quote(do: %{}), do: block) do
    quote do
      :ot_tracer.with_span(:opentelemetry.get_tracer(__MODULE__),
            unquote(name),
            unquote(opts),
            fn _ -> unquote(block) end)
    end
  end

  defmacro current_span_ctx() do
    quote do
      :ot_tracer.current_span_ctx(:opentelemetry.get_tracer(__MODULE__))
    end
  end

end
