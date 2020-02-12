defmodule OpenTelemetry.Span do

  defmacro is_recording_events() do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.is_recording_events(tracer, :ot_tracer.current_span_ctx(tracer))
    end
  end

  defmacro set_attribute(key, value) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.set_attribute(tracer, :ot_tracer.current_span_ctx(tracer), unquote(key), unquote(value))
    end
  end

  defmacro set_attributes(attributes) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.set_attributes(tracer, :ot_tracer.current_span_ctx(tracer), unquote(attributes))
    end
  end

  defmacro add_event(event) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.add_event(tracer, :ot_tracer.current_span_ctx(tracer), unquote(event))
    end
  end

  defmacro add_events(events) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.add_events(tracer, :ot_tracer.current_span_ctx(tracer), unquote(events))
    end
  end

  defmacro add_links(links) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.add_links(tracer, :ot_tracer.current_span_ctx(tracer), unquote(links))
    end
  end

  defmacro set_status(status) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.set_status(tracer, :ot_tracer.current_span_ctx(tracer), unquote(status))
    end
  end

  defmacro update_name(name) do
    quote do
      tracer = :opentelemetry.get_tracer(__MODULE__)
      :ot_span.update_name(tracer, :ot_tracer.current_span_ctx(tracer), unquote(name))
    end
  end
end
