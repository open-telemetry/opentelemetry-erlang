defmodule OpenTelemetry do
  # Helpers to build OpenTelemetry structured types
  defdelegate timestamp(), to: :opentelemetry
  defdelegate links(link_list), to: :opentelemetry
  defdelegate link(trace_id, span_id, attributes, trace_state), to: :opentelemetry
  defdelegate event(name, attributes), to: :opentelemetry
  defdelegate event(timestamp, name, attributes), to: :opentelemetry
  defdelegate events(event_list), to: :opentelemetry
  defdelegate status(code, message), to: :opentelemetry
end
