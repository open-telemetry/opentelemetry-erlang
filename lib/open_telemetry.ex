defmodule OpenTelemetry do
  defdelegate generate_trace_id(), to: :opentelemetry
  defdelegate generate_span_id(), to: :opentelemetry
end
