defmodule OpenTelemetry.Baggage do
  @moduledoc """
  Baggage is used to annotate telemetry, adding context and information to
  metrics, traces, and logs. It is represented by a set of name/value pairs
  describing user-defined properties.
  """

  defdelegate set(keyvalues), to: :otel_baggage
  defdelegate set(ctx_or_key, keyvalues), to: :otel_baggage
  defdelegate set(ctx, key, value), to: :otel_baggage
  defdelegate set(ctx, key, values, metadata), to: :otel_baggage
  defdelegate get_all(), to: :otel_baggage
  defdelegate get_all(ctx), to: :otel_baggage
  defdelegate clear(), to: :otel_baggage
  defdelegate clear(ctx), to: :otel_baggage
end
