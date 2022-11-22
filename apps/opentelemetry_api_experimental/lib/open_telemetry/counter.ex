defmodule OpenTelemetryAPIExperimental.Counter do
  @moduledoc """

  """

  defmacro create(name, value_type, opts) do
    quote bind_quoted: [name: name, value_type: value_type, opts: opts] do
      :otel_meter.create_counter(
        :opentelemetry_experimental.get_meter(__MODULE__),
        name,
        value_type,
        opts
      )
    end
  end

  defmacro add(name, number, attributes) do
    quote bind_quoted: [name: name, number: number, attributes: attributes] do
      :otel_counter.add(
        :otel_meter.lookup_instrument(:opentelemetry_experimental.get_meter(__MODULE__), name),
        number,
        attributes
      )
    end
  end
end
