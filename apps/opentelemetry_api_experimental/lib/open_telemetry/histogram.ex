defmodule OpenTelemetryAPIExperimental.Histogram do
  @moduledoc """

  """

  defmacro create(name, opts) do
    quote bind_quoted: [name: name, opts: opts] do
      :otel_meter.create_histogram(
        :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
        name,
        opts
      )
    end
  end

  defmacro register(alias_name, name, opts) do
    quote bind_quoted: [alias_name: alias_name, name: name, opts: opts] do
      instrument =
        :otel_meter.create_histogram(
          :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
          name,
          opts
        )

      :otel_instrument.register_alias(alias_name, instrument)
    end
  end

  defmacro record(target, number, attributes) do
    quote bind_quoted: [target: target, number: number, attributes: attributes] do
      :otel_histogram.record(
        OpenTelemetry.Ctx.get_current(),
        target,
        number,
        attributes
      )
    end
  end
end
