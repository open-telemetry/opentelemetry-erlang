defmodule OpenTelemetryAPIExperimental.Gauge do
  @moduledoc """
  Gauge is a synchronous instrument for recording non-additive values, such as a
  queue depth, as they change. Only the last value set per attribute set is reported.

  Use `OpenTelemetryAPIExperimental.ObservableGauge` if the value is only available by polling.
  """

  defmacro create(name, opts) do
    quote bind_quoted: [name: name, opts: opts] do
      :otel_meter.create_gauge(
        :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
        name,
        opts
      )
    end
  end

  defmacro set(name, number, attributes) do
    quote bind_quoted: [name: name, number: number, attributes: attributes] do
      :otel_gauge.set(
        OpenTelemetry.Ctx.get_current(),
        :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
        name,
        number,
        attributes
      )
    end
  end
end
