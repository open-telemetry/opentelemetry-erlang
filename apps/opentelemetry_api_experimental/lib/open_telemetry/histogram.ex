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

  defmacro record(name, number) do
    quote bind_quoted: [name: name, number: number] do
      :otel_histogram.record(
        OpenTelemetry.Ctx.get_current(),
        :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
        name,
        number
      )
    end
  end

  defmacro record(name, number, attributes) do
    quote bind_quoted: [name: name, number: number, attributes: attributes] do
      :otel_histogram.record(
        OpenTelemetry.Ctx.get_current(),
        :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
        name,
        number,
        attributes
      )
    end
  end
end
