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

  defmacro record(name_or_instrument, number) do
    quote bind_quoted: [name_or_instrument: name_or_instrument, number: number] do
      case is_atom(name_or_instrument) do
        true ->
          :otel_histogram.record(
            OpenTelemetry.Ctx.get_current(),
            :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
            name_or_instrument,
            number,
            %{}
          )
        _ ->
          :otel_histogram.record(
            OpenTelemetry.Ctx.get_current(),
            name_or_instrument,
            number,
            %{}
          )
      end
    end
  end

  defmacro record(name_or_instrument, number, attributes) do
    quote bind_quoted: [name_or_instrument: name_or_instrument, number: number, attributes: attributes] do
      case is_atom(name_or_instrument) do
        true ->
          :otel_histogram.record(
            OpenTelemetry.Ctx.get_current(),
            :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
            name_or_instrument,
            number,
            attributes
          )
        _ ->
          :otel_histogram.record(
            OpenTelemetry.Ctx.get_current(),
            name_or_instrument,
            number,
            attributes
          )
      end
    end
  end
end
