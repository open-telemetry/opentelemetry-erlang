defmodule OpenTelemetryAPIExperimental.UpDownCounter do
  @moduledoc """

  """

  defmacro create(name, opts) do
    quote bind_quoted: [name: name, opts: opts] do
      :otel_meter.create_updown_counter(
        :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
        name,
        opts
      )
    end
  end

  defmacro add(name_or_instrument, number) do
    quote bind_quoted: [name_or_instrument: name_or_instrument, number: number] do
      case is_atom(name_or_instrument) do
        true ->
          :otel_updown_counter.add(
            OpenTelemetry.Ctx.get_current(),
            :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
            name_or_instrument,
            number,
            %{}
          )
        _ ->
          :otel_updown_counter.add(
            OpenTelemetry.Ctx.get_current(),
            name_or_instrument,
            number
            %{}
          )
      end
    end
  end

  defmacro add(name_or_instrument, number, attributes) do
    quote bind_quoted: [name_or_instrument: name_or_instrument, number: number, attributes: attributes] do
      case is_atom(name_or_instrument) do
        true ->
          :otel_updown_counter.add(
            OpenTelemetry.Ctx.get_current(),
            :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
            name_or_instrument,
            number,
            attributes
          )
        _ ->
          :otel_updown_counter.add(
            OpenTelemetry.Ctx.get_current(),
            name_or_instrument,
            number
            attributes
          )
      end
    end
  end
end
