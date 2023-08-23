defmodule OpenTelemetryAPIExperimental.ObservableUpDownCounter do
  @moduledoc """

  """

  defmacro create(name, opts) do
    quote bind_quoted: [name: name, opts: opts] do
      :otel_meter.create_observable_updowncounter(
        :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
        name,
        opts
      )
    end
  end

  defmacro create(name, callback, callback_args, opts) do
    quote bind_quoted: [
            name: name,
            callback: callback,
            callback_args: callback_args,
            opts: opts
          ] do
      :otel_meter.create_observable_updowncounter(
        :opentelemetry_experimental.get_meter(:opentelemetry.get_application_scope(__MODULE__)),
        name,
        callback,
        callback_args,
        opts
      )
    end
  end
end
