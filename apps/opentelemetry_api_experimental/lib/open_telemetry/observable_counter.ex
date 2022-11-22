defmodule OpenTelemetryAPIExperimental.ObservableCounter do
  @moduledoc """

  """

  defmacro create(name, callback, callback_args, value_type, opts) do
    quote bind_quoted: [
            name: name,
            value_type: value_type,
            callback: callback,
            callback_args: callback_args,
            opts: opts
          ] do
      :otel_meter.create_observable_counter(
        :opentelemetry_experimental.get_meter(__MODULE__),
        name,
        callback,
        callback_args,
        value_type,
        opts
      )
    end
  end
end
