defmodule OpenTelemetryAPIExperimental.Meter do
  @moduledoc """

  """

  defmacro current_meter do
    quote do
      :opentelemetry_experimental.get_meter(__MODULE__)
    end
  end

  defmacro register_callback(instruments, callback, callback_args) do
    :otel_meter.register_callback(
      :opentelemetry_experimental.get_meter(__MODULE__),
      instruments,
      callback,
      callback_args
    )
  end
end
