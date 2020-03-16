defmodule OpenTelemetry.Counter do
  @moduledoc """

      require OpenTelemetry.Counter

      OpenTelemetry.Counter.add("some.counter", 3)
  """

  defmacro add(name, number, label_set) do
    quote do
      :ot_meter.record(:opentelemetry.get_meter(__MODULE__), unquote(name), unquote(number), unquote(label_set))
    end
  end

  defmacro add(bound_instrument, number) do
    quote do
      :ot_meter.record(:opentelemetry.get_meter(__MODULE__), unquote(bound_instrument), unquote(number))
    end
  end

  defdelegate measurement(name_or_instrument, number), to: :ot_counter
end
