defmodule OpenTelemetry.Measure do
  @moduledoc """

      require OpenTelemetry.Measure

      OpenTelemetry.Measure.record(\"some.counter\", 3)
  """

  defmacro record(name, number, label_set) do
    quote do
      :ot_meter.record(:opentelemetry.get_meter(__MODULE__), unquote(name), unquote(number), unquote(label_set))
    end
  end

  defmacro record(bound_instrument, number) do
    quote do
      :ot_meter.record(:opentelemetry.get_meter(__MODULE__), unquote(bound_instrument), unquote(number))
    end
  end

  defdelegate measurement(name_or_instrument, number), to: :ot_measure
end
