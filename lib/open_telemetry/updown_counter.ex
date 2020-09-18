defmodule OpenTelemetry.UpdownCounter do
  @moduledoc """

      require OpenTelemetry.UpdownCounter

      OpenTelemetry.UpdownCounter.new("some.counter")

      OpenTelemetry.UpdownCounter.add("some.counter", -3)
  """

  defmacro new(name, opts \\ %{}) do
    quote do
      :ot_updown_counter.new(:opentelemetry.get_meter(__MODULE__), unquote(name), unquote(opts))
    end
  end

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

  defdelegate definition(name, opts), to: :ot_updown_counter
  defdelegate measurement(name_or_instrument, number), to: :ot_updown_counter
end
