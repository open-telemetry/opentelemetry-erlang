defmodule OpenTelemetry.Observer do
  @moduledoc """

      require OpenTelemetry.Observer

      OpenTelemetry.Observer.set_callback(\"some.counter\", fn o -> OpenTelemetry.Observer.observe(o, 33, []))
  """

  defmacro set_callback(observer, callback) do
    quote do
      :ot_meter.set_observer_callback(:opentelemetry.get_meter(__MODULE__), unquote(observer), unquote(callback))
    end
  end

  defdelegate observe(observer_result, number, label_set), to: :ot_observer
end
