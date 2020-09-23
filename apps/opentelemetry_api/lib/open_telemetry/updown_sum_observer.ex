defmodule OpenTelemetry.UpdownSumObserver do
  @moduledoc """

      require OpenTelemetry.UpdownSumObserver

      OpenTelemetry.UpdownSumObserver.set_callback("some.counter", OpenTelemetry.UpdownSumObserver.observe(&1, -33, []))
  """

  defmacro new(name, opts \\ %{}) do
    quote do
      :ot_updown_sum_observer.new(:opentelemetry.get_meter(__MODULE__), unquote(name), unquote(opts))
    end
  end

  defmacro set_callback(observer, callback) do
    quote do
      :ot_meter.set_observer_callback(
        :opentelemetry.get_meter(__MODULE__),
        unquote(observer),
        unquote(callback)
      )
    end
  end

  defdelegate definition(name, opts), to: :ot_updown_sum_observer
  defdelegate observe(observer_result, number, label_set), to: :ot_updown_sum_observer
end
