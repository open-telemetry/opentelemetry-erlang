defmodule OpenTelemetry.Meter do
  @moduledoc """

      require OpenTelemetry.ValueRecorder
      require OpenTelemetry.Counter
      require OpenTelemetry.Meter

      OpenTelemetry.register_application_meter(Your.Application)

      OpenTelemetry.Meter.new_instruments([OpenTelemetry.ValueRecorder.instrument("some.latency"),
                                           OpenTelemetry.Counter.instrument("some.counter")])

      # use the new instrument by name
      OpenTelemetry.Counter.add("some.counter", 1)

      # or use a bound instrument
      bound = OpenTelemetry.Meter.bind("some.latency", [])
      # measure time spent on some function and then record it
      OpenTelemetry.ValueRecorder.record(bound, time)
  """

  defmacro new_instruments(list) do
    quote do
      :ot_meter.new_instruments(:opentelemetry.get_meter(__MODULE__), unquote(list))
    end
  end

  defmacro bind(name, label_set) do
    quote do
      :ot_meter.bind(:opentelemetry.get_meter(__MODULE__), unquote(name), unquote(label_set))
    end
  end

  defmacro release(bound_instrument) do
    quote do
      :ot_meter.release(:opentelemetry.get_meter(__MODULE__), unquote(bound_instrument))
    end
  end

  defmacro record(name, number, label_set) do
    quote do
      :ot_meter.record(
        :opentelemetry.get_meter(__MODULE__),
        unquote(name),
        unquote(number),
        unquote(label_set)
      )
    end
  end

  defmacro record(bound_instrument, number) do
    quote do
      :ot_meter.record(
        :opentelemetry.get_meter(__MODULE__),
        unquote(bound_instrument),
        unquote(number)
      )
    end
  end

  defmacro record_batch(label_set, measurements) do
    quote do
      :ot_meter.record_batch(
        :opentelemetry.get_meter(__MODULE__),
        unquote(label_set),
        unquote(measurements)
      )
    end
  end
end
