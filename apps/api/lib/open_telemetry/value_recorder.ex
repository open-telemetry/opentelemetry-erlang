defmodule OpenTelemetry.ValueRecorder do
  @moduledoc """

      require OpenTelemetry.ValueRecorder

      OpenTelemetry.ValueRecorder.record("some.recorder", 3)
  """

  defmacro new(name, opts \\ %{}) do
    quote do
      :ot_value_recorder.new(:opentelemetry.get_meter(__MODULE__), unquote(name), unquote(opts))
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

  defdelegate definition(name, opts), to: :ot_value_recorder
  defdelegate measurement(name_or_instrument, number), to: :ot_value_recorder
end
