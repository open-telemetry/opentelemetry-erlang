defmodule OpenTelemetryAPIExperimental.Meter do
  @moduledoc """

  """
  defmacro current_meter do
    quote do
      :opentelemetry_experimental.get_meter(__MODULE__)
    end
  end

  defmacro create_counter(name, value_type, opts) do
    quote bind_quoted: [name: name, value_type: value_type, opts: opts] do
      :otel_meter.create_counter(
        :opentelemetry_experimental.get_meter(__MODULE__),
        name,
        value_type,
        opts
      )
    end
  end

  defmacro create_observable_counter(name, callback, callback_args, value_type, opts) do
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

  defmacro create_histogram(name, value_type, opts) do
    quote bind_quoted: [name: name, value_type: value_type, opts: opts] do
      :otel_meter.create_histogram(
        :opentelemetry_experimental.get_meter(__MODULE__),
        name,
        value_type,
        opts
      )
    end
  end

  defmacro create_observable_gauge(name, callback, callback_args, value_type, opts) do
    quote bind_quoted: [
            name: name,
            value_type: value_type,
            callback: callback,
            callback_args: callback_args,
            opts: opts
          ] do
      :otel_meter.create_observable_gauge(
        :opentelemetry_experimental.get_meter(__MODULE__),
        name,
        callback,
        callback_args,
        value_type,
        opts
      )
    end
  end

  defmacro create_updown_counter(name, value_type, opts) do
    quote bind_quoted: [name: name, value_type: value_type, opts: opts] do
      :otel_meter.create_updown_counter(
        :opentelemetry_experimental.get_meter(__MODULE__),
        name,
        value_type,
        opts
      )
    end
  end

  defmacro create_observable_updowncounter(name, callback, callback_args, value_type, opts) do
    quote bind_quoted: [
            name: name,
            value_type: value_type,
            callback: callback,
            callback_args: callback_args,
            opts: opts
          ] do
      :otel_meter.create_observable_updowncounter(
        :opentelemetry_experimental.get_meter(__MODULE__),
        name,
        callback,
        callback_args,
        value_type,
        opts
      )
    end
  end

  defmacro counter_add(name, number, attributes) do
    quote bind_quoted: [name: name, number: number, attributes: attributes] do
      :otel_counter.add(
        :otel_meter.lookup_instrument(:opentelemetry_experimental.get_meter(__MODULE__), name),
        number,
        attributes
      )
    end
  end

  defmacro updown_counter_add(name, number, attributes) do
    quote bind_quoted: [name: name, number: number, attributes: attributes] do
      :otel_updown_counter.add(
        :otel_meter.lookup_instrument(:opentelemetry_experimental.get_meter(__MODULE__), name),
        number,
        attributes
      )
    end
  end

  defmacro histogram_record(name, number, attributes) do
    quote bind_quoted: [name: name, number: number, attributes: attributes] do
      :otel_histogram.record(
        :otel_meter.lookup_instrument(:opentelemetry_experimental.get_meter(__MODULE__), name),
        number,
        attributes
      )
    end
  end

  defmacro lookup_instrument(name) do
    quote bind_quoted: [name: name] do
      :otel_meter.lookup_instrument(
        :opentelemetry_experimental.get_meter(__MODULE__),
        name
      )
    end
  end
end
