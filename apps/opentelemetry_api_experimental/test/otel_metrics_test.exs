defmodule OpenTelemetryTest do
  use ExUnit.Case, async: true

  require OpenTelemetryAPIExperimental.Meter, as: Meter

  require Record
  @fields Record.extract(:instrument, from_lib: "opentelemetry_api_experimental/include/otel_metrics.hrl")
  Record.defrecordp(:instrument, @fields)

  test "create counter instrument with no-op meter" do
    c = Meter.create_counter(:a_counter, :integer, %{})
    assert instrument(module: :otel_meter_noop,
      meter: {:otel_meter_noop, []},
      name: :a_counter,
      kind: :counter,
      value_type: :integer) == c
  end

  test "create updown counter instrument with no-op meter" do
    c = Meter.create_updown_counter(:ud_counter, :float, %{})
    assert instrument(module: :otel_meter_noop,
      meter: {:otel_meter_noop, []},
      name: :ud_counter,
      kind: :updown_counter,
      value_type: :float) == c
  end

  test "create histogram instrument with no-op meter" do
    c = Meter.create_histogram(:a_histogram, :integer, %{})
    assert instrument(module: :otel_meter_noop,
      meter: {:otel_meter_noop, []},
      name: :a_histogram,
      kind: :histogram,
      value_type: :integer) == c
  end

  test "create observable counter with no-op meter" do
    o = Meter.create_observable_counter(:o_counter, fn _ -> {3, %{}} end, [], :integer, %{})
    assert instrument(name: :o_counter,
      kind: :observable_counter) = o
  end

  test "create observable gauge with no-op meter" do
    o = Meter.create_observable_gauge(:a_gauge, fn _ -> {3, %{}} end, [], :integer, %{})
    assert instrument(name: :a_gauge,
      kind: :observable_gauge) = o
  end

  test "create observable updown counter with no-op meter" do
    o = Meter.create_observable_updowncounter(:ouc_counter, fn _ -> {3, %{}} end, [], :integer, %{})
    assert instrument(name: :ouc_counter,
      kind: :observable_updowncounter) = o
  end
end
