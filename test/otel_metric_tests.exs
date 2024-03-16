defmodule OtelMetricTests do
  use ExUnit.Case, async: false

  require OpenTelemetryAPIExperimental.Counter, as: Counter
  require OpenTelemetryAPIExperimental.UpDownCounter, as: UpDownCounter
  require OpenTelemetryAPIExperimental.Histogram, as: Histogram

  require Record
  @fields Record.extract(:metric, from_lib: "opentelemetry_experimental/include/otel_metrics.hrl")
  Record.defrecordp(:metric, @fields)

  @fields Record.extract(:datapoint,
            from_lib: "opentelemetry_experimental/include/otel_metrics.hrl"
          )
  Record.defrecordp(:datapoint, @fields)

  @fields Record.extract(:sum, from_lib: "opentelemetry_experimental/include/otel_metrics.hrl")
  Record.defrecordp(:sum, @fields)

  @fields Record.extract(:histogram, from_lib: "opentelemetry_experimental/include/otel_metrics.hrl")
  Record.defrecordp(:histogram, @fields)

  setup do
    Application.load(:opentelemetry_experimental)
    Application.load(:opentelemetry)

    Application.put_env(:opentelemetry, :processors, [
      {:otel_simple_processor, %{exporter: :none}}
    ])

    Application.put_env(:opentelemetry_experimental, :readers, [
      %{
        module: :otel_metric_reader,
        config: %{
          exporter: {:otel_metric_exporter_pid, {:metric, self()}},
          default_temporality_mapping: %{
            counter: :temporality_delta,
            observable_counter: :temporality_cumulative,
            updown_counter: :temporality_delta,
            observable_updowncounter: :temporality_cumulative,
            histogram: :temporality_cumulative,
            observable_gauge: :temporality_cumulative
          }
        }
      }
    ])

    {:ok, _} = Application.ensure_all_started(:opentelemetry_experimental)

    on_exit(fn ->
      Application.stop(:opentelemetry_experimental)
      Application.unload(:opentelemetry_experimental)
      Application.stop(:opentelemetry)
      Application.unload(:opentelemetry)
    end)
  end

  test "create Counter with macros" do
    Counter.create(:counter_a, %{unit: "1", description: "some counter_a"})
    Counter.add(:counter_a, 1, [])

    :otel_meter_server.force_flush()

    assert_receive {:metric,
                    metric(
                      name: :counter_a,
                      data: sum(datapoints: [datapoint(value: 1)])
                    )}
  end

  test "create UpDownCounter with macros" do
    UpDownCounter.create(:updown_counter_a, %{unit: "1", description: "some updown_counter_a"})
    UpDownCounter.add(:updown_counter_a, 5, [])
    UpDownCounter.add(:updown_counter_a, -5, [])

    :otel_meter_server.force_flush()

    assert_receive {:metric,
                    metric(
                      name: :updown_counter_a,
                      data: sum(datapoints: [datapoint(value: 0)])
                    )}

    UpDownCounter.add(:updown_counter_a, -3, [])

    :otel_meter_server.force_flush()

    assert_receive {:metric,
                    metric(
                      name: :updown_counter_a,
                      data: sum(datapoints: [datapoint(value: -3)])
                    )}

    UpDownCounter.add(:updown_counter_a, 9, [])

    :otel_meter_server.force_flush()

    assert_receive {:metric,
                    metric(
                      name: :updown_counter_a,
                      data: sum(datapoints: [datapoint(value: 9)])
                    )}
  end

  test "create Histogram with macros" do
    Histogram.create(:histogram_a, %{unit: "1", description: "some histogram_a"})
    Histogram.record(:histogram_a, 1)

    :otel_meter_server.force_flush()

    assert_receive {:metric,
                    metric(
                      name: :histogram_a,
                      data: histogram(aggregation_temporality: :temporality_cumulative,
                        datapoints: [{:histogram_datapoint, %{}, _, _, 1, 1, [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                                      [0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 750.0, 1000.0, 2500.0, 5000.0, 7500.0, 10000.0], [], 0, 1, 1}])
                    )}

    Histogram.record(:histogram_a, 10)

    :otel_meter_server.force_flush()

    assert_receive {:metric,
                    metric(
                      name: :histogram_a,
                      data: histogram(aggregation_temporality: :temporality_cumulative,
                        datapoints: [{:histogram_datapoint, %{}, _, _, 2, 11, [0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                                      [0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 750.0, 1000.0, 2500.0, 5000.0, 7500.0, 10000.0], [], 0, 1, 10}])
                    )}
  end

end
