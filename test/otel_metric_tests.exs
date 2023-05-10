defmodule OtelMetricTests do
  use ExUnit.Case, async: true

  require OpenTelemetryAPIExperimental.Counter, as: Counter

  require Record
  @fields Record.extract(:metric, from_lib: "opentelemetry_experimental/include/otel_metrics.hrl")
  Record.defrecordp(:metric, @fields)

  setup do
    Application.load(:opentelemetry_experimental)

    Application.put_env(:opentelemetry_experimental, :readers, [
      %{
        module: :otel_metric_reader,
        config: %{
          exporter: {:otel_metric_exporter_pid, {:metric, self()}}
        }
      }
    ])

    {:ok, _} = Application.ensure_all_started(:opentelemetry_experimental)

    on_exit(fn ->
      Application.stop(:opentelemetry_experimental)
      Application.unload(:opentelemetry_experimental)
    end)
  end

  test "create Counter with macros" do
    Counter.create(:counter_a, %{unit: "1", description: "some counter_a"})
    Counter.add(:counter_a, 1, [])

    :otel_meter_server.force_flush()

    assert_receive {:metric, metric(name: :counter_a)}
  end
end
