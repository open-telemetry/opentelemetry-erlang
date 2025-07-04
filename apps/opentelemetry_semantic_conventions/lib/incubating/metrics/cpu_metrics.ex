defmodule OpenTelemetry.SemConv.Incubating.Metrics.CPUMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for CPU metrics.
  """
  @doc """
  Operating frequency of the logical CPU in Hertz.

  Instrument: `gauge`
  Unit: `Hz`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CPUMetrics.cpu_frequency()
      :"cpu.frequency"

  ### Erlang

  ```erlang
  ?CPU_FREQUENCY.
  'cpu.frequency'
  ```

  <!-- tabs-close -->
  """

  @spec cpu_frequency :: :"cpu.frequency"
  def cpu_frequency do
    :"cpu.frequency"
  end

  @doc """
  Seconds each logical CPU spent on each mode

  Instrument: `counter`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CPUMetrics.cpu_time()
      :"cpu.time"

  ### Erlang

  ```erlang
  ?CPU_TIME.
  'cpu.time'
  ```

  <!-- tabs-close -->
  """

  @spec cpu_time :: :"cpu.time"
  def cpu_time do
    :"cpu.time"
  end

  @doc """
  For each logical CPU, the utilization is calculated as the change in cumulative CPU time (cpu.time) over a measurement interval, divided by the elapsed time.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.CPUMetrics.cpu_utilization()
      :"cpu.utilization"

  ### Erlang

  ```erlang
  ?CPU_UTILIZATION.
  'cpu.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec cpu_utilization :: :"cpu.utilization"
  def cpu_utilization do
    :"cpu.utilization"
  end
end
