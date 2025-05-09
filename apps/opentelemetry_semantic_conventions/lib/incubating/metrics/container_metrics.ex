defmodule OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Container metrics.
  """
  @doc """
  Total CPU time consumed

  Instrument: `counter`
  Unit: `s`
  ### Notes

  Total CPU time consumed by the specific container on all available CPU cores


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_cpu_time()
      :"container.cpu.time"

  ### Erlang

  ```erlang
  ?CONTAINER_CPU_TIME.
  'container.cpu.time'
  ```

  <!-- tabs-close -->
  """

  @spec container_cpu_time :: :"container.cpu.time"
  def container_cpu_time do
    :"container.cpu.time"
  end

  @doc """
  Container's CPU usage, measured in cpus. Range from 0 to the number of allocatable CPUs

  Instrument: `gauge`
  Unit: `{cpu}`
  ### Notes

  CPU usage of the specific container on all available CPU cores, averaged over the sample window


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_cpu_usage()
      :"container.cpu.usage"

  ### Erlang

  ```erlang
  ?CONTAINER_CPU_USAGE.
  'container.cpu.usage'
  ```

  <!-- tabs-close -->
  """

  @spec container_cpu_usage :: :"container.cpu.usage"
  def container_cpu_usage do
    :"container.cpu.usage"
  end

  @doc """
  Disk bytes for the container.

  Instrument: `counter`
  Unit: `By`
  ### Notes

  The total number of bytes read/written successfully (aggregated from all disks).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_disk_io()
      :"container.disk.io"

  ### Erlang

  ```erlang
  ?CONTAINER_DISK_IO.
  'container.disk.io'
  ```

  <!-- tabs-close -->
  """

  @spec container_disk_io :: :"container.disk.io"
  def container_disk_io do
    :"container.disk.io"
  end

  @doc """
  Memory usage of the container.

  Instrument: `counter`
  Unit: `By`
  ### Notes

  Memory usage of the container.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_memory_usage()
      :"container.memory.usage"

  ### Erlang

  ```erlang
  ?CONTAINER_MEMORY_USAGE.
  'container.memory.usage'
  ```

  <!-- tabs-close -->
  """

  @spec container_memory_usage :: :"container.memory.usage"
  def container_memory_usage do
    :"container.memory.usage"
  end

  @doc """
  Network bytes for the container.

  Instrument: `counter`
  Unit: `By`
  ### Notes

  The number of bytes sent/received on all network interfaces by the container.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_network_io()
      :"container.network.io"

  ### Erlang

  ```erlang
  ?CONTAINER_NETWORK_IO.
  'container.network.io'
  ```

  <!-- tabs-close -->
  """

  @spec container_network_io :: :"container.network.io"
  def container_network_io do
    :"container.network.io"
  end

  @doc """
  The time the container has been running

  Instrument: `gauge`
  Unit: `s`
  ### Notes

  Instrumentations **SHOULD** use a gauge with type `double` and measure uptime in seconds as a floating point number with the highest precision available.
  The actual accuracy would depend on the instrumentation and operating system.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_uptime()
      :"container.uptime"

  ### Erlang

  ```erlang
  ?CONTAINER_UPTIME.
  'container.uptime'
  ```

  <!-- tabs-close -->
  """

  @spec container_uptime :: :"container.uptime"
  def container_uptime do
    :"container.uptime"
  end
end
