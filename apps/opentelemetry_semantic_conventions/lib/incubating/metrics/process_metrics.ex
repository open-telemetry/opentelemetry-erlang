defmodule OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Process metrics.
  """
  @doc """
  Number of times the process has been context switched.

  Instrument: `counter`
  Unit: `{context_switch}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_context_switches()
      :"process.context_switches"

  ### Erlang

  ```erlang
  ?PROCESS_CONTEXT_SWITCHES.
  'process.context_switches'
  ```

  <!-- tabs-close -->
  """

  @spec process_context_switches :: :"process.context_switches"
  def process_context_switches do
    :"process.context_switches"
  end

  @doc """
  Total CPU seconds broken down by different states.

  Instrument: `counter`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_cpu_time()
      :"process.cpu.time"

  ### Erlang

  ```erlang
  ?PROCESS_CPU_TIME.
  'process.cpu.time'
  ```

  <!-- tabs-close -->
  """

  @spec process_cpu_time :: :"process.cpu.time"
  def process_cpu_time do
    :"process.cpu.time"
  end

  @doc """
  Difference in process.cpu.time since the last measurement, divided by the elapsed time and number of CPUs available to the process.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_cpu_utilization()
      :"process.cpu.utilization"

  ### Erlang

  ```erlang
  ?PROCESS_CPU_UTILIZATION.
  'process.cpu.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec process_cpu_utilization :: :"process.cpu.utilization"
  def process_cpu_utilization do
    :"process.cpu.utilization"
  end

  @doc """
  Disk bytes transferred.

  Instrument: `counter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_disk_io()
      :"process.disk.io"

  ### Erlang

  ```erlang
  ?PROCESS_DISK_IO.
  'process.disk.io'
  ```

  <!-- tabs-close -->
  """

  @spec process_disk_io :: :"process.disk.io"
  def process_disk_io do
    :"process.disk.io"
  end

  @doc """
  The amount of physical memory in use.

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_memory_usage()
      :"process.memory.usage"

  ### Erlang

  ```erlang
  ?PROCESS_MEMORY_USAGE.
  'process.memory.usage'
  ```

  <!-- tabs-close -->
  """

  @spec process_memory_usage :: :"process.memory.usage"
  def process_memory_usage do
    :"process.memory.usage"
  end

  @doc """
  The amount of committed virtual memory.

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_memory_virtual()
      :"process.memory.virtual"

  ### Erlang

  ```erlang
  ?PROCESS_MEMORY_VIRTUAL.
  'process.memory.virtual'
  ```

  <!-- tabs-close -->
  """

  @spec process_memory_virtual :: :"process.memory.virtual"
  def process_memory_virtual do
    :"process.memory.virtual"
  end

  @doc """
  Network bytes transferred.

  Instrument: `counter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_network_io()
      :"process.network.io"

  ### Erlang

  ```erlang
  ?PROCESS_NETWORK_IO.
  'process.network.io'
  ```

  <!-- tabs-close -->
  """

  @spec process_network_io :: :"process.network.io"
  def process_network_io do
    :"process.network.io"
  end

  @deprecated """
  Replaced by `process.unix.file_descriptor.count`.
  """

  @spec process_open_file_descriptor_count :: :"process.open_file_descriptor.count"
  def process_open_file_descriptor_count do
    :"process.open_file_descriptor.count"
  end

  @doc """
  Number of page faults the process has made.

  Instrument: `counter`
  Unit: `{fault}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_paging_faults()
      :"process.paging.faults"

  ### Erlang

  ```erlang
  ?PROCESS_PAGING_FAULTS.
  'process.paging.faults'
  ```

  <!-- tabs-close -->
  """

  @spec process_paging_faults :: :"process.paging.faults"
  def process_paging_faults do
    :"process.paging.faults"
  end

  @doc """
  Process threads count.

  Instrument: `updowncounter`
  Unit: `{thread}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_thread_count()
      :"process.thread.count"

  ### Erlang

  ```erlang
  ?PROCESS_THREAD_COUNT.
  'process.thread.count'
  ```

  <!-- tabs-close -->
  """

  @spec process_thread_count :: :"process.thread.count"
  def process_thread_count do
    :"process.thread.count"
  end

  @doc """
  Number of unix file descriptors in use by the process.

  Instrument: `updowncounter`
  Unit: `{file_descriptor}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_unix_file_descriptor_count()
      :"process.unix.file_descriptor.count"

  ### Erlang

  ```erlang
  ?PROCESS_UNIX_FILE_DESCRIPTOR_COUNT.
  'process.unix.file_descriptor.count'
  ```

  <!-- tabs-close -->
  """

  @spec process_unix_file_descriptor_count :: :"process.unix.file_descriptor.count"
  def process_unix_file_descriptor_count do
    :"process.unix.file_descriptor.count"
  end

  @doc """
  The time the process has been running.

  Instrument: `gauge`
  Unit: `s`
  ### Notes

  Instrumentations **SHOULD** use a gauge with type `double` and measure uptime in seconds as a floating point number with the highest precision available.
  The actual accuracy would depend on the instrumentation and operating system.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_uptime()
      :"process.uptime"

  ### Erlang

  ```erlang
  ?PROCESS_UPTIME.
  'process.uptime'
  ```

  <!-- tabs-close -->
  """

  @spec process_uptime :: :"process.uptime"
  def process_uptime do
    :"process.uptime"
  end

  @doc """
  Number of handles held by the process.

  Instrument: `updowncounter`
  Unit: `{handle}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ProcessMetrics.process_windows_handle_count()
      :"process.windows.handle.count"

  ### Erlang

  ```erlang
  ?PROCESS_WINDOWS_HANDLE_COUNT.
  'process.windows.handle.count'
  ```

  <!-- tabs-close -->
  """

  @spec process_windows_handle_count :: :"process.windows.handle.count"
  def process_windows_handle_count do
    :"process.windows.handle.count"
  end
end
