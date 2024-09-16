defmodule OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for System metrics.
  """
  @doc """
  Reports the current frequency of the CPU in Hz

  Instrument: `gauge`
  Unit: `{Hz}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_cpu_frequency()
      :"system.cpu.frequency"

  ### Erlang

  ```erlang
  ?SYSTEM_CPU_FREQUENCY.
  'system.cpu.frequency'
  ```

  <!-- tabs-close -->
  """

  @spec system_cpu_frequency :: :"system.cpu.frequency"
  def system_cpu_frequency do
    :"system.cpu.frequency"
  end

  @doc """
  Reports the number of logical (virtual) processor cores created by the operating system to manage multitasking

  Instrument: `updowncounter`
  Unit: `{cpu}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_cpu_logical_count()
      :"system.cpu.logical.count"

  ### Erlang

  ```erlang
  ?SYSTEM_CPU_LOGICAL_COUNT.
  'system.cpu.logical.count'
  ```

  <!-- tabs-close -->
  """

  @spec system_cpu_logical_count :: :"system.cpu.logical.count"
  def system_cpu_logical_count do
    :"system.cpu.logical.count"
  end

  @doc """
  Reports the number of actual physical processor cores on the hardware

  Instrument: `updowncounter`
  Unit: `{cpu}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_cpu_physical_count()
      :"system.cpu.physical.count"

  ### Erlang

  ```erlang
  ?SYSTEM_CPU_PHYSICAL_COUNT.
  'system.cpu.physical.count'
  ```

  <!-- tabs-close -->
  """

  @spec system_cpu_physical_count :: :"system.cpu.physical.count"
  def system_cpu_physical_count do
    :"system.cpu.physical.count"
  end

  @doc """
  Seconds each logical CPU spent on each mode

  Instrument: `counter`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_cpu_time()
      :"system.cpu.time"

  ### Erlang

  ```erlang
  ?SYSTEM_CPU_TIME.
  'system.cpu.time'
  ```

  <!-- tabs-close -->
  """

  @spec system_cpu_time :: :"system.cpu.time"
  def system_cpu_time do
    :"system.cpu.time"
  end

  @doc """
  Difference in system.cpu.time since the last measurement, divided by the elapsed time and number of logical CPUs

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_cpu_utilization()
      :"system.cpu.utilization"

  ### Erlang

  ```erlang
  ?SYSTEM_CPU_UTILIZATION.
  'system.cpu.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec system_cpu_utilization :: :"system.cpu.utilization"
  def system_cpu_utilization do
    :"system.cpu.utilization"
  end

  @doc """
  none

  Instrument: `counter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_disk_io()
      :"system.disk.io"

  ### Erlang

  ```erlang
  ?SYSTEM_DISK_IO.
  'system.disk.io'
  ```

  <!-- tabs-close -->
  """

  @spec system_disk_io :: :"system.disk.io"
  def system_disk_io do
    :"system.disk.io"
  end

  @doc """
  Time disk spent activated

  Instrument: `counter`
  Unit: `s`
  ### Notes

  The real elapsed time ("wall clock") used in the I/O path (time from operations running in parallel are not counted). Measured as:

  - Linux: Field 13 from [procfs-diskstats](https://www.kernel.org/doc/Documentation/ABI/testing/procfs-diskstats)
  - Windows: The complement of
    ["Disk\% Idle Time"](https://learn.microsoft.com/archive/blogs/askcore/windows-performance-monitor-disk-counters-explained#windows-performance-monitor-disk-counters-explained)
    performance counter: `uptime * (100 - "Disk\% Idle Time") / 100`


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_disk_io_time()
      :"system.disk.io_time"

  ### Erlang

  ```erlang
  ?SYSTEM_DISK_IO_TIME.
  'system.disk.io_time'
  ```

  <!-- tabs-close -->
  """

  @spec system_disk_io_time :: :"system.disk.io_time"
  def system_disk_io_time do
    :"system.disk.io_time"
  end

  @doc """
  none

  Instrument: `counter`
  Unit: `{operation}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_disk_merged()
      :"system.disk.merged"

  ### Erlang

  ```erlang
  ?SYSTEM_DISK_MERGED.
  'system.disk.merged'
  ```

  <!-- tabs-close -->
  """

  @spec system_disk_merged :: :"system.disk.merged"
  def system_disk_merged do
    :"system.disk.merged"
  end

  @doc """
  Sum of the time each operation took to complete

  Instrument: `counter`
  Unit: `s`
  ### Notes

  Because it is the sum of time each request took, parallel-issued requests each contribute to make the count grow. Measured as:

  - Linux: Fields 7 & 11 from [procfs-diskstats](https://www.kernel.org/doc/Documentation/ABI/testing/procfs-diskstats)
  - Windows: "Avg. Disk sec/Read" perf counter multiplied by "Disk Reads/sec" perf counter (similar for Writes)


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_disk_operation_time()
      :"system.disk.operation_time"

  ### Erlang

  ```erlang
  ?SYSTEM_DISK_OPERATION_TIME.
  'system.disk.operation_time'
  ```

  <!-- tabs-close -->
  """

  @spec system_disk_operation_time :: :"system.disk.operation_time"
  def system_disk_operation_time do
    :"system.disk.operation_time"
  end

  @doc """
  none

  Instrument: `counter`
  Unit: `{operation}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_disk_operations()
      :"system.disk.operations"

  ### Erlang

  ```erlang
  ?SYSTEM_DISK_OPERATIONS.
  'system.disk.operations'
  ```

  <!-- tabs-close -->
  """

  @spec system_disk_operations :: :"system.disk.operations"
  def system_disk_operations do
    :"system.disk.operations"
  end

  @doc """
  none

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_filesystem_usage()
      :"system.filesystem.usage"

  ### Erlang

  ```erlang
  ?SYSTEM_FILESYSTEM_USAGE.
  'system.filesystem.usage'
  ```

  <!-- tabs-close -->
  """

  @spec system_filesystem_usage :: :"system.filesystem.usage"
  def system_filesystem_usage do
    :"system.filesystem.usage"
  end

  @doc """
  none

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_filesystem_utilization()
      :"system.filesystem.utilization"

  ### Erlang

  ```erlang
  ?SYSTEM_FILESYSTEM_UTILIZATION.
  'system.filesystem.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec system_filesystem_utilization :: :"system.filesystem.utilization"
  def system_filesystem_utilization do
    :"system.filesystem.utilization"
  end

  @doc """
  An estimate of how much memory is available for starting new applications, without causing swapping

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This is an alternative to `system.memory.usage` metric with `state=free`.
  Linux starting from 3.14 exports "available" memory. It takes "free" memory as a baseline, and then factors in kernel-specific values.
  This is supposed to be more accurate than just "free" memory.
  For reference, see the calculations [here](https://superuser.com/a/980821).
  See also `MemAvailable` in [/proc/meminfo](https://man7.org/linux/man-pages/man5/proc.5.html).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_linux_memory_available()
      :"system.linux.memory.available"

  ### Erlang

  ```erlang
  ?SYSTEM_LINUX_MEMORY_AVAILABLE.
  'system.linux.memory.available'
  ```

  <!-- tabs-close -->
  """

  @spec system_linux_memory_available :: :"system.linux.memory.available"
  def system_linux_memory_available do
    :"system.linux.memory.available"
  end

  @doc """
  Reports the memory used by the Linux kernel for managing caches of frequently used objects.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  The sum over the `reclaimable` and `unreclaimable` state values in `linux.memory.slab.usage` **SHOULD** be equal to the total slab memory available on the system.
  Note that the total slab memory is not constant and may vary over time.
  See also the [Slab allocator](https://blogs.oracle.com/linux/post/understanding-linux-kernel-memory-statistics) and `Slab` in [/proc/meminfo](https://man7.org/linux/man-pages/man5/proc.5.html).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_linux_memory_slab_usage()
      :"system.linux.memory.slab.usage"

  ### Erlang

  ```erlang
  ?SYSTEM_LINUX_MEMORY_SLAB_USAGE.
  'system.linux.memory.slab.usage'
  ```

  <!-- tabs-close -->
  """

  @spec system_linux_memory_slab_usage :: :"system.linux.memory.slab.usage"
  def system_linux_memory_slab_usage do
    :"system.linux.memory.slab.usage"
  end

  @doc """
  Total memory available in the system.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  Its value **SHOULD** equal the sum of `system.memory.state` over all states.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_memory_limit()
      :"system.memory.limit"

  ### Erlang

  ```erlang
  ?SYSTEM_MEMORY_LIMIT.
  'system.memory.limit'
  ```

  <!-- tabs-close -->
  """

  @spec system_memory_limit :: :"system.memory.limit"
  def system_memory_limit do
    :"system.memory.limit"
  end

  @doc """
  Shared memory used (mostly by tmpfs).

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  Equivalent of `shared` from [`free` command](https://man7.org/linux/man-pages/man1/free.1.html) or
  `Shmem` from [`/proc/meminfo`](https://man7.org/linux/man-pages/man5/proc.5.html)"


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_memory_shared()
      :"system.memory.shared"

  ### Erlang

  ```erlang
  ?SYSTEM_MEMORY_SHARED.
  'system.memory.shared'
  ```

  <!-- tabs-close -->
  """

  @spec system_memory_shared :: :"system.memory.shared"
  def system_memory_shared do
    :"system.memory.shared"
  end

  @doc """
  Reports memory in use by state.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  The sum over all `system.memory.state` values **SHOULD** equal the total memory
  available on the system, that is `system.memory.limit`.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_memory_usage()
      :"system.memory.usage"

  ### Erlang

  ```erlang
  ?SYSTEM_MEMORY_USAGE.
  'system.memory.usage'
  ```

  <!-- tabs-close -->
  """

  @spec system_memory_usage :: :"system.memory.usage"
  def system_memory_usage do
    :"system.memory.usage"
  end

  @doc """
  none

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_memory_utilization()
      :"system.memory.utilization"

  ### Erlang

  ```erlang
  ?SYSTEM_MEMORY_UTILIZATION.
  'system.memory.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec system_memory_utilization :: :"system.memory.utilization"
  def system_memory_utilization do
    :"system.memory.utilization"
  end

  @doc """
  none

  Instrument: `updowncounter`
  Unit: `{connection}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_network_connections()
      :"system.network.connections"

  ### Erlang

  ```erlang
  ?SYSTEM_NETWORK_CONNECTIONS.
  'system.network.connections'
  ```

  <!-- tabs-close -->
  """

  @spec system_network_connections :: :"system.network.connections"
  def system_network_connections do
    :"system.network.connections"
  end

  @doc """
  Count of packets that are dropped or discarded even though there was no error

  Instrument: `counter`
  Unit: `{packet}`
  ### Notes

  Measured as:

  - Linux: the `drop` column in `/proc/dev/net` ([source](https://web.archive.org/web/20180321091318/http://www.onlamp.com/pub/a/linux/2000/11/16/LinuxAdmin.html))
  - Windows: [`InDiscards`/`OutDiscards`](https://docs.microsoft.com/windows/win32/api/netioapi/ns-netioapi-mib_if_row2)
    from [`GetIfEntry2`](https://docs.microsoft.com/windows/win32/api/netioapi/nf-netioapi-getifentry2)


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_network_dropped()
      :"system.network.dropped"

  ### Erlang

  ```erlang
  ?SYSTEM_NETWORK_DROPPED.
  'system.network.dropped'
  ```

  <!-- tabs-close -->
  """

  @spec system_network_dropped :: :"system.network.dropped"
  def system_network_dropped do
    :"system.network.dropped"
  end

  @doc """
  Count of network errors detected

  Instrument: `counter`
  Unit: `{error}`
  ### Notes

  Measured as:

  - Linux: the `errs` column in `/proc/dev/net` ([source](https://web.archive.org/web/20180321091318/http://www.onlamp.com/pub/a/linux/2000/11/16/LinuxAdmin.html)).
  - Windows: [`InErrors`/`OutErrors`](https://docs.microsoft.com/windows/win32/api/netioapi/ns-netioapi-mib_if_row2)
    from [`GetIfEntry2`](https://docs.microsoft.com/windows/win32/api/netioapi/nf-netioapi-getifentry2).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_network_errors()
      :"system.network.errors"

  ### Erlang

  ```erlang
  ?SYSTEM_NETWORK_ERRORS.
  'system.network.errors'
  ```

  <!-- tabs-close -->
  """

  @spec system_network_errors :: :"system.network.errors"
  def system_network_errors do
    :"system.network.errors"
  end

  @doc """
  none

  Instrument: `counter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_network_io()
      :"system.network.io"

  ### Erlang

  ```erlang
  ?SYSTEM_NETWORK_IO.
  'system.network.io'
  ```

  <!-- tabs-close -->
  """

  @spec system_network_io :: :"system.network.io"
  def system_network_io do
    :"system.network.io"
  end

  @doc """
  none

  Instrument: `counter`
  Unit: `{packet}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_network_packets()
      :"system.network.packets"

  ### Erlang

  ```erlang
  ?SYSTEM_NETWORK_PACKETS.
  'system.network.packets'
  ```

  <!-- tabs-close -->
  """

  @spec system_network_packets :: :"system.network.packets"
  def system_network_packets do
    :"system.network.packets"
  end

  @doc """
  none

  Instrument: `counter`
  Unit: `{fault}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_paging_faults()
      :"system.paging.faults"

  ### Erlang

  ```erlang
  ?SYSTEM_PAGING_FAULTS.
  'system.paging.faults'
  ```

  <!-- tabs-close -->
  """

  @spec system_paging_faults :: :"system.paging.faults"
  def system_paging_faults do
    :"system.paging.faults"
  end

  @doc """
  none

  Instrument: `counter`
  Unit: `{operation}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_paging_operations()
      :"system.paging.operations"

  ### Erlang

  ```erlang
  ?SYSTEM_PAGING_OPERATIONS.
  'system.paging.operations'
  ```

  <!-- tabs-close -->
  """

  @spec system_paging_operations :: :"system.paging.operations"
  def system_paging_operations do
    :"system.paging.operations"
  end

  @doc """
  Unix swap or windows pagefile usage

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_paging_usage()
      :"system.paging.usage"

  ### Erlang

  ```erlang
  ?SYSTEM_PAGING_USAGE.
  'system.paging.usage'
  ```

  <!-- tabs-close -->
  """

  @spec system_paging_usage :: :"system.paging.usage"
  def system_paging_usage do
    :"system.paging.usage"
  end

  @doc """
  none

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_paging_utilization()
      :"system.paging.utilization"

  ### Erlang

  ```erlang
  ?SYSTEM_PAGING_UTILIZATION.
  'system.paging.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec system_paging_utilization :: :"system.paging.utilization"
  def system_paging_utilization do
    :"system.paging.utilization"
  end

  @doc """
  Total number of processes in each state

  Instrument: `updowncounter`
  Unit: `{process}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_process_count()
      :"system.process.count"

  ### Erlang

  ```erlang
  ?SYSTEM_PROCESS_COUNT.
  'system.process.count'
  ```

  <!-- tabs-close -->
  """

  @spec system_process_count :: :"system.process.count"
  def system_process_count do
    :"system.process.count"
  end

  @doc """
  Total number of processes created over uptime of the host

  Instrument: `counter`
  Unit: `{process}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.SystemMetrics.system_process_created()
      :"system.process.created"

  ### Erlang

  ```erlang
  ?SYSTEM_PROCESS_CREATED.
  'system.process.created'
  ```

  <!-- tabs-close -->
  """

  @spec system_process_created :: :"system.process.created"
  def system_process_created do
    :"system.process.created"
  end
end
