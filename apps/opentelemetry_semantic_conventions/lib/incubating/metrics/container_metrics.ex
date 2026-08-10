defmodule OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Container metrics.
  """
  @doc """
  Total CPU time consumed.

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
  Container's CPU usage, measured in cpus. Range from 0 to the number of allocatable CPUs.

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
  Container filesystem available bytes.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  In K8s, this metric is derived from the
  [FsStats.AvailableBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#FsStats) field
  of the [ContainerStats.Rootfs](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#ContainerStats)
  of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_filesystem_available()
      :"container.filesystem.available"

  ### Erlang

  ```erlang
  ?CONTAINER_FILESYSTEM_AVAILABLE.
  'container.filesystem.available'
  ```

  <!-- tabs-close -->
  """

  @spec container_filesystem_available :: :"container.filesystem.available"
  def container_filesystem_available do
    :"container.filesystem.available"
  end

  @doc """
  Container filesystem capacity.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  In K8s, this metric is derived from the
  [FsStats.CapacityBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#FsStats) field
  of the [ContainerStats.Rootfs](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#ContainerStats)
  of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_filesystem_capacity()
      :"container.filesystem.capacity"

  ### Erlang

  ```erlang
  ?CONTAINER_FILESYSTEM_CAPACITY.
  'container.filesystem.capacity'
  ```

  <!-- tabs-close -->
  """

  @spec container_filesystem_capacity :: :"container.filesystem.capacity"
  def container_filesystem_capacity do
    :"container.filesystem.capacity"
  end

  @doc """
  Container filesystem usage.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This may not equal capacity - available.

  In K8s, this metric is derived from the
  [FsStats.UsedBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#FsStats) field
  of the [ContainerStats.Rootfs](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#ContainerStats)
  of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_filesystem_usage()
      :"container.filesystem.usage"

  ### Erlang

  ```erlang
  ?CONTAINER_FILESYSTEM_USAGE.
  'container.filesystem.usage'
  ```

  <!-- tabs-close -->
  """

  @spec container_filesystem_usage :: :"container.filesystem.usage"
  def container_filesystem_usage do
    :"container.filesystem.usage"
  end

  @doc """
  Container memory available.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  Available memory for use.  This is defined as the memory limit - workingSetBytes. If memory limit is undefined, the available bytes is omitted.
  In general, this metric can be derived from [cadvisor](https://github.com/google/cadvisor/blob/v0.53.0/docs/storage/prometheus.md#prometheus-container-metrics) and by subtracting the `container_memory_working_set_bytes` metric from the `container_spec_memory_limit_bytes` metric.
  In K8s, this metric is derived from the [MemoryStats.AvailableBytes](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [PodStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#PodStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_memory_available()
      :"container.memory.available"

  ### Erlang

  ```erlang
  ?CONTAINER_MEMORY_AVAILABLE.
  'container.memory.available'
  ```

  <!-- tabs-close -->
  """

  @spec container_memory_available :: :"container.memory.available"
  def container_memory_available do
    :"container.memory.available"
  end

  @doc """
  Container memory paging faults.

  Instrument: `counter`
  Unit: `{fault}`
  ### Notes

  In general, this metric can be derived from [cadvisor](https://github.com/google/cadvisor/blob/v0.53.0/docs/storage/prometheus.md#prometheus-container-metrics) and specifically the `container_memory_failures_total{failure_type=pgfault, scope=container}` and `container_memory_failures_total{failure_type=pgmajfault, scope=container}`metric.
  In K8s, this metric is derived from the [MemoryStats.PageFaults](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) and [MemoryStats.MajorPageFaults](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [PodStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#PodStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_memory_paging_faults()
      :"container.memory.paging.faults"

  ### Erlang

  ```erlang
  ?CONTAINER_MEMORY_PAGING_FAULTS.
  'container.memory.paging.faults'
  ```

  <!-- tabs-close -->
  """

  @spec container_memory_paging_faults :: :"container.memory.paging.faults"
  def container_memory_paging_faults do
    :"container.memory.paging.faults"
  end

  @doc """
  Container memory RSS.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  In general, this metric can be derived from [cadvisor](https://github.com/google/cadvisor/blob/v0.53.0/docs/storage/prometheus.md#prometheus-container-metrics) and specifically the `container_memory_rss` metric.
  In K8s, this metric is derived from the [MemoryStats.RSSBytes](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [PodStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#PodStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_memory_rss()
      :"container.memory.rss"

  ### Erlang

  ```erlang
  ?CONTAINER_MEMORY_RSS.
  'container.memory.rss'
  ```

  <!-- tabs-close -->
  """

  @spec container_memory_rss :: :"container.memory.rss"
  def container_memory_rss do
    :"container.memory.rss"
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
  Container memory working set.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  In general, this metric can be derived from [cadvisor](https://github.com/google/cadvisor/blob/v0.53.0/docs/storage/prometheus.md#prometheus-container-metrics) and specifically the `container_memory_working_set_bytes` metric.
  In K8s, this metric is derived from the [MemoryStats.WorkingSetBytes](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [PodStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#PodStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.ContainerMetrics.container_memory_working_set()
      :"container.memory.working_set"

  ### Erlang

  ```erlang
  ?CONTAINER_MEMORY_WORKING_SET.
  'container.memory.working_set'
  ```

  <!-- tabs-close -->
  """

  @spec container_memory_working_set :: :"container.memory.working_set"
  def container_memory_working_set do
    :"container.memory.working_set"
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
  The time the container has been running.

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
