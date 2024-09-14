defmodule OpenTelemetry.SemConv.Incubating.HostAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Host attributes.
  """

  @typedoc """
  The CPU architecture the host system is running on.


  ### Enum Values
  * `:amd64` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - AMD64
  * `:arm32` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - ARM32
  * `:arm64` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - ARM64
  * `:ia64` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Itanium
  * `:ppc32` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 32-bit PowerPC
  * `:ppc64` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 64-bit PowerPC
  * `:s390x` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IBM z/Architecture
  * `:x86` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 32-bit x86
  """
  @type host_arch_values() :: %{
          :amd64 => :amd64,
          :arm32 => :arm32,
          :arm64 => :arm64,
          :ia64 => :ia64,
          :ppc32 => :ppc32,
          :ppc64 => :ppc64,
          :s390x => :s390x,
          :x86 => :x86
        }
  @doc """
  The CPU architecture the host system is running on.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_arch()
      :"host.arch"

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_arch_values().amd64
      :amd64

      iex> %{OpenTelemetry.SemConv.Incubating.HostAttributes.host_arch() => OpenTelemetry.SemConv.Incubating.HostAttributes.host_arch_values().amd64}
      %{:"host.arch" => :amd64}

  ### Erlang

  ```erlang
  ?HOST_ARCH.
  'host.arch'

  ?HOST_ARCH_VALUES_AMD64.
  'amd64'

  \#{?HOST_ARCH => ?HOST_ARCH_VALUES_AMD64}.
  \#{'host.arch' => 'amd64'}
  ```

  <!-- tabs-close -->
  """
  @spec host_arch :: :"host.arch"
  def host_arch do
    :"host.arch"
  end

  @spec host_arch_values() :: host_arch_values()
  def host_arch_values() do
    %{
      :amd64 => :amd64,
      :arm32 => :arm32,
      :arm64 => :arm64,
      :ia64 => :ia64,
      :ppc32 => :ppc32,
      :ppc64 => :ppc64,
      :s390x => :s390x,
      :x86 => :x86
    }
  end

  @doc """
  The amount of level 2 memory cache available to the processor (in Bytes).

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [12288000]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_cpu_cache_l2_size()
      :"host.cpu.cache.l2.size"

  ### Erlang

  ```erlang
  ?HOST_CPU_CACHE_L2_SIZE.
  'host.cpu.cache.l2.size'
  ```

  <!-- tabs-close -->
  """
  @spec host_cpu_cache_l2_size :: :"host.cpu.cache.l2.size"
  def host_cpu_cache_l2_size do
    :"host.cpu.cache.l2.size"
  end

  @doc """
  Family or generation of the CPU.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["6", "PA-RISC 1.1e"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_cpu_family()
      :"host.cpu.family"

  ### Erlang

  ```erlang
  ?HOST_CPU_FAMILY.
  'host.cpu.family'
  ```

  <!-- tabs-close -->
  """
  @spec host_cpu_family :: :"host.cpu.family"
  def host_cpu_family do
    :"host.cpu.family"
  end

  @doc """
  Model identifier. It provides more granular information about the CPU, distinguishing it from other CPUs within the same family.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["6", "9000/778/B180L"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_cpu_model_id()
      :"host.cpu.model.id"

  ### Erlang

  ```erlang
  ?HOST_CPU_MODEL_ID.
  'host.cpu.model.id'
  ```

  <!-- tabs-close -->
  """
  @spec host_cpu_model_id :: :"host.cpu.model.id"
  def host_cpu_model_id do
    :"host.cpu.model.id"
  end

  @doc """
  Model designation of the processor.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_cpu_model_name()
      :"host.cpu.model.name"

  ### Erlang

  ```erlang
  ?HOST_CPU_MODEL_NAME.
  'host.cpu.model.name'
  ```

  <!-- tabs-close -->
  """
  @spec host_cpu_model_name :: :"host.cpu.model.name"
  def host_cpu_model_name do
    :"host.cpu.model.name"
  end

  @doc """
  Stepping or core revisions.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1", "r1p1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_cpu_stepping()
      :"host.cpu.stepping"

  ### Erlang

  ```erlang
  ?HOST_CPU_STEPPING.
  'host.cpu.stepping'
  ```

  <!-- tabs-close -->
  """
  @spec host_cpu_stepping :: :"host.cpu.stepping"
  def host_cpu_stepping do
    :"host.cpu.stepping"
  end

  @doc """
  Processor manufacturer identifier. A maximum 12-character string.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  [CPUID](https://wiki.osdev.org/CPUID) command returns the vendor ID string in EBX, EDX and ECX registers. Writing these to memory in this order results in a 12-character string.

  ### Examples

  ```
  ["GenuineIntel"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_cpu_vendor_id()
      :"host.cpu.vendor.id"

  ### Erlang

  ```erlang
  ?HOST_CPU_VENDOR_ID.
  'host.cpu.vendor.id'
  ```

  <!-- tabs-close -->
  """
  @spec host_cpu_vendor_id :: :"host.cpu.vendor.id"
  def host_cpu_vendor_id do
    :"host.cpu.vendor.id"
  end

  @doc """
  Unique host ID. For Cloud, this must be the instance_id assigned by the cloud provider. For non-containerized systems, this should be the `machine-id`. See the table below for the sources to use to determine the `machine-id` based on operating system.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["fdbf79e8af94cb7f9e8df36789187052"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_id()
      :"host.id"

  ### Erlang

  ```erlang
  ?HOST_ID.
  'host.id'
  ```

  <!-- tabs-close -->
  """
  @spec host_id :: :"host.id"
  def host_id do
    :"host.id"
  end

  @doc """
  VM image ID or host OS image ID. For Cloud, this value is from the provider.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["ami-07b06b442921831e5"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_image_id()
      :"host.image.id"

  ### Erlang

  ```erlang
  ?HOST_IMAGE_ID.
  'host.image.id'
  ```

  <!-- tabs-close -->
  """
  @spec host_image_id :: :"host.image.id"
  def host_image_id do
    :"host.image.id"
  end

  @doc """
  Name of the VM image or OS install the host was instantiated from.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["infra-ami-eks-worker-node-7d4ec78312", "CentOS-8-x86_64-1905"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_image_name()
      :"host.image.name"

  ### Erlang

  ```erlang
  ?HOST_IMAGE_NAME.
  'host.image.name'
  ```

  <!-- tabs-close -->
  """
  @spec host_image_name :: :"host.image.name"
  def host_image_name do
    :"host.image.name"
  end

  @doc """
  The version string of the VM image or host OS as defined in [Version Attributes](/docs/resource/README.md#version-attributes).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["0.1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_image_version()
      :"host.image.version"

  ### Erlang

  ```erlang
  ?HOST_IMAGE_VERSION.
  'host.image.version'
  ```

  <!-- tabs-close -->
  """
  @spec host_image_version :: :"host.image.version"
  def host_image_version do
    :"host.image.version"
  end

  @doc """
  Available IP addresses of the host, excluding loopback interfaces.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  IPv4 Addresses **MUST** be specified in dotted-quad notation. IPv6 addresses **MUST** be specified in the [RFC 5952](https://www.rfc-editor.org/rfc/rfc5952.html) format.

  ### Examples

  ```
  ["192.168.1.140", "fe80::abc2:4a28:737a:609e"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_ip()
      :"host.ip"

  ### Erlang

  ```erlang
  ?HOST_IP.
  'host.ip'
  ```

  <!-- tabs-close -->
  """
  @spec host_ip :: :"host.ip"
  def host_ip do
    :"host.ip"
  end

  @doc """
  Available MAC addresses of the host, excluding loopback interfaces.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  MAC Addresses **MUST** be represented in [IEEE RA hexadecimal form](https://standards.ieee.org/wp-content/uploads/import/documents/tutorials/eui.pdf): as hyphen-separated octets in uppercase hexadecimal form from most to least significant.

  ### Examples

  ```
  ["AC-DE-48-23-45-67", "AC-DE-48-23-45-67-01-9F"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_mac()
      :"host.mac"

  ### Erlang

  ```erlang
  ?HOST_MAC.
  'host.mac'
  ```

  <!-- tabs-close -->
  """
  @spec host_mac :: :"host.mac"
  def host_mac do
    :"host.mac"
  end

  @doc """
  Name of the host. On Unix systems, it may contain what the hostname command returns, or the fully qualified hostname, or another name specified by the user.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry-test"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_name()
      :"host.name"

  ### Erlang

  ```erlang
  ?HOST_NAME.
  'host.name'
  ```

  <!-- tabs-close -->
  """
  @spec host_name :: :"host.name"
  def host_name do
    :"host.name"
  end

  @doc """
  Type of host. For Cloud, this must be the machine type.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["n1-standard-1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HostAttributes.host_type()
      :"host.type"

  ### Erlang

  ```erlang
  ?HOST_TYPE.
  'host.type'
  ```

  <!-- tabs-close -->
  """
  @spec host_type :: :"host.type"
  def host_type do
    :"host.type"
  end
end
