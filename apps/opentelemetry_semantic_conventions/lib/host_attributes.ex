defmodule OpenTelemetry.SemanticConventions.HostAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Host attributes.
  """

  @typedoc """
  The CPU architecture the host system is running on.


  ### Options
  * `:amd64` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - AMD64
  * `:arm32` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ARM32
  * `:arm64` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ARM64
  * `:ia64` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Itanium
  * `:ppc32` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 32-bit PowerPC
  * `:ppc64` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 64-bit PowerPC
  * `:s390x` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IBM z/Architecture
  * `:x86` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 32-bit x86

  """
  @type host_arch() :: :amd64 | :arm32 | :arm64 | :ia64 | :ppc32 | :ppc64 | :s390x | :x86 | atom()

  @doc """
  The CPU architecture the host system is running on.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_arch(:amd64)
      :amd64
      
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_arch(:custom_value)
      :custom_value
  """
  @spec host_arch(host_arch()) ::
          :amd64 | :arm32 | :arm64 | :ia64 | :ppc32 | :ppc64 | :s390x | :x86 | atom()
  def host_arch(option) do
    :"host.arch"

    case option do
      :amd64 -> :amd64
      :arm32 -> :arm32
      :arm64 -> :arm64
      :ia64 -> :ia64
      :ppc32 -> :ppc32
      :ppc64 -> :ppc64
      :s390x -> :s390x
      :x86 -> :x86
      _ -> option
    end
  end

  @doc """
  The amount of level 2 memory cache available to the processor (in Bytes).



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_cpu_cache_l_2_size()
      :"host.cpu.cache.l2.size"
  """
  @spec host_cpu_cache_l_2_size :: :"host.cpu.cache.l2.size"
  def host_cpu_cache_l_2_size do
    :"host.cpu.cache.l2.size"
  end

  @doc """
  Family or generation of the CPU.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_cpu_family()
      :"host.cpu.family"
  """
  @spec host_cpu_family :: :"host.cpu.family"
  def host_cpu_family do
    :"host.cpu.family"
  end

  @doc """
  Model identifier. It provides more granular information about the CPU, distinguishing it from other CPUs within the same family.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_cpu_model_id()
      :"host.cpu.model.id"
  """
  @spec host_cpu_model_id :: :"host.cpu.model.id"
  def host_cpu_model_id do
    :"host.cpu.model.id"
  end

  @doc """
  Model designation of the processor.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_cpu_model_name()
      :"host.cpu.model.name"
  """
  @spec host_cpu_model_name :: :"host.cpu.model.name"
  def host_cpu_model_name do
    :"host.cpu.model.name"
  end

  @doc """
  Stepping or core revisions.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_cpu_stepping()
      :"host.cpu.stepping"
  """
  @spec host_cpu_stepping :: :"host.cpu.stepping"
  def host_cpu_stepping do
    :"host.cpu.stepping"
  end

  @doc """
  Processor manufacturer identifier. A maximum 12-character string.

  ### Notes

  [CPUID](https://wiki.osdev.org/CPUID) command returns the vendor ID string in EBX, EDX and ECX registers. Writing these to memory in this order results in a 12-character string.


  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_cpu_vendor_id()
      :"host.cpu.vendor.id"
  """
  @spec host_cpu_vendor_id :: :"host.cpu.vendor.id"
  def host_cpu_vendor_id do
    :"host.cpu.vendor.id"
  end

  @doc """
  Unique host ID. For Cloud, this must be the instance_id assigned by the cloud provider. For non-containerized systems, this should be the `machine-id`. See the table below for the sources to use to determine the `machine-id` based on operating system.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_id()
      :"host.id"
  """
  @spec host_id :: :"host.id"
  def host_id do
    :"host.id"
  end

  @doc """
  VM image ID or host OS image ID. For Cloud, this value is from the provider.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_image_id()
      :"host.image.id"
  """
  @spec host_image_id :: :"host.image.id"
  def host_image_id do
    :"host.image.id"
  end

  @doc """
  Name of the VM image or OS install the host was instantiated from.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_image_name()
      :"host.image.name"
  """
  @spec host_image_name :: :"host.image.name"
  def host_image_name do
    :"host.image.name"
  end

  @doc """
  The version string of the VM image or host OS as defined in [Version Attributes](/docs/resource/README.md#version-attributes).



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_image_version()
      :"host.image.version"
  """
  @spec host_image_version :: :"host.image.version"
  def host_image_version do
    :"host.image.version"
  end

  @doc """
  Available IP addresses of the host, excluding loopback interfaces.

  ### Notes

  IPv4 Addresses **MUST** be specified in dotted-quad notation. IPv6 addresses **MUST** be specified in the [RFC 5952](https://www.rfc-editor.org/rfc/rfc5952.html) format.


  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_ip()
      :"host.ip"
  """
  @spec host_ip :: :"host.ip"
  def host_ip do
    :"host.ip"
  end

  @doc """
  Available MAC addresses of the host, excluding loopback interfaces.

  ### Notes

  MAC Addresses **MUST** be represented in [IEEE RA hexadecimal form](https://standards.ieee.org/wp-content/uploads/import/documents/tutorials/eui.pdf): as hyphen-separated octets in uppercase hexadecimal form from most to least significant.


  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_mac()
      :"host.mac"
  """
  @spec host_mac :: :"host.mac"
  def host_mac do
    :"host.mac"
  end

  @doc """
  Name of the host. On Unix systems, it may contain what the hostname command returns, or the fully qualified hostname, or another name specified by the user.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_name()
      :"host.name"
  """
  @spec host_name :: :"host.name"
  def host_name do
    :"host.name"
  end

  @doc """
  Type of host. For Cloud, this must be the machine type.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HostAttributes.host_type()
      :"host.type"
  """
  @spec host_type :: :"host.type"
  def host_type do
    :"host.type"
  end
end
