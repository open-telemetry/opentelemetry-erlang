defmodule OpenTelemetry.SemanticConventions.OsAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Os attributes.
  """

  @doc """
  Unique identifier for a particular build or compilation of the operating system.


  ### Example
      iex> OpenTelemetry.SemanticConventions.OsAttributes.os_buildid()
      :"os.build_id"
  """
  @spec os_buildid :: :"os.build_id"
  def os_buildid do
    :"os.build_id"
  end

  @doc """
  Human readable (not intended to be parsed) OS version information, like e.g. reported by `ver` or `lsb_release -a` commands.



  ### Example
      iex> OpenTelemetry.SemanticConventions.OsAttributes.os_description()
      :"os.description"
  """
  @spec os_description :: :"os.description"
  def os_description do
    :"os.description"
  end

  @doc """
  Human readable operating system name.


  ### Example
      iex> OpenTelemetry.SemanticConventions.OsAttributes.os_name()
      :"os.name"
  """
  @spec os_name :: :"os.name"
  def os_name do
    :"os.name"
  end

  @typedoc """
  The operating system type.


  ### Enum Values
  * `:windows` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Microsoft Windows
  * `:linux` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Linux
  * `:darwin` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apple Darwin
  * `:freebsd` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - FreeBSD
  * `:netbsd` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - NetBSD
  * `:openbsd` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - OpenBSD
  * `:dragonflybsd` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - DragonFly BSD
  * `:hpux` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HP-UX (Hewlett Packard Unix)
  * `:aix` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - AIX (Advanced Interactive eXecutive)
  * `:solaris` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - SunOS, Oracle Solaris
  * `:z_os` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IBM z/OS
  """
  @type os_type() :: %{
          :windows => :windows,
          :linux => :linux,
          :darwin => :darwin,
          :freebsd => :freebsd,
          :netbsd => :netbsd,
          :openbsd => :openbsd,
          :dragonflybsd => :dragonflybsd,
          :hpux => :hpux,
          :aix => :aix,
          :solaris => :solaris,
          :z_os => :z_os
        }
  @doc """
  The operating system type.



  ### Example
      iex> OpenTelemetry.SemanticConventions.OsAttributes.os_type().windows
      :windows
      
      iex> OpenTelemetry.SemanticConventions.OsAttributes.os_type(:custom_value)
      :custom_value
  """
  @spec os_type() :: os_type()
  def os_type() do
    %{
      :windows => :windows,
      :linux => :linux,
      :darwin => :darwin,
      :freebsd => :freebsd,
      :netbsd => :netbsd,
      :openbsd => :openbsd,
      :dragonflybsd => :dragonflybsd,
      :hpux => :hpux,
      :aix => :aix,
      :solaris => :solaris,
      :z_os => :z_os
    }
  end

  @spec os_type(atom() | String.t()) :: atom() | String.t()
  def os_type(custom_value) do
    custom_value
  end

  @doc """
  The version string of the operating system as defined in [Version Attributes](/docs/resource/README.md#version-attributes).



  ### Example
      iex> OpenTelemetry.SemanticConventions.OsAttributes.os_version()
      :"os.version"
  """
  @spec os_version :: :"os.version"
  def os_version do
    :"os.version"
  end
end
