defmodule OpenTelemetry.SemConv.Incubating.OSAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for OS attributes.
  """

  @doc """
  Unique identifier for a particular build or compilation of the operating system.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["TQ3C.230805.001.B2", "20E247", "22621"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OSAttributes.os_build_id()
      :"os.build_id"

  ### Erlang

  ```erlang
  ?OS_BUILD_ID.
  'os.build_id'
  ```

  <!-- tabs-close -->
  """
  @spec os_build_id :: :"os.build_id"
  def os_build_id do
    :"os.build_id"
  end

  @doc """
  Human readable (not intended to be parsed) OS version information, like e.g. reported by `ver` or `lsb_release -a` commands.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Microsoft Windows [Version 10.0.18363.778]", "Ubuntu 18.04.1 LTS"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OSAttributes.os_description()
      :"os.description"

  ### Erlang

  ```erlang
  ?OS_DESCRIPTION.
  'os.description'
  ```

  <!-- tabs-close -->
  """
  @spec os_description :: :"os.description"
  def os_description do
    :"os.description"
  end

  @doc """
  Human readable operating system name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["iOS", "Android", "Ubuntu"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OSAttributes.os_name()
      :"os.name"

  ### Erlang

  ```erlang
  ?OS_NAME.
  'os.name'
  ```

  <!-- tabs-close -->
  """
  @spec os_name :: :"os.name"
  def os_name do
    :"os.name"
  end

  @typedoc """
  The operating system type.


  ### Enum Values
  * `:windows` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Microsoft Windows
  * `:linux` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Linux
  * `:darwin` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apple Darwin
  * `:freebsd` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - FreeBSD
  * `:netbsd` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - NetBSD
  * `:openbsd` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OpenBSD
  * `:dragonflybsd` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - DragonFly BSD
  * `:hpux` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HP-UX (Hewlett Packard Unix)
  * `:aix` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - AIX (Advanced Interactive eXecutive)
  * `:solaris` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - SunOS, Oracle Solaris
  * `:z_os` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IBM z/OS
  """
  @type os_type_values() :: %{
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



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OSAttributes.os_type()
      :"os.type"

      iex> OpenTelemetry.SemConv.Incubating.OSAttributes.os_type_values().windows
      :windows

      iex> %{OpenTelemetry.SemConv.Incubating.OSAttributes.os_type() => OpenTelemetry.SemConv.Incubating.OSAttributes.os_type_values().windows}
      %{:"os.type" => :windows}

  ### Erlang

  ```erlang
  ?OS_TYPE.
  'os.type'

  ?OS_TYPE_VALUES_WINDOWS.
  'windows'

  \#{?OS_TYPE => ?OS_TYPE_VALUES_WINDOWS}.
  \#{'os.type' => 'windows'}
  ```

  <!-- tabs-close -->
  """
  @spec os_type :: :"os.type"
  def os_type do
    :"os.type"
  end

  @spec os_type_values() :: os_type_values()
  def os_type_values() do
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

  @doc """
  The version string of the operating system as defined in [Version Attributes](/docs/resource/README.md#version-attributes).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["14.2.1", "18.04.1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OSAttributes.os_version()
      :"os.version"

  ### Erlang

  ```erlang
  ?OS_VERSION.
  'os.version'
  ```

  <!-- tabs-close -->
  """
  @spec os_version :: :"os.version"
  def os_version do
    :"os.version"
  end
end
