defmodule OpenTelemetry.SemConv.Incubating.SystemAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for System attributes.
  """

  @doc """
  The logical CPU number [0..n-1]
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [1]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_cpu_logicalnumber()
      :"system.cpu.logical_number"

  ### Erlang

  ```erlang
  ?SYSTEM_CPU_LOGICALNUMBER.
  'system.cpu.logical_number'
  ```

  <!-- tabs-close -->
  """
  @spec system_cpu_logicalnumber :: :"system.cpu.logical_number"
  def system_cpu_logicalnumber do
    :"system.cpu.logical_number"
  end

  @typedoc """
  The state of the CPU

  ### Enum Values
  * `:user` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:system` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:nice` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:idle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:iowait` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:interrupt` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:steal` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type system_cpu_state() :: %{
          :user => :user,
          :system => :system,
          :nice => :nice,
          :idle => :idle,
          :iowait => :iowait,
          :interrupt => :interrupt,
          :steal => :steal
        }
  @doc """
  The state of the CPU

  ### Examples

  ```
  ["idle", "interrupt"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_cpu_state().user
      :user
      
      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_cpu_state(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'system_cpu_state.user'.
  user

  ?system_cpu_state(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec system_cpu_state() :: system_cpu_state()
  def system_cpu_state() do
    %{
      :user => :user,
      :system => :system,
      :nice => :nice,
      :idle => :idle,
      :iowait => :iowait,
      :interrupt => :interrupt,
      :steal => :steal
    }
  end

  @spec system_cpu_state(atom() | String.t()) :: atom() | String.t()
  def system_cpu_state(custom_value) do
    custom_value
  end

  @doc """
  The device identifier
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["(identifier)"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_device()
      :"system.device"

  ### Erlang

  ```erlang
  ?SYSTEM_DEVICE.
  'system.device'
  ```

  <!-- tabs-close -->
  """
  @spec system_device :: :"system.device"
  def system_device do
    :"system.device"
  end

  @doc """
  The filesystem mode
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["rw, ro"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_filesystem_mode()
      :"system.filesystem.mode"

  ### Erlang

  ```erlang
  ?SYSTEM_FILESYSTEM_MODE.
  'system.filesystem.mode'
  ```

  <!-- tabs-close -->
  """
  @spec system_filesystem_mode :: :"system.filesystem.mode"
  def system_filesystem_mode do
    :"system.filesystem.mode"
  end

  @doc """
  The filesystem mount path
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["/mnt/data"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_filesystem_mountpoint()
      :"system.filesystem.mountpoint"

  ### Erlang

  ```erlang
  ?SYSTEM_FILESYSTEM_MOUNTPOINT.
  'system.filesystem.mountpoint'
  ```

  <!-- tabs-close -->
  """
  @spec system_filesystem_mountpoint :: :"system.filesystem.mountpoint"
  def system_filesystem_mountpoint do
    :"system.filesystem.mountpoint"
  end

  @typedoc """
  The filesystem state

  ### Enum Values
  * `:used` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:free` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:reserved` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type system_filesystem_state() :: %{
          :used => :used,
          :free => :free,
          :reserved => :reserved
        }
  @doc """
  The filesystem state

  ### Examples

  ```
  ["used"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_filesystem_state().used
      :used
      
      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_filesystem_state(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'system_filesystem_state.used'.
  used

  ?system_filesystem_state(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec system_filesystem_state() :: system_filesystem_state()
  def system_filesystem_state() do
    %{
      :used => :used,
      :free => :free,
      :reserved => :reserved
    }
  end

  @spec system_filesystem_state(atom() | String.t()) :: atom() | String.t()
  def system_filesystem_state(custom_value) do
    custom_value
  end

  @typedoc """
  The filesystem type

  ### Enum Values
  * `:fat32` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:exfat` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:ntfs` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:refs` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:hfsplus` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:ext4` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type system_filesystem_type() :: %{
          :fat32 => :fat32,
          :exfat => :exfat,
          :ntfs => :ntfs,
          :refs => :refs,
          :hfsplus => :hfsplus,
          :ext4 => :ext4
        }
  @doc """
  The filesystem type

  ### Examples

  ```
  ["ext4"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_filesystem_type().fat32
      :fat32
      
      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_filesystem_type(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'system_filesystem_type.fat32'.
  fat32

  ?system_filesystem_type(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec system_filesystem_type() :: system_filesystem_type()
  def system_filesystem_type() do
    %{
      :fat32 => :fat32,
      :exfat => :exfat,
      :ntfs => :ntfs,
      :refs => :refs,
      :hfsplus => :hfsplus,
      :ext4 => :ext4
    }
  end

  @spec system_filesystem_type(atom() | String.t()) :: atom() | String.t()
  def system_filesystem_type(custom_value) do
    custom_value
  end

  @typedoc """
  The memory state

  ### Enum Values
  * `:used` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:free` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:shared` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:buffers` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:cached` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type system_memory_state() :: %{
          :used => :used,
          :free => :free,
          :shared => :shared,
          :buffers => :buffers,
          :cached => :cached
        }
  @doc """
  The memory state

  ### Examples

  ```
  ["free", "cached"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_memory_state().used
      :used
      
      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_memory_state(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'system_memory_state.used'.
  used

  ?system_memory_state(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec system_memory_state() :: system_memory_state()
  def system_memory_state() do
    %{
      :used => :used,
      :free => :free,
      :shared => :shared,
      :buffers => :buffers,
      :cached => :cached
    }
  end

  @spec system_memory_state(atom() | String.t()) :: atom() | String.t()
  def system_memory_state(custom_value) do
    custom_value
  end

  @typedoc """
  A stateless protocol **MUST** **NOT** set this attribute

  ### Enum Values
  * `:close` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:close_wait` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:closing` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:delete` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:established` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:fin_wait_1` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:fin_wait_2` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:last_ack` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:listen` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:syn_recv` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:syn_sent` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:time_wait` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type system_network_state() :: %{
          :close => :close,
          :close_wait => :close_wait,
          :closing => :closing,
          :delete => :delete,
          :established => :established,
          :fin_wait_1 => :fin_wait_1,
          :fin_wait_2 => :fin_wait_2,
          :last_ack => :last_ack,
          :listen => :listen,
          :syn_recv => :syn_recv,
          :syn_sent => :syn_sent,
          :time_wait => :time_wait
        }
  @doc """
  A stateless protocol **MUST** **NOT** set this attribute

  ### Examples

  ```
  ["close_wait"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_network_state().close
      :close
      
      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_network_state(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'system_network_state.close'.
  close

  ?system_network_state(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec system_network_state() :: system_network_state()
  def system_network_state() do
    %{
      :close => :close,
      :close_wait => :close_wait,
      :closing => :closing,
      :delete => :delete,
      :established => :established,
      :fin_wait_1 => :fin_wait_1,
      :fin_wait_2 => :fin_wait_2,
      :last_ack => :last_ack,
      :listen => :listen,
      :syn_recv => :syn_recv,
      :syn_sent => :syn_sent,
      :time_wait => :time_wait
    }
  end

  @spec system_network_state(atom() | String.t()) :: atom() | String.t()
  def system_network_state(custom_value) do
    custom_value
  end

  @typedoc """
  The paging access direction

  ### Enum Values
  * `:in` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:out` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type system_paging_direction() :: %{
          :in => :in,
          :out => :out
        }
  @doc """
  The paging access direction

  ### Examples

  ```
  ["in"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_paging_direction().in
      :in
      
      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_paging_direction(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'system_paging_direction.in'.
  in

  ?system_paging_direction(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec system_paging_direction() :: system_paging_direction()
  def system_paging_direction() do
    %{
      :in => :in,
      :out => :out
    }
  end

  @spec system_paging_direction(atom() | String.t()) :: atom() | String.t()
  def system_paging_direction(custom_value) do
    custom_value
  end

  @typedoc """
  The memory paging state

  ### Enum Values
  * `:used` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:free` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type system_paging_state() :: %{
          :used => :used,
          :free => :free
        }
  @doc """
  The memory paging state

  ### Examples

  ```
  ["free"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_paging_state().used
      :used
      
      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_paging_state(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'system_paging_state.used'.
  used

  ?system_paging_state(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec system_paging_state() :: system_paging_state()
  def system_paging_state() do
    %{
      :used => :used,
      :free => :free
    }
  end

  @spec system_paging_state(atom() | String.t()) :: atom() | String.t()
  def system_paging_state(custom_value) do
    custom_value
  end

  @typedoc """
  The memory paging type

  ### Enum Values
  * `:major` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:minor` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type system_paging_type() :: %{
          :major => :major,
          :minor => :minor
        }
  @doc """
  The memory paging type

  ### Examples

  ```
  ["minor"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_paging_type().major
      :major
      
      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_paging_type(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'system_paging_type.major'.
  major

  ?system_paging_type(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec system_paging_type() :: system_paging_type()
  def system_paging_type() do
    %{
      :major => :major,
      :minor => :minor
    }
  end

  @spec system_paging_type(atom() | String.t()) :: atom() | String.t()
  def system_paging_type(custom_value) do
    custom_value
  end

  @typedoc """
  The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)


  ### Enum Values
  * `:running` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:sleeping` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:stopped` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:defunct` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type system_process_status() :: %{
          :running => :running,
          :sleeping => :sleeping,
          :stopped => :stopped,
          :defunct => :defunct
        }
  @doc """
  The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)


  ### Examples

  ```
  ["running"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_process_status().running
      :running
      
      iex> OpenTelemetry.SemConv.Incubating.SystemAttributes.system_process_status(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'system_process_status.running'.
  running

  ?system_process_status(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec system_process_status() :: system_process_status()
  def system_process_status() do
    %{
      :running => :running,
      :sleeping => :sleeping,
      :stopped => :stopped,
      :defunct => :defunct
    }
  end

  @spec system_process_status(atom() | String.t()) :: atom() | String.t()
  def system_process_status(custom_value) do
    custom_value
  end

  @typedoc """
  Deprecated, use `system.process.status` instead.

  ### Enum Values
  * `:running` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:sleeping` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:stopped` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:defunct` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type system_processes_status() :: %{
          :running => :running,
          :sleeping => :sleeping,
          :stopped => :stopped,
          :defunct => :defunct
        }
  @deprecated """
  Replaced by `system.process.status`.
  """
  @spec system_processes_status() :: system_processes_status()
  def system_processes_status() do
    %{
      :running => :running,
      :sleeping => :sleeping,
      :stopped => :stopped,
      :defunct => :defunct
    }
  end

  @spec system_processes_status(atom() | String.t()) :: atom() | String.t()
  def system_processes_status(custom_value) do
    custom_value
  end
end
