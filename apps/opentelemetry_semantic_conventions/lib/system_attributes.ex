defmodule OpenTelemetry.SemanticConventions.SystemAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for System attributes.
  """

  @doc """
  The logical CPU number [0..n-1]


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_cpu_logicalnumber()
      :"system.cpu.logical_number"
  """
  @spec system_cpu_logicalnumber :: :"system.cpu.logical_number"
  def system_cpu_logicalnumber do
    :"system.cpu.logical_number"
  end

  @typedoc """
  The state of the CPU

  ### Options
  * `:user` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:system` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:nice` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:idle` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:iowait` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:interrupt` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:steal` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type system_cpu_state() ::
          :user | :system | :nice | :idle | :iowait | :interrupt | :steal | atom()

  @doc """
  The state of the CPU


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_cpu_state(:user)
      :user
      
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_cpu_state(:custom_value)
      :custom_value
  """
  @spec system_cpu_state(system_cpu_state()) ::
          :user | :system | :nice | :idle | :iowait | :interrupt | :steal | atom()
  def system_cpu_state(option) do
    case option do
      :user -> :user
      :system -> :system
      :nice -> :nice
      :idle -> :idle
      :iowait -> :iowait
      :interrupt -> :interrupt
      :steal -> :steal
      _ -> option
    end
  end

  @doc """
  The device identifier


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_device()
      :"system.device"
  """
  @spec system_device :: :"system.device"
  def system_device do
    :"system.device"
  end

  @doc """
  The filesystem mode


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_filesystem_mode()
      :"system.filesystem.mode"
  """
  @spec system_filesystem_mode :: :"system.filesystem.mode"
  def system_filesystem_mode do
    :"system.filesystem.mode"
  end

  @doc """
  The filesystem mount path


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_filesystem_mountpoint()
      :"system.filesystem.mountpoint"
  """
  @spec system_filesystem_mountpoint :: :"system.filesystem.mountpoint"
  def system_filesystem_mountpoint do
    :"system.filesystem.mountpoint"
  end

  @typedoc """
  The filesystem state

  ### Options
  * `:used` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:free` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:reserved` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type system_filesystem_state() :: :used | :free | :reserved | atom()

  @doc """
  The filesystem state


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_filesystem_state(:used)
      :used
      
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_filesystem_state(:custom_value)
      :custom_value
  """
  @spec system_filesystem_state(system_filesystem_state()) :: :used | :free | :reserved | atom()
  def system_filesystem_state(option) do
    case option do
      :used -> :used
      :free -> :free
      :reserved -> :reserved
      _ -> option
    end
  end

  @typedoc """
  The filesystem type

  ### Options
  * `:fat32` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:exfat` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:ntfs` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:refs` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:hfsplus` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:ext4` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type system_filesystem_type() :: :fat32 | :exfat | :ntfs | :refs | :hfsplus | :ext4 | atom()

  @doc """
  The filesystem type


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_filesystem_type(:fat32)
      :fat32
      
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_filesystem_type(:custom_value)
      :custom_value
  """
  @spec system_filesystem_type(system_filesystem_type()) ::
          :fat32 | :exfat | :ntfs | :refs | :hfsplus | :ext4 | atom()
  def system_filesystem_type(option) do
    case option do
      :fat32 -> :fat32
      :exfat -> :exfat
      :ntfs -> :ntfs
      :refs -> :refs
      :hfsplus -> :hfsplus
      :ext4 -> :ext4
      _ -> option
    end
  end

  @typedoc """
  The memory state

  ### Options
  * `:used` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:free` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:shared` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:buffers` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:cached` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type system_memory_state() :: :used | :free | :shared | :buffers | :cached | atom()

  @doc """
  The memory state


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_memory_state(:used)
      :used
      
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_memory_state(:custom_value)
      :custom_value
  """
  @spec system_memory_state(system_memory_state()) ::
          :used | :free | :shared | :buffers | :cached | atom()
  def system_memory_state(option) do
    case option do
      :used -> :used
      :free -> :free
      :shared -> :shared
      :buffers -> :buffers
      :cached -> :cached
      _ -> option
    end
  end

  @typedoc """
  A stateless protocol **MUST** **NOT** set this attribute

  ### Options
  * `:close` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:close_wait` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:closing` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:delete` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:established` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:fin_wait_1` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:fin_wait_2` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:last_ack` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:listen` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:syn_recv` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:syn_sent` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:time_wait` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type system_network_state() ::
          :close
          | :close_wait
          | :closing
          | :delete
          | :established
          | :fin_wait_1
          | :fin_wait_2
          | :last_ack
          | :listen
          | :syn_recv
          | :syn_sent
          | :time_wait
          | atom()

  @doc """
  A stateless protocol **MUST** **NOT** set this attribute


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_network_state(:close)
      :close
      
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_network_state(:custom_value)
      :custom_value
  """
  @spec system_network_state(system_network_state()) ::
          :close
          | :close_wait
          | :closing
          | :delete
          | :established
          | :fin_wait_1
          | :fin_wait_2
          | :last_ack
          | :listen
          | :syn_recv
          | :syn_sent
          | :time_wait
          | atom()
  def system_network_state(option) do
    case option do
      :close -> :close
      :close_wait -> :close_wait
      :closing -> :closing
      :delete -> :delete
      :established -> :established
      :fin_wait_1 -> :fin_wait_1
      :fin_wait_2 -> :fin_wait_2
      :last_ack -> :last_ack
      :listen -> :listen
      :syn_recv -> :syn_recv
      :syn_sent -> :syn_sent
      :time_wait -> :time_wait
      _ -> option
    end
  end

  @typedoc """
  The paging access direction

  ### Options
  * `:in` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:out` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type system_paging_direction() :: :in | :out | atom()

  @doc """
  The paging access direction


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_paging_direction(:in)
      :in
      
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_paging_direction(:custom_value)
      :custom_value
  """
  @spec system_paging_direction(system_paging_direction()) :: :in | :out | atom()
  def system_paging_direction(option) do
    case option do
      :in -> :in
      :out -> :out
      _ -> option
    end
  end

  @typedoc """
  The memory paging state

  ### Options
  * `:used` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:free` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type system_paging_state() :: :used | :free | atom()

  @doc """
  The memory paging state


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_paging_state(:used)
      :used
      
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_paging_state(:custom_value)
      :custom_value
  """
  @spec system_paging_state(system_paging_state()) :: :used | :free | atom()
  def system_paging_state(option) do
    case option do
      :used -> :used
      :free -> :free
      _ -> option
    end
  end

  @typedoc """
  The memory paging type

  ### Options
  * `:major` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:minor` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type system_paging_type() :: :major | :minor | atom()

  @doc """
  The memory paging type


  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_paging_type(:major)
      :major
      
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_paging_type(:custom_value)
      :custom_value
  """
  @spec system_paging_type(system_paging_type()) :: :major | :minor | atom()
  def system_paging_type(option) do
    case option do
      :major -> :major
      :minor -> :minor
      _ -> option
    end
  end

  @typedoc """
  The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)


  ### Options
  * `:running` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:sleeping` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:stopped` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:defunct` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type system_process_status() :: :running | :sleeping | :stopped | :defunct | atom()

  @doc """
  The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)



  ### Example
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_process_status(:running)
      :running
      
      iex> OpenTelemetry.SemanticConventions.SystemAttributes.system_process_status(:custom_value)
      :custom_value
  """
  @spec system_process_status(system_process_status()) ::
          :running | :sleeping | :stopped | :defunct | atom()
  def system_process_status(option) do
    case option do
      :running -> :running
      :sleeping -> :sleeping
      :stopped -> :stopped
      :defunct -> :defunct
      _ -> option
    end
  end

  @typedoc """
  Deprecated, use `system.process.status` instead.

  ### Options
  * `:running` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:sleeping` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:stopped` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:defunct` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type system_processes_status() :: :running | :sleeping | :stopped | :defunct | atom()

  @deprecated """
  Replaced by `system.process.status`.
  """

  @spec system_processes_status(system_processes_status()) ::
          :running | :sleeping | :stopped | :defunct | atom()
  def system_processes_status(option) do
    case option do
      :running -> :running
      :sleeping -> :sleeping
      :stopped -> :stopped
      :defunct -> :defunct
      _ -> option
    end
  end
end
