defmodule OpenTelemetry.SemConv.Incubating.ProcessAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Process attributes.
  """

  @doc """
  The command used to launch the process (i.e. the command name). On Linux based systems, can be set to the zeroth string in `proc/[pid]/cmdline`. On Windows, can be set to the first parameter extracted from `GetCommandLineW`.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["cmd/otelcol"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_command()
      :"process.command"

  ### Erlang

  ```erlang
  ?PROCESS_COMMAND.
  'process.command'
  ```

  <!-- tabs-close -->
  """
  @spec process_command :: :"process.command"
  def process_command do
    :"process.command"
  end

  @doc """
  All the command arguments (including the command/executable itself) as received by the process. On Linux-based systems (and some other Unixoid systems supporting procfs), can be set according to the list of null-delimited strings extracted from `proc/[pid]/cmdline`. For libc-based executables, this would be the full argv vector passed to `main`.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["cmd/otecol", "--config=config.yaml"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_commandargs()
      :"process.command_args"

  ### Erlang

  ```erlang
  ?PROCESS_COMMANDARGS.
  'process.command_args'
  ```

  <!-- tabs-close -->
  """
  @spec process_commandargs :: :"process.command_args"
  def process_commandargs do
    :"process.command_args"
  end

  @doc """
  The full command used to launch the process as a single string representing the full command. On Windows, can be set to the result of `GetCommandLineW`. Do not set this if you have to assemble it just for monitoring; use `process.command_args` instead.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["C:\\cmd\\otecol --config=\"my directory\\config.yaml\""]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_commandline()
      :"process.command_line"

  ### Erlang

  ```erlang
  ?PROCESS_COMMANDLINE.
  'process.command_line'
  ```

  <!-- tabs-close -->
  """
  @spec process_commandline :: :"process.command_line"
  def process_commandline do
    :"process.command_line"
  end

  @typedoc """
  Specifies whether the context switches for this data point were voluntary or involuntary.

  ### Enum Values
  * `:voluntary` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:involuntary` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type process_contextswitchtype_values() :: %{
          :voluntary => :voluntary,
          :involuntary => :involuntary
        }
  @doc """
  Specifies whether the context switches for this data point were voluntary or involuntary.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_contextswitchtype()
      :"process.context_switch_type"
      
      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_contextswitchtype_values().voluntary
      :voluntary
      
      iex> %{OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_contextswitchtype() => OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_contextswitchtype_values().voluntary}
      %{:"process.context_switch_type" => :voluntary}
      
      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_contextswitchtype_values(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?PROCESS_CONTEXTSWITCHTYPE.
  'process.context_switch_type'

  \#{?PROCESS_CONTEXTSWITCHTYPE => ?PROCESS_CONTEXTSWITCHTYPE_VALUES.voluntary}.
  \#{'process.context_switch_type' => voluntary}

  ?'PROCESS_CONTEXTSWITCHTYPE_VALUES.voluntary'.
  voluntary

  ?PROCESS_CONTEXTSWITCHTYPE_VALUES(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec process_contextswitchtype :: :"process.context_switch_type"
  def process_contextswitchtype do
    :"process.context_switch_type"
  end

  @spec process_contextswitchtype_values() :: process_contextswitchtype_values()
  def process_contextswitchtype_values() do
    %{
      :voluntary => :voluntary,
      :involuntary => :involuntary
    }
  end

  @spec process_contextswitchtype_values(atom() | String.t()) :: atom() | String.t()
  def process_contextswitchtype_values(custom_value) do
    custom_value
  end

  @typedoc """
  The CPU state of the process.


  ### Enum Values
  * `:system` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:user` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:wait` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type process_cpu_state_values() :: %{
          :system => :system,
          :user => :user,
          :wait => :wait
        }
  @doc """
  The CPU state of the process.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_cpu_state()
      :"process.cpu.state"
      
      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_cpu_state_values().system
      :system
      
      iex> %{OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_cpu_state() => OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_cpu_state_values().system}
      %{:"process.cpu.state" => :system}
      
      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_cpu_state_values(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?PROCESS_CPU_STATE.
  'process.cpu.state'

  \#{?PROCESS_CPU_STATE => ?PROCESS_CPU_STATE_VALUES.system}.
  \#{'process.cpu.state' => system}

  ?'PROCESS_CPU_STATE_VALUES.system'.
  system

  ?PROCESS_CPU_STATE_VALUES(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec process_cpu_state :: :"process.cpu.state"
  def process_cpu_state do
    :"process.cpu.state"
  end

  @spec process_cpu_state_values() :: process_cpu_state_values()
  def process_cpu_state_values() do
    %{
      :system => :system,
      :user => :user,
      :wait => :wait
    }
  end

  @spec process_cpu_state_values(atom() | String.t()) :: atom() | String.t()
  def process_cpu_state_values(custom_value) do
    custom_value
  end

  @doc """
  The date and time the process was created, in ISO 8601 format.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2023-11-21T09:25:34.853Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_creation_time()
      :"process.creation.time"

  ### Erlang

  ```erlang
  ?PROCESS_CREATION_TIME.
  'process.creation.time'
  ```

  <!-- tabs-close -->
  """
  @spec process_creation_time :: :"process.creation.time"
  def process_creation_time do
    :"process.creation.time"
  end

  @doc """
  The name of the process executable. On Linux based systems, can be set to the `Name` in `proc/[pid]/status`. On Windows, can be set to the base name of `GetProcessImageFileNameW`.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["otelcol"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_executable_name()
      :"process.executable.name"

  ### Erlang

  ```erlang
  ?PROCESS_EXECUTABLE_NAME.
  'process.executable.name'
  ```

  <!-- tabs-close -->
  """
  @spec process_executable_name :: :"process.executable.name"
  def process_executable_name do
    :"process.executable.name"
  end

  @doc """
  The full path to the process executable. On Linux based systems, can be set to the target of `proc/[pid]/exe`. On Windows, can be set to the result of `GetProcessImageFileNameW`.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["/usr/bin/cmd/otelcol"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_executable_path()
      :"process.executable.path"

  ### Erlang

  ```erlang
  ?PROCESS_EXECUTABLE_PATH.
  'process.executable.path'
  ```

  <!-- tabs-close -->
  """
  @spec process_executable_path :: :"process.executable.path"
  def process_executable_path do
    :"process.executable.path"
  end

  @doc """
  The exit code of the process.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [127]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_exit_code()
      :"process.exit.code"

  ### Erlang

  ```erlang
  ?PROCESS_EXIT_CODE.
  'process.exit.code'
  ```

  <!-- tabs-close -->
  """
  @spec process_exit_code :: :"process.exit.code"
  def process_exit_code do
    :"process.exit.code"
  end

  @doc """
  The date and time the process exited, in ISO 8601 format.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2023-11-21T09:26:12.315Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_exit_time()
      :"process.exit.time"

  ### Erlang

  ```erlang
  ?PROCESS_EXIT_TIME.
  'process.exit.time'
  ```

  <!-- tabs-close -->
  """
  @spec process_exit_time :: :"process.exit.time"
  def process_exit_time do
    :"process.exit.time"
  end

  @doc """
  The PID of the process's group leader. This is also the process group ID (PGID) of the process.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [23]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_groupleader_pid()
      :"process.group_leader.pid"

  ### Erlang

  ```erlang
  ?PROCESS_GROUPLEADER_PID.
  'process.group_leader.pid'
  ```

  <!-- tabs-close -->
  """
  @spec process_groupleader_pid :: :"process.group_leader.pid"
  def process_groupleader_pid do
    :"process.group_leader.pid"
  end

  @doc """
  Whether the process is connected to an interactive shell.

  ### Value type

  Value must be of type `boolean()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_interactive()
      :"process.interactive"

  ### Erlang

  ```erlang
  ?PROCESS_INTERACTIVE.
  'process.interactive'
  ```

  <!-- tabs-close -->
  """
  @spec process_interactive :: :"process.interactive"
  def process_interactive do
    :"process.interactive"
  end

  @doc """
  The username of the user that owns the process.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["root"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_owner()
      :"process.owner"

  ### Erlang

  ```erlang
  ?PROCESS_OWNER.
  'process.owner'
  ```

  <!-- tabs-close -->
  """
  @spec process_owner :: :"process.owner"
  def process_owner do
    :"process.owner"
  end

  @typedoc """
  The type of page fault for this data point. Type `major` is for major/hard page faults, and `minor` is for minor/soft page faults.


  ### Enum Values
  * `:major` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:minor` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type process_paging_faulttype_values() :: %{
          :major => :major,
          :minor => :minor
        }
  @doc """
  The type of page fault for this data point. Type `major` is for major/hard page faults, and `minor` is for minor/soft page faults.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_paging_faulttype()
      :"process.paging.fault_type"
      
      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_paging_faulttype_values().major
      :major
      
      iex> %{OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_paging_faulttype() => OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_paging_faulttype_values().major}
      %{:"process.paging.fault_type" => :major}
      
      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_paging_faulttype_values(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?PROCESS_PAGING_FAULTTYPE.
  'process.paging.fault_type'

  \#{?PROCESS_PAGING_FAULTTYPE => ?PROCESS_PAGING_FAULTTYPE_VALUES.major}.
  \#{'process.paging.fault_type' => major}

  ?'PROCESS_PAGING_FAULTTYPE_VALUES.major'.
  major

  ?PROCESS_PAGING_FAULTTYPE_VALUES(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec process_paging_faulttype :: :"process.paging.fault_type"
  def process_paging_faulttype do
    :"process.paging.fault_type"
  end

  @spec process_paging_faulttype_values() :: process_paging_faulttype_values()
  def process_paging_faulttype_values() do
    %{
      :major => :major,
      :minor => :minor
    }
  end

  @spec process_paging_faulttype_values(atom() | String.t()) :: atom() | String.t()
  def process_paging_faulttype_values(custom_value) do
    custom_value
  end

  @doc """
  Parent Process identifier (PPID).

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [111]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_parentpid()
      :"process.parent_pid"

  ### Erlang

  ```erlang
  ?PROCESS_PARENTPID.
  'process.parent_pid'
  ```

  <!-- tabs-close -->
  """
  @spec process_parentpid :: :"process.parent_pid"
  def process_parentpid do
    :"process.parent_pid"
  end

  @doc """
  Process identifier (PID).

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [1234]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_pid()
      :"process.pid"

  ### Erlang

  ```erlang
  ?PROCESS_PID.
  'process.pid'
  ```

  <!-- tabs-close -->
  """
  @spec process_pid :: :"process.pid"
  def process_pid do
    :"process.pid"
  end

  @doc """
  The real user ID (RUID) of the process.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [1000]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_realuser_id()
      :"process.real_user.id"

  ### Erlang

  ```erlang
  ?PROCESS_REALUSER_ID.
  'process.real_user.id'
  ```

  <!-- tabs-close -->
  """
  @spec process_realuser_id :: :"process.real_user.id"
  def process_realuser_id do
    :"process.real_user.id"
  end

  @doc """
  The username of the real user of the process.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["operator"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_realuser_name()
      :"process.real_user.name"

  ### Erlang

  ```erlang
  ?PROCESS_REALUSER_NAME.
  'process.real_user.name'
  ```

  <!-- tabs-close -->
  """
  @spec process_realuser_name :: :"process.real_user.name"
  def process_realuser_name do
    :"process.real_user.name"
  end

  @doc """
  An additional description about the runtime of the process, for example a specific vendor customization of the runtime environment.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  Eclipse OpenJ9 Eclipse OpenJ9 VM openj9-0.21.0
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_runtime_description()
      :"process.runtime.description"

  ### Erlang

  ```erlang
  ?PROCESS_RUNTIME_DESCRIPTION.
  'process.runtime.description'
  ```

  <!-- tabs-close -->
  """
  @spec process_runtime_description :: :"process.runtime.description"
  def process_runtime_description do
    :"process.runtime.description"
  end

  @doc """
  The name of the runtime of this process. For compiled native binaries, this **SHOULD** be the name of the compiler.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["OpenJDK Runtime Environment"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_runtime_name()
      :"process.runtime.name"

  ### Erlang

  ```erlang
  ?PROCESS_RUNTIME_NAME.
  'process.runtime.name'
  ```

  <!-- tabs-close -->
  """
  @spec process_runtime_name :: :"process.runtime.name"
  def process_runtime_name do
    :"process.runtime.name"
  end

  @doc """
  The version of the runtime of this process, as returned by the runtime without modification.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  14.0.2
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_runtime_version()
      :"process.runtime.version"

  ### Erlang

  ```erlang
  ?PROCESS_RUNTIME_VERSION.
  'process.runtime.version'
  ```

  <!-- tabs-close -->
  """
  @spec process_runtime_version :: :"process.runtime.version"
  def process_runtime_version do
    :"process.runtime.version"
  end

  @doc """
  The saved user ID (SUID) of the process.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [1002]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_saveduser_id()
      :"process.saved_user.id"

  ### Erlang

  ```erlang
  ?PROCESS_SAVEDUSER_ID.
  'process.saved_user.id'
  ```

  <!-- tabs-close -->
  """
  @spec process_saveduser_id :: :"process.saved_user.id"
  def process_saveduser_id do
    :"process.saved_user.id"
  end

  @doc """
  The username of the saved user.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["operator"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_saveduser_name()
      :"process.saved_user.name"

  ### Erlang

  ```erlang
  ?PROCESS_SAVEDUSER_NAME.
  'process.saved_user.name'
  ```

  <!-- tabs-close -->
  """
  @spec process_saveduser_name :: :"process.saved_user.name"
  def process_saveduser_name do
    :"process.saved_user.name"
  end

  @doc """
  The PID of the process's session leader. This is also the session ID (SID) of the process.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [14]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_sessionleader_pid()
      :"process.session_leader.pid"

  ### Erlang

  ```erlang
  ?PROCESS_SESSIONLEADER_PID.
  'process.session_leader.pid'
  ```

  <!-- tabs-close -->
  """
  @spec process_sessionleader_pid :: :"process.session_leader.pid"
  def process_sessionleader_pid do
    :"process.session_leader.pid"
  end

  @doc """
  The effective user ID (EUID) of the process.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [1001]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_user_id()
      :"process.user.id"

  ### Erlang

  ```erlang
  ?PROCESS_USER_ID.
  'process.user.id'
  ```

  <!-- tabs-close -->
  """
  @spec process_user_id :: :"process.user.id"
  def process_user_id do
    :"process.user.id"
  end

  @doc """
  The username of the effective user of the process.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["root"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_user_name()
      :"process.user.name"

  ### Erlang

  ```erlang
  ?PROCESS_USER_NAME.
  'process.user.name'
  ```

  <!-- tabs-close -->
  """
  @spec process_user_name :: :"process.user.name"
  def process_user_name do
    :"process.user.name"
  end

  @doc """
  Virtual process identifier.

  ### Value type

  Value must be of type `integer()`.
  ### Notes

  The process ID within a PID namespace. This is not necessarily unique across all processes on the host but it is unique within the process namespace that the process exists within.

  ### Examples

  ```
  [12]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_vpid()
      :"process.vpid"

  ### Erlang

  ```erlang
  ?PROCESS_VPID.
  'process.vpid'
  ```

  <!-- tabs-close -->
  """
  @spec process_vpid :: :"process.vpid"
  def process_vpid do
    :"process.vpid"
  end
end
