defmodule OpenTelemetry.SemConv.Incubating.ProcessAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Process attributes.
  """

  @doc """
  Length of the process.command_args array

  ### Value type

  Value must be of type `integer()`.
  ### Notes

  This field can be useful for querying or performing bucket analysis on how many arguments were provided to start a process. More arguments may be an indication of suspicious activity.

  ### Examples

  ```
  [4]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_args_count()
      :"process.args_count"

  ### Erlang

  ```erlang
  ?PROCESS_ARGS_COUNT.
  'process.args_count'
  ```

  <!-- tabs-close -->
  """
  @spec process_args_count :: :"process.args_count"
  def process_args_count do
    :"process.args_count"
  end

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
  [["cmd/otecol", "--config=config.yaml"]]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_command_args()
      :"process.command_args"

  ### Erlang

  ```erlang
  ?PROCESS_COMMAND_ARGS.
  'process.command_args'
  ```

  <!-- tabs-close -->
  """
  @spec process_command_args :: :"process.command_args"
  def process_command_args do
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

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_command_line()
      :"process.command_line"

  ### Erlang

  ```erlang
  ?PROCESS_COMMAND_LINE.
  'process.command_line'
  ```

  <!-- tabs-close -->
  """
  @spec process_command_line :: :"process.command_line"
  def process_command_line do
    :"process.command_line"
  end

  @typedoc """
  Specifies whether the context switches for this data point were voluntary or involuntary.

  ### Enum Values
  * `:voluntary` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:involuntary` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type process_context_switch_type_values() :: %{
          :voluntary => :voluntary,
          :involuntary => :involuntary
        }
  @doc """
  Specifies whether the context switches for this data point were voluntary or involuntary.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_context_switch_type()
      :"process.context_switch_type"

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_context_switch_type_values().voluntary
      :voluntary

      iex> %{OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_context_switch_type() => OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_context_switch_type_values().voluntary}
      %{:"process.context_switch_type" => :voluntary}

  ### Erlang

  ```erlang
  ?PROCESS_CONTEXT_SWITCH_TYPE.
  'process.context_switch_type'

  ?PROCESS_CONTEXT_SWITCH_TYPE_VALUES_VOLUNTARY.
  'voluntary'

  \#{?PROCESS_CONTEXT_SWITCH_TYPE => ?PROCESS_CONTEXT_SWITCH_TYPE_VALUES_VOLUNTARY}.
  \#{'process.context_switch_type' => 'voluntary'}
  ```

  <!-- tabs-close -->
  """
  @spec process_context_switch_type :: :"process.context_switch_type"
  def process_context_switch_type do
    :"process.context_switch_type"
  end

  @spec process_context_switch_type_values() :: process_context_switch_type_values()
  def process_context_switch_type_values() do
    %{
      :voluntary => :voluntary,
      :involuntary => :involuntary
    }
  end

  @typedoc """
  Deprecated, use `cpu.mode` instead.

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
  @deprecated """
  Replaced by `cpu.mode`
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
  Process environment variables, <key> being the environment variable name, the value being the environment variable value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Examples:

  - an environment variable `USER` with value `"ubuntu"` **SHOULD** be recorded
  as the `process.environment_variable.USER` attribute with value `"ubuntu"`.

  - an environment variable `PATH` with value `"/usr/local/bin:/usr/bin"`
  **SHOULD** be recorded as the `process.environment_variable.PATH` attribute
  with value `"/usr/local/bin:/usr/bin"`.

  ### Examples

  ```
  ["ubuntu", "/usr/local/bin:/usr/bin"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_environment_variable()
      :"process.environment_variable"

  ### Erlang

  ```erlang
  ?PROCESS_ENVIRONMENT_VARIABLE.
  'process.environment_variable'
  ```

  <!-- tabs-close -->
  """
  @spec process_environment_variable :: :"process.environment_variable"
  def process_environment_variable do
    :"process.environment_variable"
  end

  @doc """
  The GNU build ID as found in the `.note.gnu.build-id` ELF section (hex string).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["c89b11207f6479603b0d49bf291c092c2b719293"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_executable_build_id_gnu()
      :"process.executable.build_id.gnu"

  ### Erlang

  ```erlang
  ?PROCESS_EXECUTABLE_BUILD_ID_GNU.
  'process.executable.build_id.gnu'
  ```

  <!-- tabs-close -->
  """
  @spec process_executable_build_id_gnu :: :"process.executable.build_id.gnu"
  def process_executable_build_id_gnu do
    :"process.executable.build_id.gnu"
  end

  @doc """
  The Go build ID as retrieved by `go tool buildid <go executable>`.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["foh3mEXu7BLZjsN9pOwG/kATcXlYVCDEFouRMQed_/WwRFB1hPo9LBkekthSPG/x8hMC8emW2cCjXD0_1aY"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_executable_build_id_go()
      :"process.executable.build_id.go"

  ### Erlang

  ```erlang
  ?PROCESS_EXECUTABLE_BUILD_ID_GO.
  'process.executable.build_id.go'
  ```

  <!-- tabs-close -->
  """
  @spec process_executable_build_id_go :: :"process.executable.build_id.go"
  def process_executable_build_id_go do
    :"process.executable.build_id.go"
  end

  @doc """
  Profiling specific build ID for executables. See the OTel specification for Profiles for more information.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["600DCAFE4A110000F2BF38C493F5FB92"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_executable_build_id_htlhash()
      :"process.executable.build_id.htlhash"

  ### Erlang

  ```erlang
  ?PROCESS_EXECUTABLE_BUILD_ID_HTLHASH.
  'process.executable.build_id.htlhash'
  ```

  <!-- tabs-close -->
  """
  @spec process_executable_build_id_htlhash :: :"process.executable.build_id.htlhash"
  def process_executable_build_id_htlhash do
    :"process.executable.build_id.htlhash"
  end

  @deprecated """
  Replaced by `process.executable.build_id.htlhash`
  """
  @spec process_executable_build_id_profiling :: :"process.executable.build_id.profiling"
  def process_executable_build_id_profiling do
    :"process.executable.build_id.profiling"
  end

  @doc """
  The name of the process executable. On Linux based systems, this **SHOULD** be set to the base name of the target of `/proc/[pid]/exe`. On Windows, this **SHOULD** be set to the base name of `GetProcessImageFileNameW`.

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

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_group_leader_pid()
      :"process.group_leader.pid"

  ### Erlang

  ```erlang
  ?PROCESS_GROUP_LEADER_PID.
  'process.group_leader.pid'
  ```

  <!-- tabs-close -->
  """
  @spec process_group_leader_pid :: :"process.group_leader.pid"
  def process_group_leader_pid do
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
  The control group associated with the process.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Control groups (cgroups) are a kernel feature used to organize and manage process resources. This attribute provides the path(s) to the cgroup(s) associated with the process, which should match the contents of the [/proc/\[PID\]/cgroup](https://man7.org/linux/man-pages/man7/cgroups.7.html) file.

  ### Examples

  ```
  ["1:name=systemd:/user.slice/user-1000.slice/session-3.scope", "0::/user.slice/user-1000.slice/user@1000.service/tmux-spawn-0267755b-4639-4a27-90ed-f19f88e53748.scope"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_linux_cgroup()
      :"process.linux.cgroup"

  ### Erlang

  ```erlang
  ?PROCESS_LINUX_CGROUP.
  'process.linux.cgroup'
  ```

  <!-- tabs-close -->
  """
  @spec process_linux_cgroup :: :"process.linux.cgroup"
  def process_linux_cgroup do
    :"process.linux.cgroup"
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
  @type process_paging_fault_type_values() :: %{
          :major => :major,
          :minor => :minor
        }
  @doc """
  The type of page fault for this data point. Type `major` is for major/hard page faults, and `minor` is for minor/soft page faults.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_paging_fault_type()
      :"process.paging.fault_type"

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_paging_fault_type_values().major
      :major

      iex> %{OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_paging_fault_type() => OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_paging_fault_type_values().major}
      %{:"process.paging.fault_type" => :major}

  ### Erlang

  ```erlang
  ?PROCESS_PAGING_FAULT_TYPE.
  'process.paging.fault_type'

  ?PROCESS_PAGING_FAULT_TYPE_VALUES_MAJOR.
  'major'

  \#{?PROCESS_PAGING_FAULT_TYPE => ?PROCESS_PAGING_FAULT_TYPE_VALUES_MAJOR}.
  \#{'process.paging.fault_type' => 'major'}
  ```

  <!-- tabs-close -->
  """
  @spec process_paging_fault_type :: :"process.paging.fault_type"
  def process_paging_fault_type do
    :"process.paging.fault_type"
  end

  @spec process_paging_fault_type_values() :: process_paging_fault_type_values()
  def process_paging_fault_type_values() do
    %{
      :major => :major,
      :minor => :minor
    }
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

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_parent_pid()
      :"process.parent_pid"

  ### Erlang

  ```erlang
  ?PROCESS_PARENT_PID.
  'process.parent_pid'
  ```

  <!-- tabs-close -->
  """
  @spec process_parent_pid :: :"process.parent_pid"
  def process_parent_pid do
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

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_real_user_id()
      :"process.real_user.id"

  ### Erlang

  ```erlang
  ?PROCESS_REAL_USER_ID.
  'process.real_user.id'
  ```

  <!-- tabs-close -->
  """
  @spec process_real_user_id :: :"process.real_user.id"
  def process_real_user_id do
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

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_real_user_name()
      :"process.real_user.name"

  ### Erlang

  ```erlang
  ?PROCESS_REAL_USER_NAME.
  'process.real_user.name'
  ```

  <!-- tabs-close -->
  """
  @spec process_real_user_name :: :"process.real_user.name"
  def process_real_user_name do
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
  The name of the runtime of this process.

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

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_saved_user_id()
      :"process.saved_user.id"

  ### Erlang

  ```erlang
  ?PROCESS_SAVED_USER_ID.
  'process.saved_user.id'
  ```

  <!-- tabs-close -->
  """
  @spec process_saved_user_id :: :"process.saved_user.id"
  def process_saved_user_id do
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

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_saved_user_name()
      :"process.saved_user.name"

  ### Erlang

  ```erlang
  ?PROCESS_SAVED_USER_NAME.
  'process.saved_user.name'
  ```

  <!-- tabs-close -->
  """
  @spec process_saved_user_name :: :"process.saved_user.name"
  def process_saved_user_name do
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

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_session_leader_pid()
      :"process.session_leader.pid"

  ### Erlang

  ```erlang
  ?PROCESS_SESSION_LEADER_PID.
  'process.session_leader.pid'
  ```

  <!-- tabs-close -->
  """
  @spec process_session_leader_pid :: :"process.session_leader.pid"
  def process_session_leader_pid do
    :"process.session_leader.pid"
  end

  @doc """
  Process title (proctitle)

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  In many Unix-like systems, process title (proctitle), is the string that represents the name or command line of a running process, displayed by system monitoring tools like ps, top, and htop.

  ### Examples

  ```
  ["cat /etc/hostname", "xfce4-session", "bash"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_title()
      :"process.title"

  ### Erlang

  ```erlang
  ?PROCESS_TITLE.
  'process.title'
  ```

  <!-- tabs-close -->
  """
  @spec process_title :: :"process.title"
  def process_title do
    :"process.title"
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

  @doc """
  The working directory of the process.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["/root"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProcessAttributes.process_working_directory()
      :"process.working_directory"

  ### Erlang

  ```erlang
  ?PROCESS_WORKING_DIRECTORY.
  'process.working_directory'
  ```

  <!-- tabs-close -->
  """
  @spec process_working_directory :: :"process.working_directory"
  def process_working_directory do
    :"process.working_directory"
  end
end
