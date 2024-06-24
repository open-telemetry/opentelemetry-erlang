defmodule OpenTelemetry.SemanticConventions.ProcessAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Process attributes.
  """

  @doc """
  The command used to launch the process (i.e. the command name). On Linux based systems, can be set to the zeroth string in `proc/[pid]/cmdline`. On Windows, can be set to the first parameter extracted from `GetCommandLineW`.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_command()
      :"process.command"
  """
  @spec process_command :: :"process.command"
  def process_command do
    :"process.command"
  end

  @doc """
  All the command arguments (including the command/executable itself) as received by the process. On Linux-based systems (and some other Unixoid systems supporting procfs), can be set according to the list of null-delimited strings extracted from `proc/[pid]/cmdline`. For libc-based executables, this would be the full argv vector passed to `main`.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_commandargs()
      :"process.command_args"
  """
  @spec process_commandargs :: :"process.command_args"
  def process_commandargs do
    :"process.command_args"
  end

  @doc """
  The full command used to launch the process as a single string representing the full command. On Windows, can be set to the result of `GetCommandLineW`. Do not set this if you have to assemble it just for monitoring; use `process.command_args` instead.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_commandline()
      :"process.command_line"
  """
  @spec process_commandline :: :"process.command_line"
  def process_commandline do
    :"process.command_line"
  end

  @typedoc """
  Specifies whether the context switches for this data point were voluntary or involuntary.

  ### Options
  * `:voluntary` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:involuntary` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type process_contextswitchtype() :: :voluntary | :involuntary | atom()

  @doc """
  Specifies whether the context switches for this data point were voluntary or involuntary.


  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_contextswitchtype(:voluntary)
      :voluntary
      
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_contextswitchtype(:custom_value)
      :custom_value
  """
  @spec process_contextswitchtype(process_contextswitchtype()) ::
          :voluntary | :involuntary | atom()
  def process_contextswitchtype(option) do
    case option do
      :voluntary -> :voluntary
      :involuntary -> :involuntary
      _ -> option
    end
  end

  @typedoc """
  The CPU state of the process.


  ### Options
  * `:system` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:user` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:wait` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type process_cpu_state() :: :system | :user | :wait | atom()

  @doc """
  The CPU state of the process.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_cpu_state(:system)
      :system
      
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_cpu_state(:custom_value)
      :custom_value
  """
  @spec process_cpu_state(process_cpu_state()) :: :system | :user | :wait | atom()
  def process_cpu_state(option) do
    case option do
      :system -> :system
      :user -> :user
      :wait -> :wait
      _ -> option
    end
  end

  @doc """
  The date and time the process was created, in ISO 8601 format.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_creation_time()
      :"process.creation.time"
  """
  @spec process_creation_time :: :"process.creation.time"
  def process_creation_time do
    :"process.creation.time"
  end

  @doc """
  The name of the process executable. On Linux based systems, can be set to the `Name` in `proc/[pid]/status`. On Windows, can be set to the base name of `GetProcessImageFileNameW`.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_executable_name()
      :"process.executable.name"
  """
  @spec process_executable_name :: :"process.executable.name"
  def process_executable_name do
    :"process.executable.name"
  end

  @doc """
  The full path to the process executable. On Linux based systems, can be set to the target of `proc/[pid]/exe`. On Windows, can be set to the result of `GetProcessImageFileNameW`.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_executable_path()
      :"process.executable.path"
  """
  @spec process_executable_path :: :"process.executable.path"
  def process_executable_path do
    :"process.executable.path"
  end

  @doc """
  The exit code of the process.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_exit_code()
      :"process.exit.code"
  """
  @spec process_exit_code :: :"process.exit.code"
  def process_exit_code do
    :"process.exit.code"
  end

  @doc """
  The date and time the process exited, in ISO 8601 format.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_exit_time()
      :"process.exit.time"
  """
  @spec process_exit_time :: :"process.exit.time"
  def process_exit_time do
    :"process.exit.time"
  end

  @doc """
  The PID of the process's group leader. This is also the process group ID (PGID) of the process.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_groupleader_pid()
      :"process.group_leader.pid"
  """
  @spec process_groupleader_pid :: :"process.group_leader.pid"
  def process_groupleader_pid do
    :"process.group_leader.pid"
  end

  @doc """
  Whether the process is connected to an interactive shell.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_interactive()
      :"process.interactive"
  """
  @spec process_interactive :: :"process.interactive"
  def process_interactive do
    :"process.interactive"
  end

  @doc """
  The username of the user that owns the process.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_owner()
      :"process.owner"
  """
  @spec process_owner :: :"process.owner"
  def process_owner do
    :"process.owner"
  end

  @typedoc """
  The type of page fault for this data point. Type `major` is for major/hard page faults, and `minor` is for minor/soft page faults.


  ### Options
  * `:major` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:minor` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type process_paging_faulttype() :: :major | :minor | atom()

  @doc """
  The type of page fault for this data point. Type `major` is for major/hard page faults, and `minor` is for minor/soft page faults.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_paging_faulttype(:major)
      :major
      
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_paging_faulttype(:custom_value)
      :custom_value
  """
  @spec process_paging_faulttype(process_paging_faulttype()) :: :major | :minor | atom()
  def process_paging_faulttype(option) do
    case option do
      :major -> :major
      :minor -> :minor
      _ -> option
    end
  end

  @doc """
  Parent Process identifier (PPID).



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_parentpid()
      :"process.parent_pid"
  """
  @spec process_parentpid :: :"process.parent_pid"
  def process_parentpid do
    :"process.parent_pid"
  end

  @doc """
  Process identifier (PID).



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_pid()
      :"process.pid"
  """
  @spec process_pid :: :"process.pid"
  def process_pid do
    :"process.pid"
  end

  @doc """
  The real user ID (RUID) of the process.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_realuser_id()
      :"process.real_user.id"
  """
  @spec process_realuser_id :: :"process.real_user.id"
  def process_realuser_id do
    :"process.real_user.id"
  end

  @doc """
  The username of the real user of the process.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_realuser_name()
      :"process.real_user.name"
  """
  @spec process_realuser_name :: :"process.real_user.name"
  def process_realuser_name do
    :"process.real_user.name"
  end

  @doc """
  An additional description about the runtime of the process, for example a specific vendor customization of the runtime environment.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_runtime_description()
      :"process.runtime.description"
  """
  @spec process_runtime_description :: :"process.runtime.description"
  def process_runtime_description do
    :"process.runtime.description"
  end

  @doc """
  The name of the runtime of this process. For compiled native binaries, this **SHOULD** be the name of the compiler.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_runtime_name()
      :"process.runtime.name"
  """
  @spec process_runtime_name :: :"process.runtime.name"
  def process_runtime_name do
    :"process.runtime.name"
  end

  @doc """
  The version of the runtime of this process, as returned by the runtime without modification.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_runtime_version()
      :"process.runtime.version"
  """
  @spec process_runtime_version :: :"process.runtime.version"
  def process_runtime_version do
    :"process.runtime.version"
  end

  @doc """
  The saved user ID (SUID) of the process.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_saveduser_id()
      :"process.saved_user.id"
  """
  @spec process_saveduser_id :: :"process.saved_user.id"
  def process_saveduser_id do
    :"process.saved_user.id"
  end

  @doc """
  The username of the saved user.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_saveduser_name()
      :"process.saved_user.name"
  """
  @spec process_saveduser_name :: :"process.saved_user.name"
  def process_saveduser_name do
    :"process.saved_user.name"
  end

  @doc """
  The PID of the process's session leader. This is also the session ID (SID) of the process.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_sessionleader_pid()
      :"process.session_leader.pid"
  """
  @spec process_sessionleader_pid :: :"process.session_leader.pid"
  def process_sessionleader_pid do
    :"process.session_leader.pid"
  end

  @doc """
  The effective user ID (EUID) of the process.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_user_id()
      :"process.user.id"
  """
  @spec process_user_id :: :"process.user.id"
  def process_user_id do
    :"process.user.id"
  end

  @doc """
  The username of the effective user of the process.



  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_user_name()
      :"process.user.name"
  """
  @spec process_user_name :: :"process.user.name"
  def process_user_name do
    :"process.user.name"
  end

  @doc """
  Virtual process identifier.

  ### Notes

  The process ID within a PID namespace. This is not necessarily unique across all processes on the host but it is unique within the process namespace that the process exists within.


  ### Example
      iex> OpenTelemetry.SemanticConventions.ProcessAttributes.process_vpid()
      :"process.vpid"
  """
  @spec process_vpid :: :"process.vpid"
  def process_vpid do
    :"process.vpid"
  end
end
