defmodule OpenTelemetry.SemConv.Incubating.CpuAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cpu attributes.
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

      iex> OpenTelemetry.SemConv.Incubating.CpuAttributes.cpu_logical_number()
      :"cpu.logical_number"

  ### Erlang

  ```erlang
  ?CPU_LOGICAL_NUMBER.
  'cpu.logical_number'
  ```

  <!-- tabs-close -->
  """
  @spec cpu_logical_number :: :"cpu.logical_number"
  def cpu_logical_number do
    :"cpu.logical_number"
  end

  @typedoc """
  The mode of the CPU

  ### Enum Values
  * `:user` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - User
  * `:system` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - System
  * `:nice` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Nice
  * `:idle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Idle
  * `:iowait` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IO Wait
  * `:interrupt` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Interrupt
  * `:steal` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Steal
  * `:kernel` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Kernel
  """
  @type cpu_mode_values() :: %{
          :user => :user,
          :system => :system,
          :nice => :nice,
          :idle => :idle,
          :iowait => :iowait,
          :interrupt => :interrupt,
          :steal => :steal,
          :kernel => :kernel
        }
  @doc """
  The mode of the CPU

  ### Examples

  ```
  ["user", "system"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CpuAttributes.cpu_mode()
      :"cpu.mode"

      iex> OpenTelemetry.SemConv.Incubating.CpuAttributes.cpu_mode_values().user
      :user

      iex> %{OpenTelemetry.SemConv.Incubating.CpuAttributes.cpu_mode() => OpenTelemetry.SemConv.Incubating.CpuAttributes.cpu_mode_values().user}
      %{:"cpu.mode" => :user}

  ### Erlang

  ```erlang
  ?CPU_MODE.
  'cpu.mode'

  ?CPU_MODE_VALUES_USER.
  'user'

  \#{?CPU_MODE => ?CPU_MODE_VALUES_USER}.
  \#{'cpu.mode' => 'user'}
  ```

  <!-- tabs-close -->
  """
  @spec cpu_mode :: :"cpu.mode"
  def cpu_mode do
    :"cpu.mode"
  end

  @spec cpu_mode_values() :: cpu_mode_values()
  def cpu_mode_values() do
    %{
      :user => :user,
      :system => :system,
      :nice => :nice,
      :idle => :idle,
      :iowait => :iowait,
      :interrupt => :interrupt,
      :steal => :steal,
      :kernel => :kernel
    }
  end
end
