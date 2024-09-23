defmodule OpenTelemetry.SemConv.Incubating.CpuAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cpu attributes.
  """

  @typedoc """
  The mode of the CPU

  ### Enum Values
  * `:user` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:system` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:nice` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:idle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:iowait` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:interrupt` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:steal` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:kernel` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
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
