defmodule OpenTelemetry.SemConv.Incubating.LinuxAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Linux attributes.
  """

  @typedoc """
  The Linux Slab memory state

  ### Enum Values
  * `:reclaimable` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:unreclaimable` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type linux_memory_slab_state_values() :: %{
          :reclaimable => :reclaimable,
          :unreclaimable => :unreclaimable
        }
  @doc """
  The Linux Slab memory state

  ### Examples

  ```
  ["reclaimable", "unreclaimable"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.LinuxAttributes.linux_memory_slab_state()
      :"linux.memory.slab.state"

      iex> OpenTelemetry.SemConv.Incubating.LinuxAttributes.linux_memory_slab_state_values().reclaimable
      :reclaimable

      iex> %{OpenTelemetry.SemConv.Incubating.LinuxAttributes.linux_memory_slab_state() => OpenTelemetry.SemConv.Incubating.LinuxAttributes.linux_memory_slab_state_values().reclaimable}
      %{:"linux.memory.slab.state" => :reclaimable}

  ### Erlang

  ```erlang
  ?LINUX_MEMORY_SLAB_STATE.
  'linux.memory.slab.state'

  ?LINUX_MEMORY_SLAB_STATE_VALUES_RECLAIMABLE.
  'reclaimable'

  \#{?LINUX_MEMORY_SLAB_STATE => ?LINUX_MEMORY_SLAB_STATE_VALUES_RECLAIMABLE}.
  \#{'linux.memory.slab.state' => 'reclaimable'}
  ```

  <!-- tabs-close -->
  """
  @spec linux_memory_slab_state :: :"linux.memory.slab.state"
  def linux_memory_slab_state do
    :"linux.memory.slab.state"
  end

  @spec linux_memory_slab_state_values() :: linux_memory_slab_state_values()
  def linux_memory_slab_state_values() do
    %{
      :reclaimable => :reclaimable,
      :unreclaimable => :unreclaimable
    }
  end
end
