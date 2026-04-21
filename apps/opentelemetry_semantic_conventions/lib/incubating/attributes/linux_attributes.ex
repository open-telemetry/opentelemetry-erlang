defmodule OpenTelemetry.SemConv.Incubating.LinuxAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Linux attributes.
  """

  @typedoc """
  The Linux Slab memory state

  ### Enum Values
  * `:reclaimable` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:unreclaimable` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  """
  @type linux_memory_slab_state_values() :: %{
          :reclaimable => :reclaimable,
          :unreclaimable => :unreclaimable
        }
  @deprecated """
  Replaced by `system.memory.linux.slab.state`.
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
