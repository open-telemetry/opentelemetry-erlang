defmodule OpenTelemetry.SemConv.Incubating.HWAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for HW attributes.
  """

  @doc """
  An identifier for the hardware component, unique within the monitored host

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["win32battery_battery_testsysa33_1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HWAttributes.hw_id()
      :"hw.id"

  ### Erlang

  ```erlang
  ?HW_ID.
  'hw.id'
  ```

  <!-- tabs-close -->
  """
  @spec hw_id :: :"hw.id"
  def hw_id do
    :"hw.id"
  end

  @doc """
  An easily-recognizable name for the hardware component

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["eth0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HWAttributes.hw_name()
      :"hw.name"

  ### Erlang

  ```erlang
  ?HW_NAME.
  'hw.name'
  ```

  <!-- tabs-close -->
  """
  @spec hw_name :: :"hw.name"
  def hw_name do
    :"hw.name"
  end

  @doc """
  Unique identifier of the parent component (typically the `hw.id` attribute of the enclosure, or disk controller)

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["dellStorage_perc_0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HWAttributes.hw_parent()
      :"hw.parent"

  ### Erlang

  ```erlang
  ?HW_PARENT.
  'hw.parent'
  ```

  <!-- tabs-close -->
  """
  @spec hw_parent :: :"hw.parent"
  def hw_parent do
    :"hw.parent"
  end

  @typedoc """
  The current state of the component


  ### Enum Values
  * `:ok` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Ok
  * `:degraded` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Degraded
  * `:failed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Failed
  """
  @type hw_state_values() :: %{
          :ok => :ok,
          :degraded => :degraded,
          :failed => :failed
        }
  @doc """
  The current state of the component



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HWAttributes.hw_state()
      :"hw.state"

      iex> OpenTelemetry.SemConv.Incubating.HWAttributes.hw_state_values().ok
      :ok

      iex> %{OpenTelemetry.SemConv.Incubating.HWAttributes.hw_state() => OpenTelemetry.SemConv.Incubating.HWAttributes.hw_state_values().ok}
      %{:"hw.state" => :ok}

  ### Erlang

  ```erlang
  ?HW_STATE.
  'hw.state'

  ?HW_STATE_VALUES_OK.
  'ok'

  \#{?HW_STATE => ?HW_STATE_VALUES_OK}.
  \#{'hw.state' => 'ok'}
  ```

  <!-- tabs-close -->
  """
  @spec hw_state :: :"hw.state"
  def hw_state do
    :"hw.state"
  end

  @spec hw_state_values() :: hw_state_values()
  def hw_state_values() do
    %{
      :ok => :ok,
      :degraded => :degraded,
      :failed => :failed
    }
  end

  @typedoc """
  Type of the component


  ### Enum Values
  * `:battery` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Battery
  * `:cpu` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - CPU
  * `:disk_controller` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Disk controller
  * `:enclosure` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Enclosure
  * `:fan` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Fan
  * `:gpu` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - GPU
  * `:logical_disk` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Logical disk
  * `:memory` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Memory
  * `:network` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Network
  * `:physical_disk` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Physical disk
  * `:power_supply` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Power supply
  * `:tape_drive` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Tape drive
  * `:temperature` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Temperature
  * `:voltage` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Voltage
  """
  @type hw_type_values() :: %{
          :battery => :battery,
          :cpu => :cpu,
          :disk_controller => :disk_controller,
          :enclosure => :enclosure,
          :fan => :fan,
          :gpu => :gpu,
          :logical_disk => :logical_disk,
          :memory => :memory,
          :network => :network,
          :physical_disk => :physical_disk,
          :power_supply => :power_supply,
          :tape_drive => :tape_drive,
          :temperature => :temperature,
          :voltage => :voltage
        }
  @doc """
  Type of the component


  ### Notes

  Describes the category of the hardware component for which `hw.state` is being reported. For example, `hw.type=temperature` along with `hw.state=degraded` would indicate that the temperature of the hardware component has been reported as `degraded`.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HWAttributes.hw_type()
      :"hw.type"

      iex> OpenTelemetry.SemConv.Incubating.HWAttributes.hw_type_values().battery
      :battery

      iex> %{OpenTelemetry.SemConv.Incubating.HWAttributes.hw_type() => OpenTelemetry.SemConv.Incubating.HWAttributes.hw_type_values().battery}
      %{:"hw.type" => :battery}

  ### Erlang

  ```erlang
  ?HW_TYPE.
  'hw.type'

  ?HW_TYPE_VALUES_BATTERY.
  'battery'

  \#{?HW_TYPE => ?HW_TYPE_VALUES_BATTERY}.
  \#{'hw.type' => 'battery'}
  ```

  <!-- tabs-close -->
  """
  @spec hw_type :: :"hw.type"
  def hw_type do
    :"hw.type"
  end

  @spec hw_type_values() :: hw_type_values()
  def hw_type_values() do
    %{
      :battery => :battery,
      :cpu => :cpu,
      :disk_controller => :disk_controller,
      :enclosure => :enclosure,
      :fan => :fan,
      :gpu => :gpu,
      :logical_disk => :logical_disk,
      :memory => :memory,
      :network => :network,
      :physical_disk => :physical_disk,
      :power_supply => :power_supply,
      :tape_drive => :tape_drive,
      :temperature => :temperature,
      :voltage => :voltage
    }
  end
end
