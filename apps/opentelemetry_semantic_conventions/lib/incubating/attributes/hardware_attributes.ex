defmodule OpenTelemetry.SemConv.Incubating.HardwareAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Hardware attributes.
  """

  @doc """
  Design capacity in Watts-hours or Amper-hours

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["9.3Ah", "50Wh"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_battery_capacity()
      :"hw.battery.capacity"

  ### Erlang

  ```erlang
  ?HW_BATTERY_CAPACITY.
  'hw.battery.capacity'
  ```

  <!-- tabs-close -->
  """
  @spec hw_battery_capacity :: :"hw.battery.capacity"
  def hw_battery_capacity do
    :"hw.battery.capacity"
  end

  @doc """
  Battery [chemistry](https://schemas.dmtf.org/wbem/cim-html/2.31.0/CIM_Battery.html), e.g. Lithium-Ion, Nickel-Cadmium, etc.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Li-ion", "NiMH"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_battery_chemistry()
      :"hw.battery.chemistry"

  ### Erlang

  ```erlang
  ?HW_BATTERY_CHEMISTRY.
  'hw.battery.chemistry'
  ```

  <!-- tabs-close -->
  """
  @spec hw_battery_chemistry :: :"hw.battery.chemistry"
  def hw_battery_chemistry do
    :"hw.battery.chemistry"
  end

  @typedoc """
  The current state of the battery


  ### Enum Values
  * `:charging` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Charging
  * `:discharging` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Discharging
  """
  @type hw_battery_state_values() :: %{
          :charging => :charging,
          :discharging => :discharging
        }
  @doc """
  The current state of the battery



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_battery_state()
      :"hw.battery.state"

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_battery_state_values().charging
      :charging

      iex> %{OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_battery_state() => OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_battery_state_values().charging}
      %{:"hw.battery.state" => :charging}

  ### Erlang

  ```erlang
  ?HW_BATTERY_STATE.
  'hw.battery.state'

  ?HW_BATTERY_STATE_VALUES_CHARGING.
  'charging'

  \#{?HW_BATTERY_STATE => ?HW_BATTERY_STATE_VALUES_CHARGING}.
  \#{'hw.battery.state' => 'charging'}
  ```

  <!-- tabs-close -->
  """
  @spec hw_battery_state :: :"hw.battery.state"
  def hw_battery_state do
    :"hw.battery.state"
  end

  @spec hw_battery_state_values() :: hw_battery_state_values()
  def hw_battery_state_values() do
    %{
      :charging => :charging,
      :discharging => :discharging
    }
  end

  @doc """
  BIOS version of the hardware component

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1.2.3"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_bios_version()
      :"hw.bios_version"

  ### Erlang

  ```erlang
  ?HW_BIOS_VERSION.
  'hw.bios_version'
  ```

  <!-- tabs-close -->
  """
  @spec hw_bios_version :: :"hw.bios_version"
  def hw_bios_version do
    :"hw.bios_version"
  end

  @doc """
  Driver version for the hardware component

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["10.2.1-3"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_driver_version()
      :"hw.driver_version"

  ### Erlang

  ```erlang
  ?HW_DRIVER_VERSION.
  'hw.driver_version'
  ```

  <!-- tabs-close -->
  """
  @spec hw_driver_version :: :"hw.driver_version"
  def hw_driver_version do
    :"hw.driver_version"
  end

  @doc """
  Type of the enclosure (useful for modular systems)

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Computer", "Storage", "Switch"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_enclosure_type()
      :"hw.enclosure.type"

  ### Erlang

  ```erlang
  ?HW_ENCLOSURE_TYPE.
  'hw.enclosure.type'
  ```

  <!-- tabs-close -->
  """
  @spec hw_enclosure_type :: :"hw.enclosure.type"
  def hw_enclosure_type do
    :"hw.enclosure.type"
  end

  @doc """
  Firmware version of the hardware component

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2.0.1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_firmware_version()
      :"hw.firmware_version"

  ### Erlang

  ```erlang
  ?HW_FIRMWARE_VERSION.
  'hw.firmware_version'
  ```

  <!-- tabs-close -->
  """
  @spec hw_firmware_version :: :"hw.firmware_version"
  def hw_firmware_version do
    :"hw.firmware_version"
  end

  @typedoc """
  Type of task the GPU is performing


  ### Enum Values
  * `:decoder` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Decoder
  * `:encoder` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Encoder
  * `:general` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - General
  """
  @type hw_gpu_task_values() :: %{
          :decoder => :decoder,
          :encoder => :encoder,
          :general => :general
        }
  @doc """
  Type of task the GPU is performing



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_gpu_task()
      :"hw.gpu.task"

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_gpu_task_values().decoder
      :decoder

      iex> %{OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_gpu_task() => OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_gpu_task_values().decoder}
      %{:"hw.gpu.task" => :decoder}

  ### Erlang

  ```erlang
  ?HW_GPU_TASK.
  'hw.gpu.task'

  ?HW_GPU_TASK_VALUES_DECODER.
  'decoder'

  \#{?HW_GPU_TASK => ?HW_GPU_TASK_VALUES_DECODER}.
  \#{'hw.gpu.task' => 'decoder'}
  ```

  <!-- tabs-close -->
  """
  @spec hw_gpu_task :: :"hw.gpu.task"
  def hw_gpu_task do
    :"hw.gpu.task"
  end

  @spec hw_gpu_task_values() :: hw_gpu_task_values()
  def hw_gpu_task_values() do
    %{
      :decoder => :decoder,
      :encoder => :encoder,
      :general => :general
    }
  end

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

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_id()
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

  @typedoc """
  Type of limit for hardware components


  ### Enum Values
  * `:critical` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Critical
  * `:degraded` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Degraded
  * `:high_critical` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - High Critical
  * `:high_degraded` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - High Degraded
  * `:low_critical` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Low Critical
  * `:low_degraded` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Low Degraded
  * `:max` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Maximum
  * `:throttled` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Throttled
  * `:turbo` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Turbo
  """
  @type hw_limit_type_values() :: %{
          :critical => :critical,
          :degraded => :degraded,
          :high_critical => :"high.critical",
          :high_degraded => :"high.degraded",
          :low_critical => :"low.critical",
          :low_degraded => :"low.degraded",
          :max => :max,
          :throttled => :throttled,
          :turbo => :turbo
        }
  @doc """
  Type of limit for hardware components



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_limit_type()
      :"hw.limit_type"

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_limit_type_values().critical
      :critical

      iex> %{OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_limit_type() => OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_limit_type_values().critical}
      %{:"hw.limit_type" => :critical}

  ### Erlang

  ```erlang
  ?HW_LIMIT_TYPE.
  'hw.limit_type'

  ?HW_LIMIT_TYPE_VALUES_CRITICAL.
  'critical'

  \#{?HW_LIMIT_TYPE => ?HW_LIMIT_TYPE_VALUES_CRITICAL}.
  \#{'hw.limit_type' => 'critical'}
  ```

  <!-- tabs-close -->
  """
  @spec hw_limit_type :: :"hw.limit_type"
  def hw_limit_type do
    :"hw.limit_type"
  end

  @spec hw_limit_type_values() :: hw_limit_type_values()
  def hw_limit_type_values() do
    %{
      :critical => :critical,
      :degraded => :degraded,
      :high_critical => :"high.critical",
      :high_degraded => :"high.degraded",
      :low_critical => :"low.critical",
      :low_degraded => :"low.degraded",
      :max => :max,
      :throttled => :throttled,
      :turbo => :turbo
    }
  end

  @doc """
  RAID Level of the logical disk

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["RAID0+1", "RAID5", "RAID10"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_logical_disk_raid_level()
      :"hw.logical_disk.raid_level"

  ### Erlang

  ```erlang
  ?HW_LOGICAL_DISK_RAID_LEVEL.
  'hw.logical_disk.raid_level'
  ```

  <!-- tabs-close -->
  """
  @spec hw_logical_disk_raid_level :: :"hw.logical_disk.raid_level"
  def hw_logical_disk_raid_level do
    :"hw.logical_disk.raid_level"
  end

  @typedoc """
  State of the logical disk space usage


  ### Enum Values
  * `:used` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Used
  * `:free` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Free
  """
  @type hw_logical_disk_state_values() :: %{
          :used => :used,
          :free => :free
        }
  @doc """
  State of the logical disk space usage



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_logical_disk_state()
      :"hw.logical_disk.state"

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_logical_disk_state_values().used
      :used

      iex> %{OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_logical_disk_state() => OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_logical_disk_state_values().used}
      %{:"hw.logical_disk.state" => :used}

  ### Erlang

  ```erlang
  ?HW_LOGICAL_DISK_STATE.
  'hw.logical_disk.state'

  ?HW_LOGICAL_DISK_STATE_VALUES_USED.
  'used'

  \#{?HW_LOGICAL_DISK_STATE => ?HW_LOGICAL_DISK_STATE_VALUES_USED}.
  \#{'hw.logical_disk.state' => 'used'}
  ```

  <!-- tabs-close -->
  """
  @spec hw_logical_disk_state :: :"hw.logical_disk.state"
  def hw_logical_disk_state do
    :"hw.logical_disk.state"
  end

  @spec hw_logical_disk_state_values() :: hw_logical_disk_state_values()
  def hw_logical_disk_state_values() do
    %{
      :used => :used,
      :free => :free
    }
  end

  @doc """
  Type of the memory module

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["DDR4", "DDR5", "LPDDR5"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_memory_type()
      :"hw.memory.type"

  ### Erlang

  ```erlang
  ?HW_MEMORY_TYPE.
  'hw.memory.type'
  ```

  <!-- tabs-close -->
  """
  @spec hw_memory_type :: :"hw.memory.type"
  def hw_memory_type do
    :"hw.memory.type"
  end

  @doc """
  Descriptive model name of the hardware component

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["PERC H740P", "Intel(R) Core(TM) i7-10700K", "Dell XPS 15 Battery"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_model()
      :"hw.model"

  ### Erlang

  ```erlang
  ?HW_MODEL.
  'hw.model'
  ```

  <!-- tabs-close -->
  """
  @spec hw_model :: :"hw.model"
  def hw_model do
    :"hw.model"
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

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_name()
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
  Logical addresses of the adapter (e.g. IP address, or WWPN)

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  [["172.16.8.21", "57.11.193.42"]]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_network_logical_addresses()
      :"hw.network.logical_addresses"

  ### Erlang

  ```erlang
  ?HW_NETWORK_LOGICAL_ADDRESSES.
  'hw.network.logical_addresses'
  ```

  <!-- tabs-close -->
  """
  @spec hw_network_logical_addresses :: :"hw.network.logical_addresses"
  def hw_network_logical_addresses do
    :"hw.network.logical_addresses"
  end

  @doc """
  Physical address of the adapter (e.g. MAC address, or WWNN)

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["00-90-F5-E9-7B-36"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_network_physical_address()
      :"hw.network.physical_address"

  ### Erlang

  ```erlang
  ?HW_NETWORK_PHYSICAL_ADDRESS.
  'hw.network.physical_address'
  ```

  <!-- tabs-close -->
  """
  @spec hw_network_physical_address :: :"hw.network.physical_address"
  def hw_network_physical_address do
    :"hw.network.physical_address"
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

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_parent()
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

  @doc """
  [S.M.A.R.T.](https://wikipedia.org/wiki/S.M.A.R.T.) (Self-Monitoring, Analysis, and Reporting Technology) attribute of the physical disk

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Spin Retry Count", "Seek Error Rate", "Raw Read Error Rate"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_physical_disk_smart_attribute()
      :"hw.physical_disk.smart_attribute"

  ### Erlang

  ```erlang
  ?HW_PHYSICAL_DISK_SMART_ATTRIBUTE.
  'hw.physical_disk.smart_attribute'
  ```

  <!-- tabs-close -->
  """
  @spec hw_physical_disk_smart_attribute :: :"hw.physical_disk.smart_attribute"
  def hw_physical_disk_smart_attribute do
    :"hw.physical_disk.smart_attribute"
  end

  @typedoc """
  State of the physical disk endurance utilization


  ### Enum Values
  * `:remaining` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Remaining
  """
  @type hw_physical_disk_state_values() :: %{
          :remaining => :remaining
        }
  @doc """
  State of the physical disk endurance utilization



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_physical_disk_state()
      :"hw.physical_disk.state"

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_physical_disk_state_values().remaining
      :remaining

      iex> %{OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_physical_disk_state() => OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_physical_disk_state_values().remaining}
      %{:"hw.physical_disk.state" => :remaining}

  ### Erlang

  ```erlang
  ?HW_PHYSICAL_DISK_STATE.
  'hw.physical_disk.state'

  ?HW_PHYSICAL_DISK_STATE_VALUES_REMAINING.
  'remaining'

  \#{?HW_PHYSICAL_DISK_STATE => ?HW_PHYSICAL_DISK_STATE_VALUES_REMAINING}.
  \#{'hw.physical_disk.state' => 'remaining'}
  ```

  <!-- tabs-close -->
  """
  @spec hw_physical_disk_state :: :"hw.physical_disk.state"
  def hw_physical_disk_state do
    :"hw.physical_disk.state"
  end

  @spec hw_physical_disk_state_values() :: hw_physical_disk_state_values()
  def hw_physical_disk_state_values() do
    %{
      :remaining => :remaining
    }
  end

  @doc """
  Type of the physical disk

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["HDD", "SSD", "10K"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_physical_disk_type()
      :"hw.physical_disk.type"

  ### Erlang

  ```erlang
  ?HW_PHYSICAL_DISK_TYPE.
  'hw.physical_disk.type'
  ```

  <!-- tabs-close -->
  """
  @spec hw_physical_disk_type :: :"hw.physical_disk.type"
  def hw_physical_disk_type do
    :"hw.physical_disk.type"
  end

  @doc """
  Location of the sensor

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["cpu0", "ps1", "INLET", "CPU0_DIE", "AMBIENT", "MOTHERBOARD", "PS0 V3_3", "MAIN_12V", "CPU_VCORE"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_sensor_location()
      :"hw.sensor_location"

  ### Erlang

  ```erlang
  ?HW_SENSOR_LOCATION.
  'hw.sensor_location'
  ```

  <!-- tabs-close -->
  """
  @spec hw_sensor_location :: :"hw.sensor_location"
  def hw_sensor_location do
    :"hw.sensor_location"
  end

  @doc """
  Serial number of the hardware component

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["CNFCP0123456789"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_serial_number()
      :"hw.serial_number"

  ### Erlang

  ```erlang
  ?HW_SERIAL_NUMBER.
  'hw.serial_number'
  ```

  <!-- tabs-close -->
  """
  @spec hw_serial_number :: :"hw.serial_number"
  def hw_serial_number do
    :"hw.serial_number"
  end

  @typedoc """
  The current state of the component


  ### Enum Values
  * `:degraded` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Degraded
  * `:failed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Failed
  * `:needs_cleaning` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Needs Cleaning
  * `:ok` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OK
  * `:predicted_failure` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Predicted Failure
  """
  @type hw_state_values() :: %{
          :degraded => :degraded,
          :failed => :failed,
          :needs_cleaning => :needs_cleaning,
          :ok => :ok,
          :predicted_failure => :predicted_failure
        }
  @doc """
  The current state of the component



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_state()
      :"hw.state"

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_state_values().degraded
      :degraded

      iex> %{OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_state() => OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_state_values().degraded}
      %{:"hw.state" => :degraded}

  ### Erlang

  ```erlang
  ?HW_STATE.
  'hw.state'

  ?HW_STATE_VALUES_DEGRADED.
  'degraded'

  \#{?HW_STATE => ?HW_STATE_VALUES_DEGRADED}.
  \#{'hw.state' => 'degraded'}
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
      :degraded => :degraded,
      :failed => :failed,
      :needs_cleaning => :needs_cleaning,
      :ok => :ok,
      :predicted_failure => :predicted_failure
    }
  end

  @typedoc """
  Type of tape drive operation


  ### Enum Values
  * `:mount` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Mount
  * `:unmount` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Unmount
  * `:clean` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Clean
  """
  @type hw_tape_drive_operation_type_values() :: %{
          :mount => :mount,
          :unmount => :unmount,
          :clean => :clean
        }
  @doc """
  Type of tape drive operation



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_tape_drive_operation_type()
      :"hw.tape_drive.operation_type"

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_tape_drive_operation_type_values().mount
      :mount

      iex> %{OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_tape_drive_operation_type() => OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_tape_drive_operation_type_values().mount}
      %{:"hw.tape_drive.operation_type" => :mount}

  ### Erlang

  ```erlang
  ?HW_TAPE_DRIVE_OPERATION_TYPE.
  'hw.tape_drive.operation_type'

  ?HW_TAPE_DRIVE_OPERATION_TYPE_VALUES_MOUNT.
  'mount'

  \#{?HW_TAPE_DRIVE_OPERATION_TYPE => ?HW_TAPE_DRIVE_OPERATION_TYPE_VALUES_MOUNT}.
  \#{'hw.tape_drive.operation_type' => 'mount'}
  ```

  <!-- tabs-close -->
  """
  @spec hw_tape_drive_operation_type :: :"hw.tape_drive.operation_type"
  def hw_tape_drive_operation_type do
    :"hw.tape_drive.operation_type"
  end

  @spec hw_tape_drive_operation_type_values() :: hw_tape_drive_operation_type_values()
  def hw_tape_drive_operation_type_values() do
    %{
      :mount => :mount,
      :unmount => :unmount,
      :clean => :clean
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

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_type()
      :"hw.type"

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_type_values().battery
      :battery

      iex> %{OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_type() => OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_type_values().battery}
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

  @doc """
  Vendor name of the hardware component

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Dell", "HP", "Intel", "AMD", "LSI", "Lenovo"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HardwareAttributes.hw_vendor()
      :"hw.vendor"

  ### Erlang

  ```erlang
  ?HW_VENDOR.
  'hw.vendor'
  ```

  <!-- tabs-close -->
  """
  @spec hw_vendor :: :"hw.vendor"
  def hw_vendor do
    :"hw.vendor"
  end
end
