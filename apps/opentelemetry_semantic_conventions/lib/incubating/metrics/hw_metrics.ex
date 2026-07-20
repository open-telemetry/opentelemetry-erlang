defmodule OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Hw metrics.
  """
  @doc """
  Remaining fraction of battery charge.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_battery_charge()
      :"hw.battery.charge"

  ### Erlang

  ```erlang
  ?HW_BATTERY_CHARGE.
  'hw.battery.charge'
  ```

  <!-- tabs-close -->
  """

  @spec hw_battery_charge :: :"hw.battery.charge"
  def hw_battery_charge do
    :"hw.battery.charge"
  end

  @doc """
  Lower limit of battery charge fraction to ensure proper operation.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_battery_charge_limit()
      :"hw.battery.charge.limit"

  ### Erlang

  ```erlang
  ?HW_BATTERY_CHARGE_LIMIT.
  'hw.battery.charge.limit'
  ```

  <!-- tabs-close -->
  """

  @spec hw_battery_charge_limit :: :"hw.battery.charge.limit"
  def hw_battery_charge_limit do
    :"hw.battery.charge.limit"
  end

  @doc """
  Time left before battery is completely charged or discharged.

  Instrument: `gauge`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_battery_time_left()
      :"hw.battery.time_left"

  ### Erlang

  ```erlang
  ?HW_BATTERY_TIME_LEFT.
  'hw.battery.time_left'
  ```

  <!-- tabs-close -->
  """

  @spec hw_battery_time_left :: :"hw.battery.time_left"
  def hw_battery_time_left do
    :"hw.battery.time_left"
  end

  @doc """
  CPU current frequency.

  Instrument: `gauge`
  Unit: `Hz`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_cpu_speed()
      :"hw.cpu.speed"

  ### Erlang

  ```erlang
  ?HW_CPU_SPEED.
  'hw.cpu.speed'
  ```

  <!-- tabs-close -->
  """

  @spec hw_cpu_speed :: :"hw.cpu.speed"
  def hw_cpu_speed do
    :"hw.cpu.speed"
  end

  @doc """
  CPU maximum frequency.

  Instrument: `gauge`
  Unit: `Hz`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_cpu_speed_limit()
      :"hw.cpu.speed.limit"

  ### Erlang

  ```erlang
  ?HW_CPU_SPEED_LIMIT.
  'hw.cpu.speed.limit'
  ```

  <!-- tabs-close -->
  """

  @spec hw_cpu_speed_limit :: :"hw.cpu.speed.limit"
  def hw_cpu_speed_limit do
    :"hw.cpu.speed.limit"
  end

  @doc """
  Energy consumed by the component.

  Instrument: `counter`
  Unit: `J`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_energy()
      :"hw.energy"

  ### Erlang

  ```erlang
  ?HW_ENERGY.
  'hw.energy'
  ```

  <!-- tabs-close -->
  """

  @spec hw_energy :: :"hw.energy"
  def hw_energy do
    :"hw.energy"
  end

  @doc """
  Number of errors encountered by the component.

  Instrument: `counter`
  Unit: `{error}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_errors()
      :"hw.errors"

  ### Erlang

  ```erlang
  ?HW_ERRORS.
  'hw.errors'
  ```

  <!-- tabs-close -->
  """

  @spec hw_errors :: :"hw.errors"
  def hw_errors do
    :"hw.errors"
  end

  @doc """
  Fan speed in revolutions per minute.

  Instrument: `gauge`
  Unit: `rpm`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_fan_speed()
      :"hw.fan.speed"

  ### Erlang

  ```erlang
  ?HW_FAN_SPEED.
  'hw.fan.speed'
  ```

  <!-- tabs-close -->
  """

  @spec hw_fan_speed :: :"hw.fan.speed"
  def hw_fan_speed do
    :"hw.fan.speed"
  end

  @doc """
  Speed limit in rpm.

  Instrument: `gauge`
  Unit: `rpm`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_fan_speed_limit()
      :"hw.fan.speed.limit"

  ### Erlang

  ```erlang
  ?HW_FAN_SPEED_LIMIT.
  'hw.fan.speed.limit'
  ```

  <!-- tabs-close -->
  """

  @spec hw_fan_speed_limit :: :"hw.fan.speed.limit"
  def hw_fan_speed_limit do
    :"hw.fan.speed.limit"
  end

  @doc """
  Fan speed expressed as a fraction of its maximum speed.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_fan_speed_ratio()
      :"hw.fan.speed_ratio"

  ### Erlang

  ```erlang
  ?HW_FAN_SPEED_RATIO.
  'hw.fan.speed_ratio'
  ```

  <!-- tabs-close -->
  """

  @spec hw_fan_speed_ratio :: :"hw.fan.speed_ratio"
  def hw_fan_speed_ratio do
    :"hw.fan.speed_ratio"
  end

  @doc """
  Received and transmitted bytes by the GPU.

  Instrument: `counter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_gpu_io()
      :"hw.gpu.io"

  ### Erlang

  ```erlang
  ?HW_GPU_IO.
  'hw.gpu.io'
  ```

  <!-- tabs-close -->
  """

  @spec hw_gpu_io :: :"hw.gpu.io"
  def hw_gpu_io do
    :"hw.gpu.io"
  end

  @doc """
  Size of the GPU memory.

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_gpu_memory_limit()
      :"hw.gpu.memory.limit"

  ### Erlang

  ```erlang
  ?HW_GPU_MEMORY_LIMIT.
  'hw.gpu.memory.limit'
  ```

  <!-- tabs-close -->
  """

  @spec hw_gpu_memory_limit :: :"hw.gpu.memory.limit"
  def hw_gpu_memory_limit do
    :"hw.gpu.memory.limit"
  end

  @doc """
  GPU memory used.

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_gpu_memory_usage()
      :"hw.gpu.memory.usage"

  ### Erlang

  ```erlang
  ?HW_GPU_MEMORY_USAGE.
  'hw.gpu.memory.usage'
  ```

  <!-- tabs-close -->
  """

  @spec hw_gpu_memory_usage :: :"hw.gpu.memory.usage"
  def hw_gpu_memory_usage do
    :"hw.gpu.memory.usage"
  end

  @doc """
  Fraction of GPU memory used.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_gpu_memory_utilization()
      :"hw.gpu.memory.utilization"

  ### Erlang

  ```erlang
  ?HW_GPU_MEMORY_UTILIZATION.
  'hw.gpu.memory.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec hw_gpu_memory_utilization :: :"hw.gpu.memory.utilization"
  def hw_gpu_memory_utilization do
    :"hw.gpu.memory.utilization"
  end

  @doc """
  Fraction of time spent in a specific task.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_gpu_utilization()
      :"hw.gpu.utilization"

  ### Erlang

  ```erlang
  ?HW_GPU_UTILIZATION.
  'hw.gpu.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec hw_gpu_utilization :: :"hw.gpu.utilization"
  def hw_gpu_utilization do
    :"hw.gpu.utilization"
  end

  @doc """
  Ambient (external) temperature of the physical host.

  Instrument: `gauge`
  Unit: `Cel`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_host_ambient_temperature()
      :"hw.host.ambient_temperature"

  ### Erlang

  ```erlang
  ?HW_HOST_AMBIENT_TEMPERATURE.
  'hw.host.ambient_temperature'
  ```

  <!-- tabs-close -->
  """

  @spec hw_host_ambient_temperature :: :"hw.host.ambient_temperature"
  def hw_host_ambient_temperature do
    :"hw.host.ambient_temperature"
  end

  @doc """
  Total energy consumed by the entire physical host, in joules.

  Instrument: `counter`
  Unit: `J`
  ### Notes

  The overall energy usage of a host **MUST** be reported using the specific `hw.host.energy` and `hw.host.power` metrics **only**, instead of the generic `hw.energy` and `hw.power` described in the previous section, to prevent summing up overlapping values.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_host_energy()
      :"hw.host.energy"

  ### Erlang

  ```erlang
  ?HW_HOST_ENERGY.
  'hw.host.energy'
  ```

  <!-- tabs-close -->
  """

  @spec hw_host_energy :: :"hw.host.energy"
  def hw_host_energy do
    :"hw.host.energy"
  end

  @doc """
  By how many degrees Celsius the temperature of the physical host can be increased, before reaching a warning threshold on one of the internal sensors.


  Instrument: `gauge`
  Unit: `Cel`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_host_heating_margin()
      :"hw.host.heating_margin"

  ### Erlang

  ```erlang
  ?HW_HOST_HEATING_MARGIN.
  'hw.host.heating_margin'
  ```

  <!-- tabs-close -->
  """

  @spec hw_host_heating_margin :: :"hw.host.heating_margin"
  def hw_host_heating_margin do
    :"hw.host.heating_margin"
  end

  @doc """
  Instantaneous power consumed by the entire physical host in Watts (`hw.host.energy` is preferred).


  Instrument: `gauge`
  Unit: `W`
  ### Notes

  The overall energy usage of a host **MUST** be reported using the specific `hw.host.energy` and `hw.host.power` metrics **only**, instead of the generic `hw.energy` and `hw.power` described in the previous section, to prevent summing up overlapping values.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_host_power()
      :"hw.host.power"

  ### Erlang

  ```erlang
  ?HW_HOST_POWER.
  'hw.host.power'
  ```

  <!-- tabs-close -->
  """

  @spec hw_host_power :: :"hw.host.power"
  def hw_host_power do
    :"hw.host.power"
  end

  @doc """
  Size of the logical disk.

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_logical_disk_limit()
      :"hw.logical_disk.limit"

  ### Erlang

  ```erlang
  ?HW_LOGICAL_DISK_LIMIT.
  'hw.logical_disk.limit'
  ```

  <!-- tabs-close -->
  """

  @spec hw_logical_disk_limit :: :"hw.logical_disk.limit"
  def hw_logical_disk_limit do
    :"hw.logical_disk.limit"
  end

  @doc """
  Logical disk space usage.

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_logical_disk_usage()
      :"hw.logical_disk.usage"

  ### Erlang

  ```erlang
  ?HW_LOGICAL_DISK_USAGE.
  'hw.logical_disk.usage'
  ```

  <!-- tabs-close -->
  """

  @spec hw_logical_disk_usage :: :"hw.logical_disk.usage"
  def hw_logical_disk_usage do
    :"hw.logical_disk.usage"
  end

  @doc """
  Logical disk space utilization as a fraction.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_logical_disk_utilization()
      :"hw.logical_disk.utilization"

  ### Erlang

  ```erlang
  ?HW_LOGICAL_DISK_UTILIZATION.
  'hw.logical_disk.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec hw_logical_disk_utilization :: :"hw.logical_disk.utilization"
  def hw_logical_disk_utilization do
    :"hw.logical_disk.utilization"
  end

  @doc """
  Size of the memory module.

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_memory_size()
      :"hw.memory.size"

  ### Erlang

  ```erlang
  ?HW_MEMORY_SIZE.
  'hw.memory.size'
  ```

  <!-- tabs-close -->
  """

  @spec hw_memory_size :: :"hw.memory.size"
  def hw_memory_size do
    :"hw.memory.size"
  end

  @doc """
  Link speed.

  Instrument: `updowncounter`
  Unit: `By/s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_network_bandwidth_limit()
      :"hw.network.bandwidth.limit"

  ### Erlang

  ```erlang
  ?HW_NETWORK_BANDWIDTH_LIMIT.
  'hw.network.bandwidth.limit'
  ```

  <!-- tabs-close -->
  """

  @spec hw_network_bandwidth_limit :: :"hw.network.bandwidth.limit"
  def hw_network_bandwidth_limit do
    :"hw.network.bandwidth.limit"
  end

  @doc """
  Utilization of the network bandwidth as a fraction.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_network_bandwidth_utilization()
      :"hw.network.bandwidth.utilization"

  ### Erlang

  ```erlang
  ?HW_NETWORK_BANDWIDTH_UTILIZATION.
  'hw.network.bandwidth.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec hw_network_bandwidth_utilization :: :"hw.network.bandwidth.utilization"
  def hw_network_bandwidth_utilization do
    :"hw.network.bandwidth.utilization"
  end

  @doc """
  Received and transmitted network traffic in bytes.

  Instrument: `counter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_network_io()
      :"hw.network.io"

  ### Erlang

  ```erlang
  ?HW_NETWORK_IO.
  'hw.network.io'
  ```

  <!-- tabs-close -->
  """

  @spec hw_network_io :: :"hw.network.io"
  def hw_network_io do
    :"hw.network.io"
  end

  @doc """
  Received and transmitted network traffic in packets (or frames).

  Instrument: `counter`
  Unit: `{packet}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_network_packets()
      :"hw.network.packets"

  ### Erlang

  ```erlang
  ?HW_NETWORK_PACKETS.
  'hw.network.packets'
  ```

  <!-- tabs-close -->
  """

  @spec hw_network_packets :: :"hw.network.packets"
  def hw_network_packets do
    :"hw.network.packets"
  end

  @doc """
  Link status: `1` (up) or `0` (down).

  Instrument: `updowncounter`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_network_up()
      :"hw.network.up"

  ### Erlang

  ```erlang
  ?HW_NETWORK_UP.
  'hw.network.up'
  ```

  <!-- tabs-close -->
  """

  @spec hw_network_up :: :"hw.network.up"
  def hw_network_up do
    :"hw.network.up"
  end

  @doc """
  Endurance remaining for this SSD disk.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_physical_disk_endurance_utilization()
      :"hw.physical_disk.endurance_utilization"

  ### Erlang

  ```erlang
  ?HW_PHYSICAL_DISK_ENDURANCE_UTILIZATION.
  'hw.physical_disk.endurance_utilization'
  ```

  <!-- tabs-close -->
  """

  @spec hw_physical_disk_endurance_utilization :: :"hw.physical_disk.endurance_utilization"
  def hw_physical_disk_endurance_utilization do
    :"hw.physical_disk.endurance_utilization"
  end

  @doc """
  Size of the disk.

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_physical_disk_size()
      :"hw.physical_disk.size"

  ### Erlang

  ```erlang
  ?HW_PHYSICAL_DISK_SIZE.
  'hw.physical_disk.size'
  ```

  <!-- tabs-close -->
  """

  @spec hw_physical_disk_size :: :"hw.physical_disk.size"
  def hw_physical_disk_size do
    :"hw.physical_disk.size"
  end

  @doc """
  Value of the corresponding [S.M.A.R.T.](https://wikipedia.org/wiki/S.M.A.R.T.) (Self-Monitoring, Analysis, and Reporting Technology) attribute.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_physical_disk_smart()
      :"hw.physical_disk.smart"

  ### Erlang

  ```erlang
  ?HW_PHYSICAL_DISK_SMART.
  'hw.physical_disk.smart'
  ```

  <!-- tabs-close -->
  """

  @spec hw_physical_disk_smart :: :"hw.physical_disk.smart"
  def hw_physical_disk_smart do
    :"hw.physical_disk.smart"
  end

  @doc """
  Instantaneous power consumed by the component.

  Instrument: `gauge`
  Unit: `W`
  ### Notes

  It is recommended to report `hw.energy` instead of `hw.power` when possible.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_power()
      :"hw.power"

  ### Erlang

  ```erlang
  ?HW_POWER.
  'hw.power'
  ```

  <!-- tabs-close -->
  """

  @spec hw_power :: :"hw.power"
  def hw_power do
    :"hw.power"
  end

  @doc """
  Maximum power output of the power supply.

  Instrument: `updowncounter`
  Unit: `W`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_power_supply_limit()
      :"hw.power_supply.limit"

  ### Erlang

  ```erlang
  ?HW_POWER_SUPPLY_LIMIT.
  'hw.power_supply.limit'
  ```

  <!-- tabs-close -->
  """

  @spec hw_power_supply_limit :: :"hw.power_supply.limit"
  def hw_power_supply_limit do
    :"hw.power_supply.limit"
  end

  @doc """
  Current power output of the power supply.

  Instrument: `updowncounter`
  Unit: `W`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_power_supply_usage()
      :"hw.power_supply.usage"

  ### Erlang

  ```erlang
  ?HW_POWER_SUPPLY_USAGE.
  'hw.power_supply.usage'
  ```

  <!-- tabs-close -->
  """

  @spec hw_power_supply_usage :: :"hw.power_supply.usage"
  def hw_power_supply_usage do
    :"hw.power_supply.usage"
  end

  @doc """
  Utilization of the power supply as a fraction of its maximum output.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_power_supply_utilization()
      :"hw.power_supply.utilization"

  ### Erlang

  ```erlang
  ?HW_POWER_SUPPLY_UTILIZATION.
  'hw.power_supply.utilization'
  ```

  <!-- tabs-close -->
  """

  @spec hw_power_supply_utilization :: :"hw.power_supply.utilization"
  def hw_power_supply_utilization do
    :"hw.power_supply.utilization"
  end

  @doc """
  Operational status: `1` (true) or `0` (false) for each of the possible states.

  Instrument: `updowncounter`
  Unit: `1`
  ### Notes

  `hw.status` is currently specified as an *UpDownCounter* but would ideally be represented using a [*StateSet* as defined in OpenMetrics](https://github.com/prometheus/OpenMetrics/blob/v1.0.0/specification/OpenMetrics.md#stateset). This semantic convention will be updated once *StateSet* is specified in OpenTelemetry. This planned change is not expected to have any consequence on the way users query their timeseries backend to retrieve the values of `hw.status` over time.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_status()
      :"hw.status"

  ### Erlang

  ```erlang
  ?HW_STATUS.
  'hw.status'
  ```

  <!-- tabs-close -->
  """

  @spec hw_status :: :"hw.status"
  def hw_status do
    :"hw.status"
  end

  @doc """
  Operations performed by the tape drive.

  Instrument: `counter`
  Unit: `{operation}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_tape_drive_operations()
      :"hw.tape_drive.operations"

  ### Erlang

  ```erlang
  ?HW_TAPE_DRIVE_OPERATIONS.
  'hw.tape_drive.operations'
  ```

  <!-- tabs-close -->
  """

  @spec hw_tape_drive_operations :: :"hw.tape_drive.operations"
  def hw_tape_drive_operations do
    :"hw.tape_drive.operations"
  end

  @doc """
  Temperature in degrees Celsius.

  Instrument: `gauge`
  Unit: `Cel`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_temperature()
      :"hw.temperature"

  ### Erlang

  ```erlang
  ?HW_TEMPERATURE.
  'hw.temperature'
  ```

  <!-- tabs-close -->
  """

  @spec hw_temperature :: :"hw.temperature"
  def hw_temperature do
    :"hw.temperature"
  end

  @doc """
  Temperature limit in degrees Celsius.

  Instrument: `gauge`
  Unit: `Cel`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_temperature_limit()
      :"hw.temperature.limit"

  ### Erlang

  ```erlang
  ?HW_TEMPERATURE_LIMIT.
  'hw.temperature.limit'
  ```

  <!-- tabs-close -->
  """

  @spec hw_temperature_limit :: :"hw.temperature.limit"
  def hw_temperature_limit do
    :"hw.temperature.limit"
  end

  @doc """
  Voltage measured by the sensor.

  Instrument: `gauge`
  Unit: `V`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_voltage()
      :"hw.voltage"

  ### Erlang

  ```erlang
  ?HW_VOLTAGE.
  'hw.voltage'
  ```

  <!-- tabs-close -->
  """

  @spec hw_voltage :: :"hw.voltage"
  def hw_voltage do
    :"hw.voltage"
  end

  @doc """
  Voltage limit in Volts.

  Instrument: `gauge`
  Unit: `V`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_voltage_limit()
      :"hw.voltage.limit"

  ### Erlang

  ```erlang
  ?HW_VOLTAGE_LIMIT.
  'hw.voltage.limit'
  ```

  <!-- tabs-close -->
  """

  @spec hw_voltage_limit :: :"hw.voltage.limit"
  def hw_voltage_limit do
    :"hw.voltage.limit"
  end

  @doc """
  Nominal (expected) voltage.

  Instrument: `gauge`
  Unit: `V`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HwMetrics.hw_voltage_nominal()
      :"hw.voltage.nominal"

  ### Erlang

  ```erlang
  ?HW_VOLTAGE_NOMINAL.
  'hw.voltage.nominal'
  ```

  <!-- tabs-close -->
  """

  @spec hw_voltage_nominal :: :"hw.voltage.nominal"
  def hw_voltage_nominal do
    :"hw.voltage.nominal"
  end
end
