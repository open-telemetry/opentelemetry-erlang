
%%%------------------------------------------------------------------------
%% Copyright The OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%-------------------------------------------------------------------------

%% Remaining fraction of battery charge.
-define(HW_BATTERY_CHARGE, 'hw.battery.charge').


%% Lower limit of battery charge fraction to ensure proper operation.
-define(HW_BATTERY_CHARGE_LIMIT, 'hw.battery.charge.limit').


%% Time left before battery is completely charged or discharged.
-define(HW_BATTERY_TIME_LEFT, 'hw.battery.time_left').


%% CPU current frequency.
-define(HW_CPU_SPEED, 'hw.cpu.speed').


%% CPU maximum frequency.
-define(HW_CPU_SPEED_LIMIT, 'hw.cpu.speed.limit').


%% Energy consumed by the component.
-define(HW_ENERGY, 'hw.energy').


%% Number of errors encountered by the component.
-define(HW_ERRORS, 'hw.errors').


%% Fan speed in revolutions per minute.
-define(HW_FAN_SPEED, 'hw.fan.speed').


%% Speed limit in rpm.
-define(HW_FAN_SPEED_LIMIT, 'hw.fan.speed.limit').


%% Fan speed expressed as a fraction of its maximum speed.
-define(HW_FAN_SPEED_RATIO, 'hw.fan.speed_ratio').


%% Received and transmitted bytes by the GPU.
-define(HW_GPU_IO, 'hw.gpu.io').


%% Size of the GPU memory.
-define(HW_GPU_MEMORY_LIMIT, 'hw.gpu.memory.limit').


%% GPU memory used.
-define(HW_GPU_MEMORY_USAGE, 'hw.gpu.memory.usage').


%% Fraction of GPU memory used.
-define(HW_GPU_MEMORY_UTILIZATION, 'hw.gpu.memory.utilization').


%% Fraction of time spent in a specific task.
-define(HW_GPU_UTILIZATION, 'hw.gpu.utilization').


%% Ambient (external) temperature of the physical host.
-define(HW_HOST_AMBIENT_TEMPERATURE, 'hw.host.ambient_temperature').


%% Total energy consumed by the entire physical host, in joules.
-define(HW_HOST_ENERGY, 'hw.host.energy').


%% By how many degrees Celsius the temperature of the physical host can be increased, before reaching a warning threshold on one of the internal sensors.
%%  
-define(HW_HOST_HEATING_MARGIN, 'hw.host.heating_margin').


%% Instantaneous power consumed by the entire physical host in Watts (`hw.host.energy` is preferred).
%%  
-define(HW_HOST_POWER, 'hw.host.power').


%% Size of the logical disk.
-define(HW_LOGICAL_DISK_LIMIT, 'hw.logical_disk.limit').


%% Logical disk space usage.
-define(HW_LOGICAL_DISK_USAGE, 'hw.logical_disk.usage').


%% Logical disk space utilization as a fraction.
-define(HW_LOGICAL_DISK_UTILIZATION, 'hw.logical_disk.utilization').


%% Size of the memory module.
-define(HW_MEMORY_SIZE, 'hw.memory.size').


%% Link speed.
-define(HW_NETWORK_BANDWIDTH_LIMIT, 'hw.network.bandwidth.limit').


%% Utilization of the network bandwidth as a fraction.
-define(HW_NETWORK_BANDWIDTH_UTILIZATION, 'hw.network.bandwidth.utilization').


%% Received and transmitted network traffic in bytes.
-define(HW_NETWORK_IO, 'hw.network.io').


%% Received and transmitted network traffic in packets (or frames).
-define(HW_NETWORK_PACKETS, 'hw.network.packets').


%% Link status: `1` (up) or `0` (down).
-define(HW_NETWORK_UP, 'hw.network.up').


%% Endurance remaining for this SSD disk.
-define(HW_PHYSICAL_DISK_ENDURANCE_UTILIZATION, 'hw.physical_disk.endurance_utilization').


%% Size of the disk.
-define(HW_PHYSICAL_DISK_SIZE, 'hw.physical_disk.size').


%% Value of the corresponding [S.M.A.R.T.](https://wikipedia.org/wiki/S.M.A.R.T.) (Self-Monitoring, Analysis, and Reporting Technology) attribute.
-define(HW_PHYSICAL_DISK_SMART, 'hw.physical_disk.smart').


%% Instantaneous power consumed by the component.
-define(HW_POWER, 'hw.power').


%% Maximum power output of the power supply.
-define(HW_POWER_SUPPLY_LIMIT, 'hw.power_supply.limit').


%% Current power output of the power supply.
-define(HW_POWER_SUPPLY_USAGE, 'hw.power_supply.usage').


%% Utilization of the power supply as a fraction of its maximum output.
-define(HW_POWER_SUPPLY_UTILIZATION, 'hw.power_supply.utilization').


%% Operational status: `1` (true) or `0` (false) for each of the possible states.
-define(HW_STATUS, 'hw.status').


%% Operations performed by the tape drive.
-define(HW_TAPE_DRIVE_OPERATIONS, 'hw.tape_drive.operations').


%% Temperature in degrees Celsius.
-define(HW_TEMPERATURE, 'hw.temperature').


%% Temperature limit in degrees Celsius.
-define(HW_TEMPERATURE_LIMIT, 'hw.temperature.limit').


%% Voltage measured by the sensor.
-define(HW_VOLTAGE, 'hw.voltage').


%% Voltage limit in Volts.
-define(HW_VOLTAGE_LIMIT, 'hw.voltage.limit').


%% Nominal (expected) voltage.
-define(HW_VOLTAGE_NOMINAL, 'hw.voltage.nominal').
