
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

%% Design capacity in Watts-hours or Amper-hours
%%  
-define(HW_BATTERY_CAPACITY, 'hw.battery.capacity').


%% Battery [chemistry](https://schemas.dmtf.org/wbem/cim-html/2.31.0/CIM_Battery.html), e.g. Lithium-Ion, Nickel-Cadmium, etc.
%%  
-define(HW_BATTERY_CHEMISTRY, 'hw.battery.chemistry').


%% The current state of the battery
%%  
-define(HW_BATTERY_STATE, 'hw.battery.state').

-define(HW_BATTERY_STATE_VALUES_CHARGING, 'charging').

-define(HW_BATTERY_STATE_VALUES_DISCHARGING, 'discharging').



%% BIOS version of the hardware component
%%  
-define(HW_BIOS_VERSION, 'hw.bios_version').


%% Driver version for the hardware component
%%  
-define(HW_DRIVER_VERSION, 'hw.driver_version').


%% Type of the enclosure (useful for modular systems)
%%  
-define(HW_ENCLOSURE_TYPE, 'hw.enclosure.type').


%% Firmware version of the hardware component
%%  
-define(HW_FIRMWARE_VERSION, 'hw.firmware_version').


%% Type of task the GPU is performing
%%  
-define(HW_GPU_TASK, 'hw.gpu.task').

-define(HW_GPU_TASK_VALUES_DECODER, 'decoder').

-define(HW_GPU_TASK_VALUES_ENCODER, 'encoder').

-define(HW_GPU_TASK_VALUES_GENERAL, 'general').



%% An identifier for the hardware component, unique within the monitored host
%%  
-define(HW_ID, 'hw.id').


%% Type of limit for hardware components
%%  
-define(HW_LIMIT_TYPE, 'hw.limit_type').

-define(HW_LIMIT_TYPE_VALUES_CRITICAL, 'critical').

-define(HW_LIMIT_TYPE_VALUES_DEGRADED, 'degraded').

-define(HW_LIMIT_TYPE_VALUES_HIGH_CRITICAL, 'high.critical').

-define(HW_LIMIT_TYPE_VALUES_HIGH_DEGRADED, 'high.degraded').

-define(HW_LIMIT_TYPE_VALUES_LOW_CRITICAL, 'low.critical').

-define(HW_LIMIT_TYPE_VALUES_LOW_DEGRADED, 'low.degraded').

-define(HW_LIMIT_TYPE_VALUES_MAX, 'max').

-define(HW_LIMIT_TYPE_VALUES_THROTTLED, 'throttled').

-define(HW_LIMIT_TYPE_VALUES_TURBO, 'turbo').



%% RAID Level of the logical disk
%%  
-define(HW_LOGICAL_DISK_RAID_LEVEL, 'hw.logical_disk.raid_level').


%% State of the logical disk space usage
%%  
-define(HW_LOGICAL_DISK_STATE, 'hw.logical_disk.state').

-define(HW_LOGICAL_DISK_STATE_VALUES_USED, 'used').

-define(HW_LOGICAL_DISK_STATE_VALUES_FREE, 'free').



%% Type of the memory module
%%  
-define(HW_MEMORY_TYPE, 'hw.memory.type').


%% Descriptive model name of the hardware component
%%  
-define(HW_MODEL, 'hw.model').


%% An easily-recognizable name for the hardware component
%%  
-define(HW_NAME, 'hw.name').


%% Logical addresses of the adapter (e.g. IP address, or WWPN)
%%  
-define(HW_NETWORK_LOGICAL_ADDRESSES, 'hw.network.logical_addresses').


%% Physical address of the adapter (e.g. MAC address, or WWNN)
%%  
-define(HW_NETWORK_PHYSICAL_ADDRESS, 'hw.network.physical_address').


%% Unique identifier of the parent component (typically the `hw.id` attribute of the enclosure, or disk controller)
%%  
-define(HW_PARENT, 'hw.parent').


%% [S.M.A.R.T.](https://wikipedia.org/wiki/S.M.A.R.T.) (Self-Monitoring, Analysis, and Reporting Technology) attribute of the physical disk
%%  
-define(HW_PHYSICAL_DISK_SMART_ATTRIBUTE, 'hw.physical_disk.smart_attribute').


%% State of the physical disk endurance utilization
%%  
-define(HW_PHYSICAL_DISK_STATE, 'hw.physical_disk.state').

-define(HW_PHYSICAL_DISK_STATE_VALUES_REMAINING, 'remaining').



%% Type of the physical disk
%%  
-define(HW_PHYSICAL_DISK_TYPE, 'hw.physical_disk.type').


%% Location of the sensor
%%  
-define(HW_SENSOR_LOCATION, 'hw.sensor_location').


%% Serial number of the hardware component
%%  
-define(HW_SERIAL_NUMBER, 'hw.serial_number').


%% The current state of the component
%%  
-define(HW_STATE, 'hw.state').

-define(HW_STATE_VALUES_DEGRADED, 'degraded').

-define(HW_STATE_VALUES_FAILED, 'failed').

-define(HW_STATE_VALUES_NEEDS_CLEANING, 'needs_cleaning').

-define(HW_STATE_VALUES_OK, 'ok').

-define(HW_STATE_VALUES_PREDICTED_FAILURE, 'predicted_failure').



%% Type of tape drive operation
%%  
-define(HW_TAPE_DRIVE_OPERATION_TYPE, 'hw.tape_drive.operation_type').

-define(HW_TAPE_DRIVE_OPERATION_TYPE_VALUES_MOUNT, 'mount').

-define(HW_TAPE_DRIVE_OPERATION_TYPE_VALUES_UNMOUNT, 'unmount').

-define(HW_TAPE_DRIVE_OPERATION_TYPE_VALUES_CLEAN, 'clean').



%% Type of the component
%%  
-define(HW_TYPE, 'hw.type').

-define(HW_TYPE_VALUES_BATTERY, 'battery').

-define(HW_TYPE_VALUES_CPU, 'cpu').

-define(HW_TYPE_VALUES_DISK_CONTROLLER, 'disk_controller').

-define(HW_TYPE_VALUES_ENCLOSURE, 'enclosure').

-define(HW_TYPE_VALUES_FAN, 'fan').

-define(HW_TYPE_VALUES_GPU, 'gpu').

-define(HW_TYPE_VALUES_LOGICAL_DISK, 'logical_disk').

-define(HW_TYPE_VALUES_MEMORY, 'memory').

-define(HW_TYPE_VALUES_NETWORK, 'network').

-define(HW_TYPE_VALUES_PHYSICAL_DISK, 'physical_disk').

-define(HW_TYPE_VALUES_POWER_SUPPLY, 'power_supply').

-define(HW_TYPE_VALUES_TAPE_DRIVE, 'tape_drive').

-define(HW_TYPE_VALUES_TEMPERATURE, 'temperature').

-define(HW_TYPE_VALUES_VOLTAGE, 'voltage').



%% Vendor name of the hardware component
%%  
-define(HW_VENDOR, 'hw.vendor').
