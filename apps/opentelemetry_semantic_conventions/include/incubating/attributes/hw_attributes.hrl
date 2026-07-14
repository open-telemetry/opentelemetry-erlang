
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/hw_attributes.hrl").


%% An identifier for the hardware component, unique within the monitored host
%%  
-define(HW_ID, 'hw.id').


%% An easily-recognizable name for the hardware component
%%  
-define(HW_NAME, 'hw.name').


%% Unique identifier of the parent component (typically the `hw.id` attribute of the enclosure, or disk controller)
%%  
-define(HW_PARENT, 'hw.parent').


%% The current state of the component
%%  
-define(HW_STATE, 'hw.state').

-define(HW_STATE_VALUES_OK, 'ok').

-define(HW_STATE_VALUES_DEGRADED, 'degraded').

-define(HW_STATE_VALUES_FAILED, 'failed').



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

