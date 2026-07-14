
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

%% Energy consumed by the component
-define(HW_ENERGY, 'hw.energy').


%% Number of errors encountered by the component
-define(HW_ERRORS, 'hw.errors').


%% Ambient (external) temperature of the physical host
-define(HW_HOST_AMBIENT_TEMPERATURE, 'hw.host.ambient_temperature').


%% Total energy consumed by the entire physical host, in joules
-define(HW_HOST_ENERGY, 'hw.host.energy').


%% By how many degrees Celsius the temperature of the physical host can be increased, before reaching a warning threshold on one of the internal sensors
%%  
-define(HW_HOST_HEATING_MARGIN, 'hw.host.heating_margin').


%% Instantaneous power consumed by the entire physical host in Watts (`hw.host.energy` is preferred)
%%  
-define(HW_HOST_POWER, 'hw.host.power').


%% Instantaneous power consumed by the component
-define(HW_POWER, 'hw.power').


%% Operational status: `1` (true) or `0` (false) for each of the possible states
-define(HW_STATUS, 'hw.status').
