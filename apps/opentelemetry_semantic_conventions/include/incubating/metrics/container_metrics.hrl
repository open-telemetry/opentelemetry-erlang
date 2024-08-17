
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

%% Total CPU time consumed
-define(CONTAINER_CPU_TIME, 'container.cpu.time').


%% Disk bytes for the container.
-define(CONTAINER_DISK_IO, 'container.disk.io').


%% Memory usage of the container.
-define(CONTAINER_MEMORY_USAGE, 'container.memory.usage').


%% Network bytes for the container.
-define(CONTAINER_NETWORK_IO, 'container.network.io').
