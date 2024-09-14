
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

%% Reports the current frequency of the CPU in Hz
-define(SYSTEM_CPU_FREQUENCY, 'system.cpu.frequency').


%% Reports the number of logical (virtual) processor cores created by the operating system to manage multitasking
-define(SYSTEM_CPU_LOGICAL_COUNT, 'system.cpu.logical.count').


%% Reports the number of actual physical processor cores on the hardware
-define(SYSTEM_CPU_PHYSICAL_COUNT, 'system.cpu.physical.count').


%% Seconds each logical CPU spent on each mode
-define(SYSTEM_CPU_TIME, 'system.cpu.time').


%% Difference in system.cpu.time since the last measurement, divided by the elapsed time and number of logical CPUs
-define(SYSTEM_CPU_UTILIZATION, 'system.cpu.utilization').


%% none
-define(SYSTEM_DISK_IO, 'system.disk.io').


%% Time disk spent activated
-define(SYSTEM_DISK_IO_TIME, 'system.disk.io_time').


%% none
-define(SYSTEM_DISK_MERGED, 'system.disk.merged').


%% Sum of the time each operation took to complete
-define(SYSTEM_DISK_OPERATION_TIME, 'system.disk.operation_time').


%% none
-define(SYSTEM_DISK_OPERATIONS, 'system.disk.operations').


%% none
-define(SYSTEM_FILESYSTEM_USAGE, 'system.filesystem.usage').


%% none
-define(SYSTEM_FILESYSTEM_UTILIZATION, 'system.filesystem.utilization').


%% An estimate of how much memory is available for starting new applications, without causing swapping
-define(SYSTEM_LINUX_MEMORY_AVAILABLE, 'system.linux.memory.available').


%% Reports the memory used by the Linux kernel for managing caches of frequently used objects.
-define(SYSTEM_LINUX_MEMORY_SLAB_USAGE, 'system.linux.memory.slab.usage').


%% Total memory available in the system.
-define(SYSTEM_MEMORY_LIMIT, 'system.memory.limit').


%% Shared memory used (mostly by tmpfs).
-define(SYSTEM_MEMORY_SHARED, 'system.memory.shared').


%% Reports memory in use by state.
-define(SYSTEM_MEMORY_USAGE, 'system.memory.usage').


%% none
-define(SYSTEM_MEMORY_UTILIZATION, 'system.memory.utilization').


%% none
-define(SYSTEM_NETWORK_CONNECTIONS, 'system.network.connections').


%% Count of packets that are dropped or discarded even though there was no error
-define(SYSTEM_NETWORK_DROPPED, 'system.network.dropped').


%% Count of network errors detected
-define(SYSTEM_NETWORK_ERRORS, 'system.network.errors').


%% none
-define(SYSTEM_NETWORK_IO, 'system.network.io').


%% none
-define(SYSTEM_NETWORK_PACKETS, 'system.network.packets').


%% none
-define(SYSTEM_PAGING_FAULTS, 'system.paging.faults').


%% none
-define(SYSTEM_PAGING_OPERATIONS, 'system.paging.operations').


%% Unix swap or windows pagefile usage
-define(SYSTEM_PAGING_USAGE, 'system.paging.usage').


%% none
-define(SYSTEM_PAGING_UTILIZATION, 'system.paging.utilization').


%% Total number of processes in each state
-define(SYSTEM_PROCESS_COUNT, 'system.process.count').


%% Total number of processes created over uptime of the host
-define(SYSTEM_PROCESS_CREATED, 'system.process.created').
