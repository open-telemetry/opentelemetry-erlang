
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

%% Operating frequency of the logical CPU in Hertz.
-define(SYSTEM_CPU_FREQUENCY, 'system.cpu.frequency').


%% Reports the number of logical (virtual) processor cores created by the operating system to manage multitasking.
-define(SYSTEM_CPU_LOGICAL_COUNT, 'system.cpu.logical.count').


%% Reports the number of actual physical processor cores on the hardware.
-define(SYSTEM_CPU_PHYSICAL_COUNT, 'system.cpu.physical.count').


%% Seconds each logical CPU spent on each mode.
-define(SYSTEM_CPU_TIME, 'system.cpu.time').


%% For each logical CPU, the utilization is calculated as the change in cumulative CPU time (cpu.time) over a measurement interval, divided by the elapsed time.
-define(SYSTEM_CPU_UTILIZATION, 'system.cpu.utilization').


%% Disk bytes transferred.
-define(SYSTEM_DISK_IO, 'system.disk.io').


%% Time disk spent activated.
-define(SYSTEM_DISK_IO_TIME, 'system.disk.io_time').


%% The total storage capacity of the disk.
-define(SYSTEM_DISK_LIMIT, 'system.disk.limit').


%% The number of disk reads/writes merged into single physical disk access operations.
-define(SYSTEM_DISK_MERGED, 'system.disk.merged').


%% Sum of the time each operation took to complete.
-define(SYSTEM_DISK_OPERATION_TIME, 'system.disk.operation_time').


%% Disk operations count.
-define(SYSTEM_DISK_OPERATIONS, 'system.disk.operations').


%% The total storage capacity of the filesystem.
-define(SYSTEM_FILESYSTEM_LIMIT, 'system.filesystem.limit').


%% Reports a filesystem's space usage across different states.
-define(SYSTEM_FILESYSTEM_USAGE, 'system.filesystem.usage').


%% Fraction of filesystem bytes used.
-define(SYSTEM_FILESYSTEM_UTILIZATION, 'system.filesystem.utilization').

%% @deprecated Replaced by `system.memory.linux.available`.
%% The number of packets transferred.
-define(SYSTEM_LINUX_MEMORY_AVAILABLE, 'system.linux.memory.available').

%% @deprecated Replaced by `system.memory.linux.slab.usage`.
%% The number of packets transferred.
-define(SYSTEM_LINUX_MEMORY_SLAB_USAGE, 'system.linux.memory.slab.usage').


%% Total virtual memory available in the system.
-define(SYSTEM_MEMORY_LIMIT, 'system.memory.limit').


%% An estimate of how much memory is available for starting new applications, without causing swapping.
-define(SYSTEM_MEMORY_LINUX_AVAILABLE, 'system.memory.linux.available').


%% Shared memory used (mostly by tmpfs).
-define(SYSTEM_MEMORY_LINUX_SHARED, 'system.memory.linux.shared').


%% Reports the memory used by the Linux kernel for managing caches of frequently used objects.
-define(SYSTEM_MEMORY_LINUX_SLAB_USAGE, 'system.memory.linux.slab.usage').

%% @deprecated Replaced by `system.memory.linux.shared`.
%% Deprecated, use `system.memory.linux.shared` instead.
-define(SYSTEM_MEMORY_SHARED, 'system.memory.shared').


%% Reports memory in use by state.
-define(SYSTEM_MEMORY_USAGE, 'system.memory.usage').


%% Percentage of memory bytes in use.
-define(SYSTEM_MEMORY_UTILIZATION, 'system.memory.utilization').


%% The number of connections.
-define(SYSTEM_NETWORK_CONNECTION_COUNT, 'system.network.connection.count').

%% @deprecated Replaced by `system.network.connection.count`.
%% Deprecated, use `system.network.connection.count` instead.
-define(SYSTEM_NETWORK_CONNECTIONS, 'system.network.connections').

%% @deprecated Replaced by `system.network.packet.dropped`.
%% Count of packets that are dropped or discarded even though there was no error.
-define(SYSTEM_NETWORK_DROPPED, 'system.network.dropped').


%% Count of network errors detected.
-define(SYSTEM_NETWORK_ERRORS, 'system.network.errors').


%% The number of bytes transmitted and received.
-define(SYSTEM_NETWORK_IO, 'system.network.io').


%% The number of packets transferred.
-define(SYSTEM_NETWORK_PACKET_COUNT, 'system.network.packet.count').


%% Count of packets that are dropped or discarded even though there was no error.
-define(SYSTEM_NETWORK_PACKET_DROPPED, 'system.network.packet.dropped').

%% @deprecated Replaced by `system.network.packet.count`.
%% The number of packets transferred.
-define(SYSTEM_NETWORK_PACKETS, 'system.network.packets').


%% The number of page faults.
-define(SYSTEM_PAGING_FAULTS, 'system.paging.faults').


%% The number of paging operations.
-define(SYSTEM_PAGING_OPERATIONS, 'system.paging.operations').


%% Unix swap or windows pagefile usage.
-define(SYSTEM_PAGING_USAGE, 'system.paging.usage').


%% Swap (unix) or pagefile (windows) utilization.
-define(SYSTEM_PAGING_UTILIZATION, 'system.paging.utilization').


%% Total number of processes in each state.
-define(SYSTEM_PROCESS_COUNT, 'system.process.count').


%% Total number of processes created over uptime of the host.
-define(SYSTEM_PROCESS_CREATED, 'system.process.created').


%% The time the system has been running.
-define(SYSTEM_UPTIME, 'system.uptime').
