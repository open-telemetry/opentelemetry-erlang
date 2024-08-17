
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

%% Number of times the process has been context switched.
-define(PROCESS_CONTEXT_SWITCHES, 'process.context_switches').


%% Total CPU seconds broken down by different states.
-define(PROCESS_CPU_TIME, 'process.cpu.time').


%% Difference in process.cpu.time since the last measurement, divided by the elapsed time and number of CPUs available to the process.
-define(PROCESS_CPU_UTILIZATION, 'process.cpu.utilization').


%% Disk bytes transferred.
-define(PROCESS_DISK_IO, 'process.disk.io').


%% The amount of physical memory in use.
-define(PROCESS_MEMORY_USAGE, 'process.memory.usage').


%% The amount of committed virtual memory.
-define(PROCESS_MEMORY_VIRTUAL, 'process.memory.virtual').


%% Network bytes transferred.
-define(PROCESS_NETWORK_IO, 'process.network.io').


%% Number of file descriptors in use by the process.
-define(PROCESS_OPEN_FILE_DESCRIPTOR_COUNT, 'process.open_file_descriptor.count').


%% Number of page faults the process has made.
-define(PROCESS_PAGING_FAULTS, 'process.paging.faults').


%% Process threads count.
-define(PROCESS_THREAD_COUNT, 'process.thread.count').
