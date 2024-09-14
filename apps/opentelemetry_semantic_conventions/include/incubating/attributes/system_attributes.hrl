
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

%% The logical CPU number [0..n-1]
-define(SYSTEM_CPU_LOGICAL_NUMBER, 'system.cpu.logical_number').

%% @deprecated Replaced by `cpu.mode`
%% Deprecated, use `cpu.mode` instead.
-define(SYSTEM_CPU_STATE, 'system.cpu.state').

-define(SYSTEM_CPU_STATE_VALUES_USER, 'user').

-define(SYSTEM_CPU_STATE_VALUES_SYSTEM, 'system').

-define(SYSTEM_CPU_STATE_VALUES_NICE, 'nice').

-define(SYSTEM_CPU_STATE_VALUES_IDLE, 'idle').

-define(SYSTEM_CPU_STATE_VALUES_IOWAIT, 'iowait').

-define(SYSTEM_CPU_STATE_VALUES_INTERRUPT, 'interrupt').

-define(SYSTEM_CPU_STATE_VALUES_STEAL, 'steal').



%% The device identifier
-define(SYSTEM_DEVICE, 'system.device').


%% The filesystem mode
-define(SYSTEM_FILESYSTEM_MODE, 'system.filesystem.mode').


%% The filesystem mount path
-define(SYSTEM_FILESYSTEM_MOUNTPOINT, 'system.filesystem.mountpoint').


%% The filesystem state
-define(SYSTEM_FILESYSTEM_STATE, 'system.filesystem.state').

-define(SYSTEM_FILESYSTEM_STATE_VALUES_USED, 'used').

-define(SYSTEM_FILESYSTEM_STATE_VALUES_FREE, 'free').

-define(SYSTEM_FILESYSTEM_STATE_VALUES_RESERVED, 'reserved').



%% The filesystem type
-define(SYSTEM_FILESYSTEM_TYPE, 'system.filesystem.type').

-define(SYSTEM_FILESYSTEM_TYPE_VALUES_FAT32, 'fat32').

-define(SYSTEM_FILESYSTEM_TYPE_VALUES_EXFAT, 'exfat').

-define(SYSTEM_FILESYSTEM_TYPE_VALUES_NTFS, 'ntfs').

-define(SYSTEM_FILESYSTEM_TYPE_VALUES_REFS, 'refs').

-define(SYSTEM_FILESYSTEM_TYPE_VALUES_HFSPLUS, 'hfsplus').

-define(SYSTEM_FILESYSTEM_TYPE_VALUES_EXT4, 'ext4').



%% The memory state
-define(SYSTEM_MEMORY_STATE, 'system.memory.state').

-define(SYSTEM_MEMORY_STATE_VALUES_USED, 'used').

-define(SYSTEM_MEMORY_STATE_VALUES_FREE, 'free').

-define(SYSTEM_MEMORY_STATE_VALUES_SHARED, 'shared').

-define(SYSTEM_MEMORY_STATE_VALUES_BUFFERS, 'buffers').

-define(SYSTEM_MEMORY_STATE_VALUES_CACHED, 'cached').



%% A stateless protocol MUST NOT set this attribute
-define(SYSTEM_NETWORK_STATE, 'system.network.state').

-define(SYSTEM_NETWORK_STATE_VALUES_CLOSE, 'close').

-define(SYSTEM_NETWORK_STATE_VALUES_CLOSE_WAIT, 'close_wait').

-define(SYSTEM_NETWORK_STATE_VALUES_CLOSING, 'closing').

-define(SYSTEM_NETWORK_STATE_VALUES_DELETE, 'delete').

-define(SYSTEM_NETWORK_STATE_VALUES_ESTABLISHED, 'established').

-define(SYSTEM_NETWORK_STATE_VALUES_FIN_WAIT_1, 'fin_wait_1').

-define(SYSTEM_NETWORK_STATE_VALUES_FIN_WAIT_2, 'fin_wait_2').

-define(SYSTEM_NETWORK_STATE_VALUES_LAST_ACK, 'last_ack').

-define(SYSTEM_NETWORK_STATE_VALUES_LISTEN, 'listen').

-define(SYSTEM_NETWORK_STATE_VALUES_SYN_RECV, 'syn_recv').

-define(SYSTEM_NETWORK_STATE_VALUES_SYN_SENT, 'syn_sent').

-define(SYSTEM_NETWORK_STATE_VALUES_TIME_WAIT, 'time_wait').



%% The paging access direction
-define(SYSTEM_PAGING_DIRECTION, 'system.paging.direction').

-define(SYSTEM_PAGING_DIRECTION_VALUES_IN, 'in').

-define(SYSTEM_PAGING_DIRECTION_VALUES_OUT, 'out').



%% The memory paging state
-define(SYSTEM_PAGING_STATE, 'system.paging.state').

-define(SYSTEM_PAGING_STATE_VALUES_USED, 'used').

-define(SYSTEM_PAGING_STATE_VALUES_FREE, 'free').



%% The memory paging type
-define(SYSTEM_PAGING_TYPE, 'system.paging.type').

-define(SYSTEM_PAGING_TYPE_VALUES_MAJOR, 'major').

-define(SYSTEM_PAGING_TYPE_VALUES_MINOR, 'minor').



%% The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)
%%  
-define(SYSTEM_PROCESS_STATUS, 'system.process.status').

-define(SYSTEM_PROCESS_STATUS_VALUES_RUNNING, 'running').

-define(SYSTEM_PROCESS_STATUS_VALUES_SLEEPING, 'sleeping').

-define(SYSTEM_PROCESS_STATUS_VALUES_STOPPED, 'stopped').

-define(SYSTEM_PROCESS_STATUS_VALUES_DEFUNCT, 'defunct').


%% @deprecated Replaced by `system.process.status`.
%% Deprecated, use `system.process.status` instead.
-define(SYSTEM_PROCESSES_STATUS, 'system.processes.status').

-define(SYSTEM_PROCESSES_STATUS_VALUES_RUNNING, 'running').

-define(SYSTEM_PROCESSES_STATUS_VALUES_SLEEPING, 'sleeping').

-define(SYSTEM_PROCESSES_STATUS_VALUES_STOPPED, 'stopped').

-define(SYSTEM_PROCESSES_STATUS_VALUES_DEFUNCT, 'defunct').

