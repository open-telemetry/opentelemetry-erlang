
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
-define(SYSTEM_CPU_LOGICALNUMBER, 'system.cpu.logical_number').


%% The state of the CPU
-define(SYSTEM_CPU_STATE, 'system.cpu.state').

-define('SYSTEM_CPU_STATE_VALUES.user', 'user').

-define('SYSTEM_CPU_STATE_VALUES.system', 'system').

-define('SYSTEM_CPU_STATE_VALUES.nice', 'nice').

-define('SYSTEM_CPU_STATE_VALUES.idle', 'idle').

-define('SYSTEM_CPU_STATE_VALUES.iowait', 'iowait').

-define('SYSTEM_CPU_STATE_VALUES.interrupt', 'interrupt').

-define('SYSTEM_CPU_STATE_VALUES.steal', 'steal').

-define(SYSTEM_CPU_STATE_VALUES(Custom), Custom).


%% The device identifier
-define(SYSTEM_DEVICE, 'system.device').


%% The filesystem mode
-define(SYSTEM_FILESYSTEM_MODE, 'system.filesystem.mode').


%% The filesystem mount path
-define(SYSTEM_FILESYSTEM_MOUNTPOINT, 'system.filesystem.mountpoint').


%% The filesystem state
-define(SYSTEM_FILESYSTEM_STATE, 'system.filesystem.state').

-define('SYSTEM_FILESYSTEM_STATE_VALUES.used', 'used').

-define('SYSTEM_FILESYSTEM_STATE_VALUES.free', 'free').

-define('SYSTEM_FILESYSTEM_STATE_VALUES.reserved', 'reserved').

-define(SYSTEM_FILESYSTEM_STATE_VALUES(Custom), Custom).


%% The filesystem type
-define(SYSTEM_FILESYSTEM_TYPE, 'system.filesystem.type').

-define('SYSTEM_FILESYSTEM_TYPE_VALUES.fat32', 'fat32').

-define('SYSTEM_FILESYSTEM_TYPE_VALUES.exfat', 'exfat').

-define('SYSTEM_FILESYSTEM_TYPE_VALUES.ntfs', 'ntfs').

-define('SYSTEM_FILESYSTEM_TYPE_VALUES.refs', 'refs').

-define('SYSTEM_FILESYSTEM_TYPE_VALUES.hfsplus', 'hfsplus').

-define('SYSTEM_FILESYSTEM_TYPE_VALUES.ext4', 'ext4').

-define(SYSTEM_FILESYSTEM_TYPE_VALUES(Custom), Custom).


%% The memory state
-define(SYSTEM_MEMORY_STATE, 'system.memory.state').

-define('SYSTEM_MEMORY_STATE_VALUES.used', 'used').

-define('SYSTEM_MEMORY_STATE_VALUES.free', 'free').

-define('SYSTEM_MEMORY_STATE_VALUES.shared', 'shared').

-define('SYSTEM_MEMORY_STATE_VALUES.buffers', 'buffers').

-define('SYSTEM_MEMORY_STATE_VALUES.cached', 'cached').

-define(SYSTEM_MEMORY_STATE_VALUES(Custom), Custom).


%% A stateless protocol MUST NOT set this attribute
-define(SYSTEM_NETWORK_STATE, 'system.network.state').

-define('SYSTEM_NETWORK_STATE_VALUES.close', 'close').

-define('SYSTEM_NETWORK_STATE_VALUES.close_wait', 'close_wait').

-define('SYSTEM_NETWORK_STATE_VALUES.closing', 'closing').

-define('SYSTEM_NETWORK_STATE_VALUES.delete', 'delete').

-define('SYSTEM_NETWORK_STATE_VALUES.established', 'established').

-define('SYSTEM_NETWORK_STATE_VALUES.fin_wait_1', 'fin_wait_1').

-define('SYSTEM_NETWORK_STATE_VALUES.fin_wait_2', 'fin_wait_2').

-define('SYSTEM_NETWORK_STATE_VALUES.last_ack', 'last_ack').

-define('SYSTEM_NETWORK_STATE_VALUES.listen', 'listen').

-define('SYSTEM_NETWORK_STATE_VALUES.syn_recv', 'syn_recv').

-define('SYSTEM_NETWORK_STATE_VALUES.syn_sent', 'syn_sent').

-define('SYSTEM_NETWORK_STATE_VALUES.time_wait', 'time_wait').

-define(SYSTEM_NETWORK_STATE_VALUES(Custom), Custom).


%% The paging access direction
-define(SYSTEM_PAGING_DIRECTION, 'system.paging.direction').

-define('SYSTEM_PAGING_DIRECTION_VALUES.in', 'in').

-define('SYSTEM_PAGING_DIRECTION_VALUES.out', 'out').

-define(SYSTEM_PAGING_DIRECTION_VALUES(Custom), Custom).


%% The memory paging state
-define(SYSTEM_PAGING_STATE, 'system.paging.state').

-define('SYSTEM_PAGING_STATE_VALUES.used', 'used').

-define('SYSTEM_PAGING_STATE_VALUES.free', 'free').

-define(SYSTEM_PAGING_STATE_VALUES(Custom), Custom).


%% The memory paging type
-define(SYSTEM_PAGING_TYPE, 'system.paging.type').

-define('SYSTEM_PAGING_TYPE_VALUES.major', 'major').

-define('SYSTEM_PAGING_TYPE_VALUES.minor', 'minor').

-define(SYSTEM_PAGING_TYPE_VALUES(Custom), Custom).


%% The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)
%%  
-define(SYSTEM_PROCESS_STATUS, 'system.process.status').

-define('SYSTEM_PROCESS_STATUS_VALUES.running', 'running').

-define('SYSTEM_PROCESS_STATUS_VALUES.sleeping', 'sleeping').

-define('SYSTEM_PROCESS_STATUS_VALUES.stopped', 'stopped').

-define('SYSTEM_PROCESS_STATUS_VALUES.defunct', 'defunct').

-define(SYSTEM_PROCESS_STATUS_VALUES(Custom), Custom).

%% @deprecated Replaced by `system.process.status`.
%% Deprecated, use `system.process.status` instead.
-define(SYSTEM_PROCESSES_STATUS, 'system.processes.status').

-define('SYSTEM_PROCESSES_STATUS_VALUES.running', 'running').

-define('SYSTEM_PROCESSES_STATUS_VALUES.sleeping', 'sleeping').

-define('SYSTEM_PROCESSES_STATUS_VALUES.stopped', 'stopped').

-define('SYSTEM_PROCESSES_STATUS_VALUES.defunct', 'defunct').

-define(SYSTEM_PROCESSES_STATUS_VALUES(Custom), Custom).
