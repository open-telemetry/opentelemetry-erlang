

%% The logical CPU number [0..n-1]
-define(SYSTEM_CPU_LOGICALNUMBER, 'system.cpu.logical_number').


%% The state of the CPU

-define('system_cpu_state.user', 'user').

-define('system_cpu_state.system', 'system').

-define('system_cpu_state.nice', 'nice').

-define('system_cpu_state.idle', 'idle').

-define('system_cpu_state.iowait', 'iowait').

-define('system_cpu_state.interrupt', 'interrupt').

-define('system_cpu_state.steal', 'steal').

-define(system_cpu_state(Custom), Custom).


%% The device identifier
-define(SYSTEM_DEVICE, 'system.device').


%% The filesystem mode
-define(SYSTEM_FILESYSTEM_MODE, 'system.filesystem.mode').


%% The filesystem mount path
-define(SYSTEM_FILESYSTEM_MOUNTPOINT, 'system.filesystem.mountpoint').


%% The filesystem state

-define('system_filesystem_state.used', 'used').

-define('system_filesystem_state.free', 'free').

-define('system_filesystem_state.reserved', 'reserved').

-define(system_filesystem_state(Custom), Custom).


%% The filesystem type

-define('system_filesystem_type.fat32', 'fat32').

-define('system_filesystem_type.exfat', 'exfat').

-define('system_filesystem_type.ntfs', 'ntfs').

-define('system_filesystem_type.refs', 'refs').

-define('system_filesystem_type.hfsplus', 'hfsplus').

-define('system_filesystem_type.ext4', 'ext4').

-define(system_filesystem_type(Custom), Custom).


%% The memory state

-define('system_memory_state.used', 'used').

-define('system_memory_state.free', 'free').

-define('system_memory_state.shared', 'shared').

-define('system_memory_state.buffers', 'buffers').

-define('system_memory_state.cached', 'cached').

-define(system_memory_state(Custom), Custom).


%% A stateless protocol MUST NOT set this attribute

-define('system_network_state.close', 'close').

-define('system_network_state.close_wait', 'close_wait').

-define('system_network_state.closing', 'closing').

-define('system_network_state.delete', 'delete').

-define('system_network_state.established', 'established').

-define('system_network_state.fin_wait_1', 'fin_wait_1').

-define('system_network_state.fin_wait_2', 'fin_wait_2').

-define('system_network_state.last_ack', 'last_ack').

-define('system_network_state.listen', 'listen').

-define('system_network_state.syn_recv', 'syn_recv').

-define('system_network_state.syn_sent', 'syn_sent').

-define('system_network_state.time_wait', 'time_wait').

-define(system_network_state(Custom), Custom).


%% The paging access direction

-define('system_paging_direction.in', 'in').

-define('system_paging_direction.out', 'out').

-define(system_paging_direction(Custom), Custom).


%% The memory paging state

-define('system_paging_state.used', 'used').

-define('system_paging_state.free', 'free').

-define(system_paging_state(Custom), Custom).


%% The memory paging type

-define('system_paging_type.major', 'major').

-define('system_paging_type.minor', 'minor').

-define(system_paging_type(Custom), Custom).


%% The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)
%%  

-define('system_process_status.running', 'running').

-define('system_process_status.sleeping', 'sleeping').

-define('system_process_status.stopped', 'stopped').

-define('system_process_status.defunct', 'defunct').

-define(system_process_status(Custom), Custom).

%% @deprecated Replaced by `system.process.status`.
%% Deprecated, use `system.process.status` instead.

-define('system_processes_status.running', 'running').

-define('system_processes_status.sleeping', 'sleeping').

-define('system_processes_status.stopped', 'stopped').

-define('system_processes_status.defunct', 'defunct').

-define(system_processes_status(Custom), Custom).
