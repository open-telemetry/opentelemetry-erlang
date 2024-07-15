
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

%% Unique identifier for a particular build or compilation of the operating system.
-define(OS_BUILDID, 'os.build_id').


%% Human readable (not intended to be parsed) OS version information, like e.g. reported by `ver` or `lsb_release -a` commands.
%%  
-define(OS_DESCRIPTION, 'os.description').


%% Human readable operating system name.
-define(OS_NAME, 'os.name').


%% The operating system type.
%%  

-define('os_type.windows', 'windows').

-define('os_type.linux', 'linux').

-define('os_type.darwin', 'darwin').

-define('os_type.freebsd', 'freebsd').

-define('os_type.netbsd', 'netbsd').

-define('os_type.openbsd', 'openbsd').

-define('os_type.dragonflybsd', 'dragonflybsd').

-define('os_type.hpux', 'hpux').

-define('os_type.aix', 'aix').

-define('os_type.solaris', 'solaris').

-define('os_type.z_os', 'z_os').

-define(os_type(Custom), Custom).


%% The version string of the operating system as defined in [Version Attributes](/docs/resource/README.md#version-attributes).
%%  
-define(OS_VERSION, 'os.version').
