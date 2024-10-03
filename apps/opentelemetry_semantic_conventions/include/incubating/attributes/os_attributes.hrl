
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
-define(OS_BUILD_ID, 'os.build_id').


%% Human readable (not intended to be parsed) OS version information, like e.g. reported by `ver` or `lsb_release -a` commands.
%%  
-define(OS_DESCRIPTION, 'os.description').


%% Human readable operating system name.
-define(OS_NAME, 'os.name').


%% The operating system type.
%%  
-define(OS_TYPE, 'os.type').

-define(OS_TYPE_VALUES_WINDOWS, 'windows').

-define(OS_TYPE_VALUES_LINUX, 'linux').

-define(OS_TYPE_VALUES_DARWIN, 'darwin').

-define(OS_TYPE_VALUES_FREEBSD, 'freebsd').

-define(OS_TYPE_VALUES_NETBSD, 'netbsd').

-define(OS_TYPE_VALUES_OPENBSD, 'openbsd').

-define(OS_TYPE_VALUES_DRAGONFLYBSD, 'dragonflybsd').

-define(OS_TYPE_VALUES_HPUX, 'hpux').

-define(OS_TYPE_VALUES_AIX, 'aix').

-define(OS_TYPE_VALUES_SOLARIS, 'solaris').

-define(OS_TYPE_VALUES_Z_OS, 'z_os').



%% The version string of the operating system as defined in [Version Attributes](/docs/resource/README.md#version-attributes).
%%  
-define(OS_VERSION, 'os.version').
