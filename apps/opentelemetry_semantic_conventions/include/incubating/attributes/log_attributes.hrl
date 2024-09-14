
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

%% The basename of the file.
%%  
-define(LOG_FILE_NAME, 'log.file.name').


%% The basename of the file, with symlinks resolved.
%%  
-define(LOG_FILE_NAME_RESOLVED, 'log.file.name_resolved').


%% The full path to the file.
%%  
-define(LOG_FILE_PATH, 'log.file.path').


%% The full path to the file, with symlinks resolved.
%%  
-define(LOG_FILE_PATH_RESOLVED, 'log.file.path_resolved').


%% The stream associated with the log. See below for a list of well-known values.
%%  
-define(LOG_IOSTREAM, 'log.iostream').

-define(LOG_IOSTREAM_VALUES_STDOUT, 'stdout').

-define(LOG_IOSTREAM_VALUES_STDERR, 'stderr').



%% The complete orignal Log Record.
%%  
-define(LOG_RECORD_ORIGINAL, 'log.record.original').


%% A unique identifier for the Log Record.
%%  
-define(LOG_RECORD_UID, 'log.record.uid').
