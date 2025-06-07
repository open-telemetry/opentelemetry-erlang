
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/profile_attributes.hrl").


%% Describes the interpreter or compiler of a single frame.
%%  
-define(PROFILE_FRAME_TYPE, 'profile.frame.type').

-define(PROFILE_FRAME_TYPE_VALUES_DOTNET, 'dotnet').

-define(PROFILE_FRAME_TYPE_VALUES_JVM, 'jvm').

-define(PROFILE_FRAME_TYPE_VALUES_KERNEL, 'kernel').

-define(PROFILE_FRAME_TYPE_VALUES_NATIVE, 'native').

-define(PROFILE_FRAME_TYPE_VALUES_PERL, 'perl').

-define(PROFILE_FRAME_TYPE_VALUES_PHP, 'php').

-define(PROFILE_FRAME_TYPE_VALUES_CPYTHON, 'cpython').

-define(PROFILE_FRAME_TYPE_VALUES_RUBY, 'ruby').

-define(PROFILE_FRAME_TYPE_VALUES_V8JS, 'v8js').

-define(PROFILE_FRAME_TYPE_VALUES_BEAM, 'beam').

-define(PROFILE_FRAME_TYPE_VALUES_GO, 'go').

-define(PROFILE_FRAME_TYPE_VALUES_RUST, 'rust').

