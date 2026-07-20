
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/user_agent_attributes.hrl").


%% Name of the user-agent extracted from original. Usually refers to the browser's name.
%%  
-define(USER_AGENT_NAME, 'user_agent.name').


%% Human readable operating system name.
-define(USER_AGENT_OS_NAME, 'user_agent.os.name').


%% The version string of the operating system as defined in [Version Attributes](/docs/resource/README.md#version-attributes).
%%  
-define(USER_AGENT_OS_VERSION, 'user_agent.os.version').


%% Specifies the category of synthetic traffic, such as tests or bots.
%%  
-define(USER_AGENT_SYNTHETIC_TYPE, 'user_agent.synthetic.type').

-define(USER_AGENT_SYNTHETIC_TYPE_VALUES_BOT, 'bot').

-define(USER_AGENT_SYNTHETIC_TYPE_VALUES_TEST, 'test').



%% Version of the user-agent extracted from original. Usually refers to the browser's version
%%  
-define(USER_AGENT_VERSION, 'user_agent.version').
