
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

%% The language of the telemetry SDK.
%%  

-define('telemetry_sdk_language.cpp', 'cpp').

-define('telemetry_sdk_language.dotnet', 'dotnet').

-define('telemetry_sdk_language.erlang', 'erlang').

-define('telemetry_sdk_language.go', 'go').

-define('telemetry_sdk_language.java', 'java').

-define('telemetry_sdk_language.nodejs', 'nodejs').

-define('telemetry_sdk_language.php', 'php').

-define('telemetry_sdk_language.python', 'python').

-define('telemetry_sdk_language.ruby', 'ruby').

-define('telemetry_sdk_language.rust', 'rust').

-define('telemetry_sdk_language.swift', 'swift').

-define('telemetry_sdk_language.webjs', 'webjs').

-define(telemetry_sdk_language(Custom), Custom).


%% The name of the telemetry SDK as defined above.
%%  
-define(TELEMETRY_SDK_NAME, 'telemetry.sdk.name').


%% The version string of the telemetry SDK.
%%  
-define(TELEMETRY_SDK_VERSION, 'telemetry.sdk.version').
