
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/android_attributes.hrl").


%% This attribute represents the state of the application.
%%  
-define(ANDROID_APP_STATE, 'android.app.state').

-define(ANDROID_APP_STATE_VALUES_CREATED, 'created').

-define(ANDROID_APP_STATE_VALUES_BACKGROUND, 'background').

-define(ANDROID_APP_STATE_VALUES_FOREGROUND, 'foreground').



%% Uniquely identifies the framework API revision offered by a version (`os.version`) of the android operating system. More information can be found [here](https://developer.android.com/guide/topics/manifest/uses-sdk-element#ApiLevels).
%%  
-define(ANDROID_OS_API_LEVEL, 'android.os.api_level').

%% @deprecated Renamed to `android.app.state`
%% Deprecated. Use `android.app.state` instead.
-define(ANDROID_STATE, 'android.state').

-define(ANDROID_STATE_VALUES_CREATED, 'created').

-define(ANDROID_STATE_VALUES_BACKGROUND, 'background').

-define(ANDROID_STATE_VALUES_FOREGROUND, 'foreground').

