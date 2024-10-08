
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

%% The name of the instrumentation scope - (`InstrumentationScope.Name` in OTLP).
-define(OTEL_SCOPE_NAME, 'otel.scope.name').


%% The version of the instrumentation scope - (`InstrumentationScope.Version` in OTLP).
-define(OTEL_SCOPE_VERSION, 'otel.scope.version').


%% Name of the code, either "OK" or "ERROR". MUST NOT be set if the status code is UNSET.
-define(OTEL_STATUS_CODE, 'otel.status_code').

-define(OTEL_STATUS_CODE_VALUES_OK, 'OK').

-define(OTEL_STATUS_CODE_VALUES_ERROR, 'ERROR').



%% Description of the Status if it has a value, otherwise not set.
-define(OTEL_STATUS_DESCRIPTION, 'otel.status_description').
