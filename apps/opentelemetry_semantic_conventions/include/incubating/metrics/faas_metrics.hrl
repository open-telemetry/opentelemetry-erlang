
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

%% Number of invocation cold starts
-define(FAAS_COLDSTARTS, 'faas.coldstarts').


%% Distribution of CPU usage per invocation
-define(FAAS_CPU_USAGE, 'faas.cpu_usage').


%% Number of invocation errors
-define(FAAS_ERRORS, 'faas.errors').


%% Measures the duration of the function's initialization, such as a cold start
-define(FAAS_INIT_DURATION, 'faas.init_duration').


%% Number of successful invocations
-define(FAAS_INVOCATIONS, 'faas.invocations').


%% Measures the duration of the function's logic execution
-define(FAAS_INVOKE_DURATION, 'faas.invoke_duration').


%% Distribution of max memory usage per invocation
-define(FAAS_MEM_USAGE, 'faas.mem_usage').


%% Distribution of net I/O usage per invocation
-define(FAAS_NET_IO, 'faas.net_io').


%% Number of invocation timeouts
-define(FAAS_TIMEOUTS, 'faas.timeouts').
