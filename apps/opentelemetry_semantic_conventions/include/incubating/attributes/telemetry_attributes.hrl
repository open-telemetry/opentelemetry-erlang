
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/telemetry_attributes.hrl").


%% The name of the auto instrumentation agent or distribution, if used.
%%  
-define(TELEMETRY_DISTRO_NAME, 'telemetry.distro.name').


%% The version string of the auto instrumentation agent or distribution, if used.
%%  
-define(TELEMETRY_DISTRO_VERSION, 'telemetry.distro.version').
