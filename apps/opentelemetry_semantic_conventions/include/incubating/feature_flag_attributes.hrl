
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

%% The unique identifier of the feature flag.
-define(FEATURE_FLAG_KEY, 'feature_flag.key').


%% The name of the service provider that performs the flag evaluation.
-define(FEATURE_FLAG_PROVIDER_NAME, 'feature_flag.provider_name').


%% SHOULD be a semantic identifier for a value. If one is unavailable, a stringified version of the value can be used.
%%  
-define(FEATURE_FLAG_VARIANT, 'feature_flag.variant').
