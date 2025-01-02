
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
%% @deprecated Deprecated, use `deployment.environment.name` instead.
%% 'Deprecated, use `deployment.environment.name` instead.'
%%  
-define(DEPLOYMENT_ENVIRONMENT, 'deployment.environment').


%% Name of the [deployment environment](https://wikipedia.org/wiki/Deployment_environment) (aka deployment tier).
%%  
-define(DEPLOYMENT_ENVIRONMENT_NAME, 'deployment.environment.name').


%% The id of the deployment.
%%  
-define(DEPLOYMENT_ID, 'deployment.id').


%% The name of the deployment.
%%  
-define(DEPLOYMENT_NAME, 'deployment.name').


%% The status of the deployment.
%%  
-define(DEPLOYMENT_STATUS, 'deployment.status').

-define(DEPLOYMENT_STATUS_VALUES_FAILED, 'failed').

-define(DEPLOYMENT_STATUS_VALUES_SUCCEEDED, 'succeeded').

