
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/cloudfoundry_attributes.hrl").


%% The guid of the application.
%%  
-define(CLOUDFOUNDRY_APP_ID, 'cloudfoundry.app.id').


%% The index of the application instance. 0 when just one instance is active.
%%  
-define(CLOUDFOUNDRY_APP_INSTANCE_ID, 'cloudfoundry.app.instance.id').


%% The name of the application.
%%  
-define(CLOUDFOUNDRY_APP_NAME, 'cloudfoundry.app.name').


%% The guid of the CloudFoundry org the application is running in.
%%  
-define(CLOUDFOUNDRY_ORG_ID, 'cloudfoundry.org.id').


%% The name of the CloudFoundry organization the app is running in.
%%  
-define(CLOUDFOUNDRY_ORG_NAME, 'cloudfoundry.org.name').


%% The UID identifying the process.
%%  
-define(CLOUDFOUNDRY_PROCESS_ID, 'cloudfoundry.process.id').


%% The type of process.
%%  
-define(CLOUDFOUNDRY_PROCESS_TYPE, 'cloudfoundry.process.type').


%% The guid of the CloudFoundry space the application is running in.
%%  
-define(CLOUDFOUNDRY_SPACE_ID, 'cloudfoundry.space.id').


%% The name of the CloudFoundry space the application is running in.
%%  
-define(CLOUDFOUNDRY_SPACE_NAME, 'cloudfoundry.space.name').


%% A guid or another name describing the event source.
%%  
-define(CLOUDFOUNDRY_SYSTEM_ID, 'cloudfoundry.system.id').


%% A guid describing the concrete instance of the event source.
%%  
-define(CLOUDFOUNDRY_SYSTEM_INSTANCE_ID, 'cloudfoundry.system.instance.id').
