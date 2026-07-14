
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

%% Number of active client instances
-define(AZURE_COSMOSDB_CLIENT_ACTIVE_INSTANCE_COUNT, 'azure.cosmosdb.client.active_instance.count').


%% [Request units](https://learn.microsoft.com/azure/cosmos-db/request-units) consumed by the operation
-define(AZURE_COSMOSDB_CLIENT_OPERATION_REQUEST_CHARGE, 'azure.cosmosdb.client.operation.request_charge').
