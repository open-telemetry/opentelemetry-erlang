
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/azure_attributes.hrl").


%% The unique identifier of the client instance.
-define(AZURE_CLIENT_ID, 'azure.client.id').


%% Cosmos client connection mode.
-define(AZURE_COSMOSDB_CONNECTION_MODE, 'azure.cosmosdb.connection.mode').

-define(AZURE_COSMOSDB_CONNECTION_MODE_VALUES_GATEWAY, 'gateway').

-define(AZURE_COSMOSDB_CONNECTION_MODE_VALUES_DIRECT, 'direct').



%% Account or request [consistency level](https://learn.microsoft.com/azure/cosmos-db/consistency-levels).
-define(AZURE_COSMOSDB_CONSISTENCY_LEVEL, 'azure.cosmosdb.consistency.level').

-define(AZURE_COSMOSDB_CONSISTENCY_LEVEL_VALUES_STRONG, 'Strong').

-define(AZURE_COSMOSDB_CONSISTENCY_LEVEL_VALUES_BOUNDED_STALENESS, 'BoundedStaleness').

-define(AZURE_COSMOSDB_CONSISTENCY_LEVEL_VALUES_SESSION, 'Session').

-define(AZURE_COSMOSDB_CONSISTENCY_LEVEL_VALUES_EVENTUAL, 'Eventual').

-define(AZURE_COSMOSDB_CONSISTENCY_LEVEL_VALUES_CONSISTENT_PREFIX, 'ConsistentPrefix').



%% List of regions contacted during operation in the order that they were contacted. If there is more than one region listed, it indicates that the operation was performed on multiple regions i.e. cross-regional call.
%%  
-define(AZURE_COSMOSDB_OPERATION_CONTACTED_REGIONS, 'azure.cosmosdb.operation.contacted_regions').


%% The number of request units consumed by the operation.
%%  
-define(AZURE_COSMOSDB_OPERATION_REQUEST_CHARGE, 'azure.cosmosdb.operation.request_charge').


%% Request payload size in bytes.
-define(AZURE_COSMOSDB_REQUEST_BODY_SIZE, 'azure.cosmosdb.request.body.size').


%% Cosmos DB sub status code.
-define(AZURE_COSMOSDB_RESPONSE_SUB_STATUS_CODE, 'azure.cosmosdb.response.sub_status_code').
