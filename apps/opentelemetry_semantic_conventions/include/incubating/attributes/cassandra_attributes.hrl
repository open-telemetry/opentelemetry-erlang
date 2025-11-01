
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/cassandra_attributes.hrl").


%% The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html).
%%  
-define(CASSANDRA_CONSISTENCY_LEVEL, 'cassandra.consistency.level').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_ALL, 'all').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_EACH_QUORUM, 'each_quorum').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_QUORUM, 'quorum').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_LOCAL_QUORUM, 'local_quorum').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_ONE, 'one').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_TWO, 'two').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_THREE, 'three').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_LOCAL_ONE, 'local_one').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_ANY, 'any').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_SERIAL, 'serial').

-define(CASSANDRA_CONSISTENCY_LEVEL_VALUES_LOCAL_SERIAL, 'local_serial').



%% The data center of the coordinating node for a query.
%%  
-define(CASSANDRA_COORDINATOR_DC, 'cassandra.coordinator.dc').


%% The ID of the coordinating node for a query.
%%  
-define(CASSANDRA_COORDINATOR_ID, 'cassandra.coordinator.id').


%% The fetch size used for paging, i.e. how many rows will be returned at once.
%%  
-define(CASSANDRA_PAGE_SIZE, 'cassandra.page.size').


%% Whether or not the query is idempotent.
%%  
-define(CASSANDRA_QUERY_IDEMPOTENT, 'cassandra.query.idempotent').


%% The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively.
%%  
-define(CASSANDRA_SPECULATIVE_EXECUTION_COUNT, 'cassandra.speculative_execution.count').
