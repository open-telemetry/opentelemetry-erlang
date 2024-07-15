
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

%% The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html).
%%  

-define('db_cassandra_consistencylevel.all', 'all').

-define('db_cassandra_consistencylevel.each_quorum', 'each_quorum').

-define('db_cassandra_consistencylevel.quorum', 'quorum').

-define('db_cassandra_consistencylevel.local_quorum', 'local_quorum').

-define('db_cassandra_consistencylevel.one', 'one').

-define('db_cassandra_consistencylevel.two', 'two').

-define('db_cassandra_consistencylevel.three', 'three').

-define('db_cassandra_consistencylevel.local_one', 'local_one').

-define('db_cassandra_consistencylevel.any', 'any').

-define('db_cassandra_consistencylevel.serial', 'serial').

-define('db_cassandra_consistencylevel.local_serial', 'local_serial').

-define(db_cassandra_consistencylevel(Custom), Custom).


%% The data center of the coordinating node for a query.
%%  
-define(DB_CASSANDRA_COORDINATOR_DC, 'db.cassandra.coordinator.dc').


%% The ID of the coordinating node for a query.
%%  
-define(DB_CASSANDRA_COORDINATOR_ID, 'db.cassandra.coordinator.id').


%% Whether or not the query is idempotent.
%%  
-define(DB_CASSANDRA_IDEMPOTENCE, 'db.cassandra.idempotence').


%% The fetch size used for paging, i.e. how many rows will be returned at once.
%%  
-define(DB_CASSANDRA_PAGESIZE, 'db.cassandra.page_size').


%% The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively.
%%  
-define(DB_CASSANDRA_SPECULATIVEEXECUTIONCOUNT, 'db.cassandra.speculative_execution_count').

%% @deprecated Replaced by `db.collection.name`.
%% Deprecated, use `db.collection.name` instead.
-define(DB_CASSANDRA_TABLE, 'db.cassandra.table').


%% The name of the connection pool; unique within the instrumented application. In case the connection pool implementation doesn't provide a name, instrumentation should use a combination of `server.address` and `server.port` attributes formatted as `server.address:server.port`.
%%  
-define(DB_CLIENT_CONNECTIONS_POOL_NAME, 'db.client.connections.pool.name').


%% The state of a connection in the pool

-define('db_client_connections_state.idle', 'idle').

-define('db_client_connections_state.used', 'used').

-define(db_client_connections_state(Custom), Custom).


%% The name of a collection (table, container) within the database.
-define(DB_COLLECTION_NAME, 'db.collection.name').

%% @deprecated "Replaced by `server.address` and `server.port`."
%%  
%% Deprecated, use `server.address`, `server.port` attributes instead.
-define(DB_CONNECTIONSTRING, 'db.connection_string').


%% Unique Cosmos client instance id.
-define(DB_COSMOSDB_CLIENTID, 'db.cosmosdb.client_id').


%% Cosmos client connection mode.

-define('db_cosmosdb_connectionmode.gateway', 'gateway').

-define('db_cosmosdb_connectionmode.direct', 'direct').

-define(db_cosmosdb_connectionmode(Custom), Custom).

%% @deprecated Replaced by `db.collection.name`.
%% Deprecated, use `db.collection.name` instead.
-define(DB_COSMOSDB_CONTAINER, 'db.cosmosdb.container').


%% CosmosDB Operation Type.

-define('db_cosmosdb_operationtype.invalid', 'Invalid').

-define('db_cosmosdb_operationtype.create', 'Create').

-define('db_cosmosdb_operationtype.patch', 'Patch').

-define('db_cosmosdb_operationtype.read', 'Read').

-define('db_cosmosdb_operationtype.read_feed', 'ReadFeed').

-define('db_cosmosdb_operationtype.delete', 'Delete').

-define('db_cosmosdb_operationtype.replace', 'Replace').

-define('db_cosmosdb_operationtype.execute', 'Execute').

-define('db_cosmosdb_operationtype.query', 'Query').

-define('db_cosmosdb_operationtype.head', 'Head').

-define('db_cosmosdb_operationtype.head_feed', 'HeadFeed').

-define('db_cosmosdb_operationtype.upsert', 'Upsert').

-define('db_cosmosdb_operationtype.batch', 'Batch').

-define('db_cosmosdb_operationtype.query_plan', 'QueryPlan').

-define('db_cosmosdb_operationtype.execute_javascript', 'ExecuteJavaScript').

-define(db_cosmosdb_operationtype(Custom), Custom).


%% RU consumed for that operation
-define(DB_COSMOSDB_REQUESTCHARGE, 'db.cosmosdb.request_charge').


%% Request payload size in bytes
-define(DB_COSMOSDB_REQUESTCONTENTLENGTH, 'db.cosmosdb.request_content_length').


%% Cosmos DB status code.
-define(DB_COSMOSDB_STATUSCODE, 'db.cosmosdb.status_code').


%% Cosmos DB sub status code.
-define(DB_COSMOSDB_SUBSTATUSCODE, 'db.cosmosdb.sub_status_code').


%% Represents the identifier of an Elasticsearch cluster.
%%  
-define(DB_ELASTICSEARCH_CLUSTER_NAME, 'db.elasticsearch.cluster.name').


%% Represents the human-readable identifier of the node/instance to which a request was routed.
%%  
-define(DB_ELASTICSEARCH_NODE_NAME, 'db.elasticsearch.node.name').


%% A dynamic value in the url path.
%%  
-define(DB_ELASTICSEARCH_PATHPARTS, 'db.elasticsearch.path_parts').

%% @deprecated Deprecated, no general replacement at this time. For Elasticsearch, use `db.elasticsearch.node.name` instead.
%% Deprecated, no general replacement at this time. For Elasticsearch, use `db.elasticsearch.node.name` instead.
-define(DB_INSTANCE_ID, 'db.instance.id').

%% @deprecated Removed as not used.
%% Removed, no replacement at this time.
-define(DB_JDBC_DRIVERCLASSNAME, 'db.jdbc.driver_classname').

%% @deprecated Replaced by `db.collection.name`.
%% Deprecated, use `db.collection.name` instead.
-define(DB_MONGODB_COLLECTION, 'db.mongodb.collection').

%% @deprecated Deprecated, no replacement at this time.
%% Deprecated, SQL Server instance is now populated as a part of `db.namespace` attribute.
-define(DB_MSSQL_INSTANCENAME, 'db.mssql.instance_name').

%% @deprecated Replaced by `db.namespace`.
%% Deprecated, use `db.namespace` instead.
-define(DB_NAME, 'db.name').


%% The name of the database, fully qualified within the server address and port.
%%  
-define(DB_NAMESPACE, 'db.namespace').

%% @deprecated Replaced by `db.operation.name`.
%% Deprecated, use `db.operation.name` instead.
-define(DB_OPERATION, 'db.operation').


%% The name of the operation or command being executed.
%%  
-define(DB_OPERATION_NAME, 'db.operation.name').


%% The query parameters used in `db.query.text`, with `<key>` being the parameter name, and the attribute value being the parameter value.
%%  
-define(DB_QUERY_PARAMETER, 'db.query.parameter').


%% The database query being executed.
%%  
-define(DB_QUERY_TEXT, 'db.query.text').

%% @deprecated Replaced by `db.namespace`.
%% Deprecated, use `db.namespace` instead.
-define(DB_REDIS_DATABASEINDEX, 'db.redis.database_index').

%% @deprecated Replaced by `db.collection.name`.
%% Deprecated, use `db.collection.name` instead.
-define(DB_SQL_TABLE, 'db.sql.table').

%% @deprecated Replaced by `db.query.text`.
%% The database statement being executed.
-define(DB_STATEMENT, 'db.statement').


%% The database management system (DBMS) product as identified by the client instrumentation.

-define('db_system.other_sql', 'other_sql').

-define('db_system.mssql', 'mssql').

-define('db_system.mssqlcompact', 'mssqlcompact').

-define('db_system.mysql', 'mysql').

-define('db_system.oracle', 'oracle').

-define('db_system.db2', 'db2').

-define('db_system.postgresql', 'postgresql').

-define('db_system.redshift', 'redshift').

-define('db_system.hive', 'hive').

-define('db_system.cloudscape', 'cloudscape').

-define('db_system.hsqldb', 'hsqldb').

-define('db_system.progress', 'progress').

-define('db_system.maxdb', 'maxdb').

-define('db_system.hanadb', 'hanadb').

-define('db_system.ingres', 'ingres').

-define('db_system.firstsql', 'firstsql').

-define('db_system.edb', 'edb').

-define('db_system.cache', 'cache').

-define('db_system.adabas', 'adabas').

-define('db_system.firebird', 'firebird').

-define('db_system.derby', 'derby').

-define('db_system.filemaker', 'filemaker').

-define('db_system.informix', 'informix').

-define('db_system.instantdb', 'instantdb').

-define('db_system.interbase', 'interbase').

-define('db_system.mariadb', 'mariadb').

-define('db_system.netezza', 'netezza').

-define('db_system.pervasive', 'pervasive').

-define('db_system.pointbase', 'pointbase').

-define('db_system.sqlite', 'sqlite').

-define('db_system.sybase', 'sybase').

-define('db_system.teradata', 'teradata').

-define('db_system.vertica', 'vertica').

-define('db_system.h2', 'h2').

-define('db_system.coldfusion', 'coldfusion').

-define('db_system.cassandra', 'cassandra').

-define('db_system.hbase', 'hbase').

-define('db_system.mongodb', 'mongodb').

-define('db_system.redis', 'redis').

-define('db_system.couchbase', 'couchbase').

-define('db_system.couchdb', 'couchdb').

-define('db_system.cosmosdb', 'cosmosdb').

-define('db_system.dynamodb', 'dynamodb').

-define('db_system.neo4j', 'neo4j').

-define('db_system.geode', 'geode').

-define('db_system.elasticsearch', 'elasticsearch').

-define('db_system.memcached', 'memcached').

-define('db_system.cockroachdb', 'cockroachdb').

-define('db_system.opensearch', 'opensearch').

-define('db_system.clickhouse', 'clickhouse').

-define('db_system.spanner', 'spanner').

-define('db_system.trino', 'trino').

-define(db_system(Custom), Custom).

%% @deprecated No replacement at this time.
%% Deprecated, no replacement at this time.
-define(DB_USER, 'db.user').

%% @deprecated Replaced by `db.client.connections.pool.name`.
%% Deprecated, use `db.client.connections.pool.name` instead.
-define(POOL_NAME, 'pool.name').

%% @deprecated Replaced by `db.client.connections.state`.
%% Deprecated, use `db.client.connections.state` instead.

-define('state.idle', 'idle').

-define('state.used', 'used').

-define(state(Custom), Custom).
