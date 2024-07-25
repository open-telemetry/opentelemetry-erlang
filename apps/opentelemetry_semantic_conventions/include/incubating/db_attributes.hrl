
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
-define(DB_CASSANDRA_CONSISTENCYLEVEL, 'db.cassandra.consistency_level').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.all', 'all').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.each_quorum', 'each_quorum').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.quorum', 'quorum').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.local_quorum', 'local_quorum').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.one', 'one').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.two', 'two').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.three', 'three').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.local_one', 'local_one').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.any', 'any').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.serial', 'serial').

-define('DB_CASSANDRA_CONSISTENCYLEVEL_VALUES.local_serial', 'local_serial').

-define(DB_CASSANDRA_CONSISTENCYLEVEL_VALUES(Custom), Custom).


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
-define(DB_CLIENT_CONNECTIONS_STATE, 'db.client.connections.state').

-define('DB_CLIENT_CONNECTIONS_STATE_VALUES.idle', 'idle').

-define('DB_CLIENT_CONNECTIONS_STATE_VALUES.used', 'used').

-define(DB_CLIENT_CONNECTIONS_STATE_VALUES(Custom), Custom).


%% The name of a collection (table, container) within the database.
-define(DB_COLLECTION_NAME, 'db.collection.name').

%% @deprecated "Replaced by `server.address` and `server.port`."
%%  
%% Deprecated, use `server.address`, `server.port` attributes instead.
-define(DB_CONNECTIONSTRING, 'db.connection_string').


%% Unique Cosmos client instance id.
-define(DB_COSMOSDB_CLIENTID, 'db.cosmosdb.client_id').


%% Cosmos client connection mode.
-define(DB_COSMOSDB_CONNECTIONMODE, 'db.cosmosdb.connection_mode').

-define('DB_COSMOSDB_CONNECTIONMODE_VALUES.gateway', 'gateway').

-define('DB_COSMOSDB_CONNECTIONMODE_VALUES.direct', 'direct').

-define(DB_COSMOSDB_CONNECTIONMODE_VALUES(Custom), Custom).

%% @deprecated Replaced by `db.collection.name`.
%% Deprecated, use `db.collection.name` instead.
-define(DB_COSMOSDB_CONTAINER, 'db.cosmosdb.container').


%% CosmosDB Operation Type.
-define(DB_COSMOSDB_OPERATIONTYPE, 'db.cosmosdb.operation_type').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.invalid', 'Invalid').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.create', 'Create').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.patch', 'Patch').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.read', 'Read').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.read_feed', 'ReadFeed').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.delete', 'Delete').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.replace', 'Replace').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.execute', 'Execute').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.query', 'Query').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.head', 'Head').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.head_feed', 'HeadFeed').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.upsert', 'Upsert').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.batch', 'Batch').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.query_plan', 'QueryPlan').

-define('DB_COSMOSDB_OPERATIONTYPE_VALUES.execute_javascript', 'ExecuteJavaScript').

-define(DB_COSMOSDB_OPERATIONTYPE_VALUES(Custom), Custom).


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
-define(DB_SYSTEM, 'db.system').

-define('DB_SYSTEM_VALUES.other_sql', 'other_sql').

-define('DB_SYSTEM_VALUES.mssql', 'mssql').

-define('DB_SYSTEM_VALUES.mssqlcompact', 'mssqlcompact').

-define('DB_SYSTEM_VALUES.mysql', 'mysql').

-define('DB_SYSTEM_VALUES.oracle', 'oracle').

-define('DB_SYSTEM_VALUES.db2', 'db2').

-define('DB_SYSTEM_VALUES.postgresql', 'postgresql').

-define('DB_SYSTEM_VALUES.redshift', 'redshift').

-define('DB_SYSTEM_VALUES.hive', 'hive').

-define('DB_SYSTEM_VALUES.cloudscape', 'cloudscape').

-define('DB_SYSTEM_VALUES.hsqldb', 'hsqldb').

-define('DB_SYSTEM_VALUES.progress', 'progress').

-define('DB_SYSTEM_VALUES.maxdb', 'maxdb').

-define('DB_SYSTEM_VALUES.hanadb', 'hanadb').

-define('DB_SYSTEM_VALUES.ingres', 'ingres').

-define('DB_SYSTEM_VALUES.firstsql', 'firstsql').

-define('DB_SYSTEM_VALUES.edb', 'edb').

-define('DB_SYSTEM_VALUES.cache', 'cache').

-define('DB_SYSTEM_VALUES.adabas', 'adabas').

-define('DB_SYSTEM_VALUES.firebird', 'firebird').

-define('DB_SYSTEM_VALUES.derby', 'derby').

-define('DB_SYSTEM_VALUES.filemaker', 'filemaker').

-define('DB_SYSTEM_VALUES.informix', 'informix').

-define('DB_SYSTEM_VALUES.instantdb', 'instantdb').

-define('DB_SYSTEM_VALUES.interbase', 'interbase').

-define('DB_SYSTEM_VALUES.mariadb', 'mariadb').

-define('DB_SYSTEM_VALUES.netezza', 'netezza').

-define('DB_SYSTEM_VALUES.pervasive', 'pervasive').

-define('DB_SYSTEM_VALUES.pointbase', 'pointbase').

-define('DB_SYSTEM_VALUES.sqlite', 'sqlite').

-define('DB_SYSTEM_VALUES.sybase', 'sybase').

-define('DB_SYSTEM_VALUES.teradata', 'teradata').

-define('DB_SYSTEM_VALUES.vertica', 'vertica').

-define('DB_SYSTEM_VALUES.h2', 'h2').

-define('DB_SYSTEM_VALUES.coldfusion', 'coldfusion').

-define('DB_SYSTEM_VALUES.cassandra', 'cassandra').

-define('DB_SYSTEM_VALUES.hbase', 'hbase').

-define('DB_SYSTEM_VALUES.mongodb', 'mongodb').

-define('DB_SYSTEM_VALUES.redis', 'redis').

-define('DB_SYSTEM_VALUES.couchbase', 'couchbase').

-define('DB_SYSTEM_VALUES.couchdb', 'couchdb').

-define('DB_SYSTEM_VALUES.cosmosdb', 'cosmosdb').

-define('DB_SYSTEM_VALUES.dynamodb', 'dynamodb').

-define('DB_SYSTEM_VALUES.neo4j', 'neo4j').

-define('DB_SYSTEM_VALUES.geode', 'geode').

-define('DB_SYSTEM_VALUES.elasticsearch', 'elasticsearch').

-define('DB_SYSTEM_VALUES.memcached', 'memcached').

-define('DB_SYSTEM_VALUES.cockroachdb', 'cockroachdb').

-define('DB_SYSTEM_VALUES.opensearch', 'opensearch').

-define('DB_SYSTEM_VALUES.clickhouse', 'clickhouse').

-define('DB_SYSTEM_VALUES.spanner', 'spanner').

-define('DB_SYSTEM_VALUES.trino', 'trino').

-define(DB_SYSTEM_VALUES(Custom), Custom).

%% @deprecated No replacement at this time.
%% Deprecated, no replacement at this time.
-define(DB_USER, 'db.user').

%% @deprecated Replaced by `db.client.connections.pool.name`.
%% Deprecated, use `db.client.connections.pool.name` instead.
-define(POOL_NAME, 'pool.name').

%% @deprecated Replaced by `db.client.connections.state`.
%% Deprecated, use `db.client.connections.state` instead.
-define(STATE, 'state').

-define('STATE_VALUES.idle', 'idle').

-define('STATE_VALUES.used', 'used').

-define(STATE_VALUES(Custom), Custom).
