
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
-define(DB_CASSANDRA_CONSISTENCY_LEVEL, 'db.cassandra.consistency_level').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_ALL, 'all').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_EACH_QUORUM, 'each_quorum').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_QUORUM, 'quorum').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_LOCAL_QUORUM, 'local_quorum').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_ONE, 'one').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_TWO, 'two').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_THREE, 'three').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_LOCAL_ONE, 'local_one').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_ANY, 'any').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_SERIAL, 'serial').

-define(DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_LOCAL_SERIAL, 'local_serial').



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
-define(DB_CASSANDRA_PAGE_SIZE, 'db.cassandra.page_size').


%% The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively.
%%  
-define(DB_CASSANDRA_SPECULATIVE_EXECUTION_COUNT, 'db.cassandra.speculative_execution_count').

%% @deprecated Replaced by `db.collection.name`.
%% Deprecated, use `db.collection.name` instead.
-define(DB_CASSANDRA_TABLE, 'db.cassandra.table').


%% The name of the connection pool; unique within the instrumented application. In case the connection pool implementation doesn't provide a name, instrumentation SHOULD use a combination of parameters that would make the name unique, for example, combining attributes `server.address`, `server.port`, and `db.namespace`, formatted as `server.address:server.port/db.namespace`. Instrumentations that generate connection pool name following different patterns SHOULD document it.
%%  
-define(DB_CLIENT_CONNECTION_POOL_NAME, 'db.client.connection.pool.name').


%% The state of a connection in the pool
-define(DB_CLIENT_CONNECTION_STATE, 'db.client.connection.state').

-define(DB_CLIENT_CONNECTION_STATE_VALUES_IDLE, 'idle').

-define(DB_CLIENT_CONNECTION_STATE_VALUES_USED, 'used').


%% @deprecated Replaced by `db.client.connection.pool.name`.
%% Deprecated, use `db.client.connection.pool.name` instead.
-define(DB_CLIENT_CONNECTIONS_POOL_NAME, 'db.client.connections.pool.name').

%% @deprecated Replaced by `db.client.connection.state`.
%% Deprecated, use `db.client.connection.state` instead.
-define(DB_CLIENT_CONNECTIONS_STATE, 'db.client.connections.state').

-define(DB_CLIENT_CONNECTIONS_STATE_VALUES_IDLE, 'idle').

-define(DB_CLIENT_CONNECTIONS_STATE_VALUES_USED, 'used').



%% The name of a collection (table, container) within the database.
-define(DB_COLLECTION_NAME, 'db.collection.name').

%% @deprecated "Replaced by `server.address` and `server.port`."
%%  
%% Deprecated, use `server.address`, `server.port` attributes instead.
-define(DB_CONNECTION_STRING, 'db.connection_string').


%% Unique Cosmos client instance id.
-define(DB_COSMOSDB_CLIENT_ID, 'db.cosmosdb.client_id').


%% Cosmos client connection mode.
-define(DB_COSMOSDB_CONNECTION_MODE, 'db.cosmosdb.connection_mode').

-define(DB_COSMOSDB_CONNECTION_MODE_VALUES_GATEWAY, 'gateway').

-define(DB_COSMOSDB_CONNECTION_MODE_VALUES_DIRECT, 'direct').


%% @deprecated Replaced by `db.collection.name`.
%% Deprecated, use `db.collection.name` instead.
-define(DB_COSMOSDB_CONTAINER, 'db.cosmosdb.container').


%% CosmosDB Operation Type.
-define(DB_COSMOSDB_OPERATION_TYPE, 'db.cosmosdb.operation_type').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_INVALID, 'Invalid').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_CREATE, 'Create').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_PATCH, 'Patch').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_READ, 'Read').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_READ_FEED, 'ReadFeed').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_DELETE, 'Delete').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_REPLACE, 'Replace').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_EXECUTE, 'Execute').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_QUERY, 'Query').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_HEAD, 'Head').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_HEAD_FEED, 'HeadFeed').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_UPSERT, 'Upsert').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_BATCH, 'Batch').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_QUERY_PLAN, 'QueryPlan').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_EXECUTE_JAVASCRIPT, 'ExecuteJavaScript').



%% RU consumed for that operation
-define(DB_COSMOSDB_REQUEST_CHARGE, 'db.cosmosdb.request_charge').


%% Request payload size in bytes
-define(DB_COSMOSDB_REQUEST_CONTENT_LENGTH, 'db.cosmosdb.request_content_length').


%% Cosmos DB status code.
-define(DB_COSMOSDB_STATUS_CODE, 'db.cosmosdb.status_code').


%% Cosmos DB sub status code.
-define(DB_COSMOSDB_SUB_STATUS_CODE, 'db.cosmosdb.sub_status_code').

%% @deprecated Replaced by `db.namespace`.
%% Deprecated, use `db.namespace` instead.
%%  
-define(DB_ELASTICSEARCH_CLUSTER_NAME, 'db.elasticsearch.cluster.name').


%% Represents the human-readable identifier of the node/instance to which a request was routed.
%%  
-define(DB_ELASTICSEARCH_NODE_NAME, 'db.elasticsearch.node.name').


%% A dynamic value in the url path.
%%  
-define(DB_ELASTICSEARCH_PATH_PARTS, 'db.elasticsearch.path_parts').

%% @deprecated Deprecated, no general replacement at this time. For Elasticsearch, use `db.elasticsearch.node.name` instead.
%% Deprecated, no general replacement at this time. For Elasticsearch, use `db.elasticsearch.node.name` instead.
-define(DB_INSTANCE_ID, 'db.instance.id').

%% @deprecated Removed as not used.
%% Removed, no replacement at this time.
-define(DB_JDBC_DRIVER_CLASSNAME, 'db.jdbc.driver_classname').

%% @deprecated Replaced by `db.collection.name`.
%% Deprecated, use `db.collection.name` instead.
-define(DB_MONGODB_COLLECTION, 'db.mongodb.collection').

%% @deprecated Deprecated, no replacement at this time.
%% Deprecated, SQL Server instance is now populated as a part of `db.namespace` attribute.
-define(DB_MSSQL_INSTANCE_NAME, 'db.mssql.instance_name').

%% @deprecated Replaced by `db.namespace`.
%% Deprecated, use `db.namespace` instead.
-define(DB_NAME, 'db.name').


%% The name of the database, fully qualified within the server address and port.
%%  
-define(DB_NAMESPACE, 'db.namespace').

%% @deprecated Replaced by `db.operation.name`.
%% Deprecated, use `db.operation.name` instead.
-define(DB_OPERATION, 'db.operation').


%% The number of queries included in a [batch operation](/docs/database/database-spans.md#batch-operations).
-define(DB_OPERATION_BATCH_SIZE, 'db.operation.batch.size').


%% The name of the operation or command being executed.
%%  
-define(DB_OPERATION_NAME, 'db.operation.name').


%% A query parameter used in `db.query.text`, with `<key>` being the parameter name, and the attribute value being a string representation of the parameter value.
%%  
-define(DB_QUERY_PARAMETER, 'db.query.parameter').


%% The database query being executed.
%%  
-define(DB_QUERY_TEXT, 'db.query.text').

%% @deprecated Replaced by `db.namespace`.
%% Deprecated, use `db.namespace` instead.
-define(DB_REDIS_DATABASE_INDEX, 'db.redis.database_index').

%% @deprecated Replaced by `db.collection.name`.
%% Deprecated, use `db.collection.name` instead.
-define(DB_SQL_TABLE, 'db.sql.table').

%% @deprecated Replaced by `db.query.text`.
%% The database statement being executed.
-define(DB_STATEMENT, 'db.statement').


%% The database management system (DBMS) product as identified by the client instrumentation.
-define(DB_SYSTEM, 'db.system').

-define(DB_SYSTEM_VALUES_OTHER_SQL, 'other_sql').

-define(DB_SYSTEM_VALUES_ADABAS, 'adabas').

-define(DB_SYSTEM_VALUES_CACHE, 'cache').

-define(DB_SYSTEM_VALUES_INTERSYSTEMS_CACHE, 'intersystems_cache').

-define(DB_SYSTEM_VALUES_CASSANDRA, 'cassandra').

-define(DB_SYSTEM_VALUES_CLICKHOUSE, 'clickhouse').

-define(DB_SYSTEM_VALUES_CLOUDSCAPE, 'cloudscape').

-define(DB_SYSTEM_VALUES_COCKROACHDB, 'cockroachdb').

-define(DB_SYSTEM_VALUES_COLDFUSION, 'coldfusion').

-define(DB_SYSTEM_VALUES_COSMOSDB, 'cosmosdb').

-define(DB_SYSTEM_VALUES_COUCHBASE, 'couchbase').

-define(DB_SYSTEM_VALUES_COUCHDB, 'couchdb').

-define(DB_SYSTEM_VALUES_DB2, 'db2').

-define(DB_SYSTEM_VALUES_DERBY, 'derby').

-define(DB_SYSTEM_VALUES_DYNAMODB, 'dynamodb').

-define(DB_SYSTEM_VALUES_EDB, 'edb').

-define(DB_SYSTEM_VALUES_ELASTICSEARCH, 'elasticsearch').

-define(DB_SYSTEM_VALUES_FILEMAKER, 'filemaker').

-define(DB_SYSTEM_VALUES_FIREBIRD, 'firebird').

-define(DB_SYSTEM_VALUES_FIRSTSQL, 'firstsql').

-define(DB_SYSTEM_VALUES_GEODE, 'geode').

-define(DB_SYSTEM_VALUES_H2, 'h2').

-define(DB_SYSTEM_VALUES_HANADB, 'hanadb').

-define(DB_SYSTEM_VALUES_HBASE, 'hbase').

-define(DB_SYSTEM_VALUES_HIVE, 'hive').

-define(DB_SYSTEM_VALUES_HSQLDB, 'hsqldb').

-define(DB_SYSTEM_VALUES_INFLUXDB, 'influxdb').

-define(DB_SYSTEM_VALUES_INFORMIX, 'informix').

-define(DB_SYSTEM_VALUES_INGRES, 'ingres').

-define(DB_SYSTEM_VALUES_INSTANTDB, 'instantdb').

-define(DB_SYSTEM_VALUES_INTERBASE, 'interbase').

-define(DB_SYSTEM_VALUES_MARIADB, 'mariadb').

-define(DB_SYSTEM_VALUES_MAXDB, 'maxdb').

-define(DB_SYSTEM_VALUES_MEMCACHED, 'memcached').

-define(DB_SYSTEM_VALUES_MONGODB, 'mongodb').

-define(DB_SYSTEM_VALUES_MSSQL, 'mssql').

-define(DB_SYSTEM_VALUES_MSSQLCOMPACT, 'mssqlcompact').

-define(DB_SYSTEM_VALUES_MYSQL, 'mysql').

-define(DB_SYSTEM_VALUES_NEO4J, 'neo4j').

-define(DB_SYSTEM_VALUES_NETEZZA, 'netezza').

-define(DB_SYSTEM_VALUES_OPENSEARCH, 'opensearch').

-define(DB_SYSTEM_VALUES_ORACLE, 'oracle').

-define(DB_SYSTEM_VALUES_PERVASIVE, 'pervasive').

-define(DB_SYSTEM_VALUES_POINTBASE, 'pointbase').

-define(DB_SYSTEM_VALUES_POSTGRESQL, 'postgresql').

-define(DB_SYSTEM_VALUES_PROGRESS, 'progress').

-define(DB_SYSTEM_VALUES_REDIS, 'redis').

-define(DB_SYSTEM_VALUES_REDSHIFT, 'redshift').

-define(DB_SYSTEM_VALUES_SPANNER, 'spanner').

-define(DB_SYSTEM_VALUES_SQLITE, 'sqlite').

-define(DB_SYSTEM_VALUES_SYBASE, 'sybase').

-define(DB_SYSTEM_VALUES_TERADATA, 'teradata').

-define(DB_SYSTEM_VALUES_TRINO, 'trino').

-define(DB_SYSTEM_VALUES_VERTICA, 'vertica').


%% @deprecated No replacement at this time.
%% Deprecated, no replacement at this time.
-define(DB_USER, 'db.user').

%% @deprecated Replaced by `db.client.connection.pool.name`.
%% Deprecated, use `db.client.connection.pool.name` instead.
-define(POOL_NAME, 'pool.name').

%% @deprecated Replaced by `db.client.connection.state`.
%% Deprecated, use `db.client.connection.state` instead.
-define(STATE, 'state').

-define(STATE_VALUES_IDLE, 'idle').

-define(STATE_VALUES_USED, 'used').

