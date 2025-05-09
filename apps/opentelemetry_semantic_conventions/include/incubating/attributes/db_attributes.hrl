
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/db_attributes.hrl").

%% @deprecated Replaced by `cassandra.consistency.level`.
%% Deprecated, use `cassandra.consistency.level` instead.
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


%% @deprecated Replaced by `cassandra.coordinator.dc`.
%% Deprecated, use `cassandra.coordinator.dc` instead.
-define(DB_CASSANDRA_COORDINATOR_DC, 'db.cassandra.coordinator.dc').

%% @deprecated Replaced by `cassandra.coordinator.id`.
%% Deprecated, use `cassandra.coordinator.id` instead.
-define(DB_CASSANDRA_COORDINATOR_ID, 'db.cassandra.coordinator.id').

%% @deprecated Replaced by `cassandra.query.idempotent`.
%% Deprecated, use `cassandra.query.idempotent` instead.
-define(DB_CASSANDRA_IDEMPOTENCE, 'db.cassandra.idempotence').

%% @deprecated Replaced by `cassandra.page.size`.
%% Deprecated, use `cassandra.page.size` instead.
-define(DB_CASSANDRA_PAGE_SIZE, 'db.cassandra.page_size').

%% @deprecated Replaced by `cassandra.speculative_execution.count`.
%% Deprecated, use `cassandra.speculative_execution.count` instead.
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


%% @deprecated Replaced by `server.address` and `server.port`.
%% Deprecated, use `server.address`, `server.port` attributes instead.
-define(DB_CONNECTION_STRING, 'db.connection_string').

%% @deprecated Replaced by `azure.client.id`.
%% Deprecated, use `azure.client.id` instead.
-define(DB_COSMOSDB_CLIENT_ID, 'db.cosmosdb.client_id').

%% @deprecated Replaced by `azure.cosmosdb.connection.mode`.
%% Deprecated, use `azure.cosmosdb.connection.mode` instead.
-define(DB_COSMOSDB_CONNECTION_MODE, 'db.cosmosdb.connection_mode').

-define(DB_COSMOSDB_CONNECTION_MODE_VALUES_GATEWAY, 'gateway').

-define(DB_COSMOSDB_CONNECTION_MODE_VALUES_DIRECT, 'direct').


%% @deprecated Replaced by `azure.cosmosdb.consistency.level`.
%% Deprecated, use `cosmosdb.consistency.level` instead.
-define(DB_COSMOSDB_CONSISTENCY_LEVEL, 'db.cosmosdb.consistency_level').

-define(DB_COSMOSDB_CONSISTENCY_LEVEL_VALUES_STRONG, 'Strong').

-define(DB_COSMOSDB_CONSISTENCY_LEVEL_VALUES_BOUNDED_STALENESS, 'BoundedStaleness').

-define(DB_COSMOSDB_CONSISTENCY_LEVEL_VALUES_SESSION, 'Session').

-define(DB_COSMOSDB_CONSISTENCY_LEVEL_VALUES_EVENTUAL, 'Eventual').

-define(DB_COSMOSDB_CONSISTENCY_LEVEL_VALUES_CONSISTENT_PREFIX, 'ConsistentPrefix').


%% @deprecated Replaced by `db.collection.name`.
%% Deprecated, use `db.collection.name` instead.
-define(DB_COSMOSDB_CONTAINER, 'db.cosmosdb.container').

%% @deprecated No replacement at this time.
%% Deprecated, no replacement at this time.
-define(DB_COSMOSDB_OPERATION_TYPE, 'db.cosmosdb.operation_type').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_BATCH, 'batch').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_CREATE, 'create').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_DELETE, 'delete').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_EXECUTE, 'execute').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_EXECUTE_JAVASCRIPT, 'execute_javascript').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_INVALID, 'invalid').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_HEAD, 'head').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_HEAD_FEED, 'head_feed').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_PATCH, 'patch').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_QUERY, 'query').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_QUERY_PLAN, 'query_plan').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_READ, 'read').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_READ_FEED, 'read_feed').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_REPLACE, 'replace').

-define(DB_COSMOSDB_OPERATION_TYPE_VALUES_UPSERT, 'upsert').


%% @deprecated Replaced by `azure.cosmosdb.operation.contacted_regions`.
%% Deprecated, use `azure.cosmosdb.operation.contacted_regions` instead.
-define(DB_COSMOSDB_REGIONS_CONTACTED, 'db.cosmosdb.regions_contacted').

%% @deprecated Replaced by `azure.cosmosdb.operation.request_charge`.
%% Deprecated, use `azure.cosmosdb.operation.request_charge` instead.
-define(DB_COSMOSDB_REQUEST_CHARGE, 'db.cosmosdb.request_charge').

%% @deprecated Replaced by `azure.cosmosdb.request.body.size`.
%% Deprecated, use `azure.cosmosdb.request.body.size` instead.
-define(DB_COSMOSDB_REQUEST_CONTENT_LENGTH, 'db.cosmosdb.request_content_length').

%% @deprecated Replaced by `db.response.status_code`.
%% Deprecated, use `db.response.status_code` instead.
-define(DB_COSMOSDB_STATUS_CODE, 'db.cosmosdb.status_code').

%% @deprecated Replaced by `azure.cosmosdb.response.sub_status_code`.
%% Deprecated, use `azure.cosmosdb.response.sub_status_code` instead.
-define(DB_COSMOSDB_SUB_STATUS_CODE, 'db.cosmosdb.sub_status_code').

%% @deprecated Replaced by `db.namespace`.
%% Deprecated, use `db.namespace` instead.
%%  
-define(DB_ELASTICSEARCH_CLUSTER_NAME, 'db.elasticsearch.cluster.name').

%% @deprecated Replaced by `elasticsearch.node.name`.
%% Deprecated, use `elasticsearch.node.name` instead.
%%  
-define(DB_ELASTICSEARCH_NODE_NAME, 'db.elasticsearch.node.name').

%% @deprecated Replaced by `db.operation.parameter`.
%% Deprecated, use `db.operation.parameter` instead.
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

%% @deprecated Replaced by `db.operation.name`.
%% Deprecated, use `db.operation.name` instead.
-define(DB_OPERATION, 'db.operation').


%% A database operation parameter, with `<key>` being the parameter name, and the attribute value being a string representation of the parameter value.
%%  
-define(DB_OPERATION_PARAMETER, 'db.operation.parameter').


%% A database query parameter, with `<key>` being the parameter name, and the attribute value being a string representation of the parameter value.
%%  
-define(DB_QUERY_PARAMETER, 'db.query.parameter').

%% @deprecated Replaced by `db.namespace`.
%% Deprecated, use `db.namespace` instead.
-define(DB_REDIS_DATABASE_INDEX, 'db.redis.database_index').


%% Number of rows returned by the operation.
-define(DB_RESPONSE_RETURNED_ROWS, 'db.response.returned_rows').

%% @deprecated Replaced by `db.collection.name`, but only if not extracting the value from `db.query.text`.
%% Deprecated, use `db.collection.name` instead, but only if not extracting the value from `db.query.text`.
-define(DB_SQL_TABLE, 'db.sql.table').

%% @deprecated Replaced by `db.query.text`.
%% The database statement being executed.
-define(DB_STATEMENT, 'db.statement').

%% @deprecated Replaced by `db.system.name`.
%% Deprecated, use `db.system.name` instead.
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
