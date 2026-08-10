
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

%% The name of a collection (table, container) within the database.
-define(DB_COLLECTION_NAME, 'db.collection.name').


%% The name of the database, fully qualified within the server address and port.
%%  
-define(DB_NAMESPACE, 'db.namespace').


%% The number of queries included in a batch operation.
-define(DB_OPERATION_BATCH_SIZE, 'db.operation.batch.size').


%% The name of the operation or command being executed.
%%  
-define(DB_OPERATION_NAME, 'db.operation.name').


%% Low cardinality summary of a database query.
%%  
-define(DB_QUERY_SUMMARY, 'db.query.summary').


%% The database query being executed.
%%  
-define(DB_QUERY_TEXT, 'db.query.text').


%% Database response status code.
-define(DB_RESPONSE_STATUS_CODE, 'db.response.status_code').


%% The name of a stored procedure within the database.
-define(DB_STORED_PROCEDURE_NAME, 'db.stored_procedure.name').


%% The database management system (DBMS) product as identified by the client instrumentation.
-define(DB_SYSTEM_NAME, 'db.system.name').

-define(DB_SYSTEM_NAME_VALUES_OTHER_SQL, 'other_sql').

-define(DB_SYSTEM_NAME_VALUES_SOFTWAREAG_ADABAS, 'softwareag.adabas').

-define(DB_SYSTEM_NAME_VALUES_ACTIAN_INGRES, 'actian.ingres').

-define(DB_SYSTEM_NAME_VALUES_AWS_DYNAMODB, 'aws.dynamodb').

-define(DB_SYSTEM_NAME_VALUES_AWS_REDSHIFT, 'aws.redshift').

-define(DB_SYSTEM_NAME_VALUES_AZURE_COSMOSDB, 'azure.cosmosdb').

-define(DB_SYSTEM_NAME_VALUES_INTERSYSTEMS_CACHE, 'intersystems.cache').

-define(DB_SYSTEM_NAME_VALUES_CASSANDRA, 'cassandra').

-define(DB_SYSTEM_NAME_VALUES_CLICKHOUSE, 'clickhouse').

-define(DB_SYSTEM_NAME_VALUES_COCKROACHDB, 'cockroachdb').

-define(DB_SYSTEM_NAME_VALUES_COUCHBASE, 'couchbase').

-define(DB_SYSTEM_NAME_VALUES_COUCHDB, 'couchdb').

-define(DB_SYSTEM_NAME_VALUES_DERBY, 'derby').

-define(DB_SYSTEM_NAME_VALUES_ELASTICSEARCH, 'elasticsearch').

-define(DB_SYSTEM_NAME_VALUES_FIREBIRDSQL, 'firebirdsql').

-define(DB_SYSTEM_NAME_VALUES_GCP_SPANNER, 'gcp.spanner').

-define(DB_SYSTEM_NAME_VALUES_GEODE, 'geode').

-define(DB_SYSTEM_NAME_VALUES_H2DATABASE, 'h2database').

-define(DB_SYSTEM_NAME_VALUES_HBASE, 'hbase').

-define(DB_SYSTEM_NAME_VALUES_HIVE, 'hive').

-define(DB_SYSTEM_NAME_VALUES_HSQLDB, 'hsqldb').

-define(DB_SYSTEM_NAME_VALUES_IBM_DB2, 'ibm.db2').

-define(DB_SYSTEM_NAME_VALUES_IBM_INFORMIX, 'ibm.informix').

-define(DB_SYSTEM_NAME_VALUES_IBM_NETEZZA, 'ibm.netezza').

-define(DB_SYSTEM_NAME_VALUES_INFLUXDB, 'influxdb').

-define(DB_SYSTEM_NAME_VALUES_INSTANTDB, 'instantdb').

-define(DB_SYSTEM_NAME_VALUES_MARIADB, 'mariadb').

-define(DB_SYSTEM_NAME_VALUES_MEMCACHED, 'memcached').

-define(DB_SYSTEM_NAME_VALUES_MONGODB, 'mongodb').

-define(DB_SYSTEM_NAME_VALUES_MICROSOFT_SQL_SERVER, 'microsoft.sql_server').

-define(DB_SYSTEM_NAME_VALUES_MYSQL, 'mysql').

-define(DB_SYSTEM_NAME_VALUES_NEO4J, 'neo4j').

-define(DB_SYSTEM_NAME_VALUES_OPENSEARCH, 'opensearch').

-define(DB_SYSTEM_NAME_VALUES_ORACLE_DB, 'oracle.db').

-define(DB_SYSTEM_NAME_VALUES_POSTGRESQL, 'postgresql').

-define(DB_SYSTEM_NAME_VALUES_REDIS, 'redis').

-define(DB_SYSTEM_NAME_VALUES_SAP_HANA, 'sap.hana').

-define(DB_SYSTEM_NAME_VALUES_SAP_MAXDB, 'sap.maxdb').

-define(DB_SYSTEM_NAME_VALUES_SQLITE, 'sqlite').

-define(DB_SYSTEM_NAME_VALUES_TERADATA, 'teradata').

-define(DB_SYSTEM_NAME_VALUES_TRINO, 'trino').

