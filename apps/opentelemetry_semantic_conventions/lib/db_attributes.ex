defmodule OpenTelemetry.SemanticConventions.DbAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Db attributes.
  """

  @typedoc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html).


  ### Options
  * `:all` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:each_quorum` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:quorum` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:local_quorum` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:one` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:two` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:three` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:local_one` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:any` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:serial` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:local_serial` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type db_cassandra_consistencylevel() ::
          :all
          | :each_quorum
          | :quorum
          | :local_quorum
          | :one
          | :two
          | :three
          | :local_one
          | :any
          | :serial
          | :local_serial
          | atom()

  @doc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html).



  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cassandra_consistencylevel(:all)
      :all
      
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cassandra_consistencylevel(:custom_value)
      :custom_value
  """
  @spec db_cassandra_consistencylevel(db_cassandra_consistencylevel()) ::
          :all
          | :each_quorum
          | :quorum
          | :local_quorum
          | :one
          | :two
          | :three
          | :local_one
          | :any
          | :serial
          | :local_serial
          | atom()
  def db_cassandra_consistencylevel(option) do
    :"db.cassandra.consistency_level"

    case option do
      :all -> :all
      :each_quorum -> :each_quorum
      :quorum -> :quorum
      :local_quorum -> :local_quorum
      :one -> :one
      :two -> :two
      :three -> :three
      :local_one -> :local_one
      :any -> :any
      :serial -> :serial
      :local_serial -> :local_serial
      _ -> option
    end
  end

  @doc """
  The data center of the coordinating node for a query.



  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cassandra_coordinator_dc()
      :"db.cassandra.coordinator.dc"
  """
  @spec db_cassandra_coordinator_dc :: :"db.cassandra.coordinator.dc"
  def db_cassandra_coordinator_dc do
    :"db.cassandra.coordinator.dc"
  end

  @doc """
  The ID of the coordinating node for a query.



  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cassandra_coordinator_id()
      :"db.cassandra.coordinator.id"
  """
  @spec db_cassandra_coordinator_id :: :"db.cassandra.coordinator.id"
  def db_cassandra_coordinator_id do
    :"db.cassandra.coordinator.id"
  end

  @doc """
  Whether or not the query is idempotent.



  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cassandra_idempotence()
      :"db.cassandra.idempotence"
  """
  @spec db_cassandra_idempotence :: :"db.cassandra.idempotence"
  def db_cassandra_idempotence do
    :"db.cassandra.idempotence"
  end

  @doc """
  The fetch size used for paging, i.e. how many rows will be returned at once.



  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cassandra_pagesize()
      :"db.cassandra.page_size"
  """
  @spec db_cassandra_pagesize :: :"db.cassandra.page_size"
  def db_cassandra_pagesize do
    :"db.cassandra.page_size"
  end

  @doc """
  The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively.



  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cassandra_speculativeexecutioncount()
      :"db.cassandra.speculative_execution_count"
  """
  @spec db_cassandra_speculativeexecutioncount :: :"db.cassandra.speculative_execution_count"
  def db_cassandra_speculativeexecutioncount do
    :"db.cassandra.speculative_execution_count"
  end

  @deprecated """
  Replaced by `db.collection.name`.
  """

  @spec db_cassandra_table :: :"db.cassandra.table"
  def db_cassandra_table do
    :"db.cassandra.table"
  end

  @doc """
  The name of the connection pool; unique within the instrumented application. In case the connection pool implementation doesn't provide a name, instrumentation should use a combination of `server.address` and `server.port` attributes formatted as `server.address:server.port`.



  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_client_connections_pool_name()
      :"db.client.connections.pool.name"
  """
  @spec db_client_connections_pool_name :: :"db.client.connections.pool.name"
  def db_client_connections_pool_name do
    :"db.client.connections.pool.name"
  end

  @typedoc """
  The state of a connection in the pool

  ### Options
  * `:idle` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:used` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type db_client_connections_state() :: :idle | :used | atom()

  @doc """
  The state of a connection in the pool


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_client_connections_state(:idle)
      :idle
      
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_client_connections_state(:custom_value)
      :custom_value
  """
  @spec db_client_connections_state(db_client_connections_state()) :: :idle | :used | atom()
  def db_client_connections_state(option) do
    :"db.client.connections.state"

    case option do
      :idle -> :idle
      :used -> :used
      _ -> option
    end
  end

  @doc """
  The name of a collection (table, container) within the database.
  ### Notes

  If the collection name is parsed from the query, it **SHOULD** match the value provided in the query and may be qualified with the schema and database name.
  It is RECOMMENDED to capture the value as provided by the application without attempting to do any case normalization.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_collection_name()
      :"db.collection.name"
  """
  @spec db_collection_name :: :"db.collection.name"
  def db_collection_name do
    :"db.collection.name"
  end

  @deprecated """
  "Replaced by `server.address` and `server.port`."

  """

  @spec db_connectionstring :: :"db.connection_string"
  def db_connectionstring do
    :"db.connection_string"
  end

  @doc """
  Unique Cosmos client instance id.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cosmosdb_clientid()
      :"db.cosmosdb.client_id"
  """
  @spec db_cosmosdb_clientid :: :"db.cosmosdb.client_id"
  def db_cosmosdb_clientid do
    :"db.cosmosdb.client_id"
  end

  @typedoc """
  Cosmos client connection mode.

  ### Options
  * `:gateway` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Gateway (HTTP) connections mode
  * `:direct` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Direct connection.

  """
  @type db_cosmosdb_connectionmode() :: :gateway | :direct | atom()

  @doc """
  Cosmos client connection mode.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cosmosdb_connectionmode(:gateway)
      :gateway
      
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cosmosdb_connectionmode(:custom_value)
      :custom_value
  """
  @spec db_cosmosdb_connectionmode(db_cosmosdb_connectionmode()) :: :gateway | :direct | atom()
  def db_cosmosdb_connectionmode(option) do
    :"db.cosmosdb.connection_mode"

    case option do
      :gateway -> :gateway
      :direct -> :direct
      _ -> option
    end
  end

  @deprecated """
  Replaced by `db.collection.name`.
  """

  @spec db_cosmosdb_container :: :"db.cosmosdb.container"
  def db_cosmosdb_container do
    :"db.cosmosdb.container"
  end

  @type invalid() :: :Invalid

  @type create() :: :Create

  @type patch() :: :Patch

  @type read() :: :Read

  @type read_feed() :: :ReadFeed

  @type delete() :: :Delete

  @type replace() :: :Replace

  @type execute() :: :Execute

  @type query() :: :Query

  @type head() :: :Head

  @type head_feed() :: :HeadFeed

  @type upsert() :: :Upsert

  @type batch() :: :Batch

  @type query_plan() :: :QueryPlan

  @type execute_javascript() :: :ExecuteJavaScript

  @typedoc """
  CosmosDB Operation Type.

  ### Options
  * `:invalid` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:create` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:patch` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:read` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:read_feed` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:delete` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:replace` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:execute` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:query` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:head` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:head_feed` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:upsert` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:batch` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:query_plan` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:execute_javascript` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type db_cosmosdb_operationtype() ::
          invalid()
          | create()
          | patch()
          | read()
          | read_feed()
          | delete()
          | replace()
          | execute()
          | query()
          | head()
          | head_feed()
          | upsert()
          | batch()
          | query_plan()
          | execute_javascript()
          | atom()

  @doc """
  CosmosDB Operation Type.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cosmosdb_operationtype(:invalid)
      :Invalid
      
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cosmosdb_operationtype(:custom_value)
      :custom_value
  """
  @spec db_cosmosdb_operationtype(db_cosmosdb_operationtype()) ::
          invalid()
          | create()
          | patch()
          | read()
          | read_feed()
          | delete()
          | replace()
          | execute()
          | query()
          | head()
          | head_feed()
          | upsert()
          | batch()
          | query_plan()
          | execute_javascript()
          | atom()
  def db_cosmosdb_operationtype(option) do
    :"db.cosmosdb.operation_type"

    case option do
      :invalid -> :Invalid
      :create -> :Create
      :patch -> :Patch
      :read -> :Read
      :read_feed -> :ReadFeed
      :delete -> :Delete
      :replace -> :Replace
      :execute -> :Execute
      :query -> :Query
      :head -> :Head
      :head_feed -> :HeadFeed
      :upsert -> :Upsert
      :batch -> :Batch
      :query_plan -> :QueryPlan
      :execute_javascript -> :ExecuteJavaScript
      _ -> option
    end
  end

  @doc """
  RU consumed for that operation


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cosmosdb_requestcharge()
      :"db.cosmosdb.request_charge"
  """
  @spec db_cosmosdb_requestcharge :: :"db.cosmosdb.request_charge"
  def db_cosmosdb_requestcharge do
    :"db.cosmosdb.request_charge"
  end

  @doc """
  Request payload size in bytes


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cosmosdb_requestcontentlength()
      :"db.cosmosdb.request_content_length"
  """
  @spec db_cosmosdb_requestcontentlength :: :"db.cosmosdb.request_content_length"
  def db_cosmosdb_requestcontentlength do
    :"db.cosmosdb.request_content_length"
  end

  @doc """
  Cosmos DB status code.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cosmosdb_statuscode()
      :"db.cosmosdb.status_code"
  """
  @spec db_cosmosdb_statuscode :: :"db.cosmosdb.status_code"
  def db_cosmosdb_statuscode do
    :"db.cosmosdb.status_code"
  end

  @doc """
  Cosmos DB sub status code.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_cosmosdb_substatuscode()
      :"db.cosmosdb.sub_status_code"
  """
  @spec db_cosmosdb_substatuscode :: :"db.cosmosdb.sub_status_code"
  def db_cosmosdb_substatuscode do
    :"db.cosmosdb.sub_status_code"
  end

  @doc """
  Represents the identifier of an Elasticsearch cluster.



  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_elasticsearch_cluster_name()
      :"db.elasticsearch.cluster.name"
  """
  @spec db_elasticsearch_cluster_name :: :"db.elasticsearch.cluster.name"
  def db_elasticsearch_cluster_name do
    :"db.elasticsearch.cluster.name"
  end

  @doc """
  Represents the human-readable identifier of the node/instance to which a request was routed.



  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_elasticsearch_node_name()
      :"db.elasticsearch.node.name"
  """
  @spec db_elasticsearch_node_name :: :"db.elasticsearch.node.name"
  def db_elasticsearch_node_name do
    :"db.elasticsearch.node.name"
  end

  @doc """
  A dynamic value in the url path.

  ### Notes

  Many Elasticsearch url paths allow dynamic values. These **SHOULD** be recorded in span attributes in the format `db.elasticsearch.path_parts.<key>`, where `<key>` is the url path part name. The implementation **SHOULD** reference the [elasticsearch schema](https://raw.githubusercontent.com/elastic/elasticsearch-specification/main/output/schema/schema.json) in order to map the path part values to their names.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_elasticsearch_pathparts()
      :"db.elasticsearch.path_parts"
  """
  @spec db_elasticsearch_pathparts :: :"db.elasticsearch.path_parts"
  def db_elasticsearch_pathparts do
    :"db.elasticsearch.path_parts"
  end

  @deprecated """
  Deprecated, no general replacement at this time. For Elasticsearch, use `db.elasticsearch.node.name` instead.
  """

  @spec db_instance_id :: :"db.instance.id"
  def db_instance_id do
    :"db.instance.id"
  end

  @deprecated """
  Removed as not used.
  """

  @spec db_jdbc_driverclassname :: :"db.jdbc.driver_classname"
  def db_jdbc_driverclassname do
    :"db.jdbc.driver_classname"
  end

  @deprecated """
  Replaced by `db.collection.name`.
  """

  @spec db_mongodb_collection :: :"db.mongodb.collection"
  def db_mongodb_collection do
    :"db.mongodb.collection"
  end

  @deprecated """
  Deprecated, no replacement at this time.
  """

  @spec db_mssql_instancename :: :"db.mssql.instance_name"
  def db_mssql_instancename do
    :"db.mssql.instance_name"
  end

  @deprecated """
  Replaced by `db.namespace`.
  """

  @spec db_name :: :"db.name"
  def db_name do
    :"db.name"
  end

  @doc """
  The name of the database, fully qualified within the server address and port.

  ### Notes

  If a database system has multiple namespace components, they **SHOULD** be concatenated (potentially using database system specific conventions) from most general to most specific namespace component, and more specific namespaces **SHOULD** **NOT** be captured without the more general namespaces, to ensure that "startswith" queries for the more general namespaces will be valid.
  Semantic conventions for individual database systems **SHOULD** document what `db.namespace` means in the context of that system.
  It is RECOMMENDED to capture the value as provided by the application without attempting to do any case normalization.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_namespace()
      :"db.namespace"
  """
  @spec db_namespace :: :"db.namespace"
  def db_namespace do
    :"db.namespace"
  end

  @deprecated """
  Replaced by `db.operation.name`.
  """

  @spec db_operation :: :"db.operation"
  def db_operation do
    :"db.operation"
  end

  @doc """
  The name of the operation or command being executed.

  ### Notes

  It is RECOMMENDED to capture the value as provided by the application without attempting to do any case normalization.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_operation_name()
      :"db.operation.name"
  """
  @spec db_operation_name :: :"db.operation.name"
  def db_operation_name do
    :"db.operation.name"
  end

  @doc """
  The query parameters used in `db.query.text`, with `<key>` being the parameter name, and the attribute value being the parameter value.

  ### Notes

  Query parameters should only be captured when `db.query.text` is parameterized with placeholders.
  If a parameter has no name and instead is referenced only by index, then `<key>` **SHOULD** be the 0-based index.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_query_parameter()
      :"db.query.parameter"
  """
  @spec db_query_parameter :: :"db.query.parameter"
  def db_query_parameter do
    :"db.query.parameter"
  end

  @doc """
  The database query being executed.



  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_query_text()
      :"db.query.text"
  """
  @spec db_query_text :: :"db.query.text"
  def db_query_text do
    :"db.query.text"
  end

  @deprecated """
  Replaced by `db.namespace`.
  """

  @spec db_redis_databaseindex :: :"db.redis.database_index"
  def db_redis_databaseindex do
    :"db.redis.database_index"
  end

  @deprecated """
  Replaced by `db.collection.name`.
  """

  @spec db_sql_table :: :"db.sql.table"
  def db_sql_table do
    :"db.sql.table"
  end

  @deprecated """
  Replaced by `db.query.text`.
  """

  @spec db_statement :: :"db.statement"
  def db_statement do
    :"db.statement"
  end

  @typedoc """
  The database management system (DBMS) product as identified by the client instrumentation.

  ### Options
  * `:other_sql` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Some other SQL database. Fallback only. See notes.
  * `:mssql` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Microsoft SQL Server
  * `:mssqlcompact` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Microsoft SQL Server Compact
  * `:mysql` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - MySQL
  * `:oracle` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Oracle Database
  * `:db2` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IBM Db2
  * `:postgresql` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - PostgreSQL
  * `:redshift` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Amazon Redshift
  * `:hive` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Hive
  * `:cloudscape` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Cloudscape
  * `:hsqldb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HyperSQL DataBase
  * `:progress` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Progress Database
  * `:maxdb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - SAP MaxDB
  * `:hanadb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - SAP HANA
  * `:ingres` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Ingres
  * `:firstsql` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - FirstSQL
  * `:edb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EnterpriseDB
  * `:cache` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - InterSystems Caché
  * `:adabas` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Adabas (Adaptable Database System)
  * `:firebird` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Firebird
  * `:derby` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Derby
  * `:filemaker` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - FileMaker
  * `:informix` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Informix
  * `:instantdb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - InstantDB
  * `:interbase` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - InterBase
  * `:mariadb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - MariaDB
  * `:netezza` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Netezza
  * `:pervasive` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Pervasive PSQL
  * `:pointbase` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - PointBase
  * `:sqlite` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - SQLite
  * `:sybase` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Sybase
  * `:teradata` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Teradata
  * `:vertica` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Vertica
  * `:h2` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - H2
  * `:coldfusion` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ColdFusion IMQ
  * `:cassandra` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Cassandra
  * `:hbase` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache HBase
  * `:mongodb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - MongoDB
  * `:redis` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Redis
  * `:couchbase` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Couchbase
  * `:couchdb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - CouchDB
  * `:cosmosdb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Microsoft Azure Cosmos DB
  * `:dynamodb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Amazon DynamoDB
  * `:neo4j` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Neo4j
  * `:geode` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Geode
  * `:elasticsearch` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Elasticsearch
  * `:memcached` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Memcached
  * `:cockroachdb` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - CockroachDB
  * `:opensearch` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - OpenSearch
  * `:clickhouse` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ClickHouse
  * `:spanner` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Cloud Spanner
  * `:trino` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Trino

  """
  @type db_system() ::
          :other_sql
          | :mssql
          | :mssqlcompact
          | :mysql
          | :oracle
          | :db2
          | :postgresql
          | :redshift
          | :hive
          | :cloudscape
          | :hsqldb
          | :progress
          | :maxdb
          | :hanadb
          | :ingres
          | :firstsql
          | :edb
          | :cache
          | :adabas
          | :firebird
          | :derby
          | :filemaker
          | :informix
          | :instantdb
          | :interbase
          | :mariadb
          | :netezza
          | :pervasive
          | :pointbase
          | :sqlite
          | :sybase
          | :teradata
          | :vertica
          | :h2
          | :coldfusion
          | :cassandra
          | :hbase
          | :mongodb
          | :redis
          | :couchbase
          | :couchdb
          | :cosmosdb
          | :dynamodb
          | :neo4j
          | :geode
          | :elasticsearch
          | :memcached
          | :cockroachdb
          | :opensearch
          | :clickhouse
          | :spanner
          | :trino
          | atom()

  @doc """
  The database management system (DBMS) product as identified by the client instrumentation.
  ### Notes

  The actual DBMS may differ from the one identified by the client. For example, when using PostgreSQL client libraries to connect to a CockroachDB, the `db.system` is set to `postgresql` based on the instrumentation's best knowledge.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_system(:other_sql)
      :other_sql
      
      iex> OpenTelemetry.SemanticConventions.DbAttributes.db_system(:custom_value)
      :custom_value
  """
  @spec db_system(db_system()) ::
          :other_sql
          | :mssql
          | :mssqlcompact
          | :mysql
          | :oracle
          | :db2
          | :postgresql
          | :redshift
          | :hive
          | :cloudscape
          | :hsqldb
          | :progress
          | :maxdb
          | :hanadb
          | :ingres
          | :firstsql
          | :edb
          | :cache
          | :adabas
          | :firebird
          | :derby
          | :filemaker
          | :informix
          | :instantdb
          | :interbase
          | :mariadb
          | :netezza
          | :pervasive
          | :pointbase
          | :sqlite
          | :sybase
          | :teradata
          | :vertica
          | :h2
          | :coldfusion
          | :cassandra
          | :hbase
          | :mongodb
          | :redis
          | :couchbase
          | :couchdb
          | :cosmosdb
          | :dynamodb
          | :neo4j
          | :geode
          | :elasticsearch
          | :memcached
          | :cockroachdb
          | :opensearch
          | :clickhouse
          | :spanner
          | :trino
          | atom()
  def db_system(option) do
    :"db.system"

    case option do
      :other_sql -> :other_sql
      :mssql -> :mssql
      :mssqlcompact -> :mssqlcompact
      :mysql -> :mysql
      :oracle -> :oracle
      :db2 -> :db2
      :postgresql -> :postgresql
      :redshift -> :redshift
      :hive -> :hive
      :cloudscape -> :cloudscape
      :hsqldb -> :hsqldb
      :progress -> :progress
      :maxdb -> :maxdb
      :hanadb -> :hanadb
      :ingres -> :ingres
      :firstsql -> :firstsql
      :edb -> :edb
      :cache -> :cache
      :adabas -> :adabas
      :firebird -> :firebird
      :derby -> :derby
      :filemaker -> :filemaker
      :informix -> :informix
      :instantdb -> :instantdb
      :interbase -> :interbase
      :mariadb -> :mariadb
      :netezza -> :netezza
      :pervasive -> :pervasive
      :pointbase -> :pointbase
      :sqlite -> :sqlite
      :sybase -> :sybase
      :teradata -> :teradata
      :vertica -> :vertica
      :h2 -> :h2
      :coldfusion -> :coldfusion
      :cassandra -> :cassandra
      :hbase -> :hbase
      :mongodb -> :mongodb
      :redis -> :redis
      :couchbase -> :couchbase
      :couchdb -> :couchdb
      :cosmosdb -> :cosmosdb
      :dynamodb -> :dynamodb
      :neo4j -> :neo4j
      :geode -> :geode
      :elasticsearch -> :elasticsearch
      :memcached -> :memcached
      :cockroachdb -> :cockroachdb
      :opensearch -> :opensearch
      :clickhouse -> :clickhouse
      :spanner -> :spanner
      :trino -> :trino
      _ -> option
    end
  end

  @deprecated """
  No replacement at this time.
  """

  @spec db_user :: :"db.user"
  def db_user do
    :"db.user"
  end

  @deprecated """
  Replaced by `db.client.connections.pool.name`.
  """

  @spec pool_name :: :"pool.name"
  def pool_name do
    :"pool.name"
  end

  @typedoc """
  Deprecated, use `db.client.connections.state` instead.

  ### Options
  * `:idle` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:used` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type state() :: :idle | :used | atom()

  @deprecated """
  Replaced by `db.client.connections.state`.
  """

  @spec state(state()) :: :idle | :used | atom()
  def state(option) do
    :state

    case option do
      :idle -> :idle
      :used -> :used
      _ -> option
    end
  end
end
