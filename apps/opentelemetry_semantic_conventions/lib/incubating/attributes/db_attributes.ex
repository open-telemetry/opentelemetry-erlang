defmodule OpenTelemetry.SemConv.Incubating.DBAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for DB attributes.
  """
  defdelegate db_collection_name(), to: OpenTelemetry.SemConv.DBAttributes

  defdelegate db_namespace(), to: OpenTelemetry.SemConv.DBAttributes

  defdelegate db_operation_batch_size(), to: OpenTelemetry.SemConv.DBAttributes

  defdelegate db_operation_name(), to: OpenTelemetry.SemConv.DBAttributes

  defdelegate db_query_summary(), to: OpenTelemetry.SemConv.DBAttributes

  defdelegate db_query_text(), to: OpenTelemetry.SemConv.DBAttributes

  defdelegate db_response_status_code(), to: OpenTelemetry.SemConv.DBAttributes

  defdelegate db_stored_procedure_name(), to: OpenTelemetry.SemConv.DBAttributes

  defdelegate db_system_name(), to: OpenTelemetry.SemConv.DBAttributes

  defdelegate db_system_name_values(), to: OpenTelemetry.SemConv.DBAttributes

  @typedoc """
  Deprecated, use `cassandra.consistency.level` instead.

  ### Enum Values
  * `:all` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:each_quorum` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:quorum` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:local_quorum` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:one` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:two` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:three` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:local_one` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:any` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:serial` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:local_serial` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type db_cassandra_consistency_level_values() :: %{
          :all => :all,
          :each_quorum => :each_quorum,
          :quorum => :quorum,
          :local_quorum => :local_quorum,
          :one => :one,
          :two => :two,
          :three => :three,
          :local_one => :local_one,
          :any => :any,
          :serial => :serial,
          :local_serial => :local_serial
        }
  @deprecated """
  Replaced by `cassandra.consistency.level`.
  """
  @spec db_cassandra_consistency_level :: :"db.cassandra.consistency_level"
  def db_cassandra_consistency_level do
    :"db.cassandra.consistency_level"
  end

  @spec db_cassandra_consistency_level_values() :: db_cassandra_consistency_level_values()
  def db_cassandra_consistency_level_values() do
    %{
      :all => :all,
      :each_quorum => :each_quorum,
      :quorum => :quorum,
      :local_quorum => :local_quorum,
      :one => :one,
      :two => :two,
      :three => :three,
      :local_one => :local_one,
      :any => :any,
      :serial => :serial,
      :local_serial => :local_serial
    }
  end

  @deprecated """
  Replaced by `cassandra.coordinator.dc`.
  """
  @spec db_cassandra_coordinator_dc :: :"db.cassandra.coordinator.dc"
  def db_cassandra_coordinator_dc do
    :"db.cassandra.coordinator.dc"
  end

  @deprecated """
  Replaced by `cassandra.coordinator.id`.
  """
  @spec db_cassandra_coordinator_id :: :"db.cassandra.coordinator.id"
  def db_cassandra_coordinator_id do
    :"db.cassandra.coordinator.id"
  end

  @deprecated """
  Replaced by `cassandra.query.idempotent`.
  """
  @spec db_cassandra_idempotence :: :"db.cassandra.idempotence"
  def db_cassandra_idempotence do
    :"db.cassandra.idempotence"
  end

  @deprecated """
  Replaced by `cassandra.page.size`.
  """
  @spec db_cassandra_page_size :: :"db.cassandra.page_size"
  def db_cassandra_page_size do
    :"db.cassandra.page_size"
  end

  @deprecated """
  Replaced by `cassandra.speculative_execution.count`.
  """
  @spec db_cassandra_speculative_execution_count :: :"db.cassandra.speculative_execution_count"
  def db_cassandra_speculative_execution_count do
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
  The name of the connection pool; unique within the instrumented application. In case the connection pool implementation doesn't provide a name, instrumentation **SHOULD** use a combination of parameters that would make the name unique, for example, combining attributes `server.address`, `server.port`, and `db.namespace`, formatted as `server.address:server.port/db.namespace`. Instrumentations that generate connection pool name following different patterns **SHOULD** document it.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["myDataSource"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_client_connection_pool_name()
      :"db.client.connection.pool.name"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_POOL_NAME.
  'db.client.connection.pool.name'
  ```

  <!-- tabs-close -->
  """
  @spec db_client_connection_pool_name :: :"db.client.connection.pool.name"
  def db_client_connection_pool_name do
    :"db.client.connection.pool.name"
  end

  @typedoc """
  The state of a connection in the pool

  ### Enum Values
  * `:idle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:used` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type db_client_connection_state_values() :: %{
          :idle => :idle,
          :used => :used
        }
  @doc """
  The state of a connection in the pool

  ### Examples

  ```
  ["idle"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_client_connection_state()
      :"db.client.connection.state"

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_client_connection_state_values().idle
      :idle

      iex> %{OpenTelemetry.SemConv.Incubating.DBAttributes.db_client_connection_state() => OpenTelemetry.SemConv.Incubating.DBAttributes.db_client_connection_state_values().idle}
      %{:"db.client.connection.state" => :idle}

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTION_STATE.
  'db.client.connection.state'

  ?DB_CLIENT_CONNECTION_STATE_VALUES_IDLE.
  'idle'

  \#{?DB_CLIENT_CONNECTION_STATE => ?DB_CLIENT_CONNECTION_STATE_VALUES_IDLE}.
  \#{'db.client.connection.state' => 'idle'}
  ```

  <!-- tabs-close -->
  """
  @spec db_client_connection_state :: :"db.client.connection.state"
  def db_client_connection_state do
    :"db.client.connection.state"
  end

  @spec db_client_connection_state_values() :: db_client_connection_state_values()
  def db_client_connection_state_values() do
    %{
      :idle => :idle,
      :used => :used
    }
  end

  @deprecated """
  Replaced by `db.client.connection.pool.name`.
  """
  @spec db_client_connections_pool_name :: :"db.client.connections.pool.name"
  def db_client_connections_pool_name do
    :"db.client.connections.pool.name"
  end

  @typedoc """
  Deprecated, use `db.client.connection.state` instead.

  ### Enum Values
  * `:idle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:used` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type db_client_connections_state_values() :: %{
          :idle => :idle,
          :used => :used
        }
  @deprecated """
  Replaced by `db.client.connection.state`.
  """
  @spec db_client_connections_state :: :"db.client.connections.state"
  def db_client_connections_state do
    :"db.client.connections.state"
  end

  @spec db_client_connections_state_values() :: db_client_connections_state_values()
  def db_client_connections_state_values() do
    %{
      :idle => :idle,
      :used => :used
    }
  end

  @deprecated """
  Replaced by `server.address` and `server.port`.
  """
  @spec db_connection_string :: :"db.connection_string"
  def db_connection_string do
    :"db.connection_string"
  end

  @deprecated """
  Replaced by `azure.client.id`.
  """
  @spec db_cosmosdb_client_id :: :"db.cosmosdb.client_id"
  def db_cosmosdb_client_id do
    :"db.cosmosdb.client_id"
  end

  @typedoc """
  Deprecated, use `azure.cosmosdb.connection.mode` instead.

  ### Enum Values
  * `:gateway` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Gateway (HTTP) connection.
  * `:direct` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Direct connection.
  """
  @type db_cosmosdb_connection_mode_values() :: %{
          :gateway => :gateway,
          :direct => :direct
        }
  @deprecated """
  Replaced by `azure.cosmosdb.connection.mode`.
  """
  @spec db_cosmosdb_connection_mode :: :"db.cosmosdb.connection_mode"
  def db_cosmosdb_connection_mode do
    :"db.cosmosdb.connection_mode"
  end

  @spec db_cosmosdb_connection_mode_values() :: db_cosmosdb_connection_mode_values()
  def db_cosmosdb_connection_mode_values() do
    %{
      :gateway => :gateway,
      :direct => :direct
    }
  end

  @typedoc """
  Deprecated, use `cosmosdb.consistency.level` instead.

  ### Enum Values
  * `:strong` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:bounded_staleness` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:session` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:eventual` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:consistent_prefix` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type db_cosmosdb_consistency_level_values() :: %{
          :strong => :Strong,
          :bounded_staleness => :BoundedStaleness,
          :session => :Session,
          :eventual => :Eventual,
          :consistent_prefix => :ConsistentPrefix
        }
  @deprecated """
  Replaced by `azure.cosmosdb.consistency.level`.
  """
  @spec db_cosmosdb_consistency_level :: :"db.cosmosdb.consistency_level"
  def db_cosmosdb_consistency_level do
    :"db.cosmosdb.consistency_level"
  end

  @spec db_cosmosdb_consistency_level_values() :: db_cosmosdb_consistency_level_values()
  def db_cosmosdb_consistency_level_values() do
    %{
      :strong => :Strong,
      :bounded_staleness => :BoundedStaleness,
      :session => :Session,
      :eventual => :Eventual,
      :consistent_prefix => :ConsistentPrefix
    }
  end

  @deprecated """
  Replaced by `db.collection.name`.
  """
  @spec db_cosmosdb_container :: :"db.cosmosdb.container"
  def db_cosmosdb_container do
    :"db.cosmosdb.container"
  end

  @typedoc """
  Deprecated, no replacement at this time.

  ### Enum Values
  * `:batch` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:create` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:delete` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:execute` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:execute_javascript` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:invalid` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:head` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:head_feed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:patch` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:query` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:query_plan` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:read` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:read_feed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:replace` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:upsert` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type db_cosmosdb_operation_type_values() :: %{
          :batch => :batch,
          :create => :create,
          :delete => :delete,
          :execute => :execute,
          :execute_javascript => :execute_javascript,
          :invalid => :invalid,
          :head => :head,
          :head_feed => :head_feed,
          :patch => :patch,
          :query => :query,
          :query_plan => :query_plan,
          :read => :read,
          :read_feed => :read_feed,
          :replace => :replace,
          :upsert => :upsert
        }
  @deprecated """
  No replacement at this time.
  """
  @spec db_cosmosdb_operation_type :: :"db.cosmosdb.operation_type"
  def db_cosmosdb_operation_type do
    :"db.cosmosdb.operation_type"
  end

  @spec db_cosmosdb_operation_type_values() :: db_cosmosdb_operation_type_values()
  def db_cosmosdb_operation_type_values() do
    %{
      :batch => :batch,
      :create => :create,
      :delete => :delete,
      :execute => :execute,
      :execute_javascript => :execute_javascript,
      :invalid => :invalid,
      :head => :head,
      :head_feed => :head_feed,
      :patch => :patch,
      :query => :query,
      :query_plan => :query_plan,
      :read => :read,
      :read_feed => :read_feed,
      :replace => :replace,
      :upsert => :upsert
    }
  end

  @deprecated """
  Replaced by `azure.cosmosdb.operation.contacted_regions`.
  """
  @spec db_cosmosdb_regions_contacted :: :"db.cosmosdb.regions_contacted"
  def db_cosmosdb_regions_contacted do
    :"db.cosmosdb.regions_contacted"
  end

  @deprecated """
  Replaced by `azure.cosmosdb.operation.request_charge`.
  """
  @spec db_cosmosdb_request_charge :: :"db.cosmosdb.request_charge"
  def db_cosmosdb_request_charge do
    :"db.cosmosdb.request_charge"
  end

  @deprecated """
  Replaced by `azure.cosmosdb.request.body.size`.
  """
  @spec db_cosmosdb_request_content_length :: :"db.cosmosdb.request_content_length"
  def db_cosmosdb_request_content_length do
    :"db.cosmosdb.request_content_length"
  end

  @deprecated """
  Replaced by `db.response.status_code`.
  """
  @spec db_cosmosdb_status_code :: :"db.cosmosdb.status_code"
  def db_cosmosdb_status_code do
    :"db.cosmosdb.status_code"
  end

  @deprecated """
  Replaced by `azure.cosmosdb.response.sub_status_code`.
  """
  @spec db_cosmosdb_sub_status_code :: :"db.cosmosdb.sub_status_code"
  def db_cosmosdb_sub_status_code do
    :"db.cosmosdb.sub_status_code"
  end

  @deprecated """
  Replaced by `db.namespace`.
  """
  @spec db_elasticsearch_cluster_name :: :"db.elasticsearch.cluster.name"
  def db_elasticsearch_cluster_name do
    :"db.elasticsearch.cluster.name"
  end

  @deprecated """
  Replaced by `elasticsearch.node.name`.
  """
  @spec db_elasticsearch_node_name :: :"db.elasticsearch.node.name"
  def db_elasticsearch_node_name do
    :"db.elasticsearch.node.name"
  end

  @deprecated """
  Replaced by `db.operation.parameter`.
  """
  @spec db_elasticsearch_path_parts :: :"db.elasticsearch.path_parts"
  def db_elasticsearch_path_parts do
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
  @spec db_jdbc_driver_classname :: :"db.jdbc.driver_classname"
  def db_jdbc_driver_classname do
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
  @spec db_mssql_instance_name :: :"db.mssql.instance_name"
  def db_mssql_instance_name do
    :"db.mssql.instance_name"
  end

  @deprecated """
  Replaced by `db.namespace`.
  """
  @spec db_name :: :"db.name"
  def db_name do
    :"db.name"
  end

  @deprecated """
  Replaced by `db.operation.name`.
  """
  @spec db_operation :: :"db.operation"
  def db_operation do
    :"db.operation"
  end

  @doc """
  A database operation parameter, with `<key>` being the parameter name, and the attribute value being a string representation of the parameter value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  For example, a client-side maximum number of rows to read from the database
  **MAY** be recorded as the `db.operation.parameter.max_rows` attribute.

  `db.query.text` parameters **SHOULD** be captured using `db.query.parameter.<key>`
  instead of `db.operation.parameter.<key>`.

  ### Examples

  ```
  ["someval", "55"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_operation_parameter()
      :"db.operation.parameter"

  ### Erlang

  ```erlang
  ?DB_OPERATION_PARAMETER.
  'db.operation.parameter'
  ```

  <!-- tabs-close -->
  """
  @spec db_operation_parameter :: :"db.operation.parameter"
  def db_operation_parameter do
    :"db.operation.parameter"
  end

  @doc """
  A database query parameter, with `<key>` being the parameter name, and the attribute value being a string representation of the parameter value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  If a query parameter has no name and instead is referenced only by index,
  then `<key>` **SHOULD** be the 0-based index.

  `db.query.parameter.<key>` **SHOULD** match
  up with the parameterized placeholders present in `db.query.text`.

  `db.query.parameter.<key>` **SHOULD** **NOT** be captured on batch operations.

  Examples:

  - For a query `SELECT * FROM users where username =  %s` with the parameter `"jdoe"`,
    the attribute `db.query.parameter.0` **SHOULD** be set to `"jdoe"`.

  - For a query `"SELECT * FROM users WHERE username = %(username)s;` with parameter
    `username = "jdoe"`, the attribute `db.query.parameter.username` **SHOULD** be set to `"jdoe"`.

  ### Examples

  ```
  ["someval", "55"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_query_parameter()
      :"db.query.parameter"

  ### Erlang

  ```erlang
  ?DB_QUERY_PARAMETER.
  'db.query.parameter'
  ```

  <!-- tabs-close -->
  """
  @spec db_query_parameter :: :"db.query.parameter"
  def db_query_parameter do
    :"db.query.parameter"
  end

  @deprecated """
  Replaced by `db.namespace`.
  """
  @spec db_redis_database_index :: :"db.redis.database_index"
  def db_redis_database_index do
    :"db.redis.database_index"
  end

  @doc """
  Number of rows returned by the operation.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [10, 30, 1000]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_response_returned_rows()
      :"db.response.returned_rows"

  ### Erlang

  ```erlang
  ?DB_RESPONSE_RETURNED_ROWS.
  'db.response.returned_rows'
  ```

  <!-- tabs-close -->
  """
  @spec db_response_returned_rows :: :"db.response.returned_rows"
  def db_response_returned_rows do
    :"db.response.returned_rows"
  end

  @deprecated """
  Replaced by `db.collection.name`, but only if not extracting the value from `db.query.text`.
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
  Deprecated, use `db.system.name` instead.

  ### Enum Values
  * `:other_sql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Some other SQL database. Fallback only. See notes.
  * `:adabas` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Adabas (Adaptable Database System)
  * `:cache` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Deprecated, use `intersystems_cache` instead.
  * `:intersystems_cache` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - InterSystems Caché
  * `:cassandra` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Cassandra
  * `:clickhouse` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - ClickHouse
  * `:cloudscape` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Deprecated, use `other_sql` instead.
  * `:cockroachdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - CockroachDB
  * `:coldfusion` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Deprecated, no replacement at this time.
  * `:cosmosdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Microsoft Azure Cosmos DB
  * `:couchbase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Couchbase
  * `:couchdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - CouchDB
  * `:db2` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IBM Db2
  * `:derby` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Derby
  * `:dynamodb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Amazon DynamoDB
  * `:edb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - EnterpriseDB
  * `:elasticsearch` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Elasticsearch
  * `:filemaker` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - FileMaker
  * `:firebird` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Firebird
  * `:firstsql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Deprecated, use `other_sql` instead.
  * `:geode` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Geode
  * `:h2` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - H2
  * `:hanadb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - SAP HANA
  * `:hbase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache HBase
  * `:hive` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Hive
  * `:hsqldb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HyperSQL DataBase
  * `:influxdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - InfluxDB
  * `:informix` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Informix
  * `:ingres` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Ingres
  * `:instantdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - InstantDB
  * `:interbase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - InterBase
  * `:mariadb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - MariaDB
  * `:maxdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - SAP MaxDB
  * `:memcached` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Memcached
  * `:mongodb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - MongoDB
  * `:mssql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Microsoft SQL Server
  * `:mssqlcompact` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Deprecated, Microsoft SQL Server Compact is discontinued.
  * `:mysql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - MySQL
  * `:neo4j` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Neo4j
  * `:netezza` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Netezza
  * `:opensearch` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OpenSearch
  * `:oracle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Oracle Database
  * `:pervasive` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Pervasive PSQL
  * `:pointbase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - PointBase
  * `:postgresql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - PostgreSQL
  * `:progress` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Progress Database
  * `:redis` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Redis
  * `:redshift` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Amazon Redshift
  * `:spanner` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Cloud Spanner
  * `:sqlite` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - SQLite
  * `:sybase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Sybase
  * `:teradata` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Teradata
  * `:trino` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Trino
  * `:vertica` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Vertica
  """
  @type db_system_values() :: %{
          :other_sql => :other_sql,
          :adabas => :adabas,
          :cache => :cache,
          :intersystems_cache => :intersystems_cache,
          :cassandra => :cassandra,
          :clickhouse => :clickhouse,
          :cloudscape => :cloudscape,
          :cockroachdb => :cockroachdb,
          :coldfusion => :coldfusion,
          :cosmosdb => :cosmosdb,
          :couchbase => :couchbase,
          :couchdb => :couchdb,
          :db2 => :db2,
          :derby => :derby,
          :dynamodb => :dynamodb,
          :edb => :edb,
          :elasticsearch => :elasticsearch,
          :filemaker => :filemaker,
          :firebird => :firebird,
          :firstsql => :firstsql,
          :geode => :geode,
          :h2 => :h2,
          :hanadb => :hanadb,
          :hbase => :hbase,
          :hive => :hive,
          :hsqldb => :hsqldb,
          :influxdb => :influxdb,
          :informix => :informix,
          :ingres => :ingres,
          :instantdb => :instantdb,
          :interbase => :interbase,
          :mariadb => :mariadb,
          :maxdb => :maxdb,
          :memcached => :memcached,
          :mongodb => :mongodb,
          :mssql => :mssql,
          :mssqlcompact => :mssqlcompact,
          :mysql => :mysql,
          :neo4j => :neo4j,
          :netezza => :netezza,
          :opensearch => :opensearch,
          :oracle => :oracle,
          :pervasive => :pervasive,
          :pointbase => :pointbase,
          :postgresql => :postgresql,
          :progress => :progress,
          :redis => :redis,
          :redshift => :redshift,
          :spanner => :spanner,
          :sqlite => :sqlite,
          :sybase => :sybase,
          :teradata => :teradata,
          :trino => :trino,
          :vertica => :vertica
        }
  @deprecated """
  Replaced by `db.system.name`.
  """
  @spec db_system :: :"db.system"
  def db_system do
    :"db.system"
  end

  @spec db_system_values() :: db_system_values()
  def db_system_values() do
    %{
      :other_sql => :other_sql,
      :adabas => :adabas,
      :cache => :cache,
      :intersystems_cache => :intersystems_cache,
      :cassandra => :cassandra,
      :clickhouse => :clickhouse,
      :cloudscape => :cloudscape,
      :cockroachdb => :cockroachdb,
      :coldfusion => :coldfusion,
      :cosmosdb => :cosmosdb,
      :couchbase => :couchbase,
      :couchdb => :couchdb,
      :db2 => :db2,
      :derby => :derby,
      :dynamodb => :dynamodb,
      :edb => :edb,
      :elasticsearch => :elasticsearch,
      :filemaker => :filemaker,
      :firebird => :firebird,
      :firstsql => :firstsql,
      :geode => :geode,
      :h2 => :h2,
      :hanadb => :hanadb,
      :hbase => :hbase,
      :hive => :hive,
      :hsqldb => :hsqldb,
      :influxdb => :influxdb,
      :informix => :informix,
      :ingres => :ingres,
      :instantdb => :instantdb,
      :interbase => :interbase,
      :mariadb => :mariadb,
      :maxdb => :maxdb,
      :memcached => :memcached,
      :mongodb => :mongodb,
      :mssql => :mssql,
      :mssqlcompact => :mssqlcompact,
      :mysql => :mysql,
      :neo4j => :neo4j,
      :netezza => :netezza,
      :opensearch => :opensearch,
      :oracle => :oracle,
      :pervasive => :pervasive,
      :pointbase => :pointbase,
      :postgresql => :postgresql,
      :progress => :progress,
      :redis => :redis,
      :redshift => :redshift,
      :spanner => :spanner,
      :sqlite => :sqlite,
      :sybase => :sybase,
      :teradata => :teradata,
      :trino => :trino,
      :vertica => :vertica
    }
  end

  @deprecated """
  No replacement at this time.
  """
  @spec db_user :: :"db.user"
  def db_user do
    :"db.user"
  end
end
