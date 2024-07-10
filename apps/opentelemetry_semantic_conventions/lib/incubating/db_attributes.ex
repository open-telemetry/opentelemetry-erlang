defmodule OpenTelemetry.SemConv.Incubating.DBAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for DB attributes.
  """

  @typedoc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html).


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
  @type db_cassandra_consistencylevel() :: %{
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
  @doc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html).



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_consistencylevel().all
      :all
      
      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_consistencylevel(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'db_cassandra_consistencylevel.all'.
  all

  ?db_cassandra_consistencylevel(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec db_cassandra_consistencylevel() :: db_cassandra_consistencylevel()
  def db_cassandra_consistencylevel() do
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

  @spec db_cassandra_consistencylevel(atom() | String.t()) :: atom() | String.t()
  def db_cassandra_consistencylevel(custom_value) do
    custom_value
  end

  @doc """
  The data center of the coordinating node for a query.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  us-west-2
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_coordinator_dc()
      :"db.cassandra.coordinator.dc"

  ### Erlang

  ```erlang
  ?DB_CASSANDRA_COORDINATOR_DC.
  'db.cassandra.coordinator.dc'
  ```

  <!-- tabs-close -->
  """
  @spec db_cassandra_coordinator_dc :: :"db.cassandra.coordinator.dc"
  def db_cassandra_coordinator_dc do
    :"db.cassandra.coordinator.dc"
  end

  @doc """
  The ID of the coordinating node for a query.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  be13faa2-8574-4d71-926d-27f16cf8a7af
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_coordinator_id()
      :"db.cassandra.coordinator.id"

  ### Erlang

  ```erlang
  ?DB_CASSANDRA_COORDINATOR_ID.
  'db.cassandra.coordinator.id'
  ```

  <!-- tabs-close -->
  """
  @spec db_cassandra_coordinator_id :: :"db.cassandra.coordinator.id"
  def db_cassandra_coordinator_id do
    :"db.cassandra.coordinator.id"
  end

  @doc """
  Whether or not the query is idempotent.

  ### Value type

  Value must be of type `boolean()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_idempotence()
      :"db.cassandra.idempotence"

  ### Erlang

  ```erlang
  ?DB_CASSANDRA_IDEMPOTENCE.
  'db.cassandra.idempotence'
  ```

  <!-- tabs-close -->
  """
  @spec db_cassandra_idempotence :: :"db.cassandra.idempotence"
  def db_cassandra_idempotence do
    :"db.cassandra.idempotence"
  end

  @doc """
  The fetch size used for paging, i.e. how many rows will be returned at once.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [5000]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_pagesize()
      :"db.cassandra.page_size"

  ### Erlang

  ```erlang
  ?DB_CASSANDRA_PAGESIZE.
  'db.cassandra.page_size'
  ```

  <!-- tabs-close -->
  """
  @spec db_cassandra_pagesize :: :"db.cassandra.page_size"
  def db_cassandra_pagesize do
    :"db.cassandra.page_size"
  end

  @doc """
  The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [0, 2]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_speculativeexecutioncount()
      :"db.cassandra.speculative_execution_count"

  ### Erlang

  ```erlang
  ?DB_CASSANDRA_SPECULATIVEEXECUTIONCOUNT.
  'db.cassandra.speculative_execution_count'
  ```

  <!-- tabs-close -->
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

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["myDataSource"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_client_connections_pool_name()
      :"db.client.connections.pool.name"

  ### Erlang

  ```erlang
  ?DB_CLIENT_CONNECTIONS_POOL_NAME.
  'db.client.connections.pool.name'
  ```

  <!-- tabs-close -->
  """
  @spec db_client_connections_pool_name :: :"db.client.connections.pool.name"
  def db_client_connections_pool_name do
    :"db.client.connections.pool.name"
  end

  @typedoc """
  The state of a connection in the pool

  ### Enum Values
  * `:idle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:used` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type db_client_connections_state() :: %{
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

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_client_connections_state().idle
      :idle
      
      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_client_connections_state(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'db_client_connections_state.idle'.
  idle

  ?db_client_connections_state(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec db_client_connections_state() :: db_client_connections_state()
  def db_client_connections_state() do
    %{
      :idle => :idle,
      :used => :used
    }
  end

  @spec db_client_connections_state(atom() | String.t()) :: atom() | String.t()
  def db_client_connections_state(custom_value) do
    custom_value
  end

  @doc """
  The name of a collection (table, container) within the database.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  If the collection name is parsed from the query, it **SHOULD** match the value provided in the query and may be qualified with the schema and database name.
  It is RECOMMENDED to capture the value as provided by the application without attempting to do any case normalization.

  ### Examples

  ```
  ["public.users", "customers"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_collection_name()
      :"db.collection.name"

  ### Erlang

  ```erlang
  ?DB_COLLECTION_NAME.
  'db.collection.name'
  ```

  <!-- tabs-close -->
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
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  3ba4827d-4422-483f-b59f-85b74211c11d
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_clientid()
      :"db.cosmosdb.client_id"

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_CLIENTID.
  'db.cosmosdb.client_id'
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_clientid :: :"db.cosmosdb.client_id"
  def db_cosmosdb_clientid do
    :"db.cosmosdb.client_id"
  end

  @typedoc """
  Cosmos client connection mode.

  ### Enum Values
  * `:gateway` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Gateway (HTTP) connections mode
  * `:direct` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Direct connection.
  """
  @type db_cosmosdb_connectionmode() :: %{
          :gateway => :gateway,
          :direct => :direct
        }
  @doc """
  Cosmos client connection mode.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_connectionmode().gateway
      :gateway
      
      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_connectionmode(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'db_cosmosdb_connectionmode.gateway'.
  gateway

  ?db_cosmosdb_connectionmode(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_connectionmode() :: db_cosmosdb_connectionmode()
  def db_cosmosdb_connectionmode() do
    %{
      :gateway => :gateway,
      :direct => :direct
    }
  end

  @spec db_cosmosdb_connectionmode(atom() | String.t()) :: atom() | String.t()
  def db_cosmosdb_connectionmode(custom_value) do
    custom_value
  end

  @deprecated """
  Replaced by `db.collection.name`.
  """
  @spec db_cosmosdb_container :: :"db.cosmosdb.container"
  def db_cosmosdb_container do
    :"db.cosmosdb.container"
  end

  @typedoc """
  CosmosDB Operation Type.

  ### Enum Values
  * `:invalid` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:create` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:patch` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:read` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:read_feed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:delete` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:replace` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:execute` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:query` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:head` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:head_feed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:upsert` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:batch` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:query_plan` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:execute_javascript` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type db_cosmosdb_operationtype() :: %{
          :invalid => :Invalid,
          :create => :Create,
          :patch => :Patch,
          :read => :Read,
          :read_feed => :ReadFeed,
          :delete => :Delete,
          :replace => :Replace,
          :execute => :Execute,
          :query => :Query,
          :head => :Head,
          :head_feed => :HeadFeed,
          :upsert => :Upsert,
          :batch => :Batch,
          :query_plan => :QueryPlan,
          :execute_javascript => :ExecuteJavaScript
        }
  @doc """
  CosmosDB Operation Type.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_operationtype().invalid
      :Invalid
      
      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_operationtype(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'db_cosmosdb_operationtype.invalid'.
  Invalid

  ?db_cosmosdb_operationtype(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_operationtype() :: db_cosmosdb_operationtype()
  def db_cosmosdb_operationtype() do
    %{
      :invalid => :Invalid,
      :create => :Create,
      :patch => :Patch,
      :read => :Read,
      :read_feed => :ReadFeed,
      :delete => :Delete,
      :replace => :Replace,
      :execute => :Execute,
      :query => :Query,
      :head => :Head,
      :head_feed => :HeadFeed,
      :upsert => :Upsert,
      :batch => :Batch,
      :query_plan => :QueryPlan,
      :execute_javascript => :ExecuteJavaScript
    }
  end

  @spec db_cosmosdb_operationtype(atom() | String.t()) :: atom() | String.t()
  def db_cosmosdb_operationtype(custom_value) do
    custom_value
  end

  @doc """
  RU consumed for that operation
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [46.18, 1.0]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_requestcharge()
      :"db.cosmosdb.request_charge"

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_REQUESTCHARGE.
  'db.cosmosdb.request_charge'
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_requestcharge :: :"db.cosmosdb.request_charge"
  def db_cosmosdb_requestcharge do
    :"db.cosmosdb.request_charge"
  end

  @doc """
  Request payload size in bytes
  ### Value type

  Value must be of type `integer()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_requestcontentlength()
      :"db.cosmosdb.request_content_length"

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_REQUESTCONTENTLENGTH.
  'db.cosmosdb.request_content_length'
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_requestcontentlength :: :"db.cosmosdb.request_content_length"
  def db_cosmosdb_requestcontentlength do
    :"db.cosmosdb.request_content_length"
  end

  @doc """
  Cosmos DB status code.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [200, 201]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_statuscode()
      :"db.cosmosdb.status_code"

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_STATUSCODE.
  'db.cosmosdb.status_code'
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_statuscode :: :"db.cosmosdb.status_code"
  def db_cosmosdb_statuscode do
    :"db.cosmosdb.status_code"
  end

  @doc """
  Cosmos DB sub status code.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [1000, 1002]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_substatuscode()
      :"db.cosmosdb.sub_status_code"

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_SUBSTATUSCODE.
  'db.cosmosdb.sub_status_code'
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_substatuscode :: :"db.cosmosdb.sub_status_code"
  def db_cosmosdb_substatuscode do
    :"db.cosmosdb.sub_status_code"
  end

  @doc """
  Represents the identifier of an Elasticsearch cluster.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["e9106fc68e3044f0b1475b04bf4ffd5f"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_elasticsearch_cluster_name()
      :"db.elasticsearch.cluster.name"

  ### Erlang

  ```erlang
  ?DB_ELASTICSEARCH_CLUSTER_NAME.
  'db.elasticsearch.cluster.name'
  ```

  <!-- tabs-close -->
  """
  @spec db_elasticsearch_cluster_name :: :"db.elasticsearch.cluster.name"
  def db_elasticsearch_cluster_name do
    :"db.elasticsearch.cluster.name"
  end

  @doc """
  Represents the human-readable identifier of the node/instance to which a request was routed.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["instance-0000000001"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_elasticsearch_node_name()
      :"db.elasticsearch.node.name"

  ### Erlang

  ```erlang
  ?DB_ELASTICSEARCH_NODE_NAME.
  'db.elasticsearch.node.name'
  ```

  <!-- tabs-close -->
  """
  @spec db_elasticsearch_node_name :: :"db.elasticsearch.node.name"
  def db_elasticsearch_node_name do
    :"db.elasticsearch.node.name"
  end

  @doc """
  A dynamic value in the url path.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Many Elasticsearch url paths allow dynamic values. These **SHOULD** be recorded in span attributes in the format `db.elasticsearch.path_parts.<key>`, where `<key>` is the url path part name. The implementation **SHOULD** reference the [elasticsearch schema](https://raw.githubusercontent.com/elastic/elasticsearch-specification/main/output/schema/schema.json) in order to map the path part values to their names.

  ### Examples

  ```
  ["db.elasticsearch.path_parts.index=test-index", "db.elasticsearch.path_parts.doc_id=123"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_elasticsearch_pathparts()
      :"db.elasticsearch.path_parts"

  ### Erlang

  ```erlang
  ?DB_ELASTICSEARCH_PATHPARTS.
  'db.elasticsearch.path_parts'
  ```

  <!-- tabs-close -->
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

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  If a database system has multiple namespace components, they **SHOULD** be concatenated (potentially using database system specific conventions) from most general to most specific namespace component, and more specific namespaces **SHOULD** **NOT** be captured without the more general namespaces, to ensure that "startswith" queries for the more general namespaces will be valid.
  Semantic conventions for individual database systems **SHOULD** document what `db.namespace` means in the context of that system.
  It is RECOMMENDED to capture the value as provided by the application without attempting to do any case normalization.

  ### Examples

  ```
  ["customers", "test.users"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_namespace()
      :"db.namespace"

  ### Erlang

  ```erlang
  ?DB_NAMESPACE.
  'db.namespace'
  ```

  <!-- tabs-close -->
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

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  It is RECOMMENDED to capture the value as provided by the application without attempting to do any case normalization.

  ### Examples

  ```
  ["findAndModify", "HMSET", "SELECT"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_operation_name()
      :"db.operation.name"

  ### Erlang

  ```erlang
  ?DB_OPERATION_NAME.
  'db.operation.name'
  ```

  <!-- tabs-close -->
  """
  @spec db_operation_name :: :"db.operation.name"
  def db_operation_name do
    :"db.operation.name"
  end

  @doc """
  The query parameters used in `db.query.text`, with `<key>` being the parameter name, and the attribute value being the parameter value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Query parameters should only be captured when `db.query.text` is parameterized with placeholders.
  If a parameter has no name and instead is referenced only by index, then `<key>` **SHOULD** be the 0-based index.

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

  @doc """
  The database query being executed.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["SELECT * FROM wuser_table where username = ?", "SET mykey \"WuValue\""]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_query_text()
      :"db.query.text"

  ### Erlang

  ```erlang
  ?DB_QUERY_TEXT.
  'db.query.text'
  ```

  <!-- tabs-close -->
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

  ### Enum Values
  * `:other_sql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Some other SQL database. Fallback only. See notes.
  * `:mssql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Microsoft SQL Server
  * `:mssqlcompact` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Microsoft SQL Server Compact
  * `:mysql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - MySQL
  * `:oracle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Oracle Database
  * `:db2` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IBM Db2
  * `:postgresql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - PostgreSQL
  * `:redshift` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Amazon Redshift
  * `:hive` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Hive
  * `:cloudscape` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Cloudscape
  * `:hsqldb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HyperSQL DataBase
  * `:progress` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Progress Database
  * `:maxdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - SAP MaxDB
  * `:hanadb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - SAP HANA
  * `:ingres` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Ingres
  * `:firstsql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - FirstSQL
  * `:edb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - EnterpriseDB
  * `:cache` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - InterSystems Caché
  * `:adabas` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Adabas (Adaptable Database System)
  * `:firebird` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Firebird
  * `:derby` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Derby
  * `:filemaker` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - FileMaker
  * `:informix` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Informix
  * `:instantdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - InstantDB
  * `:interbase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - InterBase
  * `:mariadb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - MariaDB
  * `:netezza` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Netezza
  * `:pervasive` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Pervasive PSQL
  * `:pointbase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - PointBase
  * `:sqlite` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - SQLite
  * `:sybase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Sybase
  * `:teradata` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Teradata
  * `:vertica` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Vertica
  * `:h2` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - H2
  * `:coldfusion` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - ColdFusion IMQ
  * `:cassandra` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Cassandra
  * `:hbase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache HBase
  * `:mongodb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - MongoDB
  * `:redis` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Redis
  * `:couchbase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Couchbase
  * `:couchdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - CouchDB
  * `:cosmosdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Microsoft Azure Cosmos DB
  * `:dynamodb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Amazon DynamoDB
  * `:neo4j` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Neo4j
  * `:geode` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Geode
  * `:elasticsearch` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Elasticsearch
  * `:memcached` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Memcached
  * `:cockroachdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - CockroachDB
  * `:opensearch` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OpenSearch
  * `:clickhouse` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - ClickHouse
  * `:spanner` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Cloud Spanner
  * `:trino` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Trino
  """
  @type db_system() :: %{
          :other_sql => :other_sql,
          :mssql => :mssql,
          :mssqlcompact => :mssqlcompact,
          :mysql => :mysql,
          :oracle => :oracle,
          :db2 => :db2,
          :postgresql => :postgresql,
          :redshift => :redshift,
          :hive => :hive,
          :cloudscape => :cloudscape,
          :hsqldb => :hsqldb,
          :progress => :progress,
          :maxdb => :maxdb,
          :hanadb => :hanadb,
          :ingres => :ingres,
          :firstsql => :firstsql,
          :edb => :edb,
          :cache => :cache,
          :adabas => :adabas,
          :firebird => :firebird,
          :derby => :derby,
          :filemaker => :filemaker,
          :informix => :informix,
          :instantdb => :instantdb,
          :interbase => :interbase,
          :mariadb => :mariadb,
          :netezza => :netezza,
          :pervasive => :pervasive,
          :pointbase => :pointbase,
          :sqlite => :sqlite,
          :sybase => :sybase,
          :teradata => :teradata,
          :vertica => :vertica,
          :h2 => :h2,
          :coldfusion => :coldfusion,
          :cassandra => :cassandra,
          :hbase => :hbase,
          :mongodb => :mongodb,
          :redis => :redis,
          :couchbase => :couchbase,
          :couchdb => :couchdb,
          :cosmosdb => :cosmosdb,
          :dynamodb => :dynamodb,
          :neo4j => :neo4j,
          :geode => :geode,
          :elasticsearch => :elasticsearch,
          :memcached => :memcached,
          :cockroachdb => :cockroachdb,
          :opensearch => :opensearch,
          :clickhouse => :clickhouse,
          :spanner => :spanner,
          :trino => :trino
        }
  @doc """
  The database management system (DBMS) product as identified by the client instrumentation.

  ### Notes

  The actual DBMS may differ from the one identified by the client. For example, when using PostgreSQL client libraries to connect to a CockroachDB, the `db.system` is set to `postgresql` based on the instrumentation's best knowledge.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_system().other_sql
      :other_sql
      
      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_system(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'db_system.other_sql'.
  other_sql

  ?db_system(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec db_system() :: db_system()
  def db_system() do
    %{
      :other_sql => :other_sql,
      :mssql => :mssql,
      :mssqlcompact => :mssqlcompact,
      :mysql => :mysql,
      :oracle => :oracle,
      :db2 => :db2,
      :postgresql => :postgresql,
      :redshift => :redshift,
      :hive => :hive,
      :cloudscape => :cloudscape,
      :hsqldb => :hsqldb,
      :progress => :progress,
      :maxdb => :maxdb,
      :hanadb => :hanadb,
      :ingres => :ingres,
      :firstsql => :firstsql,
      :edb => :edb,
      :cache => :cache,
      :adabas => :adabas,
      :firebird => :firebird,
      :derby => :derby,
      :filemaker => :filemaker,
      :informix => :informix,
      :instantdb => :instantdb,
      :interbase => :interbase,
      :mariadb => :mariadb,
      :netezza => :netezza,
      :pervasive => :pervasive,
      :pointbase => :pointbase,
      :sqlite => :sqlite,
      :sybase => :sybase,
      :teradata => :teradata,
      :vertica => :vertica,
      :h2 => :h2,
      :coldfusion => :coldfusion,
      :cassandra => :cassandra,
      :hbase => :hbase,
      :mongodb => :mongodb,
      :redis => :redis,
      :couchbase => :couchbase,
      :couchdb => :couchdb,
      :cosmosdb => :cosmosdb,
      :dynamodb => :dynamodb,
      :neo4j => :neo4j,
      :geode => :geode,
      :elasticsearch => :elasticsearch,
      :memcached => :memcached,
      :cockroachdb => :cockroachdb,
      :opensearch => :opensearch,
      :clickhouse => :clickhouse,
      :spanner => :spanner,
      :trino => :trino
    }
  end

  @spec db_system(atom() | String.t()) :: atom() | String.t()
  def db_system(custom_value) do
    custom_value
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

  ### Enum Values
  * `:idle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:used` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type state() :: %{
          :idle => :idle,
          :used => :used
        }
  @deprecated """
  Replaced by `db.client.connections.state`.
  """
  @spec state() :: state()
  def state() do
    %{
      :idle => :idle,
      :used => :used
    }
  end

  @spec state(atom() | String.t()) :: atom() | String.t()
  def state(custom_value) do
    custom_value
  end
end
