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
  @doc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html).



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_consistency_level()
      :"db.cassandra.consistency_level"

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_consistency_level_values().all
      :all

      iex> %{OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_consistency_level() => OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_consistency_level_values().all}
      %{:"db.cassandra.consistency_level" => :all}

  ### Erlang

  ```erlang
  ?DB_CASSANDRA_CONSISTENCY_LEVEL.
  'db.cassandra.consistency_level'

  ?DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_ALL.
  'all'

  \#{?DB_CASSANDRA_CONSISTENCY_LEVEL => ?DB_CASSANDRA_CONSISTENCY_LEVEL_VALUES_ALL}.
  \#{'db.cassandra.consistency_level' => 'all'}
  ```

  <!-- tabs-close -->
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

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_page_size()
      :"db.cassandra.page_size"

  ### Erlang

  ```erlang
  ?DB_CASSANDRA_PAGE_SIZE.
  'db.cassandra.page_size'
  ```

  <!-- tabs-close -->
  """
  @spec db_cassandra_page_size :: :"db.cassandra.page_size"
  def db_cassandra_page_size do
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

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cassandra_speculative_execution_count()
      :"db.cassandra.speculative_execution_count"

  ### Erlang

  ```erlang
  ?DB_CASSANDRA_SPECULATIVE_EXECUTION_COUNT.
  'db.cassandra.speculative_execution_count'
  ```

  <!-- tabs-close -->
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

  @doc """
  The name of a collection (table, container) within the database.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  It is RECOMMENDED to capture the value as provided by the application without attempting to do any case normalization.
  If the collection name is parsed from the query text, it **SHOULD** be the first collection name found in the query and it **SHOULD** match the value provided in the query text including any schema and database name prefix.
  For batch operations, if the individual operations are known to have the same collection name then that collection name **SHOULD** be used, otherwise `db.collection.name` **SHOULD** **NOT** be captured.

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
  @spec db_connection_string :: :"db.connection_string"
  def db_connection_string do
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

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_client_id()
      :"db.cosmosdb.client_id"

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_CLIENT_ID.
  'db.cosmosdb.client_id'
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_client_id :: :"db.cosmosdb.client_id"
  def db_cosmosdb_client_id do
    :"db.cosmosdb.client_id"
  end

  @typedoc """
  Cosmos client connection mode.

  ### Enum Values
  * `:gateway` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Gateway (HTTP) connections mode
  * `:direct` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Direct connection.
  """
  @type db_cosmosdb_connection_mode_values() :: %{
          :gateway => :gateway,
          :direct => :direct
        }
  @doc """
  Cosmos client connection mode.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_connection_mode()
      :"db.cosmosdb.connection_mode"

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_connection_mode_values().gateway
      :gateway

      iex> %{OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_connection_mode() => OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_connection_mode_values().gateway}
      %{:"db.cosmosdb.connection_mode" => :gateway}

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_CONNECTION_MODE.
  'db.cosmosdb.connection_mode'

  ?DB_COSMOSDB_CONNECTION_MODE_VALUES_GATEWAY.
  'gateway'

  \#{?DB_COSMOSDB_CONNECTION_MODE => ?DB_COSMOSDB_CONNECTION_MODE_VALUES_GATEWAY}.
  \#{'db.cosmosdb.connection_mode' => 'gateway'}
  ```

  <!-- tabs-close -->
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
  @type db_cosmosdb_operation_type_values() :: %{
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

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_operation_type()
      :"db.cosmosdb.operation_type"

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_operation_type_values().invalid
      :Invalid

      iex> %{OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_operation_type() => OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_operation_type_values().invalid}
      %{:"db.cosmosdb.operation_type" => :Invalid}

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_OPERATION_TYPE.
  'db.cosmosdb.operation_type'

  ?DB_COSMOSDB_OPERATION_TYPE_VALUES_INVALID.
  'Invalid'

  \#{?DB_COSMOSDB_OPERATION_TYPE => ?DB_COSMOSDB_OPERATION_TYPE_VALUES_INVALID}.
  \#{'db.cosmosdb.operation_type' => 'Invalid'}
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_operation_type :: :"db.cosmosdb.operation_type"
  def db_cosmosdb_operation_type do
    :"db.cosmosdb.operation_type"
  end

  @spec db_cosmosdb_operation_type_values() :: db_cosmosdb_operation_type_values()
  def db_cosmosdb_operation_type_values() do
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

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_request_charge()
      :"db.cosmosdb.request_charge"

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_REQUEST_CHARGE.
  'db.cosmosdb.request_charge'
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_request_charge :: :"db.cosmosdb.request_charge"
  def db_cosmosdb_request_charge do
    :"db.cosmosdb.request_charge"
  end

  @doc """
  Request payload size in bytes
  ### Value type

  Value must be of type `integer()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_request_content_length()
      :"db.cosmosdb.request_content_length"

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_REQUEST_CONTENT_LENGTH.
  'db.cosmosdb.request_content_length'
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_request_content_length :: :"db.cosmosdb.request_content_length"
  def db_cosmosdb_request_content_length do
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

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_status_code()
      :"db.cosmosdb.status_code"

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_STATUS_CODE.
  'db.cosmosdb.status_code'
  ```

  <!-- tabs-close -->
  """
  @spec db_cosmosdb_status_code :: :"db.cosmosdb.status_code"
  def db_cosmosdb_status_code do
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

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_cosmosdb_sub_status_code()
      :"db.cosmosdb.sub_status_code"

  ### Erlang

  ```erlang
  ?DB_COSMOSDB_SUB_STATUS_CODE.
  'db.cosmosdb.sub_status_code'
  ```

  <!-- tabs-close -->
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

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_elasticsearch_path_parts()
      :"db.elasticsearch.path_parts"

  ### Erlang

  ```erlang
  ?DB_ELASTICSEARCH_PATH_PARTS.
  'db.elasticsearch.path_parts'
  ```

  <!-- tabs-close -->
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
  The number of queries included in a [batch operation](/docs/database/database-spans.md#batch-operations).
  ### Value type

  Value must be of type `integer()`.
  ### Notes

  Operations are only considered batches when they contain two or more operations, and so `db.operation.batch.size` **SHOULD** never be `1`.

  ### Examples

  ```
  [2, 3, 4]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_operation_batch_size()
      :"db.operation.batch.size"

  ### Erlang

  ```erlang
  ?DB_OPERATION_BATCH_SIZE.
  'db.operation.batch.size'
  ```

  <!-- tabs-close -->
  """
  @spec db_operation_batch_size :: :"db.operation.batch.size"
  def db_operation_batch_size do
    :"db.operation.batch.size"
  end

  @doc """
  The name of the operation or command being executed.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  It is RECOMMENDED to capture the value as provided by the application without attempting to do any case normalization.
  If the operation name is parsed from the query text, it **SHOULD** be the first operation name found in the query.
  For batch operations, if the individual operations are known to have the same operation name then that operation name **SHOULD** be used prepended by `BATCH `, otherwise `db.operation.name` **SHOULD** be `BATCH` or some other database system specific term if more applicable.

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
  A query parameter used in `db.query.text`, with `<key>` being the parameter name, and the attribute value being a string representation of the parameter value.

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
  ### Notes

  For sanitization see [Sanitization of `db.query.text`](../../docs/database/database-spans.md#sanitization-of-dbquerytext).
  For batch operations, if the individual operations are known to have the same query text then that query text **SHOULD** be used, otherwise all of the individual query texts **SHOULD** be concatenated with separator `; ` or some other database system specific separator if more applicable.
  Even though parameterized query text can potentially have sensitive data, by using a parameterized query the user is giving a strong signal that any sensitive data will be passed as parameter values, and the benefit to observability of capturing the static part of the query text by default outweighs the risk.

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
  @spec db_redis_database_index :: :"db.redis.database_index"
  def db_redis_database_index do
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
  * `:adabas` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Adabas (Adaptable Database System)
  * `:cache` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Deprecated, use `intersystems_cache` instead.~~
  * `:intersystems_cache` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - InterSystems Caché
  * `:cassandra` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Cassandra
  * `:clickhouse` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - ClickHouse
  * `:cloudscape` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Deprecated, use `other_sql` instead.~~
  * `:cockroachdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - CockroachDB
  * `:coldfusion` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Deprecated, no replacement at this time.~~
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
  * `:firstsql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Deprecated, use `other_sql` instead.~~
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
  * `:mssqlcompact` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Deprecated, Microsoft SQL Server Compact is discontinued.~~
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
  @doc """
  The database management system (DBMS) product as identified by the client instrumentation.

  ### Notes

  The actual DBMS may differ from the one identified by the client. For example, when using PostgreSQL client libraries to connect to a CockroachDB, the `db.system` is set to `postgresql` based on the instrumentation's best knowledge.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_system()
      :"db.system"

      iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_system_values().other_sql
      :other_sql

      iex> %{OpenTelemetry.SemConv.Incubating.DBAttributes.db_system() => OpenTelemetry.SemConv.Incubating.DBAttributes.db_system_values().other_sql}
      %{:"db.system" => :other_sql}

  ### Erlang

  ```erlang
  ?DB_SYSTEM.
  'db.system'

  ?DB_SYSTEM_VALUES_OTHER_SQL.
  'other_sql'

  \#{?DB_SYSTEM => ?DB_SYSTEM_VALUES_OTHER_SQL}.
  \#{'db.system' => 'other_sql'}
  ```

  <!-- tabs-close -->
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

  @deprecated """
  Replaced by `db.client.connection.pool.name`.
  """
  @spec pool_name :: :"pool.name"
  def pool_name do
    :"pool.name"
  end

  @typedoc """
  Deprecated, use `db.client.connection.state` instead.

  ### Enum Values
  * `:idle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:used` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type state_values() :: %{
          :idle => :idle,
          :used => :used
        }
  @deprecated """
  Replaced by `db.client.connection.state`.
  """
  @spec state :: :state
  def state do
    :state
  end

  @spec state_values() :: state_values()
  def state_values() do
    %{
      :idle => :idle,
      :used => :used
    }
  end
end
