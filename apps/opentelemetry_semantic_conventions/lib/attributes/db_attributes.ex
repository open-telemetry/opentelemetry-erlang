defmodule OpenTelemetry.SemConv.DBAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for DB attributes.
  """

  @doc """
  The name of a collection (table, container) within the database.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  It is RECOMMENDED to capture the value as provided by the application
  without attempting to do any case normalization.

  The collection name **SHOULD** **NOT** be extracted from `db.query.text`,
  when the database system supports query text with multiple collections
  in non-batch operations.

  For batch operations, if the individual operations are known to have the same
  collection name then that collection name **SHOULD** be used.

  ### Examples

  ```
  ["public.users", "customers"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.DBAttributes.db_collection_name()
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

  @doc """
  The name of the database, fully qualified within the server address and port.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  If a database system has multiple namespace components, they **SHOULD** be concatenated from the most general to the most specific namespace component, using `|` as a separator between the components. Any missing components (and their associated separators) **SHOULD** be omitted.
  Semantic conventions for individual database systems **SHOULD** document what `db.namespace` means in the context of that system.
  It is RECOMMENDED to capture the value as provided by the application without attempting to do any case normalization.

  ### Examples

  ```
  ["customers", "test.users"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.DBAttributes.db_namespace()
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

  @doc """
  The number of queries included in a batch operation.
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

      iex> OpenTelemetry.SemConv.DBAttributes.db_operation_batch_size()
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

  It is RECOMMENDED to capture the value as provided by the application
  without attempting to do any case normalization.

  The operation name **SHOULD** **NOT** be extracted from `db.query.text`,
  when the database system supports query text with multiple operations
  in non-batch operations.

  If spaces can occur in the operation name, multiple consecutive spaces
  **SHOULD** be normalized to a single space.

  For batch operations, if the individual operations are known to have the same operation name
  then that operation name **SHOULD** be used prepended by `BATCH `,
  otherwise `db.operation.name` **SHOULD** be `BATCH` or some other database
  system specific term if more applicable.

  ### Examples

  ```
  ["findAndModify", "HMSET", "SELECT"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.DBAttributes.db_operation_name()
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
  Low cardinality summary of a database query.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The query summary describes a class of database queries and is useful
  as a grouping key, especially when analyzing telemetry for database
  calls involving complex queries.

  Summary may be available to the instrumentation through
  instrumentation hooks or other means. If it is not available, instrumentations
  that support query parsing **SHOULD** generate a summary following
  [Generating query summary](/docs/database/database-spans.md#generating-a-summary-of-the-query)
  section.

  ### Examples

  ```
  ["SELECT wuser_table", "INSERT shipping_details SELECT orders", "get user by id"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.DBAttributes.db_query_summary()
      :"db.query.summary"

  ### Erlang

  ```erlang
  ?DB_QUERY_SUMMARY.
  'db.query.summary'
  ```

  <!-- tabs-close -->
  """
  @spec db_query_summary :: :"db.query.summary"
  def db_query_summary do
    :"db.query.summary"
  end

  @doc """
  The database query being executed.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  For sanitization see [Sanitization of `db.query.text`](/docs/database/database-spans.md#sanitization-of-dbquerytext).
  For batch operations, if the individual operations are known to have the same query text then that query text **SHOULD** be used, otherwise all of the individual query texts **SHOULD** be concatenated with separator `; ` or some other database system specific separator if more applicable.
  Parameterized query text **SHOULD** **NOT** be sanitized. Even though parameterized query text can potentially have sensitive data, by using a parameterized query the user is giving a strong signal that any sensitive data will be passed as parameter values, and the benefit to observability of capturing the static part of the query text by default outweighs the risk.

  ### Examples

  ```
  ["SELECT * FROM wuser_table where username = ?", "SET mykey ?"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.DBAttributes.db_query_text()
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

  @doc """
  Database response status code.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The status code returned by the database. Usually it represents an error code, but may also represent partial success, warning, or differentiate between various types of successful outcomes.
  Semantic conventions for individual database systems **SHOULD** document what `db.response.status_code` means in the context of that system.

  ### Examples

  ```
  ["102", "ORA-17002", "08P01", "404"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.DBAttributes.db_response_status_code()
      :"db.response.status_code"

  ### Erlang

  ```erlang
  ?DB_RESPONSE_STATUS_CODE.
  'db.response.status_code'
  ```

  <!-- tabs-close -->
  """
  @spec db_response_status_code :: :"db.response.status_code"
  def db_response_status_code do
    :"db.response.status_code"
  end

  @doc """
  The name of a stored procedure within the database.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  It is RECOMMENDED to capture the value as provided by the application
  without attempting to do any case normalization.

  For batch operations, if the individual operations are known to have the same
  stored procedure name then that stored procedure name **SHOULD** be used.

  ### Examples

  ```
  ["GetCustomer"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.DBAttributes.db_stored_procedure_name()
      :"db.stored_procedure.name"

  ### Erlang

  ```erlang
  ?DB_STORED_PROCEDURE_NAME.
  'db.stored_procedure.name'
  ```

  <!-- tabs-close -->
  """
  @spec db_stored_procedure_name :: :"db.stored_procedure.name"
  def db_stored_procedure_name do
    :"db.stored_procedure.name"
  end

  @typedoc """
  The database management system (DBMS) product as identified by the client instrumentation.

  ### Enum Values
  * `:other_sql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Some other SQL database. Fallback only.
  * `:"softwareag.adabas"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Adabas (Adaptable Database System)](https://documentation.softwareag.com/?pf=adabas)
  * `:"actian.ingres"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Actian Ingres](https://www.actian.com/databases/ingres/)
  * `:"aws.dynamodb"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Amazon DynamoDB](https://aws.amazon.com/pm/dynamodb/)
  * `:"aws.redshift"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Amazon Redshift](https://aws.amazon.com/redshift/)
  * `:"azure.cosmosdb"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Azure Cosmos DB](https://learn.microsoft.com/azure/cosmos-db)
  * `:"intersystems.cache"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [InterSystems Caché](https://www.intersystems.com/products/cache/)
  * `:cassandra` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Apache Cassandra](https://cassandra.apache.org/)
  * `:clickhouse` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [ClickHouse](https://clickhouse.com/)
  * `:cockroachdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [CockroachDB](https://www.cockroachlabs.com/)
  * `:couchbase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Couchbase](https://www.couchbase.com/)
  * `:couchdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Apache CouchDB](https://couchdb.apache.org/)
  * `:derby` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Apache Derby](https://db.apache.org/derby/)
  * `:elasticsearch` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Elasticsearch](https://www.elastic.co/elasticsearch)
  * `:firebirdsql` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Firebird](https://www.firebirdsql.org/)
  * `:"gcp.spanner"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Google Cloud Spanner](https://cloud.google.com/spanner)
  * `:geode` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Apache Geode](https://geode.apache.org/)
  * `:h2database` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [H2 Database](https://h2database.com/)
  * `:hbase` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Apache HBase](https://hbase.apache.org/)
  * `:hive` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Apache Hive](https://hive.apache.org/)
  * `:hsqldb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [HyperSQL Database](https://hsqldb.org/)
  * `:"ibm.db2"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [IBM Db2](https://www.ibm.com/db2)
  * `:"ibm.informix"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [IBM Informix](https://www.ibm.com/products/informix)
  * `:"ibm.netezza"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [IBM Netezza](https://www.ibm.com/products/netezza)
  * `:influxdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [InfluxDB](https://www.influxdata.com/)
  * `:instantdb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Instant](https://www.instantdb.com/)
  * `:mariadb` - [MariaDB](https://mariadb.org/)
  * `:memcached` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Memcached](https://memcached.org/)
  * `:mongodb` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [MongoDB](https://www.mongodb.com/)
  * `:"microsoft.sql_server"` - [Microsoft SQL Server](https://www.microsoft.com/sql-server)
  * `:mysql` - [MySQL](https://www.mysql.com/)
  * `:neo4j` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Neo4j](https://neo4j.com/)
  * `:opensearch` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [OpenSearch](https://opensearch.org/)
  * `:"oracle.db"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Oracle Database](https://www.oracle.com/database/)
  * `:postgresql` - [PostgreSQL](https://www.postgresql.org/)
  * `:redis` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Redis](https://redis.io/)
  * `:"sap.hana"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [SAP HANA](https://www.sap.com/products/technology-platform/hana/what-is-sap-hana.html)
  * `:"sap.maxdb"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [SAP MaxDB](https://maxdb.sap.com/)
  * `:sqlite` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [SQLite](https://www.sqlite.org/)
  * `:teradata` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Teradata](https://www.teradata.com/)
  * `:trino` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Trino](https://trino.io/)
  """
  @type db_system_name_values() :: %{
          :other_sql => :other_sql,
          :"softwareag.adabas" => :"softwareag.adabas",
          :"actian.ingres" => :"actian.ingres",
          :"aws.dynamodb" => :"aws.dynamodb",
          :"aws.redshift" => :"aws.redshift",
          :"azure.cosmosdb" => :"azure.cosmosdb",
          :"intersystems.cache" => :"intersystems.cache",
          :cassandra => :cassandra,
          :clickhouse => :clickhouse,
          :cockroachdb => :cockroachdb,
          :couchbase => :couchbase,
          :couchdb => :couchdb,
          :derby => :derby,
          :elasticsearch => :elasticsearch,
          :firebirdsql => :firebirdsql,
          :"gcp.spanner" => :"gcp.spanner",
          :geode => :geode,
          :h2database => :h2database,
          :hbase => :hbase,
          :hive => :hive,
          :hsqldb => :hsqldb,
          :"ibm.db2" => :"ibm.db2",
          :"ibm.informix" => :"ibm.informix",
          :"ibm.netezza" => :"ibm.netezza",
          :influxdb => :influxdb,
          :instantdb => :instantdb,
          :mariadb => :mariadb,
          :memcached => :memcached,
          :mongodb => :mongodb,
          :"microsoft.sql_server" => :"microsoft.sql_server",
          :mysql => :mysql,
          :neo4j => :neo4j,
          :opensearch => :opensearch,
          :"oracle.db" => :"oracle.db",
          :postgresql => :postgresql,
          :redis => :redis,
          :"sap.hana" => :"sap.hana",
          :"sap.maxdb" => :"sap.maxdb",
          :sqlite => :sqlite,
          :teradata => :teradata,
          :trino => :trino
        }
  @doc """
  The database management system (DBMS) product as identified by the client instrumentation.

  ### Notes

  The actual DBMS may differ from the one identified by the client. For example, when using PostgreSQL client libraries to connect to a CockroachDB, the `db.system.name` is set to `postgresql` based on the instrumentation's best knowledge.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.DBAttributes.db_system_name()
      :"db.system.name"

      iex> OpenTelemetry.SemConv.DBAttributes.db_system_name_values().other_sql
      :other_sql

      iex> %{OpenTelemetry.SemConv.DBAttributes.db_system_name() => OpenTelemetry.SemConv.DBAttributes.db_system_name_values().other_sql}
      %{:"db.system.name" => :other_sql}

  ### Erlang

  ```erlang
  ?DB_SYSTEM_NAME.
  'db.system.name'

  ?DB_SYSTEM_NAME_VALUES_OTHER_SQL.
  'other_sql'

  \#{?DB_SYSTEM_NAME => ?DB_SYSTEM_NAME_VALUES_OTHER_SQL}.
  \#{'db.system.name' => 'other_sql'}
  ```

  <!-- tabs-close -->
  """
  @spec db_system_name :: :"db.system.name"
  def db_system_name do
    :"db.system.name"
  end

  @spec db_system_name_values() :: db_system_name_values()
  def db_system_name_values() do
    %{
      :other_sql => :other_sql,
      :"softwareag.adabas" => :"softwareag.adabas",
      :"actian.ingres" => :"actian.ingres",
      :"aws.dynamodb" => :"aws.dynamodb",
      :"aws.redshift" => :"aws.redshift",
      :"azure.cosmosdb" => :"azure.cosmosdb",
      :"intersystems.cache" => :"intersystems.cache",
      :cassandra => :cassandra,
      :clickhouse => :clickhouse,
      :cockroachdb => :cockroachdb,
      :couchbase => :couchbase,
      :couchdb => :couchdb,
      :derby => :derby,
      :elasticsearch => :elasticsearch,
      :firebirdsql => :firebirdsql,
      :"gcp.spanner" => :"gcp.spanner",
      :geode => :geode,
      :h2database => :h2database,
      :hbase => :hbase,
      :hive => :hive,
      :hsqldb => :hsqldb,
      :"ibm.db2" => :"ibm.db2",
      :"ibm.informix" => :"ibm.informix",
      :"ibm.netezza" => :"ibm.netezza",
      :influxdb => :influxdb,
      :instantdb => :instantdb,
      :mariadb => :mariadb,
      :memcached => :memcached,
      :mongodb => :mongodb,
      :"microsoft.sql_server" => :"microsoft.sql_server",
      :mysql => :mysql,
      :neo4j => :neo4j,
      :opensearch => :opensearch,
      :"oracle.db" => :"oracle.db",
      :postgresql => :postgresql,
      :redis => :redis,
      :"sap.hana" => :"sap.hana",
      :"sap.maxdb" => :"sap.maxdb",
      :sqlite => :sqlite,
      :teradata => :teradata,
      :trino => :trino
    }
  end
end
