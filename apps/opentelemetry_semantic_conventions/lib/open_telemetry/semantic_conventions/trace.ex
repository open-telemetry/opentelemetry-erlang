defmodule OpenTelemetry.SemanticConventions.Trace do
  @doc """
  The schema url for telemetry resources.

      iex> OpenTelemetry.SemanticConventions.Trace.trace_schema_url()
      "https://opentelemetry.io/schemas/1.20.0"
  """
  @spec trace_schema_url :: String.t()
  def trace_schema_url do
    "https://opentelemetry.io/schemas/1.20.0"
  end
  @doc """
  The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it

      iex> OpenTelemetry.SemanticConventions.Trace.exception_type()
      :"exception.type"
  """
  @spec exception_type :: :"exception.type"
  def exception_type do
    :"exception.type"
  end
  @doc """
  The exception message

      iex> OpenTelemetry.SemanticConventions.Trace.exception_message()
      :"exception.message"
  """
  @spec exception_message :: :"exception.message"
  def exception_message do
    :"exception.message"
  end
  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG

      iex> OpenTelemetry.SemanticConventions.Trace.exception_stacktrace()
      :"exception.stacktrace"
  """
  @spec exception_stacktrace :: :"exception.stacktrace"
  def exception_stacktrace do
    :"exception.stacktrace"
  end
  @doc """
  A unique identifier for the Log Record

  ### Notes

  If an id is provided, other log records with the same id will be considered duplicates and can be removed safely. This means, that two distinguishable log records MUST have different values.
  The id MAY be an [Universally Unique Lexicographically Sortable Identifier (ULID)](https://github.com/ulid/spec), but other identifiers (e.g. UUID) may be used as needed

      iex> OpenTelemetry.SemanticConventions.Trace.log_record_uid()
      :"log.record.uid"
  """
  @spec log_record_uid :: :"log.record.uid"
  def log_record_uid do
    :"log.record.uid"
  end
  @doc """
  The full invoked ARN as provided on the `Context` passed to the function (`Lambda-Runtime-Invoked-Function-Arn` header on the `/runtime/invocation/next` applicable)

  ### Notes

  This may be different from `cloud.resource_id` if an alias is involved

      iex> OpenTelemetry.SemanticConventions.Trace.aws_lambda_invoked_arn()
      :"aws.lambda.invoked_arn"
  """
  @spec aws_lambda_invoked_arn :: :"aws.lambda.invoked_arn"
  def aws_lambda_invoked_arn do
    :"aws.lambda.invoked_arn"
  end
  @doc """
  The [event_id](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#id) uniquely identifies the event

      iex> OpenTelemetry.SemanticConventions.Trace.cloudevents_event_id()
      :"cloudevents.event_id"
  """
  @spec cloudevents_event_id :: :"cloudevents.event_id"
  def cloudevents_event_id do
    :"cloudevents.event_id"
  end
  @doc """
  The [source](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#source-1) identifies the context in which an event happened

      iex> OpenTelemetry.SemanticConventions.Trace.cloudevents_event_source()
      :"cloudevents.event_source"
  """
  @spec cloudevents_event_source :: :"cloudevents.event_source"
  def cloudevents_event_source do
    :"cloudevents.event_source"
  end
  @doc """
  The [version of the CloudEvents specification](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#specversion) which the event uses

      iex> OpenTelemetry.SemanticConventions.Trace.cloudevents_event_spec_version()
      :"cloudevents.event_spec_version"
  """
  @spec cloudevents_event_spec_version :: :"cloudevents.event_spec_version"
  def cloudevents_event_spec_version do
    :"cloudevents.event_spec_version"
  end
  @doc """
  The [event_type](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#type) contains a value describing the type of event related to the originating occurrence

      iex> OpenTelemetry.SemanticConventions.Trace.cloudevents_event_type()
      :"cloudevents.event_type"
  """
  @spec cloudevents_event_type :: :"cloudevents.event_type"
  def cloudevents_event_type do
    :"cloudevents.event_type"
  end
  @doc """
  The [subject](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#subject) of the event in the context of the event producer (identified by source)

      iex> OpenTelemetry.SemanticConventions.Trace.cloudevents_event_subject()
      :"cloudevents.event_subject"
  """
  @spec cloudevents_event_subject :: :"cloudevents.event_subject"
  def cloudevents_event_subject do
    :"cloudevents.event_subject"
  end
  @doc """
  Parent-child Reference type

  ### Notes

  The causal relationship between a child Span and a parent Span

      iex> OpenTelemetry.SemanticConventions.Trace.opentracing_ref_type()
      :"opentracing.ref_type"
  """
  @spec opentracing_ref_type :: :"opentracing.ref_type"
  def opentracing_ref_type do
    :"opentracing.ref_type"
  end
  @doc """
  An identifier for the database management system (DBMS) product being used. See below for a list of well-known identifiers

      iex> OpenTelemetry.SemanticConventions.Trace.db_system()
      :"db.system"
  """
  @spec db_system :: :"db.system"
  def db_system do
    :"db.system"
  end
  @doc """
  The connection string used to connect to the database. It is recommended to remove embedded credentials

      iex> OpenTelemetry.SemanticConventions.Trace.db_connection_string()
      :"db.connection_string"
  """
  @spec db_connection_string :: :"db.connection_string"
  def db_connection_string do
    :"db.connection_string"
  end
  @doc """
  Username for accessing the database

      iex> OpenTelemetry.SemanticConventions.Trace.db_user()
      :"db.user"
  """
  @spec db_user :: :"db.user"
  def db_user do
    :"db.user"
  end
  @doc """
  The fully-qualified class name of the [Java Database Connectivity (JDBC)](https://docs.oracle.com/javase/8/docs/technotes/guides/jdbc/) driver used to connect

      iex> OpenTelemetry.SemanticConventions.Trace.db_jdbc_driver_classname()
      :"db.jdbc.driver_classname"
  """
  @spec db_jdbc_driver_classname :: :"db.jdbc.driver_classname"
  def db_jdbc_driver_classname do
    :"db.jdbc.driver_classname"
  end
  @doc """
  This attribute is used to report the name of the database being accessed. For commands that switch the database, this should be set to the target database (even if the command fails)

  ### Notes

  In some SQL databases, the database name to be used is called "schema name". In case there are multiple layers that could be considered for database name (e.g. Oracle instance name and schema name), the database name to be used is the more specific layer (e.g. Oracle schema name)

      iex> OpenTelemetry.SemanticConventions.Trace.db_name()
      :"db.name"
  """
  @spec db_name :: :"db.name"
  def db_name do
    :"db.name"
  end
  @doc """
  The database statement being executed

      iex> OpenTelemetry.SemanticConventions.Trace.db_statement()
      :"db.statement"
  """
  @spec db_statement :: :"db.statement"
  def db_statement do
    :"db.statement"
  end
  @doc """
  The name of the operation being executed, e.g. the [MongoDB command name](https://docs.mongodb.com/manual/reference/command/#database-operations) such as `findAndModify`, or the SQL keyword

  ### Notes

  When setting this to an SQL keyword, it is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if the operation name is provided by the library being instrumented. If the SQL statement has an ambiguous operation, or performs more than one operation, this value may be omitted

      iex> OpenTelemetry.SemanticConventions.Trace.db_operation()
      :"db.operation"
  """
  @spec db_operation :: :"db.operation"
  def db_operation do
    :"db.operation"
  end
  @doc """
  Name of the database host

  ### Notes

  `net.peer.name` SHOULD NOT be set if capturing it would require an extra DNS lookup

      iex> OpenTelemetry.SemanticConventions.Trace.net_peer_name()
      :"net.peer.name"
  """
  @spec net_peer_name :: :"net.peer.name"
  def net_peer_name do
    :"net.peer.name"
  end
  @doc """
  Logical remote port number

      iex> OpenTelemetry.SemanticConventions.Trace.net_peer_port()
      :"net.peer.port"
  """
  @spec net_peer_port :: :"net.peer.port"
  def net_peer_port do
    :"net.peer.port"
  end
  @doc """
  Remote socket peer address: IPv4 or IPv6 for internet protocols, path for local communication, [etc](https://man7.org/linux/man-pages/man7/address_families.7.html)

      iex> OpenTelemetry.SemanticConventions.Trace.net_sock_peer_addr()
      :"net.sock.peer.addr"
  """
  @spec net_sock_peer_addr :: :"net.sock.peer.addr"
  def net_sock_peer_addr do
    :"net.sock.peer.addr"
  end
  @doc """
  Remote socket peer port

      iex> OpenTelemetry.SemanticConventions.Trace.net_sock_peer_port()
      :"net.sock.peer.port"
  """
  @spec net_sock_peer_port :: :"net.sock.peer.port"
  def net_sock_peer_port do
    :"net.sock.peer.port"
  end
  @doc """
  Protocol [address family](https://man7.org/linux/man-pages/man7/address_families.7.html) which is used for communication

      iex> OpenTelemetry.SemanticConventions.Trace.net_sock_family()
      :"net.sock.family"
  """
  @spec net_sock_family :: :"net.sock.family"
  def net_sock_family do
    :"net.sock.family"
  end
  @doc """
  Remote socket peer name

      iex> OpenTelemetry.SemanticConventions.Trace.net_sock_peer_name()
      :"net.sock.peer.name"
  """
  @spec net_sock_peer_name :: :"net.sock.peer.name"
  def net_sock_peer_name do
    :"net.sock.peer.name"
  end
  @doc """
  Transport protocol used. See note below

      iex> OpenTelemetry.SemanticConventions.Trace.net_transport()
      :"net.transport"
  """
  @spec net_transport :: :"net.transport"
  def net_transport do
    :"net.transport"
  end
  @doc """
  The Microsoft SQL Server [instance name](https://docs.microsoft.com/en-us/sql/connect/jdbc/building-the-connection-url?view=sql-server-ver15) connecting to. This name is used to determine the port of a named instance

  ### Notes

  If setting a `db.mssql.instance_name`, `net.peer.port` is no longer required (but still recommended if non-standard)

      iex> OpenTelemetry.SemanticConventions.Trace.db_mssql_instance_name()
      :"db.mssql.instance_name"
  """
  @spec db_mssql_instance_name :: :"db.mssql.instance_name"
  def db_mssql_instance_name do
    :"db.mssql.instance_name"
  end
  @doc """
  The fetch size used for paging, i.e. how many rows will be returned at once

      iex> OpenTelemetry.SemanticConventions.Trace.db_cassandra_page_size()
      :"db.cassandra.page_size"
  """
  @spec db_cassandra_page_size :: :"db.cassandra.page_size"
  def db_cassandra_page_size do
    :"db.cassandra.page_size"
  end
  @doc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html)

      iex> OpenTelemetry.SemanticConventions.Trace.db_cassandra_consistency_level()
      :"db.cassandra.consistency_level"
  """
  @spec db_cassandra_consistency_level :: :"db.cassandra.consistency_level"
  def db_cassandra_consistency_level do
    :"db.cassandra.consistency_level"
  end
  @doc """
  The name of the primary table that the operation is acting upon, including the keyspace name (if applicable)

  ### Notes

  This mirrors the db.sql.table attribute but references cassandra rather than sql. It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set

      iex> OpenTelemetry.SemanticConventions.Trace.db_cassandra_table()
      :"db.cassandra.table"
  """
  @spec db_cassandra_table :: :"db.cassandra.table"
  def db_cassandra_table do
    :"db.cassandra.table"
  end
  @doc """
  Whether or not the query is idempotent

      iex> OpenTelemetry.SemanticConventions.Trace.db_cassandra_idempotence()
      :"db.cassandra.idempotence"
  """
  @spec db_cassandra_idempotence :: :"db.cassandra.idempotence"
  def db_cassandra_idempotence do
    :"db.cassandra.idempotence"
  end
  @doc """
  The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively

      iex> OpenTelemetry.SemanticConventions.Trace.db_cassandra_speculative_execution_count()
      :"db.cassandra.speculative_execution_count"
  """
  @spec db_cassandra_speculative_execution_count :: :"db.cassandra.speculative_execution_count"
  def db_cassandra_speculative_execution_count do
    :"db.cassandra.speculative_execution_count"
  end
  @doc """
  The ID of the coordinating node for a query

      iex> OpenTelemetry.SemanticConventions.Trace.db_cassandra_coordinator_id()
      :"db.cassandra.coordinator.id"
  """
  @spec db_cassandra_coordinator_id :: :"db.cassandra.coordinator.id"
  def db_cassandra_coordinator_id do
    :"db.cassandra.coordinator.id"
  end
  @doc """
  The data center of the coordinating node for a query

      iex> OpenTelemetry.SemanticConventions.Trace.db_cassandra_coordinator_dc()
      :"db.cassandra.coordinator.dc"
  """
  @spec db_cassandra_coordinator_dc :: :"db.cassandra.coordinator.dc"
  def db_cassandra_coordinator_dc do
    :"db.cassandra.coordinator.dc"
  end
  @doc """
  The index of the database being accessed as used in the [`SELECT` command](https://redis.io/commands/select), provided as an integer. To be used instead of the generic `db.name` attribute

      iex> OpenTelemetry.SemanticConventions.Trace.db_redis_database_index()
      :"db.redis.database_index"
  """
  @spec db_redis_database_index :: :"db.redis.database_index"
  def db_redis_database_index do
    :"db.redis.database_index"
  end
  @doc """
  The collection being accessed within the database stated in `db.name`

      iex> OpenTelemetry.SemanticConventions.Trace.db_mongodb_collection()
      :"db.mongodb.collection"
  """
  @spec db_mongodb_collection :: :"db.mongodb.collection"
  def db_mongodb_collection do
    :"db.mongodb.collection"
  end
  @doc """
  The name of the primary table that the operation is acting upon, including the database name (if applicable)

  ### Notes

  It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set

      iex> OpenTelemetry.SemanticConventions.Trace.db_sql_table()
      :"db.sql.table"
  """
  @spec db_sql_table :: :"db.sql.table"
  def db_sql_table do
    :"db.sql.table"
  end
  @doc """
  Unique Cosmos client instance id

      iex> OpenTelemetry.SemanticConventions.Trace.db_cosmosdb_client_id()
      :"db.cosmosdb.client_id"
  """
  @spec db_cosmosdb_client_id :: :"db.cosmosdb.client_id"
  def db_cosmosdb_client_id do
    :"db.cosmosdb.client_id"
  end
  @doc """
  CosmosDB Operation Type

      iex> OpenTelemetry.SemanticConventions.Trace.db_cosmosdb_operation_type()
      :"db.cosmosdb.operation_type"
  """
  @spec db_cosmosdb_operation_type :: :"db.cosmosdb.operation_type"
  def db_cosmosdb_operation_type do
    :"db.cosmosdb.operation_type"
  end
  @doc """
  Full user-agent string is generated by Cosmos DB SDK

  ### Notes

  The user-agent value is generated by SDK which is a combination of<br> `sdk_version` : Current version of SDK. e.g. 'cosmos-netstandard-sdk/3.23.0'<br> `direct_pkg_version` : Direct package version used by Cosmos DB SDK. e.g. '3.23.1'<br> `number_of_client_instances` : Number of cosmos client instances created by the application. e.g. '1'<br> `type_of_machine_architecture` : Machine architecture. e.g. 'X64'<br> `operating_system` : Operating System. e.g. 'Linux 5.4.0-1098-azure 104 18'<br> `runtime_framework` : Runtime Framework. e.g. '.NET Core 3.1.32'<br> `failover_information` : Generated key to determine if region failover enabled.
     Format Reg-{D (Disabled discovery)}-S(application region)|L(List of preferred regions)|N(None, user did not configure it).
     Default value is "NS"

      iex> OpenTelemetry.SemanticConventions.Trace.user_agent_original()
      :"user_agent.original"
  """
  @spec user_agent_original :: :"user_agent.original"
  def user_agent_original do
    :"user_agent.original"
  end
  @doc """
  Cosmos client connection mode

      iex> OpenTelemetry.SemanticConventions.Trace.db_cosmosdb_connection_mode()
      :"db.cosmosdb.connection_mode"
  """
  @spec db_cosmosdb_connection_mode :: :"db.cosmosdb.connection_mode"
  def db_cosmosdb_connection_mode do
    :"db.cosmosdb.connection_mode"
  end
  @doc """
  Cosmos DB container name

      iex> OpenTelemetry.SemanticConventions.Trace.db_cosmosdb_container()
      :"db.cosmosdb.container"
  """
  @spec db_cosmosdb_container :: :"db.cosmosdb.container"
  def db_cosmosdb_container do
    :"db.cosmosdb.container"
  end
  @doc """
  Request payload size in bytes

      iex> OpenTelemetry.SemanticConventions.Trace.db_cosmosdb_request_content_length()
      :"db.cosmosdb.request_content_length"
  """
  @spec db_cosmosdb_request_content_length :: :"db.cosmosdb.request_content_length"
  def db_cosmosdb_request_content_length do
    :"db.cosmosdb.request_content_length"
  end
  @doc """
  Cosmos DB status code

      iex> OpenTelemetry.SemanticConventions.Trace.db_cosmosdb_status_code()
      :"db.cosmosdb.status_code"
  """
  @spec db_cosmosdb_status_code :: :"db.cosmosdb.status_code"
  def db_cosmosdb_status_code do
    :"db.cosmosdb.status_code"
  end
  @doc """
  Cosmos DB sub status code

      iex> OpenTelemetry.SemanticConventions.Trace.db_cosmosdb_sub_status_code()
      :"db.cosmosdb.sub_status_code"
  """
  @spec db_cosmosdb_sub_status_code :: :"db.cosmosdb.sub_status_code"
  def db_cosmosdb_sub_status_code do
    :"db.cosmosdb.sub_status_code"
  end
  @doc """
  RU consumed for that operation

      iex> OpenTelemetry.SemanticConventions.Trace.db_cosmosdb_request_charge()
      :"db.cosmosdb.request_charge"
  """
  @spec db_cosmosdb_request_charge :: :"db.cosmosdb.request_charge"
  def db_cosmosdb_request_charge do
    :"db.cosmosdb.request_charge"
  end
  @doc """
  Name of the code, either "OK" or "ERROR". MUST NOT be set if the status code is UNSET

      iex> OpenTelemetry.SemanticConventions.Trace.otel_status_code()
      :"otel.status_code"
  """
  @spec otel_status_code :: :"otel.status_code"
  def otel_status_code do
    :"otel.status_code"
  end
  @doc """
  Description of the Status if it has a value, otherwise not set

      iex> OpenTelemetry.SemanticConventions.Trace.otel_status_description()
      :"otel.status_description"
  """
  @spec otel_status_description :: :"otel.status_description"
  def otel_status_description do
    :"otel.status_description"
  end
  @doc """
  Type of the trigger which caused this function invocation

  ### Notes

  For the server/consumer span on the incoming side,
  `faas.trigger` MUST be set.
  
  Clients invoking FaaS instances usually cannot set `faas.trigger`,
  since they would typically need to look in the payload to determine
  the event type. If clients set it, it should be the same as the
  trigger that corresponding incoming would have (i.e., this has
  nothing to do with the underlying transport used to make the API
  call to invoke the lambda, which is often HTTP)

      iex> OpenTelemetry.SemanticConventions.Trace.faas_trigger()
      :"faas.trigger"
  """
  @spec faas_trigger :: :"faas.trigger"
  def faas_trigger do
    :"faas.trigger"
  end
  @doc """
  The invocation ID of the current function invocation

      iex> OpenTelemetry.SemanticConventions.Trace.faas_invocation_id()
      :"faas.invocation_id"
  """
  @spec faas_invocation_id :: :"faas.invocation_id"
  def faas_invocation_id do
    :"faas.invocation_id"
  end
  @doc """
  Cloud provider-specific native identifier of the monitored cloud resource (e.g. an [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html) on AWS, a [fully qualified resource ID](https://learn.microsoft.com/en-us/rest/api/resources/resources/get-by-id) on Azure, a [full resource name](https://cloud.google.com/apis/design/resource_names#full_resource_name) on GCP)

  ### Notes

  On some cloud providers, it may not be possible to determine the full ID at startup,
  so it may be necessary to set `cloud.resource_id` as a span attribute instead.
  
  The exact value to use for `cloud.resource_id` depends on the cloud provider.
  The following well-known definitions MUST be used if you set this attribute and they apply:
  
  * **AWS Lambda:** The function [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html).
    Take care not to use the "invoked ARN" directly but replace any
    [alias suffix](https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html)
    with the resolved function version, as the same runtime instance may be invokable with
    multiple different aliases.
  * **GCP:** The [URI of the resource](https://cloud.google.com/iam/docs/full-resource-names)
  * **Azure:** The [Fully Qualified Resource ID](https://docs.microsoft.com/en-us/rest/api/resources/resources/get-by-id) of the invoked function,
    *not* the function app, having the form
    `/subscriptions/<SUBSCIPTION_GUID>/resourceGroups/<RG>/providers/Microsoft.Web/sites/<FUNCAPP>/functions/<FUNC>`.
    This means that a span attribute MUST be used, as an Azure function app can host multiple functions that would usually share
    a TracerProvider

      iex> OpenTelemetry.SemanticConventions.Trace.cloud_resource_id()
      :"cloud.resource_id"
  """
  @spec cloud_resource_id :: :"cloud.resource_id"
  def cloud_resource_id do
    :"cloud.resource_id"
  end
  @doc """
  The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name

      iex> OpenTelemetry.SemanticConventions.Trace.faas_document_collection()
      :"faas.document.collection"
  """
  @spec faas_document_collection :: :"faas.document.collection"
  def faas_document_collection do
    :"faas.document.collection"
  end
  @doc """
  Describes the type of the operation that was performed on the data

      iex> OpenTelemetry.SemanticConventions.Trace.faas_document_operation()
      :"faas.document.operation"
  """
  @spec faas_document_operation :: :"faas.document.operation"
  def faas_document_operation do
    :"faas.document.operation"
  end
  @doc """
  A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)

      iex> OpenTelemetry.SemanticConventions.Trace.faas_document_time()
      :"faas.document.time"
  """
  @spec faas_document_time :: :"faas.document.time"
  def faas_document_time do
    :"faas.document.time"
  end
  @doc """
  The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name

      iex> OpenTelemetry.SemanticConventions.Trace.faas_document_name()
      :"faas.document.name"
  """
  @spec faas_document_name :: :"faas.document.name"
  def faas_document_name do
    :"faas.document.name"
  end
  @doc """
  The URI scheme identifying the used protocol

      iex> OpenTelemetry.SemanticConventions.Trace.http_scheme()
      :"http.scheme"
  """
  @spec http_scheme :: :"http.scheme"
  def http_scheme do
    :"http.scheme"
  end
  @doc """
  The matched route (path template in the format used by the respective server framework). See note below

  ### Notes

  MUST NOT be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can NOT substitute it.
  SHOULD include the [application root](/specification/trace/semantic_conventions/http.md#http-server-definitions) if there is one

      iex> OpenTelemetry.SemanticConventions.Trace.http_route()
      :"http.route"
  """
  @spec http_route :: :"http.route"
  def http_route do
    :"http.route"
  end
  @doc """
  Name of the local HTTP server that received the request

  ### Notes

  Determined by using the first of the following that applies
  
  - The [primary server name](/specification/trace/semantic_conventions/http.md#http-server-definitions) of the matched virtual host. MUST only
    include host identifier.
  - Host identifier of the [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource)
    if it's sent in absolute-form.
  - Host identifier of the `Host` header
  
  SHOULD NOT be set if only IP address is available and capturing name would require a reverse DNS lookup

      iex> OpenTelemetry.SemanticConventions.Trace.net_host_name()
      :"net.host.name"
  """
  @spec net_host_name :: :"net.host.name"
  def net_host_name do
    :"net.host.name"
  end
  @doc """
  Port of the local HTTP server that received the request

  ### Notes

  Determined by using the first of the following that applies
  
  - Port identifier of the [primary server host](/specification/trace/semantic_conventions/http.md#http-server-definitions) of the matched virtual host.
  - Port identifier of the [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource)
    if it's sent in absolute-form.
  - Port identifier of the `Host` header

      iex> OpenTelemetry.SemanticConventions.Trace.net_host_port()
      :"net.host.port"
  """
  @spec net_host_port :: :"net.host.port"
  def net_host_port do
    :"net.host.port"
  end
  @doc """
  The full request target as passed in a HTTP request line or equivalent

      iex> OpenTelemetry.SemanticConventions.Trace.http_target()
      :"http.target"
  """
  @spec http_target :: :"http.target"
  def http_target do
    :"http.target"
  end
  @doc """
  The IP address of the original client behind all proxies, if known (e.g. from [X-Forwarded-For](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For))

  ### Notes

  This is not necessarily the same as `net.sock.peer.addr`, which would
  identify the network-level peer, which may be a proxy.
  
  This attribute should be set when a source of information different
  from the one used for `net.sock.peer.addr`, is available even if that other
  source just confirms the same value as `net.sock.peer.addr`.
  Rationale: For `net.sock.peer.addr`, one typically does not know if it
  comes from a proxy, reverse proxy, or the actual client. Setting
  `http.client_ip` when it's the same as `net.sock.peer.addr` means that
  one is at least somewhat confident that the address is not that of
  the closest proxy

      iex> OpenTelemetry.SemanticConventions.Trace.http_client_ip()
      :"http.client_ip"
  """
  @spec http_client_ip :: :"http.client_ip"
  def http_client_ip do
    :"http.client_ip"
  end
  @doc """
  Local socket address. Useful in case of a multi-IP host

      iex> OpenTelemetry.SemanticConventions.Trace.net_sock_host_addr()
      :"net.sock.host.addr"
  """
  @spec net_sock_host_addr :: :"net.sock.host.addr"
  def net_sock_host_addr do
    :"net.sock.host.addr"
  end
  @doc """
  Local socket port number

      iex> OpenTelemetry.SemanticConventions.Trace.net_sock_host_port()
      :"net.sock.host.port"
  """
  @spec net_sock_host_port :: :"net.sock.host.port"
  def net_sock_host_port do
    :"net.sock.host.port"
  end
  @doc """
  A string identifying the messaging system

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_system()
      :"messaging.system"
  """
  @spec messaging_system :: :"messaging.system"
  def messaging_system do
    :"messaging.system"
  end
  @doc """
  A string identifying the kind of messaging operation as defined in the [Operation names](#operation-names) section above

  ### Notes

  If a custom value is used, it MUST be of low cardinality

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_operation()
      :"messaging.operation"
  """
  @spec messaging_operation :: :"messaging.operation"
  def messaging_operation do
    :"messaging.operation"
  end
  @doc """
  The number of messages sent, received, or processed in the scope of the batching operation

  ### Notes

  Instrumentations SHOULD NOT set `messaging.batch.message_count` on spans that operate with a single message. When a messaging client library supports both batch and single-message API for the same operation, instrumentations SHOULD use `messaging.batch.message_count` for batching APIs and SHOULD NOT use it for single-message APIs

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_batch_message_count()
      :"messaging.batch.message_count"
  """
  @spec messaging_batch_message_count :: :"messaging.batch.message_count"
  def messaging_batch_message_count do
    :"messaging.batch.message_count"
  end
  @doc """
  A value used by the messaging system as an identifier for the message, represented as a string

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_message_id()
      :"messaging.message.id"
  """
  @spec messaging_message_id :: :"messaging.message.id"
  def messaging_message_id do
    :"messaging.message.id"
  end
  @doc """
  The [conversation ID](#conversations) identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID"

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_message_conversation_id()
      :"messaging.message.conversation_id"
  """
  @spec messaging_message_conversation_id :: :"messaging.message.conversation_id"
  def messaging_message_conversation_id do
    :"messaging.message.conversation_id"
  end
  @doc """
  The (uncompressed) size of the message payload in bytes. Also use this attribute if it is unknown whether the compressed or uncompressed payload size is reported

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_message_payload_size_bytes()
      :"messaging.message.payload_size_bytes"
  """
  @spec messaging_message_payload_size_bytes :: :"messaging.message.payload_size_bytes"
  def messaging_message_payload_size_bytes do
    :"messaging.message.payload_size_bytes"
  end
  @doc """
  The compressed size of the message payload in bytes

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_message_payload_compressed_size_bytes()
      :"messaging.message.payload_compressed_size_bytes"
  """
  @spec messaging_message_payload_compressed_size_bytes :: :"messaging.message.payload_compressed_size_bytes"
  def messaging_message_payload_compressed_size_bytes do
    :"messaging.message.payload_compressed_size_bytes"
  end
  @doc """
  Application layer protocol used. The value SHOULD be normalized to lowercase

      iex> OpenTelemetry.SemanticConventions.Trace.net_protocol_name()
      :"net.protocol.name"
  """
  @spec net_protocol_name :: :"net.protocol.name"
  def net_protocol_name do
    :"net.protocol.name"
  end
  @doc """
  Version of the application layer protocol used. See note below

  ### Notes

  `net.protocol.version` refers to the version of the protocol used and might be different from the protocol client's version. If the HTTP client used has a version of `0.27.2`, but sends HTTP version `1.1`, this attribute should be set to `1.1`

      iex> OpenTelemetry.SemanticConventions.Trace.net_protocol_version()
      :"net.protocol.version"
  """
  @spec net_protocol_version :: :"net.protocol.version"
  def net_protocol_version do
    :"net.protocol.version"
  end
  @doc """
  A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)

      iex> OpenTelemetry.SemanticConventions.Trace.faas_time()
      :"faas.time"
  """
  @spec faas_time :: :"faas.time"
  def faas_time do
    :"faas.time"
  end
  @doc """
  A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm)

      iex> OpenTelemetry.SemanticConventions.Trace.faas_cron()
      :"faas.cron"
  """
  @spec faas_cron :: :"faas.cron"
  def faas_cron do
    :"faas.cron"
  end
  @doc """
  A boolean that is true if the serverless function is executed for the first time (aka cold-start)

      iex> OpenTelemetry.SemanticConventions.Trace.faas_coldstart()
      :"faas.coldstart"
  """
  @spec faas_coldstart :: :"faas.coldstart"
  def faas_coldstart do
    :"faas.coldstart"
  end
  @doc """
  The name of the invoked function

  ### Notes

  SHOULD be equal to the `faas.name` resource attribute of the invoked function

      iex> OpenTelemetry.SemanticConventions.Trace.faas_invoked_name()
      :"faas.invoked_name"
  """
  @spec faas_invoked_name :: :"faas.invoked_name"
  def faas_invoked_name do
    :"faas.invoked_name"
  end
  @doc """
  The cloud provider of the invoked function

  ### Notes

  SHOULD be equal to the `cloud.provider` resource attribute of the invoked function

      iex> OpenTelemetry.SemanticConventions.Trace.faas_invoked_provider()
      :"faas.invoked_provider"
  """
  @spec faas_invoked_provider :: :"faas.invoked_provider"
  def faas_invoked_provider do
    :"faas.invoked_provider"
  end
  @doc """
  The cloud region of the invoked function

  ### Notes

  SHOULD be equal to the `cloud.region` resource attribute of the invoked function

      iex> OpenTelemetry.SemanticConventions.Trace.faas_invoked_region()
      :"faas.invoked_region"
  """
  @spec faas_invoked_region :: :"faas.invoked_region"
  def faas_invoked_region do
    :"faas.invoked_region"
  end
  @doc """
  The [`service.name`](../../resource/semantic_conventions/README.md#service) of the remote service. SHOULD be equal to the actual `service.name` resource attribute of the remote service if any

      iex> OpenTelemetry.SemanticConventions.Trace.peer_service()
      :"peer.service"
  """
  @spec peer_service :: :"peer.service"
  def peer_service do
    :"peer.service"
  end
  @doc """
  Username or client_id extracted from the access token or [Authorization](https://tools.ietf.org/html/rfc7235#section-4.2) header in the inbound request from outside the system

      iex> OpenTelemetry.SemanticConventions.Trace.enduser_id()
      :"enduser.id"
  """
  @spec enduser_id :: :"enduser.id"
  def enduser_id do
    :"enduser.id"
  end
  @doc """
  Actual/assumed role the client is making the request under extracted from token or application security context

      iex> OpenTelemetry.SemanticConventions.Trace.enduser_role()
      :"enduser.role"
  """
  @spec enduser_role :: :"enduser.role"
  def enduser_role do
    :"enduser.role"
  end
  @doc """
  Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an [OAuth 2.0 Access Token](https://tools.ietf.org/html/rfc6749#section-3.3) or an attribute value in a [SAML 2.0 Assertion](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html)

      iex> OpenTelemetry.SemanticConventions.Trace.enduser_scope()
      :"enduser.scope"
  """
  @spec enduser_scope :: :"enduser.scope"
  def enduser_scope do
    :"enduser.scope"
  end
  @doc """
  Current "managed" thread ID (as opposed to OS thread ID)

      iex> OpenTelemetry.SemanticConventions.Trace.thread_id()
      :"thread.id"
  """
  @spec thread_id :: :"thread.id"
  def thread_id do
    :"thread.id"
  end
  @doc """
  Current thread name

      iex> OpenTelemetry.SemanticConventions.Trace.thread_name()
      :"thread.name"
  """
  @spec thread_name :: :"thread.name"
  def thread_name do
    :"thread.name"
  end
  @doc """
  The method or function name, or equivalent (usually rightmost part of the code unit's name)

      iex> OpenTelemetry.SemanticConventions.Trace.code_function()
      :"code.function"
  """
  @spec code_function :: :"code.function"
  def code_function do
    :"code.function"
  end
  @doc """
  The "namespace" within which `code.function` is defined. Usually the qualified class or module name, such that `code.namespace` + some separator + `code.function` form a unique identifier for the code unit

      iex> OpenTelemetry.SemanticConventions.Trace.code_namespace()
      :"code.namespace"
  """
  @spec code_namespace :: :"code.namespace"
  def code_namespace do
    :"code.namespace"
  end
  @doc """
  The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path)

      iex> OpenTelemetry.SemanticConventions.Trace.code_filepath()
      :"code.filepath"
  """
  @spec code_filepath :: :"code.filepath"
  def code_filepath do
    :"code.filepath"
  end
  @doc """
  The line number in `code.filepath` best representing the operation. It SHOULD point within the code unit named in `code.function`

      iex> OpenTelemetry.SemanticConventions.Trace.code_lineno()
      :"code.lineno"
  """
  @spec code_lineno :: :"code.lineno"
  def code_lineno do
    :"code.lineno"
  end
  @doc """
  The column number in `code.filepath` best representing the operation. It SHOULD point within the code unit named in `code.function`

      iex> OpenTelemetry.SemanticConventions.Trace.code_column()
      :"code.column"
  """
  @spec code_column :: :"code.column"
  def code_column do
    :"code.column"
  end
  @doc """
  Full HTTP request URL in the form `scheme://host[:port]/path?query[#fragment]`. Usually the fragment is not transmitted over HTTP, but if it is known, it should be included nevertheless

  ### Notes

  `http.url` MUST NOT contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case the attribute's value should be `https://www.example.com/`

      iex> OpenTelemetry.SemanticConventions.Trace.http_url()
      :"http.url"
  """
  @spec http_url :: :"http.url"
  def http_url do
    :"http.url"
  end
  @doc """
  The ordinal number of request resending attempt (for any reason, including redirects)

  ### Notes

  The resend count SHOULD be updated each time an HTTP request gets resent by the client, regardless of what was the cause of the resending (e.g. redirection, authorization failure, 503 Server Unavailable, network issues, or any other)

      iex> OpenTelemetry.SemanticConventions.Trace.http_resend_count()
      :"http.resend_count"
  """
  @spec http_resend_count :: :"http.resend_count"
  def http_resend_count do
    :"http.resend_count"
  end
  @doc """
  The value `aws-api`

      iex> OpenTelemetry.SemanticConventions.Trace.rpc_system()
      :"rpc.system"
  """
  @spec rpc_system :: :"rpc.system"
  def rpc_system do
    :"rpc.system"
  end
  @doc """
  The name of the service to which a request is made, as returned by the AWS SDK

  ### Notes

  This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side)

      iex> OpenTelemetry.SemanticConventions.Trace.rpc_service()
      :"rpc.service"
  """
  @spec rpc_service :: :"rpc.service"
  def rpc_service do
    :"rpc.service"
  end
  @doc """
  The name of the operation corresponding to the request, as returned by the AWS SDK

  ### Notes

  This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side)

      iex> OpenTelemetry.SemanticConventions.Trace.rpc_method()
      :"rpc.method"
  """
  @spec rpc_method :: :"rpc.method"
  def rpc_method do
    :"rpc.method"
  end
  @doc """
  The AWS request ID as returned in the response headers `x-amz-request-id` or `x-amz-requestid`

      iex> OpenTelemetry.SemanticConventions.Trace.aws_request_id()
      :"aws.request_id"
  """
  @spec aws_request_id :: :"aws.request_id"
  def aws_request_id do
    :"aws.request_id"
  end
  @doc """
  The keys in the `RequestItems` object field

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_table_names()
      :"aws.dynamodb.table_names"
  """
  @spec aws_dynamodb_table_names :: :"aws.dynamodb.table_names"
  def aws_dynamodb_table_names do
    :"aws.dynamodb.table_names"
  end
  @doc """
  The JSON-serialized value of each item in the `ConsumedCapacity` response field

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_consumed_capacity()
      :"aws.dynamodb.consumed_capacity"
  """
  @spec aws_dynamodb_consumed_capacity :: :"aws.dynamodb.consumed_capacity"
  def aws_dynamodb_consumed_capacity do
    :"aws.dynamodb.consumed_capacity"
  end
  @doc """
  The JSON-serialized value of the `ItemCollectionMetrics` response field

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_item_collection_metrics()
      :"aws.dynamodb.item_collection_metrics"
  """
  @spec aws_dynamodb_item_collection_metrics :: :"aws.dynamodb.item_collection_metrics"
  def aws_dynamodb_item_collection_metrics do
    :"aws.dynamodb.item_collection_metrics"
  end
  @doc """
  The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_provisioned_read_capacity()
      :"aws.dynamodb.provisioned_read_capacity"
  """
  @spec aws_dynamodb_provisioned_read_capacity :: :"aws.dynamodb.provisioned_read_capacity"
  def aws_dynamodb_provisioned_read_capacity do
    :"aws.dynamodb.provisioned_read_capacity"
  end
  @doc """
  The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_provisioned_write_capacity()
      :"aws.dynamodb.provisioned_write_capacity"
  """
  @spec aws_dynamodb_provisioned_write_capacity :: :"aws.dynamodb.provisioned_write_capacity"
  def aws_dynamodb_provisioned_write_capacity do
    :"aws.dynamodb.provisioned_write_capacity"
  end
  @doc """
  The value of the `ConsistentRead` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_consistent_read()
      :"aws.dynamodb.consistent_read"
  """
  @spec aws_dynamodb_consistent_read :: :"aws.dynamodb.consistent_read"
  def aws_dynamodb_consistent_read do
    :"aws.dynamodb.consistent_read"
  end
  @doc """
  The value of the `ProjectionExpression` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_projection()
      :"aws.dynamodb.projection"
  """
  @spec aws_dynamodb_projection :: :"aws.dynamodb.projection"
  def aws_dynamodb_projection do
    :"aws.dynamodb.projection"
  end
  @doc """
  The value of the `Limit` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_limit()
      :"aws.dynamodb.limit"
  """
  @spec aws_dynamodb_limit :: :"aws.dynamodb.limit"
  def aws_dynamodb_limit do
    :"aws.dynamodb.limit"
  end
  @doc """
  The value of the `AttributesToGet` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_attributes_to_get()
      :"aws.dynamodb.attributes_to_get"
  """
  @spec aws_dynamodb_attributes_to_get :: :"aws.dynamodb.attributes_to_get"
  def aws_dynamodb_attributes_to_get do
    :"aws.dynamodb.attributes_to_get"
  end
  @doc """
  The value of the `IndexName` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_index_name()
      :"aws.dynamodb.index_name"
  """
  @spec aws_dynamodb_index_name :: :"aws.dynamodb.index_name"
  def aws_dynamodb_index_name do
    :"aws.dynamodb.index_name"
  end
  @doc """
  The value of the `Select` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_select()
      :"aws.dynamodb.select"
  """
  @spec aws_dynamodb_select :: :"aws.dynamodb.select"
  def aws_dynamodb_select do
    :"aws.dynamodb.select"
  end
  @doc """
  The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_global_secondary_indexes()
      :"aws.dynamodb.global_secondary_indexes"
  """
  @spec aws_dynamodb_global_secondary_indexes :: :"aws.dynamodb.global_secondary_indexes"
  def aws_dynamodb_global_secondary_indexes do
    :"aws.dynamodb.global_secondary_indexes"
  end
  @doc """
  The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_local_secondary_indexes()
      :"aws.dynamodb.local_secondary_indexes"
  """
  @spec aws_dynamodb_local_secondary_indexes :: :"aws.dynamodb.local_secondary_indexes"
  def aws_dynamodb_local_secondary_indexes do
    :"aws.dynamodb.local_secondary_indexes"
  end
  @doc """
  The value of the `ExclusiveStartTableName` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_exclusive_start_table()
      :"aws.dynamodb.exclusive_start_table"
  """
  @spec aws_dynamodb_exclusive_start_table :: :"aws.dynamodb.exclusive_start_table"
  def aws_dynamodb_exclusive_start_table do
    :"aws.dynamodb.exclusive_start_table"
  end
  @doc """
  The the number of items in the `TableNames` response parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_table_count()
      :"aws.dynamodb.table_count"
  """
  @spec aws_dynamodb_table_count :: :"aws.dynamodb.table_count"
  def aws_dynamodb_table_count do
    :"aws.dynamodb.table_count"
  end
  @doc """
  The value of the `ScanIndexForward` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_scan_forward()
      :"aws.dynamodb.scan_forward"
  """
  @spec aws_dynamodb_scan_forward :: :"aws.dynamodb.scan_forward"
  def aws_dynamodb_scan_forward do
    :"aws.dynamodb.scan_forward"
  end
  @doc """
  The value of the `Segment` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_segment()
      :"aws.dynamodb.segment"
  """
  @spec aws_dynamodb_segment :: :"aws.dynamodb.segment"
  def aws_dynamodb_segment do
    :"aws.dynamodb.segment"
  end
  @doc """
  The value of the `TotalSegments` request parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_total_segments()
      :"aws.dynamodb.total_segments"
  """
  @spec aws_dynamodb_total_segments :: :"aws.dynamodb.total_segments"
  def aws_dynamodb_total_segments do
    :"aws.dynamodb.total_segments"
  end
  @doc """
  The value of the `Count` response parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_count()
      :"aws.dynamodb.count"
  """
  @spec aws_dynamodb_count :: :"aws.dynamodb.count"
  def aws_dynamodb_count do
    :"aws.dynamodb.count"
  end
  @doc """
  The value of the `ScannedCount` response parameter

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_scanned_count()
      :"aws.dynamodb.scanned_count"
  """
  @spec aws_dynamodb_scanned_count :: :"aws.dynamodb.scanned_count"
  def aws_dynamodb_scanned_count do
    :"aws.dynamodb.scanned_count"
  end
  @doc """
  The JSON-serialized value of each item in the `AttributeDefinitions` request field

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_attribute_definitions()
      :"aws.dynamodb.attribute_definitions"
  """
  @spec aws_dynamodb_attribute_definitions :: :"aws.dynamodb.attribute_definitions"
  def aws_dynamodb_attribute_definitions do
    :"aws.dynamodb.attribute_definitions"
  end
  @doc """
  The JSON-serialized value of each item in the the `GlobalSecondaryIndexUpdates` request field

      iex> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_global_secondary_index_updates()
      :"aws.dynamodb.global_secondary_index_updates"
  """
  @spec aws_dynamodb_global_secondary_index_updates :: :"aws.dynamodb.global_secondary_index_updates"
  def aws_dynamodb_global_secondary_index_updates do
    :"aws.dynamodb.global_secondary_index_updates"
  end
  @doc """
  The S3 bucket name the request refers to. Corresponds to the `--bucket` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations

  ### Notes

  The `bucket` attribute is applicable to all S3 operations that reference a bucket, i.e. that require the bucket name as a mandatory parameter.
  This applies to almost all S3 operations except `list-buckets`

      iex> OpenTelemetry.SemanticConventions.Trace.aws_s3_bucket()
      :"aws.s3.bucket"
  """
  @spec aws_s3_bucket :: :"aws.s3.bucket"
  def aws_s3_bucket do
    :"aws.s3.bucket"
  end
  @doc """
  The S3 object key the request refers to. Corresponds to the `--key` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations

  ### Notes

  The `key` attribute is applicable to all object-related S3 operations, i.e. that require the object key as a mandatory parameter.
  This applies in particular to the following operations:
  
  - [copy-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html)
  - [delete-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-object.html)
  - [get-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/get-object.html)
  - [head-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/head-object.html)
  - [put-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/put-object.html)
  - [restore-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/restore-object.html)
  - [select-object-content](https://docs.aws.amazon.com/cli/latest/reference/s3api/select-object-content.html)
  - [abort-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/abort-multipart-upload.html)
  - [complete-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/complete-multipart-upload.html)
  - [create-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/create-multipart-upload.html)
  - [list-parts](https://docs.aws.amazon.com/cli/latest/reference/s3api/list-parts.html)
  - [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)

      iex> OpenTelemetry.SemanticConventions.Trace.aws_s3_key()
      :"aws.s3.key"
  """
  @spec aws_s3_key :: :"aws.s3.key"
  def aws_s3_key do
    :"aws.s3.key"
  end
  @doc """
  The source object (in the form `bucket`/`key`) for the copy operation

  ### Notes

  The `copy_source` attribute applies to S3 copy operations and corresponds to the `--copy-source` parameter
  of the [copy-object operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html).
  This applies in particular to the following operations:
  
  - [copy-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)

      iex> OpenTelemetry.SemanticConventions.Trace.aws_s3_copy_source()
      :"aws.s3.copy_source"
  """
  @spec aws_s3_copy_source :: :"aws.s3.copy_source"
  def aws_s3_copy_source do
    :"aws.s3.copy_source"
  end
  @doc """
  Upload ID that identifies the multipart upload

  ### Notes

  The `upload_id` attribute applies to S3 multipart-upload operations and corresponds to the `--upload-id` parameter
  of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) multipart operations.
  This applies in particular to the following operations:
  
  - [abort-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/abort-multipart-upload.html)
  - [complete-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/complete-multipart-upload.html)
  - [list-parts](https://docs.aws.amazon.com/cli/latest/reference/s3api/list-parts.html)
  - [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)

      iex> OpenTelemetry.SemanticConventions.Trace.aws_s3_upload_id()
      :"aws.s3.upload_id"
  """
  @spec aws_s3_upload_id :: :"aws.s3.upload_id"
  def aws_s3_upload_id do
    :"aws.s3.upload_id"
  end
  @doc """
  The delete request container that specifies the objects to be deleted

  ### Notes

  The `delete` attribute is only applicable to the [delete-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-object.html) operation.
  The `delete` attribute corresponds to the `--delete` parameter of the
  [delete-objects operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-objects.html)

      iex> OpenTelemetry.SemanticConventions.Trace.aws_s3_delete()
      :"aws.s3.delete"
  """
  @spec aws_s3_delete :: :"aws.s3.delete"
  def aws_s3_delete do
    :"aws.s3.delete"
  end
  @doc """
  The part number of the part being uploaded in a multipart-upload operation. This is a positive integer between 1 and 10,000

  ### Notes

  The `part_number` attribute is only applicable to the [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  and [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html) operations.
  The `part_number` attribute corresponds to the `--part-number` parameter of the
  [upload-part operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)

      iex> OpenTelemetry.SemanticConventions.Trace.aws_s3_part_number()
      :"aws.s3.part_number"
  """
  @spec aws_s3_part_number :: :"aws.s3.part_number"
  def aws_s3_part_number do
    :"aws.s3.part_number"
  end
  @doc """
  The name of the operation being executed

      iex> OpenTelemetry.SemanticConventions.Trace.graphql_operation_name()
      :"graphql.operation.name"
  """
  @spec graphql_operation_name :: :"graphql.operation.name"
  def graphql_operation_name do
    :"graphql.operation.name"
  end
  @doc """
  The type of the operation being executed

      iex> OpenTelemetry.SemanticConventions.Trace.graphql_operation_type()
      :"graphql.operation.type"
  """
  @spec graphql_operation_type :: :"graphql.operation.type"
  def graphql_operation_type do
    :"graphql.operation.type"
  end
  @doc """
  The GraphQL document being executed

  ### Notes

  The value may be sanitized to exclude sensitive information

      iex> OpenTelemetry.SemanticConventions.Trace.graphql_document()
      :"graphql.document"
  """
  @spec graphql_document :: :"graphql.document"
  def graphql_document do
    :"graphql.document"
  end
  @doc """
  The message destination name

  ### Notes

  Destination name SHOULD uniquely identify a specific queue, topic or other entity within the broker. If
  the broker does not have such notion, the destination name SHOULD uniquely identify the broker

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_destination_name()
      :"messaging.destination.name"
  """
  @spec messaging_destination_name :: :"messaging.destination.name"
  def messaging_destination_name do
    :"messaging.destination.name"
  end
  @doc """
  Low cardinality representation of the messaging destination name

  ### Notes

  Destination names could be constructed from templates. An example would be a destination name involving a user name or product id. Although the destination name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_destination_template()
      :"messaging.destination.template"
  """
  @spec messaging_destination_template :: :"messaging.destination.template"
  def messaging_destination_template do
    :"messaging.destination.template"
  end
  @doc """
  A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_destination_temporary()
      :"messaging.destination.temporary"
  """
  @spec messaging_destination_temporary :: :"messaging.destination.temporary"
  def messaging_destination_temporary do
    :"messaging.destination.temporary"
  end
  @doc """
  A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name)

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_destination_anonymous()
      :"messaging.destination.anonymous"
  """
  @spec messaging_destination_anonymous :: :"messaging.destination.anonymous"
  def messaging_destination_anonymous do
    :"messaging.destination.anonymous"
  end
  @doc """
  The identifier for the consumer receiving a message. For Kafka, set it to `{messaging.kafka.consumer.group} - {messaging.kafka.client_id}`, if both are present, or only `messaging.kafka.consumer.group`. For brokers, such as RabbitMQ and Artemis, set it to the `client_id` of the client consuming the message

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_consumer_id()
      :"messaging.consumer.id"
  """
  @spec messaging_consumer_id :: :"messaging.consumer.id"
  def messaging_consumer_id do
    :"messaging.consumer.id"
  end
  @doc """
  The message source name

  ### Notes

  Source name SHOULD uniquely identify a specific queue, topic, or other entity within the broker. If
  the broker does not have such notion, the source name SHOULD uniquely identify the broker

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_source_name()
      :"messaging.source.name"
  """
  @spec messaging_source_name :: :"messaging.source.name"
  def messaging_source_name do
    :"messaging.source.name"
  end
  @doc """
  Low cardinality representation of the messaging source name

  ### Notes

  Source names could be constructed from templates. An example would be a source name involving a user name or product id. Although the source name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_source_template()
      :"messaging.source.template"
  """
  @spec messaging_source_template :: :"messaging.source.template"
  def messaging_source_template do
    :"messaging.source.template"
  end
  @doc """
  A boolean that is true if the message source is temporary and might not exist anymore after messages are processed

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_source_temporary()
      :"messaging.source.temporary"
  """
  @spec messaging_source_temporary :: :"messaging.source.temporary"
  def messaging_source_temporary do
    :"messaging.source.temporary"
  end
  @doc """
  A boolean that is true if the message source is anonymous (could be unnamed or have auto-generated name)

      iex> OpenTelemetry.SemanticConventions.Trace.messaging_source_anonymous()
      :"messaging.source.anonymous"
  """
  @spec messaging_source_anonymous :: :"messaging.source.anonymous"
  def messaging_source_anonymous do
    :"messaging.source.anonymous"
  end
  @doc """
  The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request

      iex> OpenTelemetry.SemanticConventions.Trace.rpc_grpc_status_code()
      :"rpc.grpc.status_code"
  """
  @spec rpc_grpc_status_code :: :"rpc.grpc.status_code"
  def rpc_grpc_status_code do
    :"rpc.grpc.status_code"
  end
  @doc """
  Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 does not specify this, the value can be omitted

      iex> OpenTelemetry.SemanticConventions.Trace.rpc_jsonrpc_version()
      :"rpc.jsonrpc.version"
  """
  @spec rpc_jsonrpc_version :: :"rpc.jsonrpc.version"
  def rpc_jsonrpc_version do
    :"rpc.jsonrpc.version"
  end
  @doc """
  `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification

      iex> OpenTelemetry.SemanticConventions.Trace.rpc_jsonrpc_request_id()
      :"rpc.jsonrpc.request_id"
  """
  @spec rpc_jsonrpc_request_id :: :"rpc.jsonrpc.request_id"
  def rpc_jsonrpc_request_id do
    :"rpc.jsonrpc.request_id"
  end
  @doc """
  `error.code` property of response if it is an error response

      iex> OpenTelemetry.SemanticConventions.Trace.rpc_jsonrpc_error_code()
      :"rpc.jsonrpc.error_code"
  """
  @spec rpc_jsonrpc_error_code :: :"rpc.jsonrpc.error_code"
  def rpc_jsonrpc_error_code do
    :"rpc.jsonrpc.error_code"
  end
  @doc """
  `error.message` property of response if it is an error response

      iex> OpenTelemetry.SemanticConventions.Trace.rpc_jsonrpc_error_message()
      :"rpc.jsonrpc.error_message"
  """
  @spec rpc_jsonrpc_error_message :: :"rpc.jsonrpc.error_message"
  def rpc_jsonrpc_error_message do
    :"rpc.jsonrpc.error_message"
  end
  @doc """
  The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values

      iex> OpenTelemetry.SemanticConventions.Trace.rpc_connect_rpc_error_code()
      :"rpc.connect_rpc.error_code"
  """
  @spec rpc_connect_rpc_error_code :: :"rpc.connect_rpc.error_code"
  def rpc_connect_rpc_error_code do
    :"rpc.connect_rpc.error_code"
  end
end