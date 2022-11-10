defmodule OpenTelemetry.SemanticConventions.Trace do
  @doc """
  The schema url for telemetry resources.

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.trace_schema_url()
      "https://opentelemetry.io/schemas/1.13.0"
  """
  @spec trace_schema_url :: String.t()
  defmacro trace_schema_url do
    "https://opentelemetry.io/schemas/1.13.0"
  end

  @doc """
  The full invoked ARN as provided on the `Context` passed to the function (`Lambda-Runtime-Invoked-Function-Arn` header on the `/runtime/invocation/next` applicable)

  ### Notes

  This may be different from `faas.id` if an alias is involved

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_lambda_invoked_arn()
      :"aws.lambda.invoked_arn"
  """
  @spec aws_lambda_invoked_arn :: :"aws.lambda.invoked_arn"
  defmacro aws_lambda_invoked_arn do
    :"aws.lambda.invoked_arn"
  end

  @doc """
  The [event_id](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#id) uniquely identifies the event

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.cloudevents_event_id()
      :"cloudevents.event_id"
  """
  @spec cloudevents_event_id :: :"cloudevents.event_id"
  defmacro cloudevents_event_id do
    :"cloudevents.event_id"
  end

  @doc """
  The [source](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#source-1) identifies the context in which an event happened

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.cloudevents_event_source()
      :"cloudevents.event_source"
  """
  @spec cloudevents_event_source :: :"cloudevents.event_source"
  defmacro cloudevents_event_source do
    :"cloudevents.event_source"
  end

  @doc """
  The [version of the CloudEvents specification](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#specversion) which the event uses

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.cloudevents_event_spec_version()
      :"cloudevents.event_spec_version"
  """
  @spec cloudevents_event_spec_version :: :"cloudevents.event_spec_version"
  defmacro cloudevents_event_spec_version do
    :"cloudevents.event_spec_version"
  end

  @doc """
  The [event_type](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#type) contains a value describing the type of event related to the originating occurrence

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.cloudevents_event_type()
      :"cloudevents.event_type"
  """
  @spec cloudevents_event_type :: :"cloudevents.event_type"
  defmacro cloudevents_event_type do
    :"cloudevents.event_type"
  end

  @doc """
  The [subject](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#subject) of the event in the context of the event producer (identified by source)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.cloudevents_event_subject()
      :"cloudevents.event_subject"
  """
  @spec cloudevents_event_subject :: :"cloudevents.event_subject"
  defmacro cloudevents_event_subject do
    :"cloudevents.event_subject"
  end

  @doc """
  Parent-child Reference type

  ### Notes

  The causal relationship between a child Span and a parent Span

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.opentracing_ref_type()
      :"opentracing.ref_type"
  """
  @spec opentracing_ref_type :: :"opentracing.ref_type"
  defmacro opentracing_ref_type do
    :"opentracing.ref_type"
  end

  @doc """
  An identifier for the database management system (DBMS) product being used. See below for a list of well-known identifiers

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_system()
      :"db.system"
  """
  @spec db_system :: :"db.system"
  defmacro db_system do
    :"db.system"
  end

  @doc """
  The connection string used to connect to the database. It is recommended to remove embedded credentials

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_connection_string()
      :"db.connection_string"
  """
  @spec db_connection_string :: :"db.connection_string"
  defmacro db_connection_string do
    :"db.connection_string"
  end

  @doc """
  Username for accessing the database

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_user()
      :"db.user"
  """
  @spec db_user :: :"db.user"
  defmacro db_user do
    :"db.user"
  end

  @doc """
  The fully-qualified class name of the [Java Database Connectivity (JDBC)](https://docs.oracle.com/javase/8/docs/technotes/guides/jdbc/) driver used to connect

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_jdbc_driver_classname()
      :"db.jdbc.driver_classname"
  """
  @spec db_jdbc_driver_classname :: :"db.jdbc.driver_classname"
  defmacro db_jdbc_driver_classname do
    :"db.jdbc.driver_classname"
  end

  @doc """
  This attribute is used to report the name of the database being accessed. For commands that switch the database, this should be set to the target database (even if the command fails)

  ### Notes

  In some SQL databases, the database name to be used is called "schema name". In case there are multiple layers that could be considered for database name (e.g. Oracle instance name and schema name), the database name to be used is the more specific layer (e.g. Oracle schema name)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_name()
      :"db.name"
  """
  @spec db_name :: :"db.name"
  defmacro db_name do
    :"db.name"
  end

  @doc """
  The database statement being executed

  ### Notes

  The value may be sanitized to exclude sensitive information

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_statement()
      :"db.statement"
  """
  @spec db_statement :: :"db.statement"
  defmacro db_statement do
    :"db.statement"
  end

  @doc """
  The name of the operation being executed, e.g. the [MongoDB command name](https://docs.mongodb.com/manual/reference/command/#database-operations) such as `findAndModify`, or the SQL keyword

  ### Notes

  When setting this to an SQL keyword, it is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if the operation name is provided by the library being instrumented. If the SQL statement has an ambiguous operation, or performs more than one operation, this value may be omitted

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_operation()
      :"db.operation"
  """
  @spec db_operation :: :"db.operation"
  defmacro db_operation do
    :"db.operation"
  end

  @doc """
  Name of the database host

  ### Notes

  `net.peer.name` SHOULD NOT be set if capturing it would require an extra DNS lookup

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_peer_name()
      :"net.peer.name"
  """
  @spec net_peer_name :: :"net.peer.name"
  defmacro net_peer_name do
    :"net.peer.name"
  end

  @doc """
  Logical remote port number

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_peer_port()
      :"net.peer.port"
  """
  @spec net_peer_port :: :"net.peer.port"
  defmacro net_peer_port do
    :"net.peer.port"
  end

  @doc """
  Remote socket peer address: IPv4 or IPv6 for internet protocols, path for local communication, [etc](https://man7.org/linux/man-pages/man7/address_families.7.html)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_sock_peer_addr()
      :"net.sock.peer.addr"
  """
  @spec net_sock_peer_addr :: :"net.sock.peer.addr"
  defmacro net_sock_peer_addr do
    :"net.sock.peer.addr"
  end

  @doc """
  Remote socket peer port

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_sock_peer_port()
      :"net.sock.peer.port"
  """
  @spec net_sock_peer_port :: :"net.sock.peer.port"
  defmacro net_sock_peer_port do
    :"net.sock.peer.port"
  end

  @doc """
  Protocol [address family](https://man7.org/linux/man-pages/man7/address_families.7.html) which is used for communication

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_sock_family()
      :"net.sock.family"
  """
  @spec net_sock_family :: :"net.sock.family"
  defmacro net_sock_family do
    :"net.sock.family"
  end

  @doc """
  Remote socket peer name

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_sock_peer_name()
      :"net.sock.peer.name"
  """
  @spec net_sock_peer_name :: :"net.sock.peer.name"
  defmacro net_sock_peer_name do
    :"net.sock.peer.name"
  end

  @doc """
  Transport protocol used. See note below

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_transport()
      :"net.transport"
  """
  @spec net_transport :: :"net.transport"
  defmacro net_transport do
    :"net.transport"
  end

  @doc """
  The Microsoft SQL Server [instance name](https://docs.microsoft.com/en-us/sql/connect/jdbc/building-the-connection-url?view=sql-server-ver15) connecting to. This name is used to determine the port of a named instance

  ### Notes

  If setting a `db.mssql.instance_name`, `net.peer.port` is no longer required (but still recommended if non-standard)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_mssql_instance_name()
      :"db.mssql.instance_name"
  """
  @spec db_mssql_instance_name :: :"db.mssql.instance_name"
  defmacro db_mssql_instance_name do
    :"db.mssql.instance_name"
  end

  @doc """
  The fetch size used for paging, i.e. how many rows will be returned at once

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_cassandra_page_size()
      :"db.cassandra.page_size"
  """
  @spec db_cassandra_page_size :: :"db.cassandra.page_size"
  defmacro db_cassandra_page_size do
    :"db.cassandra.page_size"
  end

  @doc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_cassandra_consistency_level()
      :"db.cassandra.consistency_level"
  """
  @spec db_cassandra_consistency_level :: :"db.cassandra.consistency_level"
  defmacro db_cassandra_consistency_level do
    :"db.cassandra.consistency_level"
  end

  @doc """
  The name of the primary table that the operation is acting upon, including the keyspace name (if applicable)

  ### Notes

  This mirrors the db.sql.table attribute but references cassandra rather than sql. It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_cassandra_table()
      :"db.cassandra.table"
  """
  @spec db_cassandra_table :: :"db.cassandra.table"
  defmacro db_cassandra_table do
    :"db.cassandra.table"
  end

  @doc """
  Whether or not the query is idempotent

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_cassandra_idempotence()
      :"db.cassandra.idempotence"
  """
  @spec db_cassandra_idempotence :: :"db.cassandra.idempotence"
  defmacro db_cassandra_idempotence do
    :"db.cassandra.idempotence"
  end

  @doc """
  The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_cassandra_speculative_execution_count()
      :"db.cassandra.speculative_execution_count"
  """
  @spec db_cassandra_speculative_execution_count :: :"db.cassandra.speculative_execution_count"
  defmacro db_cassandra_speculative_execution_count do
    :"db.cassandra.speculative_execution_count"
  end

  @doc """
  The ID of the coordinating node for a query

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_cassandra_coordinator_id()
      :"db.cassandra.coordinator.id"
  """
  @spec db_cassandra_coordinator_id :: :"db.cassandra.coordinator.id"
  defmacro db_cassandra_coordinator_id do
    :"db.cassandra.coordinator.id"
  end

  @doc """
  The data center of the coordinating node for a query

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_cassandra_coordinator_dc()
      :"db.cassandra.coordinator.dc"
  """
  @spec db_cassandra_coordinator_dc :: :"db.cassandra.coordinator.dc"
  defmacro db_cassandra_coordinator_dc do
    :"db.cassandra.coordinator.dc"
  end

  @doc """
  The index of the database being accessed as used in the [`SELECT` command](https://redis.io/commands/select), provided as an integer. To be used instead of the generic `db.name` attribute

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_redis_database_index()
      :"db.redis.database_index"
  """
  @spec db_redis_database_index :: :"db.redis.database_index"
  defmacro db_redis_database_index do
    :"db.redis.database_index"
  end

  @doc """
  The collection being accessed within the database stated in `db.name`

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_mongodb_collection()
      :"db.mongodb.collection"
  """
  @spec db_mongodb_collection :: :"db.mongodb.collection"
  defmacro db_mongodb_collection do
    :"db.mongodb.collection"
  end

  @doc """
  The name of the primary table that the operation is acting upon, including the database name (if applicable)

  ### Notes

  It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.db_sql_table()
      :"db.sql.table"
  """
  @spec db_sql_table :: :"db.sql.table"
  defmacro db_sql_table do
    :"db.sql.table"
  end

  @doc """
  The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.exception_type()
      :"exception.type"
  """
  @spec exception_type :: :"exception.type"
  defmacro exception_type do
    :"exception.type"
  end

  @doc """
  The exception message

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.exception_message()
      :"exception.message"
  """
  @spec exception_message :: :"exception.message"
  defmacro exception_message do
    :"exception.message"
  end

  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.exception_stacktrace()
      :"exception.stacktrace"
  """
  @spec exception_stacktrace :: :"exception.stacktrace"
  defmacro exception_stacktrace do
    :"exception.stacktrace"
  end

  @doc """
  SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span

  ### Notes

  An exception is considered to have escaped (or left) the scope of a span,
  if that span is ended while the exception is still logically "in flight".
  This may be actually "in flight" in some languages (e.g. if the exception
  is passed to a Context manager's `__exit__` method in Python) but will
  usually be caught at the point of recording the exception in most languages.

  It is usually not possible to determine at the point where an exception is thrown
  whether it will escape the scope of a span.
  However, it is trivial to know that an exception
  will escape, if one checks for an active exception just before ending the span,
  as done in the [example above](#recording-an-exception).

  It follows that an exception may still escape the scope of the span
  even if the `exception.escaped` attribute was not set or set to false,
  since the event might have been recorded at a time where it was not
  clear whether the exception will escape

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.exception_escaped()
      :"exception.escaped"
  """
  @spec exception_escaped :: :"exception.escaped"
  defmacro exception_escaped do
    :"exception.escaped"
  end

  @doc """
  Type of the trigger which caused this function execution

  ### Notes

  For the server/consumer span on the incoming side,
  `faas.trigger` MUST be set.

  Clients invoking FaaS instances usually cannot set `faas.trigger`,
  since they would typically need to look in the payload to determine
  the event type. If clients set it, it should be the same as the
  trigger that corresponding incoming would have (i.e., this has
  nothing to do with the underlying transport used to make the API
  call to invoke the lambda, which is often HTTP)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_trigger()
      :"faas.trigger"
  """
  @spec faas_trigger :: :"faas.trigger"
  defmacro faas_trigger do
    :"faas.trigger"
  end

  @doc """
  The execution ID of the current function execution

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_execution()
      :"faas.execution"
  """
  @spec faas_execution :: :"faas.execution"
  defmacro faas_execution do
    :"faas.execution"
  end

  @doc """
  The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_document_collection()
      :"faas.document.collection"
  """
  @spec faas_document_collection :: :"faas.document.collection"
  defmacro faas_document_collection do
    :"faas.document.collection"
  end

  @doc """
  Describes the type of the operation that was performed on the data

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_document_operation()
      :"faas.document.operation"
  """
  @spec faas_document_operation :: :"faas.document.operation"
  defmacro faas_document_operation do
    :"faas.document.operation"
  end

  @doc """
  A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_document_time()
      :"faas.document.time"
  """
  @spec faas_document_time :: :"faas.document.time"
  defmacro faas_document_time do
    :"faas.document.time"
  end

  @doc """
  The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_document_name()
      :"faas.document.name"
  """
  @spec faas_document_name :: :"faas.document.name"
  defmacro faas_document_name do
    :"faas.document.name"
  end

  @doc """
  HTTP request method

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_method()
      :"http.method"
  """
  @spec http_method :: :"http.method"
  defmacro http_method do
    :"http.method"
  end

  @doc """
  [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_status_code()
      :"http.status_code"
  """
  @spec http_status_code :: :"http.status_code"
  defmacro http_status_code do
    :"http.status_code"
  end

  @doc """
  Kind of HTTP protocol used

  ### Notes

  If `net.transport` is not specified, it can be assumed to be `IP.TCP` except if `http.flavor` is `QUIC`, in which case `IP.UDP` is assumed

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_flavor()
      :"http.flavor"
  """
  @spec http_flavor :: :"http.flavor"
  defmacro http_flavor do
    :"http.flavor"
  end

  @doc """
  Value of the [HTTP User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#field.user-agent) header sent by the client

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_user_agent()
      :"http.user_agent"
  """
  @spec http_user_agent :: :"http.user_agent"
  defmacro http_user_agent do
    :"http.user_agent"
  end

  @doc """
  The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_request_content_length()
      :"http.request_content_length"
  """
  @spec http_request_content_length :: :"http.request_content_length"
  defmacro http_request_content_length do
    :"http.request_content_length"
  end

  @doc """
  The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_response_content_length()
      :"http.response_content_length"
  """
  @spec http_response_content_length :: :"http.response_content_length"
  defmacro http_response_content_length do
    :"http.response_content_length"
  end

  @doc """
  The URI scheme identifying the used protocol

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_scheme()
      :"http.scheme"
  """
  @spec http_scheme :: :"http.scheme"
  defmacro http_scheme do
    :"http.scheme"
  end

  @doc """
  The full request target as passed in a HTTP request line or equivalent

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_target()
      :"http.target"
  """
  @spec http_target :: :"http.target"
  defmacro http_target do
    :"http.target"
  end

  @doc """
  The matched route (path template in the format used by the respective server framework). See note below

  ### Notes

  'http.route' MUST NOT be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can NOT substitute it

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_route()
      :"http.route"
  """
  @spec http_route :: :"http.route"
  defmacro http_route do
    :"http.route"
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

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_client_ip()
      :"http.client_ip"
  """
  @spec http_client_ip :: :"http.client_ip"
  defmacro http_client_ip do
    :"http.client_ip"
  end

  @doc """
  Name of the local HTTP server that received the request

  ### Notes

  Determined by using the first of the following that applies

  - The [primary server name](#http-server-definitions) of the matched virtual host. MUST only
    include host identifier.
  - Host identifier of the [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource)
    if it's sent in absolute-form.
  - Host identifier of the `Host` header

  SHOULD NOT be set if only IP address is available and capturing name would require a reverse DNS lookup

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_host_name()
      :"net.host.name"
  """
  @spec net_host_name :: :"net.host.name"
  defmacro net_host_name do
    :"net.host.name"
  end

  @doc """
  Port of the local HTTP server that received the request

  ### Notes

  Determined by using the first of the following that applies

  - Port identifier of the [primary server host](#http-server-definitions) of the matched virtual host.
  - Port identifier of the [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource)
    if it's sent in absolute-form.
  - Port identifier of the `Host` header

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_host_port()
      :"net.host.port"
  """
  @spec net_host_port :: :"net.host.port"
  defmacro net_host_port do
    :"net.host.port"
  end

  @doc """
  Local socket address. Useful in case of a multi-IP host

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_sock_host_addr()
      :"net.sock.host.addr"
  """
  @spec net_sock_host_addr :: :"net.sock.host.addr"
  defmacro net_sock_host_addr do
    :"net.sock.host.addr"
  end

  @doc """
  Local socket port number

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_sock_host_port()
      :"net.sock.host.port"
  """
  @spec net_sock_host_port :: :"net.sock.host.port"
  defmacro net_sock_host_port do
    :"net.sock.host.port"
  end

  @doc """
  Application layer protocol used. The value SHOULD be normalized to lowercase

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_app_protocol_name()
      :"net.app.protocol.name"
  """
  @spec net_app_protocol_name :: :"net.app.protocol.name"
  defmacro net_app_protocol_name do
    :"net.app.protocol.name"
  end

  @doc """
  Version of the application layer protocol used. See note below

  ### Notes

  `net.app.protocol.version` refers to the version of the protocol used and might be different from the protocol client's version. If the HTTP client used has a version of `0.27.2`, but sends HTTP version `1.1`, this attribute should be set to `1.1`

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_app_protocol_version()
      :"net.app.protocol.version"
  """
  @spec net_app_protocol_version :: :"net.app.protocol.version"
  defmacro net_app_protocol_version do
    :"net.app.protocol.version"
  end

  @doc """
  The internet connection type currently being used by the host

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_host_connection_type()
      :"net.host.connection.type"
  """
  @spec net_host_connection_type :: :"net.host.connection.type"
  defmacro net_host_connection_type do
    :"net.host.connection.type"
  end

  @doc """
  This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_host_connection_subtype()
      :"net.host.connection.subtype"
  """
  @spec net_host_connection_subtype :: :"net.host.connection.subtype"
  defmacro net_host_connection_subtype do
    :"net.host.connection.subtype"
  end

  @doc """
  The name of the mobile carrier

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_host_carrier_name()
      :"net.host.carrier.name"
  """
  @spec net_host_carrier_name :: :"net.host.carrier.name"
  defmacro net_host_carrier_name do
    :"net.host.carrier.name"
  end

  @doc """
  The mobile carrier country code

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_host_carrier_mcc()
      :"net.host.carrier.mcc"
  """
  @spec net_host_carrier_mcc :: :"net.host.carrier.mcc"
  defmacro net_host_carrier_mcc do
    :"net.host.carrier.mcc"
  end

  @doc """
  The mobile carrier network code

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_host_carrier_mnc()
      :"net.host.carrier.mnc"
  """
  @spec net_host_carrier_mnc :: :"net.host.carrier.mnc"
  defmacro net_host_carrier_mnc do
    :"net.host.carrier.mnc"
  end

  @doc """
  The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.net_host_carrier_icc()
      :"net.host.carrier.icc"
  """
  @spec net_host_carrier_icc :: :"net.host.carrier.icc"
  defmacro net_host_carrier_icc do
    :"net.host.carrier.icc"
  end

  @doc """
  A string identifying the messaging system

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_system()
      :"messaging.system"
  """
  @spec messaging_system :: :"messaging.system"
  defmacro messaging_system do
    :"messaging.system"
  end

  @doc """
  The message destination name. This might be equal to the span name but is required nevertheless

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_destination()
      :"messaging.destination"
  """
  @spec messaging_destination :: :"messaging.destination"
  defmacro messaging_destination do
    :"messaging.destination"
  end

  @doc """
  The kind of message destination

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_destination_kind()
      :"messaging.destination_kind"
  """
  @spec messaging_destination_kind :: :"messaging.destination_kind"
  defmacro messaging_destination_kind do
    :"messaging.destination_kind"
  end

  @doc """
  A boolean that is true if the message destination is temporary

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_temp_destination()
      :"messaging.temp_destination"
  """
  @spec messaging_temp_destination :: :"messaging.temp_destination"
  defmacro messaging_temp_destination do
    :"messaging.temp_destination"
  end

  @doc """
  The name of the transport protocol

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_protocol()
      :"messaging.protocol"
  """
  @spec messaging_protocol :: :"messaging.protocol"
  defmacro messaging_protocol do
    :"messaging.protocol"
  end

  @doc """
  The version of the transport protocol

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_protocol_version()
      :"messaging.protocol_version"
  """
  @spec messaging_protocol_version :: :"messaging.protocol_version"
  defmacro messaging_protocol_version do
    :"messaging.protocol_version"
  end

  @doc """
  Connection string

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_url()
      :"messaging.url"
  """
  @spec messaging_url :: :"messaging.url"
  defmacro messaging_url do
    :"messaging.url"
  end

  @doc """
  A value used by the messaging system as an identifier for the message, represented as a string

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_message_id()
      :"messaging.message_id"
  """
  @spec messaging_message_id :: :"messaging.message_id"
  defmacro messaging_message_id do
    :"messaging.message_id"
  end

  @doc """
  The [conversation ID](#conversations) identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID"

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_conversation_id()
      :"messaging.conversation_id"
  """
  @spec messaging_conversation_id :: :"messaging.conversation_id"
  defmacro messaging_conversation_id do
    :"messaging.conversation_id"
  end

  @doc """
  The (uncompressed) size of the message payload in bytes. Also use this attribute if it is unknown whether the compressed or uncompressed payload size is reported

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_message_payload_size_bytes()
      :"messaging.message_payload_size_bytes"
  """
  @spec messaging_message_payload_size_bytes :: :"messaging.message_payload_size_bytes"
  defmacro messaging_message_payload_size_bytes do
    :"messaging.message_payload_size_bytes"
  end

  @doc """
  The compressed size of the message payload in bytes

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_message_payload_compressed_size_bytes()
      :"messaging.message_payload_compressed_size_bytes"
  """
  @spec messaging_message_payload_compressed_size_bytes ::
          :"messaging.message_payload_compressed_size_bytes"
  defmacro messaging_message_payload_compressed_size_bytes do
    :"messaging.message_payload_compressed_size_bytes"
  end

  @doc """
  A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_time()
      :"faas.time"
  """
  @spec faas_time :: :"faas.time"
  defmacro faas_time do
    :"faas.time"
  end

  @doc """
  A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_cron()
      :"faas.cron"
  """
  @spec faas_cron :: :"faas.cron"
  defmacro faas_cron do
    :"faas.cron"
  end

  @doc """
  A boolean that is true if the serverless function is executed for the first time (aka cold-start)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_coldstart()
      :"faas.coldstart"
  """
  @spec faas_coldstart :: :"faas.coldstart"
  defmacro faas_coldstart do
    :"faas.coldstart"
  end

  @doc """
  The name of the invoked function

  ### Notes

  SHOULD be equal to the `faas.name` resource attribute of the invoked function

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_invoked_name()
      :"faas.invoked_name"
  """
  @spec faas_invoked_name :: :"faas.invoked_name"
  defmacro faas_invoked_name do
    :"faas.invoked_name"
  end

  @doc """
  The cloud provider of the invoked function

  ### Notes

  SHOULD be equal to the `cloud.provider` resource attribute of the invoked function

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_invoked_provider()
      :"faas.invoked_provider"
  """
  @spec faas_invoked_provider :: :"faas.invoked_provider"
  defmacro faas_invoked_provider do
    :"faas.invoked_provider"
  end

  @doc """
  The cloud region of the invoked function

  ### Notes

  SHOULD be equal to the `cloud.region` resource attribute of the invoked function

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.faas_invoked_region()
      :"faas.invoked_region"
  """
  @spec faas_invoked_region :: :"faas.invoked_region"
  defmacro faas_invoked_region do
    :"faas.invoked_region"
  end

  @doc """
  The [`service.name`](../../resource/semantic_conventions/README.md#service) of the remote service. SHOULD be equal to the actual `service.name` resource attribute of the remote service if any

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.peer_service()
      :"peer.service"
  """
  @spec peer_service :: :"peer.service"
  defmacro peer_service do
    :"peer.service"
  end

  @doc """
  Username or client_id extracted from the access token or [Authorization](https://tools.ietf.org/html/rfc7235#section-4.2) header in the inbound request from outside the system

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.enduser_id()
      :"enduser.id"
  """
  @spec enduser_id :: :"enduser.id"
  defmacro enduser_id do
    :"enduser.id"
  end

  @doc """
  Actual/assumed role the client is making the request under extracted from token or application security context

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.enduser_role()
      :"enduser.role"
  """
  @spec enduser_role :: :"enduser.role"
  defmacro enduser_role do
    :"enduser.role"
  end

  @doc """
  Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an [OAuth 2.0 Access Token](https://tools.ietf.org/html/rfc6749#section-3.3) or an attribute value in a [SAML 2.0 Assertion](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.enduser_scope()
      :"enduser.scope"
  """
  @spec enduser_scope :: :"enduser.scope"
  defmacro enduser_scope do
    :"enduser.scope"
  end

  @doc """
  Current "managed" thread ID (as opposed to OS thread ID)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.thread_id()
      :"thread.id"
  """
  @spec thread_id :: :"thread.id"
  defmacro thread_id do
    :"thread.id"
  end

  @doc """
  Current thread name

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.thread_name()
      :"thread.name"
  """
  @spec thread_name :: :"thread.name"
  defmacro thread_name do
    :"thread.name"
  end

  @doc """
  The method or function name, or equivalent (usually rightmost part of the code unit's name)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.code_function()
      :"code.function"
  """
  @spec code_function :: :"code.function"
  defmacro code_function do
    :"code.function"
  end

  @doc """
  The "namespace" within which `code.function` is defined. Usually the qualified class or module name, such that `code.namespace` + some separator + `code.function` form a unique identifier for the code unit

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.code_namespace()
      :"code.namespace"
  """
  @spec code_namespace :: :"code.namespace"
  defmacro code_namespace do
    :"code.namespace"
  end

  @doc """
  The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.code_filepath()
      :"code.filepath"
  """
  @spec code_filepath :: :"code.filepath"
  defmacro code_filepath do
    :"code.filepath"
  end

  @doc """
  The line number in `code.filepath` best representing the operation. It SHOULD point within the code unit named in `code.function`

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.code_lineno()
      :"code.lineno"
  """
  @spec code_lineno :: :"code.lineno"
  defmacro code_lineno do
    :"code.lineno"
  end

  @doc """
  Full HTTP request URL in the form `scheme://host[:port]/path?query[#fragment]`. Usually the fragment is not transmitted over HTTP, but if it is known, it should be included nevertheless

  ### Notes

  `http.url` MUST NOT contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case the attribute's value should be `https://www.example.com/`

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_url()
      :"http.url"
  """
  @spec http_url :: :"http.url"
  defmacro http_url do
    :"http.url"
  end

  @doc """
  The ordinal number of request re-sending attempt

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.http_retry_count()
      :"http.retry_count"
  """
  @spec http_retry_count :: :"http.retry_count"
  defmacro http_retry_count do
    :"http.retry_count"
  end

  @doc """
  The value `aws-api`

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.rpc_system()
      :"rpc.system"
  """
  @spec rpc_system :: :"rpc.system"
  defmacro rpc_system do
    :"rpc.system"
  end

  @doc """
  The name of the service to which a request is made, as returned by the AWS SDK

  ### Notes

  This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.rpc_service()
      :"rpc.service"
  """
  @spec rpc_service :: :"rpc.service"
  defmacro rpc_service do
    :"rpc.service"
  end

  @doc """
  The name of the operation corresponding to the request, as returned by the AWS SDK

  ### Notes

  This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side)

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.rpc_method()
      :"rpc.method"
  """
  @spec rpc_method :: :"rpc.method"
  defmacro rpc_method do
    :"rpc.method"
  end

  @doc """
  The keys in the `RequestItems` object field

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_table_names()
      :"aws.dynamodb.table_names"
  """
  @spec aws_dynamodb_table_names :: :"aws.dynamodb.table_names"
  defmacro aws_dynamodb_table_names do
    :"aws.dynamodb.table_names"
  end

  @doc """
  The JSON-serialized value of each item in the `ConsumedCapacity` response field

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_consumed_capacity()
      :"aws.dynamodb.consumed_capacity"
  """
  @spec aws_dynamodb_consumed_capacity :: :"aws.dynamodb.consumed_capacity"
  defmacro aws_dynamodb_consumed_capacity do
    :"aws.dynamodb.consumed_capacity"
  end

  @doc """
  The JSON-serialized value of the `ItemCollectionMetrics` response field

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_item_collection_metrics()
      :"aws.dynamodb.item_collection_metrics"
  """
  @spec aws_dynamodb_item_collection_metrics :: :"aws.dynamodb.item_collection_metrics"
  defmacro aws_dynamodb_item_collection_metrics do
    :"aws.dynamodb.item_collection_metrics"
  end

  @doc """
  The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_provisioned_read_capacity()
      :"aws.dynamodb.provisioned_read_capacity"
  """
  @spec aws_dynamodb_provisioned_read_capacity :: :"aws.dynamodb.provisioned_read_capacity"
  defmacro aws_dynamodb_provisioned_read_capacity do
    :"aws.dynamodb.provisioned_read_capacity"
  end

  @doc """
  The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_provisioned_write_capacity()
      :"aws.dynamodb.provisioned_write_capacity"
  """
  @spec aws_dynamodb_provisioned_write_capacity :: :"aws.dynamodb.provisioned_write_capacity"
  defmacro aws_dynamodb_provisioned_write_capacity do
    :"aws.dynamodb.provisioned_write_capacity"
  end

  @doc """
  The value of the `ConsistentRead` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_consistent_read()
      :"aws.dynamodb.consistent_read"
  """
  @spec aws_dynamodb_consistent_read :: :"aws.dynamodb.consistent_read"
  defmacro aws_dynamodb_consistent_read do
    :"aws.dynamodb.consistent_read"
  end

  @doc """
  The value of the `ProjectionExpression` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_projection()
      :"aws.dynamodb.projection"
  """
  @spec aws_dynamodb_projection :: :"aws.dynamodb.projection"
  defmacro aws_dynamodb_projection do
    :"aws.dynamodb.projection"
  end

  @doc """
  The value of the `Limit` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_limit()
      :"aws.dynamodb.limit"
  """
  @spec aws_dynamodb_limit :: :"aws.dynamodb.limit"
  defmacro aws_dynamodb_limit do
    :"aws.dynamodb.limit"
  end

  @doc """
  The value of the `AttributesToGet` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_attributes_to_get()
      :"aws.dynamodb.attributes_to_get"
  """
  @spec aws_dynamodb_attributes_to_get :: :"aws.dynamodb.attributes_to_get"
  defmacro aws_dynamodb_attributes_to_get do
    :"aws.dynamodb.attributes_to_get"
  end

  @doc """
  The value of the `IndexName` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_index_name()
      :"aws.dynamodb.index_name"
  """
  @spec aws_dynamodb_index_name :: :"aws.dynamodb.index_name"
  defmacro aws_dynamodb_index_name do
    :"aws.dynamodb.index_name"
  end

  @doc """
  The value of the `Select` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_select()
      :"aws.dynamodb.select"
  """
  @spec aws_dynamodb_select :: :"aws.dynamodb.select"
  defmacro aws_dynamodb_select do
    :"aws.dynamodb.select"
  end

  @doc """
  The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_global_secondary_indexes()
      :"aws.dynamodb.global_secondary_indexes"
  """
  @spec aws_dynamodb_global_secondary_indexes :: :"aws.dynamodb.global_secondary_indexes"
  defmacro aws_dynamodb_global_secondary_indexes do
    :"aws.dynamodb.global_secondary_indexes"
  end

  @doc """
  The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_local_secondary_indexes()
      :"aws.dynamodb.local_secondary_indexes"
  """
  @spec aws_dynamodb_local_secondary_indexes :: :"aws.dynamodb.local_secondary_indexes"
  defmacro aws_dynamodb_local_secondary_indexes do
    :"aws.dynamodb.local_secondary_indexes"
  end

  @doc """
  The value of the `ExclusiveStartTableName` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_exclusive_start_table()
      :"aws.dynamodb.exclusive_start_table"
  """
  @spec aws_dynamodb_exclusive_start_table :: :"aws.dynamodb.exclusive_start_table"
  defmacro aws_dynamodb_exclusive_start_table do
    :"aws.dynamodb.exclusive_start_table"
  end

  @doc """
  The the number of items in the `TableNames` response parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_table_count()
      :"aws.dynamodb.table_count"
  """
  @spec aws_dynamodb_table_count :: :"aws.dynamodb.table_count"
  defmacro aws_dynamodb_table_count do
    :"aws.dynamodb.table_count"
  end

  @doc """
  The value of the `ScanIndexForward` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_scan_forward()
      :"aws.dynamodb.scan_forward"
  """
  @spec aws_dynamodb_scan_forward :: :"aws.dynamodb.scan_forward"
  defmacro aws_dynamodb_scan_forward do
    :"aws.dynamodb.scan_forward"
  end

  @doc """
  The value of the `Segment` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_segment()
      :"aws.dynamodb.segment"
  """
  @spec aws_dynamodb_segment :: :"aws.dynamodb.segment"
  defmacro aws_dynamodb_segment do
    :"aws.dynamodb.segment"
  end

  @doc """
  The value of the `TotalSegments` request parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_total_segments()
      :"aws.dynamodb.total_segments"
  """
  @spec aws_dynamodb_total_segments :: :"aws.dynamodb.total_segments"
  defmacro aws_dynamodb_total_segments do
    :"aws.dynamodb.total_segments"
  end

  @doc """
  The value of the `Count` response parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_count()
      :"aws.dynamodb.count"
  """
  @spec aws_dynamodb_count :: :"aws.dynamodb.count"
  defmacro aws_dynamodb_count do
    :"aws.dynamodb.count"
  end

  @doc """
  The value of the `ScannedCount` response parameter

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_scanned_count()
      :"aws.dynamodb.scanned_count"
  """
  @spec aws_dynamodb_scanned_count :: :"aws.dynamodb.scanned_count"
  defmacro aws_dynamodb_scanned_count do
    :"aws.dynamodb.scanned_count"
  end

  @doc """
  The JSON-serialized value of each item in the `AttributeDefinitions` request field

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_attribute_definitions()
      :"aws.dynamodb.attribute_definitions"
  """
  @spec aws_dynamodb_attribute_definitions :: :"aws.dynamodb.attribute_definitions"
  defmacro aws_dynamodb_attribute_definitions do
    :"aws.dynamodb.attribute_definitions"
  end

  @doc """
  The JSON-serialized value of each item in the the `GlobalSecondaryIndexUpdates` request field

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.aws_dynamodb_global_secondary_index_updates()
      :"aws.dynamodb.global_secondary_index_updates"
  """
  @spec aws_dynamodb_global_secondary_index_updates ::
          :"aws.dynamodb.global_secondary_index_updates"
  defmacro aws_dynamodb_global_secondary_index_updates do
    :"aws.dynamodb.global_secondary_index_updates"
  end

  @doc """
  The name of the operation being executed

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.graphql_operation_name()
      :"graphql.operation.name"
  """
  @spec graphql_operation_name :: :"graphql.operation.name"
  defmacro graphql_operation_name do
    :"graphql.operation.name"
  end

  @doc """
  The type of the operation being executed

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.graphql_operation_type()
      :"graphql.operation.type"
  """
  @spec graphql_operation_type :: :"graphql.operation.type"
  defmacro graphql_operation_type do
    :"graphql.operation.type"
  end

  @doc """
  The GraphQL document being executed

  ### Notes

  The value may be sanitized to exclude sensitive information

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.graphql_document()
      :"graphql.document"
  """
  @spec graphql_document :: :"graphql.document"
  defmacro graphql_document do
    :"graphql.document"
  end

  @doc """
  A string identifying the kind of message consumption as defined in the [Operation names](#operation-names) section above. If the operation is "send", this attribute MUST NOT be set, since the operation can be inferred from the span kind in that case

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_operation()
      :"messaging.operation"
  """
  @spec messaging_operation :: :"messaging.operation"
  defmacro messaging_operation do
    :"messaging.operation"
  end

  @doc """
  The identifier for the consumer receiving a message. For Kafka, set it to `{messaging.kafka.consumer_group} - {messaging.kafka.client_id}`, if both are present, or only `messaging.kafka.consumer_group`. For brokers, such as RabbitMQ and Artemis, set it to the `client_id` of the client consuming the message

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_consumer_id()
      :"messaging.consumer_id"
  """
  @spec messaging_consumer_id :: :"messaging.consumer_id"
  defmacro messaging_consumer_id do
    :"messaging.consumer_id"
  end

  @doc """
  RabbitMQ message routing key

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_rabbitmq_routing_key()
      :"messaging.rabbitmq.routing_key"
  """
  @spec messaging_rabbitmq_routing_key :: :"messaging.rabbitmq.routing_key"
  defmacro messaging_rabbitmq_routing_key do
    :"messaging.rabbitmq.routing_key"
  end

  @doc """
  Message keys in Kafka are used for grouping alike messages to ensure they're processed on the same partition. They differ from `messaging.message_id` in that they're not unique. If the key is `null`, the attribute MUST NOT be set

  ### Notes

  If the key type is not string, it's string representation has to be supplied for the attribute. If the key has no unambiguous, canonical string form, don't include its value

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_kafka_message_key()
      :"messaging.kafka.message_key"
  """
  @spec messaging_kafka_message_key :: :"messaging.kafka.message_key"
  defmacro messaging_kafka_message_key do
    :"messaging.kafka.message_key"
  end

  @doc """
  Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_kafka_consumer_group()
      :"messaging.kafka.consumer_group"
  """
  @spec messaging_kafka_consumer_group :: :"messaging.kafka.consumer_group"
  defmacro messaging_kafka_consumer_group do
    :"messaging.kafka.consumer_group"
  end

  @doc """
  Client Id for the Consumer or Producer that is handling the message

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_kafka_client_id()
      :"messaging.kafka.client_id"
  """
  @spec messaging_kafka_client_id :: :"messaging.kafka.client_id"
  defmacro messaging_kafka_client_id do
    :"messaging.kafka.client_id"
  end

  @doc """
  Partition the message is sent to

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_kafka_partition()
      :"messaging.kafka.partition"
  """
  @spec messaging_kafka_partition :: :"messaging.kafka.partition"
  defmacro messaging_kafka_partition do
    :"messaging.kafka.partition"
  end

  @doc """
  A boolean that is true if the message is a tombstone

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_kafka_tombstone()
      :"messaging.kafka.tombstone"
  """
  @spec messaging_kafka_tombstone :: :"messaging.kafka.tombstone"
  defmacro messaging_kafka_tombstone do
    :"messaging.kafka.tombstone"
  end

  @doc """
  Namespace of RocketMQ resources, resources in different namespaces are individual

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_rocketmq_namespace()
      :"messaging.rocketmq.namespace"
  """
  @spec messaging_rocketmq_namespace :: :"messaging.rocketmq.namespace"
  defmacro messaging_rocketmq_namespace do
    :"messaging.rocketmq.namespace"
  end

  @doc """
  Name of the RocketMQ producer/consumer group that is handling the message. The client type is identified by the SpanKind

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_rocketmq_client_group()
      :"messaging.rocketmq.client_group"
  """
  @spec messaging_rocketmq_client_group :: :"messaging.rocketmq.client_group"
  defmacro messaging_rocketmq_client_group do
    :"messaging.rocketmq.client_group"
  end

  @doc """
  The unique identifier for each client

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_rocketmq_client_id()
      :"messaging.rocketmq.client_id"
  """
  @spec messaging_rocketmq_client_id :: :"messaging.rocketmq.client_id"
  defmacro messaging_rocketmq_client_id do
    :"messaging.rocketmq.client_id"
  end

  @doc """
  Type of message

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_rocketmq_message_type()
      :"messaging.rocketmq.message_type"
  """
  @spec messaging_rocketmq_message_type :: :"messaging.rocketmq.message_type"
  defmacro messaging_rocketmq_message_type do
    :"messaging.rocketmq.message_type"
  end

  @doc """
  The secondary classifier of message besides topic

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_rocketmq_message_tag()
      :"messaging.rocketmq.message_tag"
  """
  @spec messaging_rocketmq_message_tag :: :"messaging.rocketmq.message_tag"
  defmacro messaging_rocketmq_message_tag do
    :"messaging.rocketmq.message_tag"
  end

  @doc """
  Key(s) of message, another way to mark message besides message id

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_rocketmq_message_keys()
      :"messaging.rocketmq.message_keys"
  """
  @spec messaging_rocketmq_message_keys :: :"messaging.rocketmq.message_keys"
  defmacro messaging_rocketmq_message_keys do
    :"messaging.rocketmq.message_keys"
  end

  @doc """
  Model of message consumption. This only applies to consumer spans

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.messaging_rocketmq_consumption_model()
      :"messaging.rocketmq.consumption_model"
  """
  @spec messaging_rocketmq_consumption_model :: :"messaging.rocketmq.consumption_model"
  defmacro messaging_rocketmq_consumption_model do
    :"messaging.rocketmq.consumption_model"
  end

  @doc """
  The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.rpc_grpc_status_code()
      :"rpc.grpc.status_code"
  """
  @spec rpc_grpc_status_code :: :"rpc.grpc.status_code"
  defmacro rpc_grpc_status_code do
    :"rpc.grpc.status_code"
  end

  @doc """
  Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 does not specify this, the value can be omitted

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.rpc_jsonrpc_version()
      :"rpc.jsonrpc.version"
  """
  @spec rpc_jsonrpc_version :: :"rpc.jsonrpc.version"
  defmacro rpc_jsonrpc_version do
    :"rpc.jsonrpc.version"
  end

  @doc """
  `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.rpc_jsonrpc_request_id()
      :"rpc.jsonrpc.request_id"
  """
  @spec rpc_jsonrpc_request_id :: :"rpc.jsonrpc.request_id"
  defmacro rpc_jsonrpc_request_id do
    :"rpc.jsonrpc.request_id"
  end

  @doc """
  `error.code` property of response if it is an error response

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.rpc_jsonrpc_error_code()
      :"rpc.jsonrpc.error_code"
  """
  @spec rpc_jsonrpc_error_code :: :"rpc.jsonrpc.error_code"
  defmacro rpc_jsonrpc_error_code do
    :"rpc.jsonrpc.error_code"
  end

  @doc """
  `error.message` property of response if it is an error response

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.rpc_jsonrpc_error_message()
      :"rpc.jsonrpc.error_message"
  """
  @spec rpc_jsonrpc_error_message :: :"rpc.jsonrpc.error_message"
  defmacro rpc_jsonrpc_error_message do
    :"rpc.jsonrpc.error_message"
  end

  @doc """
  Whether this is a received or sent message

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.message_type()
      :"message.type"
  """
  @spec message_type :: :"message.type"
  defmacro message_type do
    :"message.type"
  end

  @doc """
  MUST be calculated as two different counters starting from `1` one for sent messages and one for received message

  ### Notes

  This way we guarantee that the values will be consistent between different implementations

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.message_id()
      :"message.id"
  """
  @spec message_id :: :"message.id"
  defmacro message_id do
    :"message.id"
  end

  @doc """
  Compressed size of the message in bytes

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.message_compressed_size()
      :"message.compressed_size"
  """
  @spec message_compressed_size :: :"message.compressed_size"
  defmacro message_compressed_size do
    :"message.compressed_size"
  end

  @doc """
  Uncompressed size of the message in bytes

      iex> require OpenTelemetry.SemanticConventions.Trace
      ...> OpenTelemetry.SemanticConventions.Trace.message_uncompressed_size()
      :"message.uncompressed_size"
  """
  @spec message_uncompressed_size :: :"message.uncompressed_size"
  defmacro message_uncompressed_size do
    :"message.uncompressed_size"
  end
end
