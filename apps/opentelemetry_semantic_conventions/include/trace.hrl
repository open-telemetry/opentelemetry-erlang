%% The schema url for telemetry resources
-define(TRACE_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.20.0">>).

%% The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it
-define(EXCEPTION_TYPE, 'exception.type').

%% The exception message
-define(EXCEPTION_MESSAGE, 'exception.message').

%% A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG
-define(EXCEPTION_STACKTRACE, 'exception.stacktrace').

%% A unique identifier for the Log Record
%% If an id is provided, other log records with the same id will be considered duplicates and can be removed safely. This means, that two distinguishable log records MUST have different values.
%% The id MAY be an [Universally Unique Lexicographically Sortable Identifier (ULID)](https://github.com/ulid/spec), but other identifiers (e.g. UUID) may be used as needed
-define(LOG_RECORD_UID, 'log.record.uid').

%% The full invoked ARN as provided on the `Context` passed to the function (`Lambda-Runtime-Invoked-Function-Arn` header on the `/runtime/invocation/next` applicable)
%% This may be different from `cloud.resource_id` if an alias is involved
-define(AWS_LAMBDA_INVOKED_ARN, 'aws.lambda.invoked_arn').

%% The [event_id](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#id) uniquely identifies the event
-define(CLOUDEVENTS_EVENT_ID, 'cloudevents.event_id').

%% The [source](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#source-1) identifies the context in which an event happened
-define(CLOUDEVENTS_EVENT_SOURCE, 'cloudevents.event_source').

%% The [version of the CloudEvents specification](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#specversion) which the event uses
-define(CLOUDEVENTS_EVENT_SPEC_VERSION, 'cloudevents.event_spec_version').

%% The [event_type](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#type) contains a value describing the type of event related to the originating occurrence
-define(CLOUDEVENTS_EVENT_TYPE, 'cloudevents.event_type').

%% The [subject](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#subject) of the event in the context of the event producer (identified by source)
-define(CLOUDEVENTS_EVENT_SUBJECT, 'cloudevents.event_subject').

%% Parent-child Reference type
%% The causal relationship between a child Span and a parent Span
-define(OPENTRACING_REF_TYPE, 'opentracing.ref_type').

%% An identifier for the database management system (DBMS) product being used. See below for a list of well-known identifiers
-define(DB_SYSTEM, 'db.system').

%% The connection string used to connect to the database. It is recommended to remove embedded credentials
-define(DB_CONNECTION_STRING, 'db.connection_string').

%% Username for accessing the database
-define(DB_USER, 'db.user').

%% The fully-qualified class name of the [Java Database Connectivity (JDBC)](https://docs.oracle.com/javase/8/docs/technotes/guides/jdbc/) driver used to connect
-define(DB_JDBC_DRIVER_CLASSNAME, 'db.jdbc.driver_classname').

%% This attribute is used to report the name of the database being accessed. For commands that switch the database, this should be set to the target database (even if the command fails)
%% In some SQL databases, the database name to be used is called "schema name". In case there are multiple layers that could be considered for database name (e.g. Oracle instance name and schema name), the database name to be used is the more specific layer (e.g. Oracle schema name)
-define(DB_NAME, 'db.name').

%% The database statement being executed
-define(DB_STATEMENT, 'db.statement').

%% The name of the operation being executed, e.g. the [MongoDB command name](https://docs.mongodb.com/manual/reference/command/#database-operations) such as `findAndModify`, or the SQL keyword
%% When setting this to an SQL keyword, it is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if the operation name is provided by the library being instrumented. If the SQL statement has an ambiguous operation, or performs more than one operation, this value may be omitted
-define(DB_OPERATION, 'db.operation').

%% Name of the database host
%% `net.peer.name` SHOULD NOT be set if capturing it would require an extra DNS lookup
-define(NET_PEER_NAME, 'net.peer.name').

%% Logical remote port number
-define(NET_PEER_PORT, 'net.peer.port').

%% Remote socket peer address: IPv4 or IPv6 for internet protocols, path for local communication, [etc](https://man7.org/linux/man-pages/man7/address_families.7.html)
-define(NET_SOCK_PEER_ADDR, 'net.sock.peer.addr').

%% Remote socket peer port
-define(NET_SOCK_PEER_PORT, 'net.sock.peer.port').

%% Protocol [address family](https://man7.org/linux/man-pages/man7/address_families.7.html) which is used for communication
-define(NET_SOCK_FAMILY, 'net.sock.family').

%% Remote socket peer name
-define(NET_SOCK_PEER_NAME, 'net.sock.peer.name').

%% Transport protocol used. See note below
-define(NET_TRANSPORT, 'net.transport').

%% The Microsoft SQL Server [instance name](https://docs.microsoft.com/en-us/sql/connect/jdbc/building-the-connection-url?view=sql-server-ver15) connecting to. This name is used to determine the port of a named instance
%% If setting a `db.mssql.instance_name`, `net.peer.port` is no longer required (but still recommended if non-standard)
-define(DB_MSSQL_INSTANCE_NAME, 'db.mssql.instance_name').

%% The fetch size used for paging, i.e. how many rows will be returned at once
-define(DB_CASSANDRA_PAGE_SIZE, 'db.cassandra.page_size').

%% The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html)
-define(DB_CASSANDRA_CONSISTENCY_LEVEL, 'db.cassandra.consistency_level').

%% The name of the primary table that the operation is acting upon, including the keyspace name (if applicable)
%% This mirrors the db.sql.table attribute but references cassandra rather than sql. It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set
-define(DB_CASSANDRA_TABLE, 'db.cassandra.table').

%% Whether or not the query is idempotent
-define(DB_CASSANDRA_IDEMPOTENCE, 'db.cassandra.idempotence').

%% The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively
-define(DB_CASSANDRA_SPECULATIVE_EXECUTION_COUNT, 'db.cassandra.speculative_execution_count').

%% The ID of the coordinating node for a query
-define(DB_CASSANDRA_COORDINATOR_ID, 'db.cassandra.coordinator.id').

%% The data center of the coordinating node for a query
-define(DB_CASSANDRA_COORDINATOR_DC, 'db.cassandra.coordinator.dc').

%% The index of the database being accessed as used in the [`SELECT` command](https://redis.io/commands/select), provided as an integer. To be used instead of the generic `db.name` attribute
-define(DB_REDIS_DATABASE_INDEX, 'db.redis.database_index').

%% The collection being accessed within the database stated in `db.name`
-define(DB_MONGODB_COLLECTION, 'db.mongodb.collection').

%% The name of the primary table that the operation is acting upon, including the database name (if applicable)
%% It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set
-define(DB_SQL_TABLE, 'db.sql.table').

%% Unique Cosmos client instance id
-define(DB_COSMOSDB_CLIENT_ID, 'db.cosmosdb.client_id').

%% CosmosDB Operation Type
-define(DB_COSMOSDB_OPERATION_TYPE, 'db.cosmosdb.operation_type').

%% Full user-agent string is generated by Cosmos DB SDK
%% The user-agent value is generated by SDK which is a combination of<br> `sdk_version` : Current version of SDK. e.g. 'cosmos-netstandard-sdk/3.23.0'<br> `direct_pkg_version` : Direct package version used by Cosmos DB SDK. e.g. '3.23.1'<br> `number_of_client_instances` : Number of cosmos client instances created by the application. e.g. '1'<br> `type_of_machine_architecture` : Machine architecture. e.g. 'X64'<br> `operating_system` : Operating System. e.g. 'Linux 5.4.0-1098-azure 104 18'<br> `runtime_framework` : Runtime Framework. e.g. '.NET Core 3.1.32'<br> `failover_information` : Generated key to determine if region failover enabled.
%%    Format Reg-{D (Disabled discovery)}-S(application region)|L(List of preferred regions)|N(None, user did not configure it).
%%    Default value is "NS"
-define(USER_AGENT_ORIGINAL, 'user_agent.original').

%% Cosmos client connection mode
-define(DB_COSMOSDB_CONNECTION_MODE, 'db.cosmosdb.connection_mode').

%% Cosmos DB container name
-define(DB_COSMOSDB_CONTAINER, 'db.cosmosdb.container').

%% Request payload size in bytes
-define(DB_COSMOSDB_REQUEST_CONTENT_LENGTH, 'db.cosmosdb.request_content_length').

%% Cosmos DB status code
-define(DB_COSMOSDB_STATUS_CODE, 'db.cosmosdb.status_code').

%% Cosmos DB sub status code
-define(DB_COSMOSDB_SUB_STATUS_CODE, 'db.cosmosdb.sub_status_code').

%% RU consumed for that operation
-define(DB_COSMOSDB_REQUEST_CHARGE, 'db.cosmosdb.request_charge').

%% Name of the code, either "OK" or "ERROR". MUST NOT be set if the status code is UNSET
-define(OTEL_STATUS_CODE, 'otel.status_code').

%% Description of the Status if it has a value, otherwise not set
-define(OTEL_STATUS_DESCRIPTION, 'otel.status_description').

%% Type of the trigger which caused this function invocation
%% For the server/consumer span on the incoming side,
%% `faas.trigger` MUST be set.
%% 
%% Clients invoking FaaS instances usually cannot set `faas.trigger`,
%% since they would typically need to look in the payload to determine
%% the event type. If clients set it, it should be the same as the
%% trigger that corresponding incoming would have (i.e., this has
%% nothing to do with the underlying transport used to make the API
%% call to invoke the lambda, which is often HTTP)
-define(FAAS_TRIGGER, 'faas.trigger').

%% The invocation ID of the current function invocation
-define(FAAS_INVOCATION_ID, 'faas.invocation_id').

%% Cloud provider-specific native identifier of the monitored cloud resource (e.g. an [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html) on AWS, a [fully qualified resource ID](https://learn.microsoft.com/en-us/rest/api/resources/resources/get-by-id) on Azure, a [full resource name](https://cloud.google.com/apis/design/resource_names#full_resource_name) on GCP)
%% On some cloud providers, it may not be possible to determine the full ID at startup,
%% so it may be necessary to set `cloud.resource_id` as a span attribute instead.
%% 
%% The exact value to use for `cloud.resource_id` depends on the cloud provider.
%% The following well-known definitions MUST be used if you set this attribute and they apply:
%% 
%% * **AWS Lambda:** The function [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html).
%%   Take care not to use the "invoked ARN" directly but replace any
%%   [alias suffix](https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html)
%%   with the resolved function version, as the same runtime instance may be invokable with
%%   multiple different aliases.
%% * **GCP:** The [URI of the resource](https://cloud.google.com/iam/docs/full-resource-names)
%% * **Azure:** The [Fully Qualified Resource ID](https://docs.microsoft.com/en-us/rest/api/resources/resources/get-by-id) of the invoked function,
%%   *not* the function app, having the form
%%   `/subscriptions/<SUBSCIPTION_GUID>/resourceGroups/<RG>/providers/Microsoft.Web/sites/<FUNCAPP>/functions/<FUNC>`.
%%   This means that a span attribute MUST be used, as an Azure function app can host multiple functions that would usually share
%%   a TracerProvider
-define(CLOUD_RESOURCE_ID, 'cloud.resource_id').

%% The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name
-define(FAAS_DOCUMENT_COLLECTION, 'faas.document.collection').

%% Describes the type of the operation that was performed on the data
-define(FAAS_DOCUMENT_OPERATION, 'faas.document.operation').

%% A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)
-define(FAAS_DOCUMENT_TIME, 'faas.document.time').

%% The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name
-define(FAAS_DOCUMENT_NAME, 'faas.document.name').

%% The URI scheme identifying the used protocol
-define(HTTP_SCHEME, 'http.scheme').

%% The matched route (path template in the format used by the respective server framework). See note below
%% MUST NOT be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can NOT substitute it.
%% SHOULD include the [application root](/specification/trace/semantic_conventions/http.md#http-server-definitions) if there is one
-define(HTTP_ROUTE, 'http.route').

%% Name of the local HTTP server that received the request
%% Determined by using the first of the following that applies
%% 
%% - The [primary server name](/specification/trace/semantic_conventions/http.md#http-server-definitions) of the matched virtual host. MUST only
%%   include host identifier.
%% - Host identifier of the [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource)
%%   if it's sent in absolute-form.
%% - Host identifier of the `Host` header
%% 
%% SHOULD NOT be set if only IP address is available and capturing name would require a reverse DNS lookup
-define(NET_HOST_NAME, 'net.host.name').

%% Port of the local HTTP server that received the request
%% Determined by using the first of the following that applies
%% 
%% - Port identifier of the [primary server host](/specification/trace/semantic_conventions/http.md#http-server-definitions) of the matched virtual host.
%% - Port identifier of the [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource)
%%   if it's sent in absolute-form.
%% - Port identifier of the `Host` header
-define(NET_HOST_PORT, 'net.host.port').

%% The full request target as passed in a HTTP request line or equivalent
-define(HTTP_TARGET, 'http.target').

%% The IP address of the original client behind all proxies, if known (e.g. from [X-Forwarded-For](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For))
%% This is not necessarily the same as `net.sock.peer.addr`, which would
%% identify the network-level peer, which may be a proxy.
%% 
%% This attribute should be set when a source of information different
%% from the one used for `net.sock.peer.addr`, is available even if that other
%% source just confirms the same value as `net.sock.peer.addr`.
%% Rationale: For `net.sock.peer.addr`, one typically does not know if it
%% comes from a proxy, reverse proxy, or the actual client. Setting
%% `http.client_ip` when it's the same as `net.sock.peer.addr` means that
%% one is at least somewhat confident that the address is not that of
%% the closest proxy
-define(HTTP_CLIENT_IP, 'http.client_ip').

%% Local socket address. Useful in case of a multi-IP host
-define(NET_SOCK_HOST_ADDR, 'net.sock.host.addr').

%% Local socket port number
-define(NET_SOCK_HOST_PORT, 'net.sock.host.port').

%% A string identifying the messaging system
-define(MESSAGING_SYSTEM, 'messaging.system').

%% A string identifying the kind of messaging operation as defined in the [Operation names](#operation-names) section above
%% If a custom value is used, it MUST be of low cardinality
-define(MESSAGING_OPERATION, 'messaging.operation').

%% The number of messages sent, received, or processed in the scope of the batching operation
%% Instrumentations SHOULD NOT set `messaging.batch.message_count` on spans that operate with a single message. When a messaging client library supports both batch and single-message API for the same operation, instrumentations SHOULD use `messaging.batch.message_count` for batching APIs and SHOULD NOT use it for single-message APIs
-define(MESSAGING_BATCH_MESSAGE_COUNT, 'messaging.batch.message_count').

%% A value used by the messaging system as an identifier for the message, represented as a string
-define(MESSAGING_MESSAGE_ID, 'messaging.message.id').

%% The [conversation ID](#conversations) identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID"
-define(MESSAGING_MESSAGE_CONVERSATION_ID, 'messaging.message.conversation_id').

%% The (uncompressed) size of the message payload in bytes. Also use this attribute if it is unknown whether the compressed or uncompressed payload size is reported
-define(MESSAGING_MESSAGE_PAYLOAD_SIZE_BYTES, 'messaging.message.payload_size_bytes').

%% The compressed size of the message payload in bytes
-define(MESSAGING_MESSAGE_PAYLOAD_COMPRESSED_SIZE_BYTES, 'messaging.message.payload_compressed_size_bytes').

%% Application layer protocol used. The value SHOULD be normalized to lowercase
-define(NET_PROTOCOL_NAME, 'net.protocol.name').

%% Version of the application layer protocol used. See note below
%% `net.protocol.version` refers to the version of the protocol used and might be different from the protocol client's version. If the HTTP client used has a version of `0.27.2`, but sends HTTP version `1.1`, this attribute should be set to `1.1`
-define(NET_PROTOCOL_VERSION, 'net.protocol.version').

%% A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)
-define(FAAS_TIME, 'faas.time').

%% A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm)
-define(FAAS_CRON, 'faas.cron').

%% A boolean that is true if the serverless function is executed for the first time (aka cold-start)
-define(FAAS_COLDSTART, 'faas.coldstart').

%% The name of the invoked function
%% SHOULD be equal to the `faas.name` resource attribute of the invoked function
-define(FAAS_INVOKED_NAME, 'faas.invoked_name').

%% The cloud provider of the invoked function
%% SHOULD be equal to the `cloud.provider` resource attribute of the invoked function
-define(FAAS_INVOKED_PROVIDER, 'faas.invoked_provider').

%% The cloud region of the invoked function
%% SHOULD be equal to the `cloud.region` resource attribute of the invoked function
-define(FAAS_INVOKED_REGION, 'faas.invoked_region').

%% The [`service.name`](../../resource/semantic_conventions/README.md#service) of the remote service. SHOULD be equal to the actual `service.name` resource attribute of the remote service if any
-define(PEER_SERVICE, 'peer.service').

%% Username or client_id extracted from the access token or [Authorization](https://tools.ietf.org/html/rfc7235#section-4.2) header in the inbound request from outside the system
-define(ENDUSER_ID, 'enduser.id').

%% Actual/assumed role the client is making the request under extracted from token or application security context
-define(ENDUSER_ROLE, 'enduser.role').

%% Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an [OAuth 2.0 Access Token](https://tools.ietf.org/html/rfc6749#section-3.3) or an attribute value in a [SAML 2.0 Assertion](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html)
-define(ENDUSER_SCOPE, 'enduser.scope').

%% Current "managed" thread ID (as opposed to OS thread ID)
-define(THREAD_ID, 'thread.id').

%% Current thread name
-define(THREAD_NAME, 'thread.name').

%% The method or function name, or equivalent (usually rightmost part of the code unit's name)
-define(CODE_FUNCTION, 'code.function').

%% The "namespace" within which `code.function` is defined. Usually the qualified class or module name, such that `code.namespace` + some separator + `code.function` form a unique identifier for the code unit
-define(CODE_NAMESPACE, 'code.namespace').

%% The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path)
-define(CODE_FILEPATH, 'code.filepath').

%% The line number in `code.filepath` best representing the operation. It SHOULD point within the code unit named in `code.function`
-define(CODE_LINENO, 'code.lineno').

%% The column number in `code.filepath` best representing the operation. It SHOULD point within the code unit named in `code.function`
-define(CODE_COLUMN, 'code.column').

%% Full HTTP request URL in the form `scheme://host[:port]/path?query[#fragment]`. Usually the fragment is not transmitted over HTTP, but if it is known, it should be included nevertheless
%% `http.url` MUST NOT contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case the attribute's value should be `https://www.example.com/`
-define(HTTP_URL, 'http.url').

%% The ordinal number of request resending attempt (for any reason, including redirects)
%% The resend count SHOULD be updated each time an HTTP request gets resent by the client, regardless of what was the cause of the resending (e.g. redirection, authorization failure, 503 Server Unavailable, network issues, or any other)
-define(HTTP_RESEND_COUNT, 'http.resend_count').

%% The value `aws-api`
-define(RPC_SYSTEM, 'rpc.system').

%% The name of the service to which a request is made, as returned by the AWS SDK
%% This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side)
-define(RPC_SERVICE, 'rpc.service').

%% The name of the operation corresponding to the request, as returned by the AWS SDK
%% This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side)
-define(RPC_METHOD, 'rpc.method').

%% The AWS request ID as returned in the response headers `x-amz-request-id` or `x-amz-requestid`
-define(AWS_REQUEST_ID, 'aws.request_id').

%% The keys in the `RequestItems` object field
-define(AWS_DYNAMODB_TABLE_NAMES, 'aws.dynamodb.table_names').

%% The JSON-serialized value of each item in the `ConsumedCapacity` response field
-define(AWS_DYNAMODB_CONSUMED_CAPACITY, 'aws.dynamodb.consumed_capacity').

%% The JSON-serialized value of the `ItemCollectionMetrics` response field
-define(AWS_DYNAMODB_ITEM_COLLECTION_METRICS, 'aws.dynamodb.item_collection_metrics').

%% The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter
-define(AWS_DYNAMODB_PROVISIONED_READ_CAPACITY, 'aws.dynamodb.provisioned_read_capacity').

%% The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter
-define(AWS_DYNAMODB_PROVISIONED_WRITE_CAPACITY, 'aws.dynamodb.provisioned_write_capacity').

%% The value of the `ConsistentRead` request parameter
-define(AWS_DYNAMODB_CONSISTENT_READ, 'aws.dynamodb.consistent_read').

%% The value of the `ProjectionExpression` request parameter
-define(AWS_DYNAMODB_PROJECTION, 'aws.dynamodb.projection').

%% The value of the `Limit` request parameter
-define(AWS_DYNAMODB_LIMIT, 'aws.dynamodb.limit').

%% The value of the `AttributesToGet` request parameter
-define(AWS_DYNAMODB_ATTRIBUTES_TO_GET, 'aws.dynamodb.attributes_to_get').

%% The value of the `IndexName` request parameter
-define(AWS_DYNAMODB_INDEX_NAME, 'aws.dynamodb.index_name').

%% The value of the `Select` request parameter
-define(AWS_DYNAMODB_SELECT, 'aws.dynamodb.select').

%% The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field
-define(AWS_DYNAMODB_GLOBAL_SECONDARY_INDEXES, 'aws.dynamodb.global_secondary_indexes').

%% The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field
-define(AWS_DYNAMODB_LOCAL_SECONDARY_INDEXES, 'aws.dynamodb.local_secondary_indexes').

%% The value of the `ExclusiveStartTableName` request parameter
-define(AWS_DYNAMODB_EXCLUSIVE_START_TABLE, 'aws.dynamodb.exclusive_start_table').

%% The the number of items in the `TableNames` response parameter
-define(AWS_DYNAMODB_TABLE_COUNT, 'aws.dynamodb.table_count').

%% The value of the `ScanIndexForward` request parameter
-define(AWS_DYNAMODB_SCAN_FORWARD, 'aws.dynamodb.scan_forward').

%% The value of the `Segment` request parameter
-define(AWS_DYNAMODB_SEGMENT, 'aws.dynamodb.segment').

%% The value of the `TotalSegments` request parameter
-define(AWS_DYNAMODB_TOTAL_SEGMENTS, 'aws.dynamodb.total_segments').

%% The value of the `Count` response parameter
-define(AWS_DYNAMODB_COUNT, 'aws.dynamodb.count').

%% The value of the `ScannedCount` response parameter
-define(AWS_DYNAMODB_SCANNED_COUNT, 'aws.dynamodb.scanned_count').

%% The JSON-serialized value of each item in the `AttributeDefinitions` request field
-define(AWS_DYNAMODB_ATTRIBUTE_DEFINITIONS, 'aws.dynamodb.attribute_definitions').

%% The JSON-serialized value of each item in the the `GlobalSecondaryIndexUpdates` request field
-define(AWS_DYNAMODB_GLOBAL_SECONDARY_INDEX_UPDATES, 'aws.dynamodb.global_secondary_index_updates').

%% The S3 bucket name the request refers to. Corresponds to the `--bucket` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations
%% The `bucket` attribute is applicable to all S3 operations that reference a bucket, i.e. that require the bucket name as a mandatory parameter.
%% This applies to almost all S3 operations except `list-buckets`
-define(AWS_S3_BUCKET, 'aws.s3.bucket').

%% The S3 object key the request refers to. Corresponds to the `--key` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations
%% The `key` attribute is applicable to all object-related S3 operations, i.e. that require the object key as a mandatory parameter.
%% This applies in particular to the following operations:
%% 
%% - [copy-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html)
%% - [delete-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-object.html)
%% - [get-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/get-object.html)
%% - [head-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/head-object.html)
%% - [put-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/put-object.html)
%% - [restore-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/restore-object.html)
%% - [select-object-content](https://docs.aws.amazon.com/cli/latest/reference/s3api/select-object-content.html)
%% - [abort-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/abort-multipart-upload.html)
%% - [complete-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/complete-multipart-upload.html)
%% - [create-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/create-multipart-upload.html)
%% - [list-parts](https://docs.aws.amazon.com/cli/latest/reference/s3api/list-parts.html)
%% - [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
%% - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)
-define(AWS_S3_KEY, 'aws.s3.key').

%% The source object (in the form `bucket`/`key`) for the copy operation
%% The `copy_source` attribute applies to S3 copy operations and corresponds to the `--copy-source` parameter
%% of the [copy-object operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html).
%% This applies in particular to the following operations:
%% 
%% - [copy-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html)
%% - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)
-define(AWS_S3_COPY_SOURCE, 'aws.s3.copy_source').

%% Upload ID that identifies the multipart upload
%% The `upload_id` attribute applies to S3 multipart-upload operations and corresponds to the `--upload-id` parameter
%% of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) multipart operations.
%% This applies in particular to the following operations:
%% 
%% - [abort-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/abort-multipart-upload.html)
%% - [complete-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/complete-multipart-upload.html)
%% - [list-parts](https://docs.aws.amazon.com/cli/latest/reference/s3api/list-parts.html)
%% - [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
%% - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)
-define(AWS_S3_UPLOAD_ID, 'aws.s3.upload_id').

%% The delete request container that specifies the objects to be deleted
%% The `delete` attribute is only applicable to the [delete-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-object.html) operation.
%% The `delete` attribute corresponds to the `--delete` parameter of the
%% [delete-objects operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-objects.html)
-define(AWS_S3_DELETE, 'aws.s3.delete').

%% The part number of the part being uploaded in a multipart-upload operation. This is a positive integer between 1 and 10,000
%% The `part_number` attribute is only applicable to the [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
%% and [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html) operations.
%% The `part_number` attribute corresponds to the `--part-number` parameter of the
%% [upload-part operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
-define(AWS_S3_PART_NUMBER, 'aws.s3.part_number').

%% The name of the operation being executed
-define(GRAPHQL_OPERATION_NAME, 'graphql.operation.name').

%% The type of the operation being executed
-define(GRAPHQL_OPERATION_TYPE, 'graphql.operation.type').

%% The GraphQL document being executed
%% The value may be sanitized to exclude sensitive information
-define(GRAPHQL_DOCUMENT, 'graphql.document').

%% The message destination name
%% Destination name SHOULD uniquely identify a specific queue, topic or other entity within the broker. If
%% the broker does not have such notion, the destination name SHOULD uniquely identify the broker
-define(MESSAGING_DESTINATION_NAME, 'messaging.destination.name').

%% Low cardinality representation of the messaging destination name
%% Destination names could be constructed from templates. An example would be a destination name involving a user name or product id. Although the destination name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation
-define(MESSAGING_DESTINATION_TEMPLATE, 'messaging.destination.template').

%% A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed
-define(MESSAGING_DESTINATION_TEMPORARY, 'messaging.destination.temporary').

%% A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name)
-define(MESSAGING_DESTINATION_ANONYMOUS, 'messaging.destination.anonymous').

%% The identifier for the consumer receiving a message. For Kafka, set it to `{messaging.kafka.consumer.group} - {messaging.kafka.client_id}`, if both are present, or only `messaging.kafka.consumer.group`. For brokers, such as RabbitMQ and Artemis, set it to the `client_id` of the client consuming the message
-define(MESSAGING_CONSUMER_ID, 'messaging.consumer.id').

%% The message source name
%% Source name SHOULD uniquely identify a specific queue, topic, or other entity within the broker. If
%% the broker does not have such notion, the source name SHOULD uniquely identify the broker
-define(MESSAGING_SOURCE_NAME, 'messaging.source.name').

%% Low cardinality representation of the messaging source name
%% Source names could be constructed from templates. An example would be a source name involving a user name or product id. Although the source name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation
-define(MESSAGING_SOURCE_TEMPLATE, 'messaging.source.template').

%% A boolean that is true if the message source is temporary and might not exist anymore after messages are processed
-define(MESSAGING_SOURCE_TEMPORARY, 'messaging.source.temporary').

%% A boolean that is true if the message source is anonymous (could be unnamed or have auto-generated name)
-define(MESSAGING_SOURCE_ANONYMOUS, 'messaging.source.anonymous').

%% The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request
-define(RPC_GRPC_STATUS_CODE, 'rpc.grpc.status_code').

%% Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 does not specify this, the value can be omitted
-define(RPC_JSONRPC_VERSION, 'rpc.jsonrpc.version').

%% `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification
-define(RPC_JSONRPC_REQUEST_ID, 'rpc.jsonrpc.request_id').

%% `error.code` property of response if it is an error response
-define(RPC_JSONRPC_ERROR_CODE, 'rpc.jsonrpc.error_code').

%% `error.message` property of response if it is an error response
-define(RPC_JSONRPC_ERROR_MESSAGE, 'rpc.jsonrpc.error_message').

%% The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values
-define(RPC_CONNECT_RPC_ERROR_CODE, 'rpc.connect_rpc.error_code').
