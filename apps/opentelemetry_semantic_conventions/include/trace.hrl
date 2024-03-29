%% The schema url for telemetry resources
-define(TRACE_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.13.0">>).

%% The full invoked ARN as provided on the `Context` passed to the function (`Lambda-Runtime-Invoked-Function-Arn` header on the `/runtime/invocation/next` applicable)
%% This may be different from `faas.id` if an alias is involved
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
%% The value may be sanitized to exclude sensitive information
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

%% The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it
-define(EXCEPTION_TYPE, 'exception.type').

%% The exception message
-define(EXCEPTION_MESSAGE, 'exception.message').

%% A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG
-define(EXCEPTION_STACKTRACE, 'exception.stacktrace').

%% SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span
%% An exception is considered to have escaped (or left) the scope of a span,
%% if that span is ended while the exception is still logically "in flight".
%% This may be actually "in flight" in some languages (e.g. if the exception
%% is passed to a Context manager's `__exit__` method in Python) but will
%% usually be caught at the point of recording the exception in most languages.
%% 
%% It is usually not possible to determine at the point where an exception is thrown
%% whether it will escape the scope of a span.
%% However, it is trivial to know that an exception
%% will escape, if one checks for an active exception just before ending the span,
%% as done in the [example above](#recording-an-exception).
%% 
%% It follows that an exception may still escape the scope of the span
%% even if the `exception.escaped` attribute was not set or set to false,
%% since the event might have been recorded at a time where it was not
%% clear whether the exception will escape
-define(EXCEPTION_ESCAPED, 'exception.escaped').

%% Type of the trigger which caused this function execution
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

%% The execution ID of the current function execution
-define(FAAS_EXECUTION, 'faas.execution').

%% The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name
-define(FAAS_DOCUMENT_COLLECTION, 'faas.document.collection').

%% Describes the type of the operation that was performed on the data
-define(FAAS_DOCUMENT_OPERATION, 'faas.document.operation').

%% A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)
-define(FAAS_DOCUMENT_TIME, 'faas.document.time').

%% The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name
-define(FAAS_DOCUMENT_NAME, 'faas.document.name').

%% HTTP request method
-define(HTTP_METHOD, 'http.method').

%% [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6)
-define(HTTP_STATUS_CODE, 'http.status_code').

%% Kind of HTTP protocol used
%% If `net.transport` is not specified, it can be assumed to be `IP.TCP` except if `http.flavor` is `QUIC`, in which case `IP.UDP` is assumed
-define(HTTP_FLAVOR, 'http.flavor').

%% Value of the [HTTP User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#field.user-agent) header sent by the client
-define(HTTP_USER_AGENT, 'http.user_agent').

%% The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size
-define(HTTP_REQUEST_CONTENT_LENGTH, 'http.request_content_length').

%% The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size
-define(HTTP_RESPONSE_CONTENT_LENGTH, 'http.response_content_length').

%% The URI scheme identifying the used protocol
-define(HTTP_SCHEME, 'http.scheme').

%% The full request target as passed in a HTTP request line or equivalent
-define(HTTP_TARGET, 'http.target').

%% The matched route (path template in the format used by the respective server framework). See note below
%% 'http.route' MUST NOT be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can NOT substitute it
-define(HTTP_ROUTE, 'http.route').

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

%% Name of the local HTTP server that received the request
%% Determined by using the first of the following that applies
%% 
%% - The [primary server name](#http-server-definitions) of the matched virtual host. MUST only
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
%% - Port identifier of the [primary server host](#http-server-definitions) of the matched virtual host.
%% - Port identifier of the [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource)
%%   if it's sent in absolute-form.
%% - Port identifier of the `Host` header
-define(NET_HOST_PORT, 'net.host.port').

%% Local socket address. Useful in case of a multi-IP host
-define(NET_SOCK_HOST_ADDR, 'net.sock.host.addr').

%% Local socket port number
-define(NET_SOCK_HOST_PORT, 'net.sock.host.port').

%% Application layer protocol used. The value SHOULD be normalized to lowercase
-define(NET_APP_PROTOCOL_NAME, 'net.app.protocol.name').

%% Version of the application layer protocol used. See note below
%% `net.app.protocol.version` refers to the version of the protocol used and might be different from the protocol client's version. If the HTTP client used has a version of `0.27.2`, but sends HTTP version `1.1`, this attribute should be set to `1.1`
-define(NET_APP_PROTOCOL_VERSION, 'net.app.protocol.version').

%% The internet connection type currently being used by the host
-define(NET_HOST_CONNECTION_TYPE, 'net.host.connection.type').

%% This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection
-define(NET_HOST_CONNECTION_SUBTYPE, 'net.host.connection.subtype').

%% The name of the mobile carrier
-define(NET_HOST_CARRIER_NAME, 'net.host.carrier.name').

%% The mobile carrier country code
-define(NET_HOST_CARRIER_MCC, 'net.host.carrier.mcc').

%% The mobile carrier network code
-define(NET_HOST_CARRIER_MNC, 'net.host.carrier.mnc').

%% The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network
-define(NET_HOST_CARRIER_ICC, 'net.host.carrier.icc').

%% A string identifying the messaging system
-define(MESSAGING_SYSTEM, 'messaging.system').

%% The message destination name. This might be equal to the span name but is required nevertheless
-define(MESSAGING_DESTINATION, 'messaging.destination').

%% The kind of message destination
-define(MESSAGING_DESTINATION_KIND, 'messaging.destination_kind').

%% A boolean that is true if the message destination is temporary
-define(MESSAGING_TEMP_DESTINATION, 'messaging.temp_destination').

%% The name of the transport protocol
-define(MESSAGING_PROTOCOL, 'messaging.protocol').

%% The version of the transport protocol
-define(MESSAGING_PROTOCOL_VERSION, 'messaging.protocol_version').

%% Connection string
-define(MESSAGING_URL, 'messaging.url').

%% A value used by the messaging system as an identifier for the message, represented as a string
-define(MESSAGING_MESSAGE_ID, 'messaging.message_id').

%% The [conversation ID](#conversations) identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID"
-define(MESSAGING_CONVERSATION_ID, 'messaging.conversation_id').

%% The (uncompressed) size of the message payload in bytes. Also use this attribute if it is unknown whether the compressed or uncompressed payload size is reported
-define(MESSAGING_MESSAGE_PAYLOAD_SIZE_BYTES, 'messaging.message_payload_size_bytes').

%% The compressed size of the message payload in bytes
-define(MESSAGING_MESSAGE_PAYLOAD_COMPRESSED_SIZE_BYTES, 'messaging.message_payload_compressed_size_bytes').

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

%% Full HTTP request URL in the form `scheme://host[:port]/path?query[#fragment]`. Usually the fragment is not transmitted over HTTP, but if it is known, it should be included nevertheless
%% `http.url` MUST NOT contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case the attribute's value should be `https://www.example.com/`
-define(HTTP_URL, 'http.url').

%% The ordinal number of request re-sending attempt
-define(HTTP_RETRY_COUNT, 'http.retry_count').

%% The value `aws-api`
-define(RPC_SYSTEM, 'rpc.system').

%% The name of the service to which a request is made, as returned by the AWS SDK
%% This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side)
-define(RPC_SERVICE, 'rpc.service').

%% The name of the operation corresponding to the request, as returned by the AWS SDK
%% This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side)
-define(RPC_METHOD, 'rpc.method').

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

%% The name of the operation being executed
-define(GRAPHQL_OPERATION_NAME, 'graphql.operation.name').

%% The type of the operation being executed
-define(GRAPHQL_OPERATION_TYPE, 'graphql.operation.type').

%% The GraphQL document being executed
%% The value may be sanitized to exclude sensitive information
-define(GRAPHQL_DOCUMENT, 'graphql.document').

%% A string identifying the kind of message consumption as defined in the [Operation names](#operation-names) section above. If the operation is "send", this attribute MUST NOT be set, since the operation can be inferred from the span kind in that case
-define(MESSAGING_OPERATION, 'messaging.operation').

%% The identifier for the consumer receiving a message. For Kafka, set it to `{messaging.kafka.consumer_group} - {messaging.kafka.client_id}`, if both are present, or only `messaging.kafka.consumer_group`. For brokers, such as RabbitMQ and Artemis, set it to the `client_id` of the client consuming the message
-define(MESSAGING_CONSUMER_ID, 'messaging.consumer_id').

%% RabbitMQ message routing key
-define(MESSAGING_RABBITMQ_ROUTING_KEY, 'messaging.rabbitmq.routing_key').

%% Message keys in Kafka are used for grouping alike messages to ensure they're processed on the same partition. They differ from `messaging.message_id` in that they're not unique. If the key is `null`, the attribute MUST NOT be set
%% If the key type is not string, it's string representation has to be supplied for the attribute. If the key has no unambiguous, canonical string form, don't include its value
-define(MESSAGING_KAFKA_MESSAGE_KEY, 'messaging.kafka.message_key').

%% Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers
-define(MESSAGING_KAFKA_CONSUMER_GROUP, 'messaging.kafka.consumer_group').

%% Client Id for the Consumer or Producer that is handling the message
-define(MESSAGING_KAFKA_CLIENT_ID, 'messaging.kafka.client_id').

%% Partition the message is sent to
-define(MESSAGING_KAFKA_PARTITION, 'messaging.kafka.partition').

%% A boolean that is true if the message is a tombstone
-define(MESSAGING_KAFKA_TOMBSTONE, 'messaging.kafka.tombstone').

%% Namespace of RocketMQ resources, resources in different namespaces are individual
-define(MESSAGING_ROCKETMQ_NAMESPACE, 'messaging.rocketmq.namespace').

%% Name of the RocketMQ producer/consumer group that is handling the message. The client type is identified by the SpanKind
-define(MESSAGING_ROCKETMQ_CLIENT_GROUP, 'messaging.rocketmq.client_group').

%% The unique identifier for each client
-define(MESSAGING_ROCKETMQ_CLIENT_ID, 'messaging.rocketmq.client_id').

%% Type of message
-define(MESSAGING_ROCKETMQ_MESSAGE_TYPE, 'messaging.rocketmq.message_type').

%% The secondary classifier of message besides topic
-define(MESSAGING_ROCKETMQ_MESSAGE_TAG, 'messaging.rocketmq.message_tag').

%% Key(s) of message, another way to mark message besides message id
-define(MESSAGING_ROCKETMQ_MESSAGE_KEYS, 'messaging.rocketmq.message_keys').

%% Model of message consumption. This only applies to consumer spans
-define(MESSAGING_ROCKETMQ_CONSUMPTION_MODEL, 'messaging.rocketmq.consumption_model').

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

%% Whether this is a received or sent message
-define(MESSAGE_TYPE, 'message.type').

%% MUST be calculated as two different counters starting from `1` one for sent messages and one for received message
%% This way we guarantee that the values will be consistent between different implementations
-define(MESSAGE_ID, 'message.id').

%% Compressed size of the message in bytes
-define(MESSAGE_COMPRESSED_SIZE, 'message.compressed_size').

%% Uncompressed size of the message in bytes
-define(MESSAGE_UNCOMPRESSED_SIZE, 'message.uncompressed_size').
