%% The schema url for telemetry resources
-define(SPAN_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.25.0">>).

%% The [`service.name`](/docs/resource/README.md#service) of the remote service. SHOULD be equal to the actual `service.name` resource attribute of the remote service if any
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

%% The column number in `code.filepath` best representing the operation. It SHOULD point within the code unit named in `code.function`
-define(CODE_COLUMN, 'code.column').

%% The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path)
-define(CODE_FILEPATH, 'code.filepath').

%% The method or function name, or equivalent (usually rightmost part of the code unit's name)
-define(CODE_FUNCTION, 'code.function').

%% The line number in `code.filepath` best representing the operation. It SHOULD point within the code unit named in `code.function`
-define(CODE_LINENO, 'code.lineno').

%% The "namespace" within which `code.function` is defined. Usually the qualified class or module name, such that `code.namespace` + some separator + `code.function` form a unique identifier for the code unit
-define(CODE_NAMESPACE, 'code.namespace').

%% A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG
-define(CODE_STACKTRACE, 'code.stacktrace').

%% The full invoked ARN as provided on the `Context` passed to the function (`Lambda-Runtime-Invoked-Function-Arn` header on the `/runtime/invocation/next` applicable)
%% This may be different from `cloud.resource_id` if an alias is involved
-define(AWS_LAMBDA_INVOKED_ARN, 'aws.lambda.invoked_arn').

%% The [event_id](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#id) uniquely identifies the event
-define(CLOUDEVENTS_EVENT_ID, 'cloudevents.event_id').

%% The [source](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#source-1) identifies the context in which an event happened
-define(CLOUDEVENTS_EVENT_SOURCE, 'cloudevents.event_source').

%% The [version of the CloudEvents specification](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#specversion) which the event uses
-define(CLOUDEVENTS_EVENT_SPEC_VERSION, 'cloudevents.event_spec_version').

%% The [subject](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#subject) of the event in the context of the event producer (identified by source)
-define(CLOUDEVENTS_EVENT_SUBJECT, 'cloudevents.event_subject').

%% The [event_type](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#type) contains a value describing the type of event related to the originating occurrence
-define(CLOUDEVENTS_EVENT_TYPE, 'cloudevents.event_type').

%% Parent-child Reference type
%% The causal relationship between a child Span and a parent Span
-define(OPENTRACING_REF_TYPE, 'opentracing.ref_type').

%% An identifier for the database management system (DBMS) product being used. See below for a list of well-known identifiers
-define(DB_SYSTEM, 'db.system').

%% This attribute is used to report the name of the database being accessed. For commands that switch the database, this should be set to the target database (even if the command fails)
%% In some SQL databases, the database name to be used is called "schema name". In case there are multiple layers that could be considered for database name (e.g. Oracle instance name and schema name), the database name to be used is the more specific layer (e.g. Oracle schema name)
-define(DB_NAME, 'db.name').

%% The name of the operation being executed, e.g. the [MongoDB command name](https://docs.mongodb.com/manual/reference/command/#database-operations) such as `findAndModify`, or the SQL keyword
%% When setting this to an SQL keyword, it is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if the operation name is provided by the library being instrumented. If the SQL statement has an ambiguous operation, or performs more than one operation, this value may be omitted
-define(DB_OPERATION, 'db.operation').

%% Server port number
%% When observed from the client side, and when communicating through an intermediary, `server.port` SHOULD represent the server port behind any intermediaries, for example proxies, if it's available
-define(SERVER_PORT, 'server.port').

%% An identifier (address, unique name, or any other identifier) of the database instance that is executing queries or mutations on the current connection. This is useful in cases where the database is running in a clustered environment and the instrumentation is able to record the node executing the query. The client may obtain this value in databases like MySQL using queries like `select @@hostname`
-define(DB_INSTANCE_ID, 'db.instance.id').

%% The database statement being executed
-define(DB_STATEMENT, 'db.statement').

%% Username for accessing the database
-define(DB_USER, 'db.user').

%% Peer address of the database node where the operation was performed
%% Semantic conventions for individual database systems SHOULD document whether `network.peer.*` attributes are applicable. Network peer address and port are useful when the application interacts with individual database nodes directly.
%% If a database operation involved multiple network calls (for example retries), the address of the last contacted node SHOULD be used
-define(NETWORK_PEER_ADDRESS, 'network.peer.address').

%% Peer port number of the network connection
-define(NETWORK_PEER_PORT, 'network.peer.port').

%% Name of the database host
%% When observed from the client side, and when communicating through an intermediary, `server.address` SHOULD represent the server address behind any intermediaries, for example proxies, if it's available
-define(SERVER_ADDRESS, 'server.address').

%% The Microsoft SQL Server [instance name](https://docs.microsoft.com/sql/connect/jdbc/building-the-connection-url?view=sql-server-ver15) connecting to. This name is used to determine the port of a named instance
%% If setting a `db.mssql.instance_name`, `server.port` is no longer required (but still recommended if non-standard)
-define(DB_MSSQL_INSTANCE_NAME, 'db.mssql.instance_name').

%% The name of the primary table that the operation is acting upon, including the database name (if applicable)
%% It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set
-define(DB_SQL_TABLE, 'db.sql.table').

%% The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html)
-define(DB_CASSANDRA_CONSISTENCY_LEVEL, 'db.cassandra.consistency_level').

%% The data center of the coordinating node for a query
-define(DB_CASSANDRA_COORDINATOR_DC, 'db.cassandra.coordinator.dc').

%% The ID of the coordinating node for a query
-define(DB_CASSANDRA_COORDINATOR_ID, 'db.cassandra.coordinator.id').

%% Whether or not the query is idempotent
-define(DB_CASSANDRA_IDEMPOTENCE, 'db.cassandra.idempotence').

%% The fetch size used for paging, i.e. how many rows will be returned at once
-define(DB_CASSANDRA_PAGE_SIZE, 'db.cassandra.page_size').

%% The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively
-define(DB_CASSANDRA_SPECULATIVE_EXECUTION_COUNT, 'db.cassandra.speculative_execution_count').

%% The name of the primary Cassandra table that the operation is acting upon, including the keyspace name (if applicable)
%% This mirrors the db.sql.table attribute but references cassandra rather than sql. It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set
-define(DB_CASSANDRA_TABLE, 'db.cassandra.table').

%% The index of the database being accessed as used in the [`SELECT` command](https://redis.io/commands/select), provided as an integer. To be used instead of the generic `db.name` attribute
-define(DB_REDIS_DATABASE_INDEX, 'db.redis.database_index').

%% The MongoDB collection being accessed within the database stated in `db.name`
-define(DB_MONGODB_COLLECTION, 'db.mongodb.collection').

%% HTTP request method
%% HTTP request method value SHOULD be "known" to the instrumentation.
%% By default, this convention defines "known" methods as the ones listed in [RFC9110](https://www.rfc-editor.org/rfc/rfc9110.html#name-methods)
%% and the PATCH method defined in [RFC5789](https://www.rfc-editor.org/rfc/rfc5789.html).
%% 
%% If the HTTP request method is not known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`.
%% 
%% If the HTTP instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way to override
%% the list of known HTTP methods. If this override is done via environment variable, then the environment variable MUST be named
%% OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated list of case-sensitive known HTTP methods
%% (this list MUST be a full override of the default known method, it is not a list of known methods in addition to the defaults).
%% 
%% HTTP method names are case-sensitive and `http.request.method` attribute value MUST match a known HTTP method name exactly.
%% Instrumentations for specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical equivalent.
%% Tracing instrumentations that do so, MUST also set `http.request.method_original` to the original value
-define(HTTP_REQUEST_METHOD, 'http.request.method').

%% Absolute URL describing a network resource according to [RFC3986](https://www.rfc-editor.org/rfc/rfc3986)
%% For network calls, URL usually has `scheme://host[:port][path][?query][#fragment]` format, where the fragment is not transmitted over HTTP, but if it is known, it SHOULD be included nevertheless.
%% `url.full` MUST NOT contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case username and password SHOULD be redacted and attribute's value SHOULD be `https://REDACTED:REDACTED@www.example.com/`.
%% `url.full` SHOULD capture the absolute URL when it is available (or can be reconstructed). Sensitive content provided in `url.full` SHOULD be scrubbed when instrumentations can identify it
-define(URL_FULL, 'url.full').

%% Represents the identifier of an Elasticsearch cluster
-define(DB_ELASTICSEARCH_CLUSTER_NAME, 'db.elasticsearch.cluster.name').

%% Cosmos client connection mode
-define(DB_COSMOSDB_CONNECTION_MODE, 'db.cosmosdb.connection_mode').

%% Cosmos DB container name
-define(DB_COSMOSDB_CONTAINER, 'db.cosmosdb.container').

%% CosmosDB Operation Type
-define(DB_COSMOSDB_OPERATION_TYPE, 'db.cosmosdb.operation_type').

%% RU consumed for that operation
-define(DB_COSMOSDB_REQUEST_CHARGE, 'db.cosmosdb.request_charge').

%% Cosmos DB status code
-define(DB_COSMOSDB_STATUS_CODE, 'db.cosmosdb.status_code').

%% Cosmos DB sub status code
-define(DB_COSMOSDB_SUB_STATUS_CODE, 'db.cosmosdb.sub_status_code').

%% Unique Cosmos client instance id
-define(DB_COSMOSDB_CLIENT_ID, 'db.cosmosdb.client_id').

%% Request payload size in bytes
-define(DB_COSMOSDB_REQUEST_CONTENT_LENGTH, 'db.cosmosdb.request_content_length').

%% Full user-agent string is generated by Cosmos DB SDK
%% The user-agent value is generated by SDK which is a combination of<br> `sdk_version` : Current version of SDK. e.g. 'cosmos-netstandard-sdk/3.23.0'<br> `direct_pkg_version` : Direct package version used by Cosmos DB SDK. e.g. '3.23.1'<br> `number_of_client_instances` : Number of cosmos client instances created by the application. e.g. '1'<br> `type_of_machine_architecture` : Machine architecture. e.g. 'X64'<br> `operating_system` : Operating System. e.g. 'Linux 5.4.0-1098-azure 104 18'<br> `runtime_framework` : Runtime Framework. e.g. '.NET Core 3.1.32'<br> `failover_information` : Generated key to determine if region failover enabled.
%%    Format Reg-{D (Disabled discovery)}-S(application region)|L(List of preferred regions)|N(None, user did not configure it).
%%    Default value is "NS"
-define(USER_AGENT_ORIGINAL, 'user_agent.original').

%% Name of the code, either "OK" or "ERROR". MUST NOT be set if the status code is UNSET
-define(OTEL_STATUS_CODE, 'otel.status_code').

%% Description of the Status if it has a value, otherwise not set
-define(OTEL_STATUS_DESCRIPTION, 'otel.status_description').

%% Cloud provider-specific native identifier of the monitored cloud resource (e.g. an [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html) on AWS, a [fully qualified resource ID](https://learn.microsoft.com/rest/api/resources/resources/get-by-id) on Azure, a [full resource name](https://cloud.google.com/apis/design/resource_names#full_resource_name) on GCP)
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
%% * **Azure:** The [Fully Qualified Resource ID](https://docs.microsoft.com/rest/api/resources/resources/get-by-id) of the invoked function,
%%   *not* the function app, having the form
%%   `/subscriptions/<SUBSCIPTION_GUID>/resourceGroups/<RG>/providers/Microsoft.Web/sites/<FUNCAPP>/functions/<FUNC>`.
%%   This means that a span attribute MUST be used, as an Azure function app can host multiple functions that would usually share
%%   a TracerProvider
-define(CLOUD_RESOURCE_ID, 'cloud.resource_id').

%% The invocation ID of the current function invocation
-define(FAAS_INVOCATION_ID, 'faas.invocation_id').

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

%% The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name
-define(FAAS_DOCUMENT_COLLECTION, 'faas.document.collection').

%% Describes the type of the operation that was performed on the data
-define(FAAS_DOCUMENT_OPERATION, 'faas.document.operation').

%% The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name
-define(FAAS_DOCUMENT_NAME, 'faas.document.name').

%% A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)
-define(FAAS_DOCUMENT_TIME, 'faas.document.time').

%% A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm)
-define(FAAS_CRON, 'faas.cron').

%% A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)
-define(FAAS_TIME, 'faas.time').

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

%% Describes a class of error the operation ended with
%% If the request fails with an error before response status code was sent or received,
%% `error.type` SHOULD be set to exception type (its fully-qualified class name, if applicable)
%% or a component-specific low cardinality error identifier.
%% 
%% If response status code was sent or received and status indicates an error according to [HTTP span status definition](/docs/http/http-spans.md),
%% `error.type` SHOULD be set to the status code number (represented as a string), an exception type (if thrown) or a component-specific error identifier.
%% 
%% The `error.type` value SHOULD be predictable and SHOULD have low cardinality.
%% Instrumentations SHOULD document the list of errors they report.
%% 
%% The cardinality of `error.type` within one instrumentation library SHOULD be low, but
%% telemetry consumers that aggregate data from multiple instrumentation libraries and applications
%% should be prepared for `error.type` to have high cardinality at query time, when no
%% additional filters are applied.
%% 
%% If the request has completed successfully, instrumentations SHOULD NOT set `error.type`
-define(ERROR_TYPE, 'error.type').

%% [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6)
-define(HTTP_RESPONSE_STATUS_CODE, 'http.response.status_code').

%% [OSI application layer](https://osi-model.com/application-layer/) or non-OSI equivalent
%% The value SHOULD be normalized to lowercase
-define(NETWORK_PROTOCOL_NAME, 'network.protocol.name').

%% The ordinal number of request resending attempt (for any reason, including redirects)
%% The resend count SHOULD be updated each time an HTTP request gets resent by the client, regardless of what was the cause of the resending (e.g. redirection, authorization failure, 503 Server Unavailable, network issues, or any other)
-define(HTTP_REQUEST_RESEND_COUNT, 'http.request.resend_count').

%% The actual version of the protocol used for network communication
%% If protocol version is subject to negotiation (for example using [ALPN](https://www.rfc-editor.org/rfc/rfc7301.html)), this attribute SHOULD be set to the negotiated version. If the actual protocol version is not known, this attribute SHOULD NOT be set
-define(NETWORK_PROTOCOL_VERSION, 'network.protocol.version').

%% The [URI scheme](https://www.rfc-editor.org/rfc/rfc3986#section-3.1) component identifying the used protocol
-define(URL_SCHEME, 'url.scheme').

%% The [URI path](https://www.rfc-editor.org/rfc/rfc3986#section-3.3) component
%% Sensitive content provided in `url.path` SHOULD be scrubbed when instrumentations can identify it
-define(URL_PATH, 'url.path').

%% The matched route, that is, the path template in the format used by the respective server framework
%% MUST NOT be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can NOT substitute it.
%% SHOULD include the [application root](/docs/http/http-spans.md#http-server-definitions) if there is one
-define(HTTP_ROUTE, 'http.route').

%% The [URI query](https://www.rfc-editor.org/rfc/rfc3986#section-3.4) component
%% Sensitive content provided in `url.query` SHOULD be scrubbed when instrumentations can identify it
-define(URL_QUERY, 'url.query').

%% Client address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name
%% The IP address of the original client behind all proxies, if known (e.g. from [Forwarded#for](https://developer.mozilla.org/docs/Web/HTTP/Headers/Forwarded#for), [X-Forwarded-For](https://developer.mozilla.org/docs/Web/HTTP/Headers/X-Forwarded-For), or a similar header). Otherwise, the immediate client peer address
-define(CLIENT_ADDRESS, 'client.address').

%% The port of whichever client was captured in `client.address`
%% When observed from the server side, and when communicating through an intermediary, `client.port` SHOULD represent the client port behind any intermediaries,  for example proxies, if it's available
-define(CLIENT_PORT, 'client.port').

%% Local socket address. Useful in case of a multi-IP host
-define(NETWORK_LOCAL_ADDRESS, 'network.local.address').

%% Local socket port. Useful in case of a multi-port host
-define(NETWORK_LOCAL_PORT, 'network.local.port').

%% The value `aws-api`
-define(RPC_SYSTEM, 'rpc.system').

%% The AWS request ID as returned in the response headers `x-amz-request-id` or `x-amz-requestid`
-define(AWS_REQUEST_ID, 'aws.request_id').

%% The name of the operation corresponding to the request, as returned by the AWS SDK
%% This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side)
-define(RPC_METHOD, 'rpc.method').

%% The name of the service to which a request is made, as returned by the AWS SDK
%% This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side)
-define(RPC_SERVICE, 'rpc.service').

%% The value of the `AttributesToGet` request parameter
-define(AWS_DYNAMODB_ATTRIBUTES_TO_GET, 'aws.dynamodb.attributes_to_get').

%% The value of the `ConsistentRead` request parameter
-define(AWS_DYNAMODB_CONSISTENT_READ, 'aws.dynamodb.consistent_read').

%% The JSON-serialized value of each item in the `ConsumedCapacity` response field
-define(AWS_DYNAMODB_CONSUMED_CAPACITY, 'aws.dynamodb.consumed_capacity').

%% The value of the `IndexName` request parameter
-define(AWS_DYNAMODB_INDEX_NAME, 'aws.dynamodb.index_name').

%% The JSON-serialized value of the `ItemCollectionMetrics` response field
-define(AWS_DYNAMODB_ITEM_COLLECTION_METRICS, 'aws.dynamodb.item_collection_metrics').

%% The value of the `Limit` request parameter
-define(AWS_DYNAMODB_LIMIT, 'aws.dynamodb.limit').

%% The value of the `ProjectionExpression` request parameter
-define(AWS_DYNAMODB_PROJECTION, 'aws.dynamodb.projection').

%% The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter
-define(AWS_DYNAMODB_PROVISIONED_READ_CAPACITY, 'aws.dynamodb.provisioned_read_capacity').

%% The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter
-define(AWS_DYNAMODB_PROVISIONED_WRITE_CAPACITY, 'aws.dynamodb.provisioned_write_capacity').

%% The value of the `Select` request parameter
-define(AWS_DYNAMODB_SELECT, 'aws.dynamodb.select').

%% The keys in the `RequestItems` object field
-define(AWS_DYNAMODB_TABLE_NAMES, 'aws.dynamodb.table_names').

%% The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field
-define(AWS_DYNAMODB_GLOBAL_SECONDARY_INDEXES, 'aws.dynamodb.global_secondary_indexes').

%% The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field
-define(AWS_DYNAMODB_LOCAL_SECONDARY_INDEXES, 'aws.dynamodb.local_secondary_indexes').

%% The value of the `ExclusiveStartTableName` request parameter
-define(AWS_DYNAMODB_EXCLUSIVE_START_TABLE, 'aws.dynamodb.exclusive_start_table').

%% The number of items in the `TableNames` response parameter
-define(AWS_DYNAMODB_TABLE_COUNT, 'aws.dynamodb.table_count').

%% The value of the `ScanIndexForward` request parameter
-define(AWS_DYNAMODB_SCAN_FORWARD, 'aws.dynamodb.scan_forward').

%% The value of the `Count` response parameter
-define(AWS_DYNAMODB_COUNT, 'aws.dynamodb.count').

%% The value of the `ScannedCount` response parameter
-define(AWS_DYNAMODB_SCANNED_COUNT, 'aws.dynamodb.scanned_count').

%% The value of the `Segment` request parameter
-define(AWS_DYNAMODB_SEGMENT, 'aws.dynamodb.segment').

%% The value of the `TotalSegments` request parameter
-define(AWS_DYNAMODB_TOTAL_SEGMENTS, 'aws.dynamodb.total_segments').

%% The JSON-serialized value of each item in the `AttributeDefinitions` request field
-define(AWS_DYNAMODB_ATTRIBUTE_DEFINITIONS, 'aws.dynamodb.attribute_definitions').

%% The JSON-serialized value of each item in the `GlobalSecondaryIndexUpdates` request field
-define(AWS_DYNAMODB_GLOBAL_SECONDARY_INDEX_UPDATES, 'aws.dynamodb.global_secondary_index_updates').

%% The S3 bucket name the request refers to. Corresponds to the `--bucket` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations
%% The `bucket` attribute is applicable to all S3 operations that reference a bucket, i.e. that require the bucket name as a mandatory parameter.
%% This applies to almost all S3 operations except `list-buckets`
-define(AWS_S3_BUCKET, 'aws.s3.bucket').

%% The source object (in the form `bucket`/`key`) for the copy operation
%% The `copy_source` attribute applies to S3 copy operations and corresponds to the `--copy-source` parameter
%% of the [copy-object operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html).
%% This applies in particular to the following operations:
%% 
%% - [copy-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html)
%% - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)
-define(AWS_S3_COPY_SOURCE, 'aws.s3.copy_source').

%% The delete request container that specifies the objects to be deleted
%% The `delete` attribute is only applicable to the [delete-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-object.html) operation.
%% The `delete` attribute corresponds to the `--delete` parameter of the
%% [delete-objects operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-objects.html)
-define(AWS_S3_DELETE, 'aws.s3.delete').

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

%% The part number of the part being uploaded in a multipart-upload operation. This is a positive integer between 1 and 10,000
%% The `part_number` attribute is only applicable to the [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
%% and [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html) operations.
%% The `part_number` attribute corresponds to the `--part-number` parameter of the
%% [upload-part operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
-define(AWS_S3_PART_NUMBER, 'aws.s3.part_number').

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

%% The GraphQL document being executed
%% The value may be sanitized to exclude sensitive information
-define(GRAPHQL_DOCUMENT, 'graphql.document').

%% The name of the operation being executed
-define(GRAPHQL_OPERATION_NAME, 'graphql.operation.name').

%% The type of the operation being executed
-define(GRAPHQL_OPERATION_TYPE, 'graphql.operation.type').

%% A string identifying the kind of messaging operation
%% If a custom value is used, it MUST be of low cardinality
-define(MESSAGING_OPERATION, 'messaging.operation').

%% An identifier for the messaging system being used. See below for a list of well-known identifiers
-define(MESSAGING_SYSTEM, 'messaging.system').

%% The number of messages sent, received, or processed in the scope of the batching operation
%% Instrumentations SHOULD NOT set `messaging.batch.message_count` on spans that operate with a single message. When a messaging client library supports both batch and single-message API for the same operation, instrumentations SHOULD use `messaging.batch.message_count` for batching APIs and SHOULD NOT use it for single-message APIs
-define(MESSAGING_BATCH_MESSAGE_COUNT, 'messaging.batch.message_count').

%% A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name)
-define(MESSAGING_DESTINATION_ANONYMOUS, 'messaging.destination.anonymous').

%% The message destination name
%% Destination name SHOULD uniquely identify a specific queue, topic or other entity within the broker. If
%% the broker doesn't have such notion, the destination name SHOULD uniquely identify the broker
-define(MESSAGING_DESTINATION_NAME, 'messaging.destination.name').

%% Low cardinality representation of the messaging destination name
%% Destination names could be constructed from templates. An example would be a destination name involving a user name or product id. Although the destination name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation
-define(MESSAGING_DESTINATION_TEMPLATE, 'messaging.destination.template').

%% A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed
-define(MESSAGING_DESTINATION_TEMPORARY, 'messaging.destination.temporary').

%% A unique identifier for the client that consumes or produces a message
-define(MESSAGING_CLIENT_ID, 'messaging.client_id').

%% The identifier of the partition messages are sent to or received from, unique within the `messaging.destination.name`
-define(MESSAGING_DESTINATION_PARTITION_ID, 'messaging.destination.partition.id').

%% The size of the message body in bytes
%% This can refer to both the compressed or uncompressed body size. If both sizes are known, the uncompressed
%% body size should be used
-define(MESSAGING_MESSAGE_BODY_SIZE, 'messaging.message.body.size').

%% The conversation ID identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID"
-define(MESSAGING_MESSAGE_CONVERSATION_ID, 'messaging.message.conversation_id').

%% The size of the message body and metadata in bytes
%% This can refer to both the compressed or uncompressed size. If both sizes are known, the uncompressed
%% size should be used
-define(MESSAGING_MESSAGE_ENVELOPE_SIZE, 'messaging.message.envelope.size').

%% A value used by the messaging system as an identifier for the message, represented as a string
-define(MESSAGING_MESSAGE_ID, 'messaging.message.id').

%% [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication)
%% The value SHOULD be normalized to lowercase.
%% 
%% Consider always setting the transport when setting a port number, since
%% a port number is ambiguous without knowing the transport. For example
%% different processes could be listening on TCP port 12345 and UDP port 12345
-define(NETWORK_TRANSPORT, 'network.transport').

%% [OSI network layer](https://osi-model.com/network-layer/) or non-OSI equivalent
%% The value SHOULD be normalized to lowercase
-define(NETWORK_TYPE, 'network.type').

%% The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request
-define(RPC_GRPC_STATUS_CODE, 'rpc.grpc.status_code').

%% `error.code` property of response if it is an error response
-define(RPC_JSONRPC_ERROR_CODE, 'rpc.jsonrpc.error_code').

%% Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 doesn't specify this, the value can be omitted
-define(RPC_JSONRPC_VERSION, 'rpc.jsonrpc.version').

%% `error.message` property of response if it is an error response
-define(RPC_JSONRPC_ERROR_MESSAGE, 'rpc.jsonrpc.error_message').

%% `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification
-define(RPC_JSONRPC_REQUEST_ID, 'rpc.jsonrpc.request_id').

%% The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values
-define(RPC_CONNECT_RPC_ERROR_CODE, 'rpc.connect_rpc.error_code').
