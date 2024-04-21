%% The schema url for telemetry resources
-define(COMMON_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.25.0">>).

%% The name of the invoked function
%% SHOULD be equal to the `faas.name` resource attribute of the invoked function
-define(FAAS_INVOKED_NAME, 'faas.invoked_name').

%% The cloud provider of the invoked function
%% SHOULD be equal to the `cloud.provider` resource attribute of the invoked function
-define(FAAS_INVOKED_PROVIDER, 'faas.invoked_provider').

%% The cloud region of the invoked function
%% SHOULD be equal to the `cloud.region` resource attribute of the invoked function
-define(FAAS_INVOKED_REGION, 'faas.invoked_region').

%% Type of the trigger which caused this function invocation
-define(FAAS_TRIGGER, 'faas.trigger').

%% Client address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name
%% When observed from the server side, and when communicating through an intermediary, `client.address` SHOULD represent the client address behind any intermediaries,  for example proxies, if it's available
-define(CLIENT_ADDRESS, 'client.address').

%% Client port number
%% When observed from the server side, and when communicating through an intermediary, `client.port` SHOULD represent the client port behind any intermediaries,  for example proxies, if it's available
-define(CLIENT_PORT, 'client.port').

%% Server domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name
%% When observed from the client side, and when communicating through an intermediary, `server.address` SHOULD represent the server address behind any intermediaries, for example proxies, if it's available
-define(SERVER_ADDRESS, 'server.address').

%% Server port number
%% When observed from the client side, and when communicating through an intermediary, `server.port` SHOULD represent the server port behind any intermediaries, for example proxies, if it's available
-define(SERVER_PORT, 'server.port').

%% Source address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name
%% When observed from the destination side, and when communicating through an intermediary, `source.address` SHOULD represent the source address behind any intermediaries, for example proxies, if it's available
-define(SOURCE_ADDRESS, 'source.address').

%% Source port number
-define(SOURCE_PORT, 'source.port').

%% Destination address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name
%% When observed from the source side, and when communicating through an intermediary, `destination.address` SHOULD represent the destination address behind any intermediaries, for example proxies, if it's available
-define(DESTINATION_ADDRESS, 'destination.address').

%% Destination port number
-define(DESTINATION_PORT, 'destination.port').

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

%% The actual version of the protocol used for network communication
%% If protocol version is subject to negotiation (for example using [ALPN](https://www.rfc-editor.org/rfc/rfc7301.html)), this attribute SHOULD be set to the negotiated version. If the actual protocol version is not known, this attribute SHOULD NOT be set
-define(NETWORK_PROTOCOL_VERSION, 'network.protocol.version').

%% The [URI scheme](https://www.rfc-editor.org/rfc/rfc3986#section-3.1) component identifying the used protocol
-define(URL_SCHEME, 'url.scheme').

%% The matched route, that is, the path template in the format used by the respective server framework
%% MUST NOT be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can NOT substitute it.
%% SHOULD include the [application root](/docs/http/http-spans.md#http-server-definitions) if there is one
-define(HTTP_ROUTE, 'http.route').

%% Identifies the class / type of event
%% Event names are subject to the same rules as [attribute names](https://github.com/open-telemetry/opentelemetry-specification/tree/v1.31.0/specification/common/attribute-naming.md). Notably, event names are namespaced to avoid collisions and provide a clean separation of semantics for events in separate domains like browser, mobile, and kubernetes
-define(EVENT_NAME, 'event.name').

%% A unique identifier for the Log Record
%% If an id is provided, other log records with the same id will be considered duplicates and can be removed safely. This means, that two distinguishable log records MUST have different values.
%% The id MAY be an [Universally Unique Lexicographically Sortable Identifier (ULID)](https://github.com/ulid/spec), but other identifiers (e.g. UUID) may be used as needed
-define(LOG_RECORD_UID, 'log.record.uid').

%% The exception message
-define(EXCEPTION_MESSAGE, 'exception.message').

%% A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG
-define(EXCEPTION_STACKTRACE, 'exception.stacktrace').

%% The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it
-define(EXCEPTION_TYPE, 'exception.type').

%% The stream associated with the log. See below for a list of well-known values
-define(LOG_IOSTREAM, 'log.iostream').

%% The basename of the file
-define(LOG_FILE_NAME, 'log.file.name').

%% The basename of the file, with symlinks resolved
-define(LOG_FILE_NAME_RESOLVED, 'log.file.name_resolved').

%% The full path to the file
-define(LOG_FILE_PATH, 'log.file.path').

%% The full path to the file, with symlinks resolved
-define(LOG_FILE_PATH_RESOLVED, 'log.file.path_resolved').

%% An identifier for the messaging system being used. See below for a list of well-known identifiers
-define(MESSAGING_SYSTEM, 'messaging.system').

%% The name of the connection pool; unique within the instrumented application. In case the connection pool implementation doesn't provide a name, instrumentation should use a combination of `server.address` and `server.port` attributes formatted as `server.address:server.port`
-define(POOL_NAME, 'pool.name').

%% The state of a connection in the pool
-define(STATE, 'state').

%% Rate-limiting result, shows whether the lease was acquired or contains a rejection reason
-define(ASPNETCORE_RATE_LIMITING_RESULT, 'aspnetcore.rate_limiting.result').

%% Full type name of the [`IExceptionHandler`](https://learn.microsoft.com/dotnet/api/microsoft.aspnetcore.diagnostics.iexceptionhandler) implementation that handled the exception
-define(ASPNETCORE_DIAGNOSTICS_HANDLER_TYPE, 'aspnetcore.diagnostics.handler.type').

%% Rate limiting policy name
-define(ASPNETCORE_RATE_LIMITING_POLICY, 'aspnetcore.rate_limiting.policy').

%% Flag indicating if request was handled by the application pipeline
-define(ASPNETCORE_REQUEST_IS_UNHANDLED, 'aspnetcore.request.is_unhandled').

%% A value that indicates whether the matched route is a fallback route
-define(ASPNETCORE_ROUTING_IS_FALLBACK, 'aspnetcore.routing.is_fallback').

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

%% SignalR HTTP connection closure status
-define(SIGNALR_CONNECTION_STATUS, 'signalr.connection.status').

%% [SignalR transport type](https://github.com/dotnet/aspnetcore/blob/main/src/SignalR/docs/specs/TransportProtocols.md)
-define(SIGNALR_TRANSPORT, 'signalr.transport').

%% Name of the buffer pool
%% Pool names are generally obtained via [BufferPoolMXBean#getName()](https://docs.oracle.com/en/java/javase/11/docs/api/java.management/java/lang/management/BufferPoolMXBean.html#getName())
-define(JVM_BUFFER_POOL_NAME, 'jvm.buffer.pool.name').

%% Name of the memory pool
%% Pool names are generally obtained via [MemoryPoolMXBean#getName()](https://docs.oracle.com/en/java/javase/11/docs/api/java.management/java/lang/management/MemoryPoolMXBean.html#getName())
-define(JVM_MEMORY_POOL_NAME, 'jvm.memory.pool.name').

%% The type of memory
-define(JVM_MEMORY_TYPE, 'jvm.memory.type').

%% The message destination name
%% Destination name SHOULD uniquely identify a specific queue, topic or other entity within the broker. If
%% the broker doesn't have such notion, the destination name SHOULD uniquely identify the broker
-define(MESSAGING_DESTINATION_NAME, 'messaging.destination.name').

%% Low cardinality representation of the messaging destination name
%% Destination names could be constructed from templates. An example would be a destination name involving a user name or product id. Although the destination name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation
-define(MESSAGING_DESTINATION_TEMPLATE, 'messaging.destination.template').

%% The CPU state for this data point. A process SHOULD be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels
-define(PROCESS_CPU_STATE, 'process.cpu.state').

%% A string identifying the remoting system. See below for a list of well-known identifiers
-define(RPC_SYSTEM, 'rpc.system').

%% The name of the (logical) method being called, must be equal to the $method part in the span name
%% This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side)
-define(RPC_METHOD, 'rpc.method').

%% The full (logical) name of the service being called, including its package name, if applicable
%% This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side)
-define(RPC_SERVICE, 'rpc.service').

%% The device identifier
-define(SYSTEM_DEVICE, 'system.device').

%% The logical CPU number [0..n-1]
-define(SYSTEM_CPU_LOGICAL_NUMBER, 'system.cpu.logical_number').

%% The CPU state for this data point. A system's CPU SHOULD be characterized *either* by data points with no `state` labels, *or only* data points with `state` labels
-define(SYSTEM_CPU_STATE, 'system.cpu.state').

%% The memory state
-define(SYSTEM_MEMORY_STATE, 'system.memory.state').

%% The paging access direction
-define(SYSTEM_PAGING_DIRECTION, 'system.paging.direction').

%% The memory paging state
-define(SYSTEM_PAGING_STATE, 'system.paging.state').

%% The memory paging type
-define(SYSTEM_PAGING_TYPE, 'system.paging.type').

%% The filesystem mode
-define(SYSTEM_FILESYSTEM_MODE, 'system.filesystem.mode').

%% The filesystem mount path
-define(SYSTEM_FILESYSTEM_MOUNTPOINT, 'system.filesystem.mountpoint').

%% The filesystem state
-define(SYSTEM_FILESYSTEM_STATE, 'system.filesystem.state').

%% The filesystem type
-define(SYSTEM_FILESYSTEM_TYPE, 'system.filesystem.type').

%% A stateless protocol MUST NOT set this attribute
-define(SYSTEM_NETWORK_STATE, 'system.network.state').

%% The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)
-define(SYSTEM_PROCESS_STATUS, 'system.process.status').

%% Local address of the network connection - IP address or Unix domain socket name
-define(NETWORK_LOCAL_ADDRESS, 'network.local.address').

%% Local port number of the network connection
-define(NETWORK_LOCAL_PORT, 'network.local.port').

%% Peer address of the network connection - IP address or Unix domain socket name
-define(NETWORK_PEER_ADDRESS, 'network.peer.address').

%% Peer port number of the network connection
-define(NETWORK_PEER_PORT, 'network.peer.port').

%% The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network
-define(NETWORK_CARRIER_ICC, 'network.carrier.icc').

%% The mobile carrier country code
-define(NETWORK_CARRIER_MCC, 'network.carrier.mcc').

%% The mobile carrier network code
-define(NETWORK_CARRIER_MNC, 'network.carrier.mnc').

%% The name of the mobile carrier
-define(NETWORK_CARRIER_NAME, 'network.carrier.name').

%% This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection
-define(NETWORK_CONNECTION_SUBTYPE, 'network.connection.subtype').

%% The internet connection type
-define(NETWORK_CONNECTION_TYPE, 'network.connection.type').

%% Uniquely identifies the framework API revision offered by a version (`os.version`) of the android operating system. More information can be found [here](https://developer.android.com/guide/topics/manifest/uses-sdk-element#ApiLevels)
-define(ANDROID_OS_API_LEVEL, 'android.os.api_level').

%% The JSON-serialized value of each item in the `AttributeDefinitions` request field
-define(AWS_DYNAMODB_ATTRIBUTE_DEFINITIONS, 'aws.dynamodb.attribute_definitions').

%% The value of the `AttributesToGet` request parameter
-define(AWS_DYNAMODB_ATTRIBUTES_TO_GET, 'aws.dynamodb.attributes_to_get').

%% The value of the `ConsistentRead` request parameter
-define(AWS_DYNAMODB_CONSISTENT_READ, 'aws.dynamodb.consistent_read').

%% The JSON-serialized value of each item in the `ConsumedCapacity` response field
-define(AWS_DYNAMODB_CONSUMED_CAPACITY, 'aws.dynamodb.consumed_capacity').

%% The value of the `Count` response parameter
-define(AWS_DYNAMODB_COUNT, 'aws.dynamodb.count').

%% The value of the `ExclusiveStartTableName` request parameter
-define(AWS_DYNAMODB_EXCLUSIVE_START_TABLE, 'aws.dynamodb.exclusive_start_table').

%% The JSON-serialized value of each item in the `GlobalSecondaryIndexUpdates` request field
-define(AWS_DYNAMODB_GLOBAL_SECONDARY_INDEX_UPDATES, 'aws.dynamodb.global_secondary_index_updates').

%% The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field
-define(AWS_DYNAMODB_GLOBAL_SECONDARY_INDEXES, 'aws.dynamodb.global_secondary_indexes').

%% The value of the `IndexName` request parameter
-define(AWS_DYNAMODB_INDEX_NAME, 'aws.dynamodb.index_name').

%% The JSON-serialized value of the `ItemCollectionMetrics` response field
-define(AWS_DYNAMODB_ITEM_COLLECTION_METRICS, 'aws.dynamodb.item_collection_metrics').

%% The value of the `Limit` request parameter
-define(AWS_DYNAMODB_LIMIT, 'aws.dynamodb.limit').

%% The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field
-define(AWS_DYNAMODB_LOCAL_SECONDARY_INDEXES, 'aws.dynamodb.local_secondary_indexes').

%% The value of the `ProjectionExpression` request parameter
-define(AWS_DYNAMODB_PROJECTION, 'aws.dynamodb.projection').

%% The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter
-define(AWS_DYNAMODB_PROVISIONED_READ_CAPACITY, 'aws.dynamodb.provisioned_read_capacity').

%% The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter
-define(AWS_DYNAMODB_PROVISIONED_WRITE_CAPACITY, 'aws.dynamodb.provisioned_write_capacity').

%% The value of the `ScanIndexForward` request parameter
-define(AWS_DYNAMODB_SCAN_FORWARD, 'aws.dynamodb.scan_forward').

%% The value of the `ScannedCount` response parameter
-define(AWS_DYNAMODB_SCANNED_COUNT, 'aws.dynamodb.scanned_count').

%% The value of the `Segment` request parameter
-define(AWS_DYNAMODB_SEGMENT, 'aws.dynamodb.segment').

%% The value of the `Select` request parameter
-define(AWS_DYNAMODB_SELECT, 'aws.dynamodb.select').

%% The number of items in the `TableNames` response parameter
-define(AWS_DYNAMODB_TABLE_COUNT, 'aws.dynamodb.table_count').

%% The keys in the `RequestItems` object field
-define(AWS_DYNAMODB_TABLE_NAMES, 'aws.dynamodb.table_names').

%% The value of the `TotalSegments` request parameter
-define(AWS_DYNAMODB_TOTAL_SEGMENTS, 'aws.dynamodb.total_segments').

%% Array of brand name and version separated by a space
%% This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.brands`)
-define(BROWSER_BRANDS, 'browser.brands').

%% Preferred language of the user using the browser
%% This value is intended to be taken from the Navigator API `navigator.language`
-define(BROWSER_LANGUAGE, 'browser.language').

%% A boolean that is true if the browser is running on a mobile device
%% This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.mobile`). If unavailable, this attribute SHOULD be left unset
-define(BROWSER_MOBILE, 'browser.mobile').

%% The platform on which the browser is running
%% This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.platform`). If unavailable, the legacy `navigator.platform` API SHOULD NOT be used instead and this attribute SHOULD be left unset in order for the values to be consistent.
%% The list of possible values is defined in the [W3C User-Agent Client Hints specification](https://wicg.github.io/ua-client-hints/#sec-ch-ua-platform). Note that some (but not all) of these values can overlap with values in the [`os.type` and `os.name` attributes](./os.md). However, for consistency, the values in the `browser.platform` attribute should capture the exact value that the user agent provides
-define(BROWSER_PLATFORM, 'browser.platform').

%% The cloud account ID the resource is assigned to
-define(CLOUD_ACCOUNT_ID, 'cloud.account.id').

%% Cloud regions often have multiple, isolated locations known as zones to increase availability. Availability zone represents the zone where the resource is running
%% Availability zones are called "zones" on Alibaba Cloud and Google Cloud
-define(CLOUD_AVAILABILITY_ZONE, 'cloud.availability_zone').

%% The cloud platform in use
%% The prefix of the service SHOULD match the one specified in `cloud.provider`
-define(CLOUD_PLATFORM, 'cloud.platform').

%% Name of the cloud provider
-define(CLOUD_PROVIDER, 'cloud.provider').

%% The geographical region the resource is running
%% Refer to your provider's docs to see the available regions, for example [Alibaba Cloud regions](https://www.alibabacloud.com/help/doc-detail/40654.htm), [AWS regions](https://aws.amazon.com/about-aws/global-infrastructure/regions_az/), [Azure regions](https://azure.microsoft.com/global-infrastructure/geographies/), [Google Cloud regions](https://cloud.google.com/about/locations), or [Tencent Cloud regions](https://www.tencentcloud.com/document/product/213/6091)
-define(CLOUD_REGION, 'cloud.region').

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

%% The command used to run the container (i.e. the command name)
%% If using embedded credentials or sensitive data, it is recommended to remove them to prevent potential leakage
-define(CONTAINER_COMMAND, 'container.command').

%% All the command arguments (including the command/executable itself) run by the container. [2]
-define(CONTAINER_COMMAND_ARGS, 'container.command_args').

%% The full command run by the container as a single string representing the full command. [2]
-define(CONTAINER_COMMAND_LINE, 'container.command_line').

%% The CPU state for this data point
-define(CONTAINER_CPU_STATE, 'container.cpu.state').

%% Container ID. Usually a UUID, as for example used to [identify Docker containers](https://docs.docker.com/engine/reference/run/#container-identification). The UUID might be abbreviated
-define(CONTAINER_ID, 'container.id').

%% Runtime specific image identifier. Usually a hash algorithm followed by a UUID
%% Docker defines a sha256 of the image id; `container.image.id` corresponds to the `Image` field from the Docker container inspect [API](https://docs.docker.com/engine/api/v1.43/#tag/Container/operation/ContainerInspect) endpoint.
%% K8s defines a link to the container registry repository with digest `"imageID": "registry.azurecr.io /namespace/service/dockerfile@sha256:bdeabd40c3a8a492eaf9e8e44d0ebbb84bac7ee25ac0cf8a7159d25f62555625"`.
%% The ID is assinged by the container runtime and can vary in different environments. Consider using `oci.manifest.digest` if it is important to identify the same image in different environments/runtimes
-define(CONTAINER_IMAGE_ID, 'container.image.id').

%% Name of the image the container was built on
-define(CONTAINER_IMAGE_NAME, 'container.image.name').

%% Repo digests of the container image as provided by the container runtime
%% [Docker](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect) and [CRI](https://github.com/kubernetes/cri-api/blob/c75ef5b473bbe2d0a4fc92f82235efd665ea8e9f/pkg/apis/runtime/v1/api.proto#L1237-L1238) report those under the `RepoDigests` field
-define(CONTAINER_IMAGE_REPO_DIGESTS, 'container.image.repo_digests').

%% Container image tags. An example can be found in [Docker Image Inspect](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect). Should be only the `<tag>` section of the full name for example from `registry.example.com/my-org/my-image:<tag>`
-define(CONTAINER_IMAGE_TAGS, 'container.image.tags').

%% Container name used by container runtime
-define(CONTAINER_NAME, 'container.name').

%% The container runtime managing this container
-define(CONTAINER_RUNTIME, 'container.runtime').

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

%% Unique Cosmos client instance id
-define(DB_COSMOSDB_CLIENT_ID, 'db.cosmosdb.client_id').

%% Cosmos client connection mode
-define(DB_COSMOSDB_CONNECTION_MODE, 'db.cosmosdb.connection_mode').

%% Cosmos DB container name
-define(DB_COSMOSDB_CONTAINER, 'db.cosmosdb.container').

%% CosmosDB Operation Type
-define(DB_COSMOSDB_OPERATION_TYPE, 'db.cosmosdb.operation_type').

%% RU consumed for that operation
-define(DB_COSMOSDB_REQUEST_CHARGE, 'db.cosmosdb.request_charge').

%% Request payload size in bytes
-define(DB_COSMOSDB_REQUEST_CONTENT_LENGTH, 'db.cosmosdb.request_content_length').

%% Cosmos DB status code
-define(DB_COSMOSDB_STATUS_CODE, 'db.cosmosdb.status_code').

%% Cosmos DB sub status code
-define(DB_COSMOSDB_SUB_STATUS_CODE, 'db.cosmosdb.sub_status_code').

%% Represents the identifier of an Elasticsearch cluster
-define(DB_ELASTICSEARCH_CLUSTER_NAME, 'db.elasticsearch.cluster.name').

%% An identifier (address, unique name, or any other identifier) of the database instance that is executing queries or mutations on the current connection. This is useful in cases where the database is running in a clustered environment and the instrumentation is able to record the node executing the query. The client may obtain this value in databases like MySQL using queries like `select @@hostname`
-define(DB_INSTANCE_ID, 'db.instance.id').

%% The MongoDB collection being accessed within the database stated in `db.name`
-define(DB_MONGODB_COLLECTION, 'db.mongodb.collection').

%% The Microsoft SQL Server [instance name](https://docs.microsoft.com/sql/connect/jdbc/building-the-connection-url?view=sql-server-ver15) connecting to. This name is used to determine the port of a named instance
%% If setting a `db.mssql.instance_name`, `server.port` is no longer required (but still recommended if non-standard)
-define(DB_MSSQL_INSTANCE_NAME, 'db.mssql.instance_name').

%% This attribute is used to report the name of the database being accessed. For commands that switch the database, this should be set to the target database (even if the command fails)
%% In some SQL databases, the database name to be used is called "schema name". In case there are multiple layers that could be considered for database name (e.g. Oracle instance name and schema name), the database name to be used is the more specific layer (e.g. Oracle schema name)
-define(DB_NAME, 'db.name').

%% The name of the operation being executed, e.g. the [MongoDB command name](https://docs.mongodb.com/manual/reference/command/#database-operations) such as `findAndModify`, or the SQL keyword
%% When setting this to an SQL keyword, it is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if the operation name is provided by the library being instrumented. If the SQL statement has an ambiguous operation, or performs more than one operation, this value may be omitted
-define(DB_OPERATION, 'db.operation').

%% The index of the database being accessed as used in the [`SELECT` command](https://redis.io/commands/select), provided as an integer. To be used instead of the generic `db.name` attribute
-define(DB_REDIS_DATABASE_INDEX, 'db.redis.database_index').

%% The name of the primary table that the operation is acting upon, including the database name (if applicable)
%% It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value MUST NOT be set
-define(DB_SQL_TABLE, 'db.sql.table').

%% The database statement being executed
-define(DB_STATEMENT, 'db.statement').

%% An identifier for the database management system (DBMS) product being used. See below for a list of well-known identifiers
-define(DB_SYSTEM, 'db.system').

%% Username for accessing the database
-define(DB_USER, 'db.user').

%% Name of the [deployment environment](https://wikipedia.org/wiki/Deployment_environment) (aka deployment tier)
%% `deployment.environment` does not affect the uniqueness constraints defined through
%% the `service.namespace`, `service.name` and `service.instance.id` resource attributes.
%% This implies that resources carrying the following attribute combinations MUST be
%% considered to be identifying the same service:
%% 
%% * `service.name=frontend`, `deployment.environment=production`
%% * `service.name=frontend`, `deployment.environment=staging`
-define(DEPLOYMENT_ENVIRONMENT, 'deployment.environment').

%% Deprecated, use `server.address`, `server.port` attributes instead
%% @deprecated "Replaced by `server.address` and `server.port`."
-define(DB_CONNECTION_STRING, 'db.connection_string').

%% Deprecated, use `db.instance.id` instead
%% @deprecated Replaced by `db.instance.id`
-define(DB_ELASTICSEARCH_NODE_NAME, 'db.elasticsearch.node.name').

%% Removed, no replacement at this time
%% @deprecated Removed as not used
-define(DB_JDBC_DRIVER_CLASSNAME, 'db.jdbc.driver_classname').

%% Deprecated, use `network.protocol.name` instead
%% @deprecated Replaced by `network.protocol.name`
-define(HTTP_FLAVOR, 'http.flavor').

%% Deprecated, use `http.request.method` instead
%% @deprecated Replaced by `http.request.method`
-define(HTTP_METHOD, 'http.method').

%% Deprecated, use `http.request.header.content-length` instead
%% @deprecated Replaced by `http.request.header.content-length`
-define(HTTP_REQUEST_CONTENT_LENGTH, 'http.request_content_length').

%% Deprecated, use `http.response.header.content-length` instead
%% @deprecated Replaced by `http.response.header.content-length`
-define(HTTP_RESPONSE_CONTENT_LENGTH, 'http.response_content_length').

%% Deprecated, use `url.scheme` instead
%% @deprecated Replaced by `url.scheme` instead
-define(HTTP_SCHEME, 'http.scheme').

%% Deprecated, use `http.response.status_code` instead
%% @deprecated Replaced by `http.response.status_code`
-define(HTTP_STATUS_CODE, 'http.status_code').

%% Deprecated, use `url.path` and `url.query` instead
%% @deprecated Split to `url.path` and `url.query
-define(HTTP_TARGET, 'http.target').

%% Deprecated, use `url.full` instead
%% @deprecated Replaced by `url.full`
-define(HTTP_URL, 'http.url').

%% Deprecated, use `user_agent.original` instead
%% @deprecated Replaced by `user_agent.original`
-define(HTTP_USER_AGENT, 'http.user_agent').

%% "Deprecated, use `messaging.destination.partition.id` instead."
%% @deprecated Replaced by `messaging.destination.partition.id`
-define(MESSAGING_KAFKA_DESTINATION_PARTITION, 'messaging.kafka.destination.partition').

%% Deprecated, use `server.address`
%% @deprecated Replaced by `server.address`
-define(NET_HOST_NAME, 'net.host.name').

%% Deprecated, use `server.port`
%% @deprecated Replaced by `server.port`
-define(NET_HOST_PORT, 'net.host.port').

%% Deprecated, use `server.address` on client spans and `client.address` on server spans
%% @deprecated Replaced by `server.address` on client spans and `client.address` on server spans
-define(NET_PEER_NAME, 'net.peer.name').

%% Deprecated, use `server.port` on client spans and `client.port` on server spans
%% @deprecated Replaced by `server.port` on client spans and `client.port` on server spans
-define(NET_PEER_PORT, 'net.peer.port').

%% Deprecated, use `network.protocol.name`
%% @deprecated Replaced by `network.protocol.name`
-define(NET_PROTOCOL_NAME, 'net.protocol.name').

%% Deprecated, use `network.protocol.version`
%% @deprecated Replaced by `network.protocol.version`
-define(NET_PROTOCOL_VERSION, 'net.protocol.version').

%% Deprecated, use `network.transport` and `network.type`
%% @deprecated Split to `network.transport` and `network.type`
-define(NET_SOCK_FAMILY, 'net.sock.family').

%% Deprecated, use `network.local.address`
%% @deprecated Replaced by `network.local.address`
-define(NET_SOCK_HOST_ADDR, 'net.sock.host.addr').

%% Deprecated, use `network.local.port`
%% @deprecated Replaced by `network.local.port`
-define(NET_SOCK_HOST_PORT, 'net.sock.host.port').

%% Deprecated, use `network.peer.address`
%% @deprecated Replaced by `network.peer.address`
-define(NET_SOCK_PEER_ADDR, 'net.sock.peer.addr').

%% Deprecated, no replacement at this time
%% @deprecated Removed
-define(NET_SOCK_PEER_NAME, 'net.sock.peer.name').

%% Deprecated, use `network.peer.port`
%% @deprecated Replaced by `network.peer.port`
-define(NET_SOCK_PEER_PORT, 'net.sock.peer.port').

%% Deprecated, use `network.transport`
%% @deprecated Replaced by `network.transport`
-define(NET_TRANSPORT, 'net.transport').

%% Deprecated, use `system.process.status` instead
%% @deprecated Replaced by `system.process.status`
-define(SYSTEM_PROCESSES_STATUS, 'system.processes.status').

%% A unique identifier representing the device
%% The device identifier MUST only be defined using the values outlined below. This value is not an advertising identifier and MUST NOT be used as such. On iOS (Swift or Objective-C), this value MUST be equal to the [vendor identifier](https://developer.apple.com/documentation/uikit/uidevice/1620059-identifierforvendor). On Android (Java or Kotlin), this value MUST be equal to the Firebase Installation ID or a globally unique UUID which is persisted across sessions in your application. More information can be found [here](https://developer.android.com/training/articles/user-data-ids) on best practices and exact implementation details. Caution should be taken when storing personal data or anything which can identify a user. GDPR and data protection laws may apply, ensure you do your own due diligence
-define(DEVICE_ID, 'device.id').

%% The name of the device manufacturer
%% The Android OS provides this field via [Build](https://developer.android.com/reference/android/os/Build#MANUFACTURER). iOS apps SHOULD hardcode the value `Apple`
-define(DEVICE_MANUFACTURER, 'device.manufacturer').

%% The model identifier for the device
%% It's recommended this value represents a machine-readable version of the model identifier rather than the market or consumer-friendly name of the device
-define(DEVICE_MODEL_IDENTIFIER, 'device.model.identifier').

%% The marketing name for the device model
%% It's recommended this value represents a human-readable version of the device model rather than a machine-readable alternative
-define(DEVICE_MODEL_NAME, 'device.model.name').

%% The disk IO operation direction
-define(DISK_IO_DIRECTION, 'disk.io.direction').

%% The name being queried
%% If the name field contains non-printable characters (below 32 or above 126), those characters should be represented as escaped base 10 integers (\DDD). Back slashes and quotes should be escaped. Tabs, carriage returns, and line feeds should be converted to \t, \r, and \n respectively
-define(DNS_QUESTION_NAME, 'dns.question.name').

%% Username or client_id extracted from the access token or [Authorization](https://tools.ietf.org/html/rfc7235#section-4.2) header in the inbound request from outside the system
-define(ENDUSER_ID, 'enduser.id').

%% Actual/assumed role the client is making the request under extracted from token or application security context
-define(ENDUSER_ROLE, 'enduser.role').

%% Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an [OAuth 2.0 Access Token](https://tools.ietf.org/html/rfc6749#section-3.3) or an attribute value in a [SAML 2.0 Assertion](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html)
-define(ENDUSER_SCOPE, 'enduser.scope').

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
%% as done in the [example for recording span exceptions](#recording-an-exception).
%% 
%% It follows that an exception may still escape the scope of the span
%% even if the `exception.escaped` attribute was not set or set to false,
%% since the event might have been recorded at a time where it was not
%% clear whether the exception will escape
-define(EXCEPTION_ESCAPED, 'exception.escaped').

%% A boolean that is true if the serverless function is executed for the first time (aka cold-start)
-define(FAAS_COLDSTART, 'faas.coldstart').

%% A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm)
-define(FAAS_CRON, 'faas.cron').

%% The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name
-define(FAAS_DOCUMENT_COLLECTION, 'faas.document.collection').

%% The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name
-define(FAAS_DOCUMENT_NAME, 'faas.document.name').

%% Describes the type of the operation that was performed on the data
-define(FAAS_DOCUMENT_OPERATION, 'faas.document.operation').

%% A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)
-define(FAAS_DOCUMENT_TIME, 'faas.document.time').

%% The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version
%% * **AWS Lambda:** Use the (full) log stream name
-define(FAAS_INSTANCE, 'faas.instance').

%% The invocation ID of the current function invocation
-define(FAAS_INVOCATION_ID, 'faas.invocation_id').

%% The amount of memory available to the serverless function converted to Bytes
%% It's recommended to set this attribute since e.g. too little memory can easily stop a Java AWS Lambda function from working correctly. On AWS Lambda, the environment variable `AWS_LAMBDA_FUNCTION_MEMORY_SIZE` provides this information (which must be multiplied by 1,048,576)
-define(FAAS_MAX_MEMORY, 'faas.max_memory').

%% The name of the single function that this runtime instance executes
%% This is the name of the function as configured/deployed on the FaaS
%% platform and is usually different from the name of the callback
%% function (which may be stored in the
%% [`code.namespace`/`code.function`](/docs/general/attributes.md#source-code-attributes)
%% span attributes).
%% 
%% For some cloud providers, the above definition is ambiguous. The following
%% definition of function name MUST be used for this attribute
%% (and consequently the span name) for the listed cloud providers/products:
%% 
%% * **Azure:**  The full name `<FUNCAPP>/<FUNC>`, i.e., function app name
%%   followed by a forward slash followed by the function name (this form
%%   can also be seen in the resource JSON for the function).
%%   This means that a span attribute MUST be used, as an Azure function
%%   app can host multiple functions that would usually share
%%   a TracerProvider (see also the `cloud.resource_id` attribute)
-define(FAAS_NAME, 'faas.name').

%% A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)
-define(FAAS_TIME, 'faas.time').

%% The immutable version of the function being executed
%% Depending on the cloud provider and platform, use:
%% 
%% * **AWS Lambda:** The [function version](https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html)
%%   (an integer represented as a decimal string).
%% * **Google Cloud Run (Services):** The [revision](https://cloud.google.com/run/docs/managing/revisions)
%%   (i.e., the function name plus the revision suffix).
%% * **Google Cloud Functions:** The value of the
%%   [`K_REVISION` environment variable](https://cloud.google.com/functions/docs/env-var#runtime_environment_variables_set_automatically).
%% * **Azure Functions:** Not applicable. Do not set this attribute
-define(FAAS_VERSION, 'faas.version').

%% The unique identifier of the feature flag
-define(FEATURE_FLAG_KEY, 'feature_flag.key').

%% The name of the service provider that performs the flag evaluation
-define(FEATURE_FLAG_PROVIDER_NAME, 'feature_flag.provider_name').

%% SHOULD be a semantic identifier for a value. If one is unavailable, a stringified version of the value can be used
%% A semantic identifier, commonly referred to as a variant, provides a means
%% for referring to a value without including the value itself. This can
%% provide additional context for understanding the meaning behind a value.
%% For example, the variant `red` maybe be used for the value `#c05543`.
%% 
%% A stringified version of the value can be used in situations where a
%% semantic identifier is unavailable. String representation of the value
%% should be determined by the implementer
-define(FEATURE_FLAG_VARIANT, 'feature_flag.variant').

%% Directory where the file is located. It should include the drive letter, when appropriate
-define(FILE_DIRECTORY, 'file.directory').

%% File extension, excluding the leading dot
%% When the file name has multiple extensions (example.tar.gz), only the last one should be captured ("gz", not "tar.gz")
-define(FILE_EXTENSION, 'file.extension').

%% Name of the file including the extension, without the directory
-define(FILE_NAME, 'file.name').

%% Full path to the file, including the file name. It should include the drive letter, when appropriate
-define(FILE_PATH, 'file.path').

%% File size in bytes
-define(FILE_SIZE, 'file.size').

%% The name of the Cloud Run [execution](https://cloud.google.com/run/docs/managing/job-executions) being run for the Job, as set by the [`CLOUD_RUN_EXECUTION`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable
-define(GCP_CLOUD_RUN_JOB_EXECUTION, 'gcp.cloud_run.job.execution').

%% The index for a task within an execution as provided by the [`CLOUD_RUN_TASK_INDEX`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable
-define(GCP_CLOUD_RUN_JOB_TASK_INDEX, 'gcp.cloud_run.job.task_index').

%% The hostname of a GCE instance. This is the full value of the default or [custom hostname](https://cloud.google.com/compute/docs/instances/custom-hostname-vm)
-define(GCP_GCE_INSTANCE_HOSTNAME, 'gcp.gce.instance.hostname').

%% The instance name of a GCE instance. This is the value provided by `host.name`, the visible name of the instance in the Cloud Console UI, and the prefix for the default hostname of the instance as defined by the [default internal DNS name](https://cloud.google.com/compute/docs/internal-dns#instance-fully-qualified-domain-names)
-define(GCP_GCE_INSTANCE_NAME, 'gcp.gce.instance.name').

%% The CPU architecture the host system is running on
-define(HOST_ARCH, 'host.arch').

%% The amount of level 2 memory cache available to the processor (in Bytes)
-define(HOST_CPU_CACHE_L2_SIZE, 'host.cpu.cache.l2.size').

%% Family or generation of the CPU
-define(HOST_CPU_FAMILY, 'host.cpu.family').

%% Model identifier. It provides more granular information about the CPU, distinguishing it from other CPUs within the same family
-define(HOST_CPU_MODEL_ID, 'host.cpu.model.id').

%% Model designation of the processor
-define(HOST_CPU_MODEL_NAME, 'host.cpu.model.name').

%% Stepping or core revisions
-define(HOST_CPU_STEPPING, 'host.cpu.stepping').

%% Processor manufacturer identifier. A maximum 12-character string
%% [CPUID](https://wiki.osdev.org/CPUID) command returns the vendor ID string in EBX, EDX and ECX registers. Writing these to memory in this order results in a 12-character string
-define(HOST_CPU_VENDOR_ID, 'host.cpu.vendor.id').

%% Unique host ID. For Cloud, this must be the instance_id assigned by the cloud provider. For non-containerized systems, this should be the `machine-id`. See the table below for the sources to use to determine the `machine-id` based on operating system
-define(HOST_ID, 'host.id').

%% VM image ID or host OS image ID. For Cloud, this value is from the provider
-define(HOST_IMAGE_ID, 'host.image.id').

%% Name of the VM image or OS install the host was instantiated from
-define(HOST_IMAGE_NAME, 'host.image.name').

%% The version string of the VM image or host OS as defined in [Version Attributes](/docs/resource/README.md#version-attributes)
-define(HOST_IMAGE_VERSION, 'host.image.version').

%% Available IP addresses of the host, excluding loopback interfaces
%% IPv4 Addresses MUST be specified in dotted-quad notation. IPv6 addresses MUST be specified in the [RFC 5952](https://www.rfc-editor.org/rfc/rfc5952.html) format
-define(HOST_IP, 'host.ip').

%% Available MAC addresses of the host, excluding loopback interfaces
%% MAC Addresses MUST be represented in [IEEE RA hexadecimal form](https://standards.ieee.org/wp-content/uploads/import/documents/tutorials/eui.pdf): as hyphen-separated octets in uppercase hexadecimal form from most to least significant
-define(HOST_MAC, 'host.mac').

%% Name of the host. On Unix systems, it may contain what the hostname command returns, or the fully qualified hostname, or another name specified by the user
-define(HOST_NAME, 'host.name').

%% Type of host. For Cloud, this must be the machine type
-define(HOST_TYPE, 'host.type').

%% State of the HTTP connection in the HTTP connection pool
-define(HTTP_CONNECTION_STATE, 'http.connection.state').

%% The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size
-define(HTTP_REQUEST_BODY_SIZE, 'http.request.body.size').

%% Original HTTP method sent by the client in the request line
-define(HTTP_REQUEST_METHOD_ORIGINAL, 'http.request.method_original').

%% The ordinal number of request resending attempt (for any reason, including redirects)
%% The resend count SHOULD be updated each time an HTTP request gets resent by the client, regardless of what was the cause of the resending (e.g. redirection, authorization failure, 503 Server Unavailable, network issues, or any other)
-define(HTTP_REQUEST_RESEND_COUNT, 'http.request.resend_count').

%% The total size of the request in bytes. This should be the total number of bytes sent over the wire, including the request line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and request body if any
-define(HTTP_REQUEST_SIZE, 'http.request.size').

%% The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size
-define(HTTP_RESPONSE_BODY_SIZE, 'http.response.body.size').

%% The total size of the response in bytes. This should be the total number of bytes sent over the wire, including the status line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and response body and trailers if any
-define(HTTP_RESPONSE_SIZE, 'http.response.size').

%% The name of the cluster
-define(K8S_CLUSTER_NAME, 'k8s.cluster.name').

%% A pseudo-ID for the cluster, set to the UID of the `kube-system` namespace
%% K8s doesn't have support for obtaining a cluster ID. If this is ever
%% added, we will recommend collecting the `k8s.cluster.uid` through the
%% official APIs. In the meantime, we are able to use the `uid` of the
%% `kube-system` namespace as a proxy for cluster ID. Read on for the
%% rationale.
%% 
%% Every object created in a K8s cluster is assigned a distinct UID. The
%% `kube-system` namespace is used by Kubernetes itself and will exist
%% for the lifetime of the cluster. Using the `uid` of the `kube-system`
%% namespace is a reasonable proxy for the K8s ClusterID as it will only
%% change if the cluster is rebuilt. Furthermore, Kubernetes UIDs are
%% UUIDs as standardized by
%% [ISO/IEC 9834-8 and ITU-T X.667](https://www.itu.int/ITU-T/studygroups/com17/oid.html).
%% Which states:
%% 
%% > If generated according to one of the mechanisms defined in Rec.
%%   ITU-T X.667 | ISO/IEC 9834-8, a UUID is either guaranteed to be
%%   different from all other UUIDs generated before 3603 A.D., or is
%%   extremely likely to be different (depending on the mechanism chosen).
%% 
%% Therefore, UIDs between clusters should be extremely unlikely to
%% conflict
-define(K8S_CLUSTER_UID, 'k8s.cluster.uid').

%% The name of the Container from Pod specification, must be unique within a Pod. Container runtime usually uses different globally unique name (`container.name`)
-define(K8S_CONTAINER_NAME, 'k8s.container.name').

%% Number of times the container was restarted. This attribute can be used to identify a particular container (running or stopped) within a container spec
-define(K8S_CONTAINER_RESTART_COUNT, 'k8s.container.restart_count').

%% The name of the CronJob
-define(K8S_CRONJOB_NAME, 'k8s.cronjob.name').

%% The UID of the CronJob
-define(K8S_CRONJOB_UID, 'k8s.cronjob.uid').

%% The name of the DaemonSet
-define(K8S_DAEMONSET_NAME, 'k8s.daemonset.name').

%% The UID of the DaemonSet
-define(K8S_DAEMONSET_UID, 'k8s.daemonset.uid').

%% The name of the Deployment
-define(K8S_DEPLOYMENT_NAME, 'k8s.deployment.name').

%% The UID of the Deployment
-define(K8S_DEPLOYMENT_UID, 'k8s.deployment.uid').

%% The name of the Job
-define(K8S_JOB_NAME, 'k8s.job.name').

%% The UID of the Job
-define(K8S_JOB_UID, 'k8s.job.uid').

%% The name of the namespace that the pod is running in
-define(K8S_NAMESPACE_NAME, 'k8s.namespace.name').

%% The name of the Node
-define(K8S_NODE_NAME, 'k8s.node.name').

%% The UID of the Node
-define(K8S_NODE_UID, 'k8s.node.uid').

%% The name of the Pod
-define(K8S_POD_NAME, 'k8s.pod.name').

%% The UID of the Pod
-define(K8S_POD_UID, 'k8s.pod.uid').

%% The name of the ReplicaSet
-define(K8S_REPLICASET_NAME, 'k8s.replicaset.name').

%% The UID of the ReplicaSet
-define(K8S_REPLICASET_UID, 'k8s.replicaset.uid').

%% The name of the StatefulSet
-define(K8S_STATEFULSET_NAME, 'k8s.statefulset.name').

%% The UID of the StatefulSet
-define(K8S_STATEFULSET_UID, 'k8s.statefulset.uid').

%% The number of messages sent, received, or processed in the scope of the batching operation
%% Instrumentations SHOULD NOT set `messaging.batch.message_count` on spans that operate with a single message. When a messaging client library supports both batch and single-message API for the same operation, instrumentations SHOULD use `messaging.batch.message_count` for batching APIs and SHOULD NOT use it for single-message APIs
-define(MESSAGING_BATCH_MESSAGE_COUNT, 'messaging.batch.message_count').

%% A unique identifier for the client that consumes or produces a message
-define(MESSAGING_CLIENT_ID, 'messaging.client_id').

%% A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name)
-define(MESSAGING_DESTINATION_ANONYMOUS, 'messaging.destination.anonymous').

%% The identifier of the partition messages are sent to or received from, unique within the `messaging.destination.name`
-define(MESSAGING_DESTINATION_PARTITION_ID, 'messaging.destination.partition.id').

%% A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed
-define(MESSAGING_DESTINATION_TEMPORARY, 'messaging.destination.temporary').

%% A boolean that is true if the publish message destination is anonymous (could be unnamed or have auto-generated name)
-define(MESSAGING_DESTINATION_PUBLISH_ANONYMOUS, 'messaging.destination_publish.anonymous').

%% The name of the original destination the message was published to
%% The name SHOULD uniquely identify a specific queue, topic, or other entity within the broker. If
%% the broker doesn't have such notion, the original destination name SHOULD uniquely identify the broker
-define(MESSAGING_DESTINATION_PUBLISH_NAME, 'messaging.destination_publish.name').

%% The name of the consumer group the event consumer is associated with
-define(MESSAGING_EVENTHUBS_CONSUMER_GROUP, 'messaging.eventhubs.consumer.group').

%% The UTC epoch seconds at which the message has been accepted and stored in the entity
-define(MESSAGING_EVENTHUBS_MESSAGE_ENQUEUED_TIME, 'messaging.eventhubs.message.enqueued_time').

%% The ordering key for a given message. If the attribute is not present, the message does not have an ordering key
-define(MESSAGING_GCP_PUBSUB_MESSAGE_ORDERING_KEY, 'messaging.gcp_pubsub.message.ordering_key').

%% Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers
-define(MESSAGING_KAFKA_CONSUMER_GROUP, 'messaging.kafka.consumer.group').

%% Message keys in Kafka are used for grouping alike messages to ensure they're processed on the same partition. They differ from `messaging.message.id` in that they're not unique. If the key is `null`, the attribute MUST NOT be set
%% If the key type is not string, it's string representation has to be supplied for the attribute. If the key has no unambiguous, canonical string form, don't include its value
-define(MESSAGING_KAFKA_MESSAGE_KEY, 'messaging.kafka.message.key').

%% The offset of a record in the corresponding Kafka partition
-define(MESSAGING_KAFKA_MESSAGE_OFFSET, 'messaging.kafka.message.offset').

%% A boolean that is true if the message is a tombstone
-define(MESSAGING_KAFKA_MESSAGE_TOMBSTONE, 'messaging.kafka.message.tombstone').

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

%% A string identifying the kind of messaging operation
%% If a custom value is used, it MUST be of low cardinality
-define(MESSAGING_OPERATION, 'messaging.operation').

%% RabbitMQ message routing key
-define(MESSAGING_RABBITMQ_DESTINATION_ROUTING_KEY, 'messaging.rabbitmq.destination.routing_key').

%% RabbitMQ message delivery tag
-define(MESSAGING_RABBITMQ_MESSAGE_DELIVERY_TAG, 'messaging.rabbitmq.message.delivery_tag').

%% Name of the RocketMQ producer/consumer group that is handling the message. The client type is identified by the SpanKind
-define(MESSAGING_ROCKETMQ_CLIENT_GROUP, 'messaging.rocketmq.client_group').

%% Model of message consumption. This only applies to consumer spans
-define(MESSAGING_ROCKETMQ_CONSUMPTION_MODEL, 'messaging.rocketmq.consumption_model').

%% The delay time level for delay message, which determines the message delay time
-define(MESSAGING_ROCKETMQ_MESSAGE_DELAY_TIME_LEVEL, 'messaging.rocketmq.message.delay_time_level').

%% The timestamp in milliseconds that the delay message is expected to be delivered to consumer
-define(MESSAGING_ROCKETMQ_MESSAGE_DELIVERY_TIMESTAMP, 'messaging.rocketmq.message.delivery_timestamp').

%% It is essential for FIFO message. Messages that belong to the same message group are always processed one by one within the same consumer group
-define(MESSAGING_ROCKETMQ_MESSAGE_GROUP, 'messaging.rocketmq.message.group').

%% Key(s) of message, another way to mark message besides message id
-define(MESSAGING_ROCKETMQ_MESSAGE_KEYS, 'messaging.rocketmq.message.keys').

%% The secondary classifier of message besides topic
-define(MESSAGING_ROCKETMQ_MESSAGE_TAG, 'messaging.rocketmq.message.tag').

%% Type of message
-define(MESSAGING_ROCKETMQ_MESSAGE_TYPE, 'messaging.rocketmq.message.type').

%% Namespace of RocketMQ resources, resources in different namespaces are individual
-define(MESSAGING_ROCKETMQ_NAMESPACE, 'messaging.rocketmq.namespace').

%% The name of the subscription in the topic messages are received from
-define(MESSAGING_SERVICEBUS_DESTINATION_SUBSCRIPTION_NAME, 'messaging.servicebus.destination.subscription_name').

%% Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock)
-define(MESSAGING_SERVICEBUS_DISPOSITION_STATUS, 'messaging.servicebus.disposition_status').

%% Number of deliveries that have been attempted for this message
-define(MESSAGING_SERVICEBUS_MESSAGE_DELIVERY_COUNT, 'messaging.servicebus.message.delivery_count').

%% The UTC epoch seconds at which the message has been accepted and stored in the entity
-define(MESSAGING_SERVICEBUS_MESSAGE_ENQUEUED_TIME, 'messaging.servicebus.message.enqueued_time').

%% The network IO operation direction
-define(NETWORK_IO_DIRECTION, 'network.io.direction').

%% The digest of the OCI image manifest. For container images specifically is the digest by which the container image is known
%% Follows [OCI Image Manifest Specification](https://github.com/opencontainers/image-spec/blob/main/manifest.md), and specifically the [Digest property](https://github.com/opencontainers/image-spec/blob/main/descriptor.md#digests).
%% An example can be found in [Example Image Manifest](https://docs.docker.com/registry/spec/manifest-v2-2/#example-image-manifest)
-define(OCI_MANIFEST_DIGEST, 'oci.manifest.digest').

%% Unique identifier for a particular build or compilation of the operating system
-define(OS_BUILD_ID, 'os.build_id').

%% Human readable (not intended to be parsed) OS version information, like e.g. reported by `ver` or `lsb_release -a` commands
-define(OS_DESCRIPTION, 'os.description').

%% Human readable operating system name
-define(OS_NAME, 'os.name').

%% The operating system type
-define(OS_TYPE, 'os.type').

%% The version string of the operating system as defined in [Version Attributes](/docs/resource/README.md#version-attributes)
-define(OS_VERSION, 'os.version').

%% The command used to launch the process (i.e. the command name). On Linux based systems, can be set to the zeroth string in `proc/[pid]/cmdline`. On Windows, can be set to the first parameter extracted from `GetCommandLineW`
-define(PROCESS_COMMAND, 'process.command').

%% All the command arguments (including the command/executable itself) as received by the process. On Linux-based systems (and some other Unixoid systems supporting procfs), can be set according to the list of null-delimited strings extracted from `proc/[pid]/cmdline`. For libc-based executables, this would be the full argv vector passed to `main`
-define(PROCESS_COMMAND_ARGS, 'process.command_args').

%% The full command used to launch the process as a single string representing the full command. On Windows, can be set to the result of `GetCommandLineW`. Do not set this if you have to assemble it just for monitoring; use `process.command_args` instead
-define(PROCESS_COMMAND_LINE, 'process.command_line').

%% The name of the process executable. On Linux based systems, can be set to the `Name` in `proc/[pid]/status`. On Windows, can be set to the base name of `GetProcessImageFileNameW`
-define(PROCESS_EXECUTABLE_NAME, 'process.executable.name').

%% The full path to the process executable. On Linux based systems, can be set to the target of `proc/[pid]/exe`. On Windows, can be set to the result of `GetProcessImageFileNameW`
-define(PROCESS_EXECUTABLE_PATH, 'process.executable.path').

%% The username of the user that owns the process
-define(PROCESS_OWNER, 'process.owner').

%% Parent Process identifier (PPID)
-define(PROCESS_PARENT_PID, 'process.parent_pid').

%% Process identifier (PID)
-define(PROCESS_PID, 'process.pid').

%% An additional description about the runtime of the process, for example a specific vendor customization of the runtime environment
-define(PROCESS_RUNTIME_DESCRIPTION, 'process.runtime.description').

%% The name of the runtime of this process. For compiled native binaries, this SHOULD be the name of the compiler
-define(PROCESS_RUNTIME_NAME, 'process.runtime.name').

%% The version of the runtime of this process, as returned by the runtime without modification
-define(PROCESS_RUNTIME_VERSION, 'process.runtime.version').

%% The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values
-define(RPC_CONNECT_RPC_ERROR_CODE, 'rpc.connect_rpc.error_code').

%% The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request
-define(RPC_GRPC_STATUS_CODE, 'rpc.grpc.status_code').

%% `error.code` property of response if it is an error response
-define(RPC_JSONRPC_ERROR_CODE, 'rpc.jsonrpc.error_code').

%% `error.message` property of response if it is an error response
-define(RPC_JSONRPC_ERROR_MESSAGE, 'rpc.jsonrpc.error_message').

%% `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification
-define(RPC_JSONRPC_REQUEST_ID, 'rpc.jsonrpc.request_id').

%% Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 doesn't specify this, the value can be omitted
-define(RPC_JSONRPC_VERSION, 'rpc.jsonrpc.version').

%% The string ID of the service instance
%% MUST be unique for each instance of the same `service.namespace,service.name` pair (in other words
%% `service.namespace,service.name,service.instance.id` triplet MUST be globally unique). The ID helps to
%% distinguish instances of the same service that exist at the same time (e.g. instances of a horizontally scaled
%% service).
%% 
%% Implementations, such as SDKs, are recommended to generate a random Version 1 or Version 4 [RFC
%% 4122](https://www.ietf.org/rfc/rfc4122.txt) UUID, but are free to use an inherent unique ID as the source of
%% this value if stability is desirable. In that case, the ID SHOULD be used as source of a UUID Version 5 and
%% SHOULD use the following UUID as the namespace: `4d63009a-8d0f-11ee-aad7-4c796ed8e320`.
%% 
%% UUIDs are typically recommended, as only an opaque value for the purposes of identifying a service instance is
%% needed. Similar to what can be seen in the man page for the
%% [`/etc/machine-id`](https://www.freedesktop.org/software/systemd/man/machine-id.html) file, the underlying
%% data, such as pod name and namespace should be treated as confidential, being the user's choice to expose it
%% or not via another resource attribute.
%% 
%% For applications running behind an application server (like unicorn), we do not recommend using one identifier
%% for all processes participating in the application. Instead, it's recommended each division (e.g. a worker
%% thread in unicorn) to have its own instance.id.
%% 
%% It's not recommended for a Collector to set `service.instance.id` if it can't unambiguously determine the
%% service instance that is generating that telemetry. For instance, creating an UUID based on `pod.name` will
%% likely be wrong, as the Collector might not know from which container within that pod the telemetry originated.
%% However, Collectors can set the `service.instance.id` if they can unambiguously determine the service instance
%% for that telemetry. This is typically the case for scraping receivers, as they know the target address and
%% port
-define(SERVICE_INSTANCE_ID, 'service.instance.id').

%% Logical name of the service
%% MUST be the same for all instances of horizontally scaled services. If the value was not specified, SDKs MUST fallback to `unknown_service:` concatenated with [`process.executable.name`](process.md#process), e.g. `unknown_service:bash`. If `process.executable.name` is not available, the value MUST be set to `unknown_service`
-define(SERVICE_NAME, 'service.name').

%% A namespace for `service.name`
%% A string value having a meaning that helps to distinguish a group of services, for example the team name that owns a group of services. `service.name` is expected to be unique within the same namespace. If `service.namespace` is not specified in the Resource then `service.name` is expected to be unique for all services that have no explicit namespace defined (so the empty/unspecified namespace is simply one more valid namespace). Zero-length namespace string is assumed equal to unspecified namespace
-define(SERVICE_NAMESPACE, 'service.namespace').

%% The version string of the service API or implementation. The format is not defined by these conventions
-define(SERVICE_VERSION, 'service.version').

%% A unique id to identify a session
-define(SESSION_ID, 'session.id').

%% The previous `session.id` for this user, when known
-define(SESSION_PREVIOUS_ID, 'session.previous_id').

%% The language of the telemetry SDK
-define(TELEMETRY_SDK_LANGUAGE, 'telemetry.sdk.language').

%% The name of the telemetry SDK as defined above
%% The OpenTelemetry SDK MUST set the `telemetry.sdk.name` attribute to `opentelemetry`.
%% If another SDK, like a fork or a vendor-provided implementation, is used, this SDK MUST set the
%% `telemetry.sdk.name` attribute to the fully-qualified class or module name of this SDK's main entry point
%% or another suitable identifier depending on the language.
%% The identifier `opentelemetry` is reserved and MUST NOT be used in this case.
%% All custom identifiers SHOULD be stable across different versions of an implementation
-define(TELEMETRY_SDK_NAME, 'telemetry.sdk.name').

%% The version string of the telemetry SDK
-define(TELEMETRY_SDK_VERSION, 'telemetry.sdk.version').

%% The name of the auto instrumentation agent or distribution, if used
%% Official auto instrumentation agents and distributions SHOULD set the `telemetry.distro.name` attribute to
%% a string starting with `opentelemetry-`, e.g. `opentelemetry-java-instrumentation`
-define(TELEMETRY_DISTRO_NAME, 'telemetry.distro.name').

%% The version string of the auto instrumentation agent or distribution, if used
-define(TELEMETRY_DISTRO_VERSION, 'telemetry.distro.version').

%% Current "managed" thread ID (as opposed to OS thread ID)
-define(THREAD_ID, 'thread.id').

%% Current thread name
-define(THREAD_NAME, 'thread.name').

%% String indicating the [cipher](https://datatracker.ietf.org/doc/html/rfc5246#appendix-A.5) used during the current connection
%% The values allowed for `tls.cipher` MUST be one of the `Descriptions` of the [registered TLS Cipher Suits](https://www.iana.org/assignments/tls-parameters/tls-parameters.xhtml#table-tls-parameters-4)
-define(TLS_CIPHER, 'tls.cipher').

%% PEM-encoded stand-alone certificate offered by the client. This is usually mutually-exclusive of `client.certificate_chain` since this value also exists in that list
-define(TLS_CLIENT_CERTIFICATE, 'tls.client.certificate').

%% Array of PEM-encoded certificates that make up the certificate chain offered by the client. This is usually mutually-exclusive of `client.certificate` since that value should be the first certificate in the chain
-define(TLS_CLIENT_CERTIFICATE_CHAIN, 'tls.client.certificate_chain').

%% Certificate fingerprint using the MD5 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash
-define(TLS_CLIENT_HASH_MD5, 'tls.client.hash.md5').

%% Certificate fingerprint using the SHA1 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash
-define(TLS_CLIENT_HASH_SHA1, 'tls.client.hash.sha1').

%% Certificate fingerprint using the SHA256 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash
-define(TLS_CLIENT_HASH_SHA256, 'tls.client.hash.sha256').

%% Distinguished name of [subject](https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.6) of the issuer of the x.509 certificate presented by the client
-define(TLS_CLIENT_ISSUER, 'tls.client.issuer').

%% A hash that identifies clients based on how they perform an SSL/TLS handshake
-define(TLS_CLIENT_JA3, 'tls.client.ja3').

%% Date/Time indicating when client certificate is no longer considered valid
-define(TLS_CLIENT_NOT_AFTER, 'tls.client.not_after').

%% Date/Time indicating when client certificate is first considered valid
-define(TLS_CLIENT_NOT_BEFORE, 'tls.client.not_before').

%% Also called an SNI, this tells the server which hostname to which the client is attempting to connect to
-define(TLS_CLIENT_SERVER_NAME, 'tls.client.server_name').

%% Distinguished name of subject of the x.509 certificate presented by the client
-define(TLS_CLIENT_SUBJECT, 'tls.client.subject').

%% Array of ciphers offered by the client during the client hello
-define(TLS_CLIENT_SUPPORTED_CIPHERS, 'tls.client.supported_ciphers').

%% String indicating the curve used for the given cipher, when applicable
-define(TLS_CURVE, 'tls.curve').

%% Boolean flag indicating if the TLS negotiation was successful and transitioned to an encrypted tunnel
-define(TLS_ESTABLISHED, 'tls.established').

%% String indicating the protocol being tunneled. Per the values in the [IANA registry](https://www.iana.org/assignments/tls-extensiontype-values/tls-extensiontype-values.xhtml#alpn-protocol-ids), this string should be lower case
-define(TLS_NEXT_PROTOCOL, 'tls.next_protocol').

%% Normalized lowercase protocol name parsed from original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)
-define(TLS_PROTOCOL_NAME, 'tls.protocol.name').

%% Numeric part of the version parsed from the original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)
-define(TLS_PROTOCOL_VERSION, 'tls.protocol.version').

%% Boolean flag indicating if this TLS connection was resumed from an existing TLS negotiation
-define(TLS_RESUMED, 'tls.resumed').

%% PEM-encoded stand-alone certificate offered by the server. This is usually mutually-exclusive of `server.certificate_chain` since this value also exists in that list
-define(TLS_SERVER_CERTIFICATE, 'tls.server.certificate').

%% Array of PEM-encoded certificates that make up the certificate chain offered by the server. This is usually mutually-exclusive of `server.certificate` since that value should be the first certificate in the chain
-define(TLS_SERVER_CERTIFICATE_CHAIN, 'tls.server.certificate_chain').

%% Certificate fingerprint using the MD5 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash
-define(TLS_SERVER_HASH_MD5, 'tls.server.hash.md5').

%% Certificate fingerprint using the SHA1 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash
-define(TLS_SERVER_HASH_SHA1, 'tls.server.hash.sha1').

%% Certificate fingerprint using the SHA256 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash
-define(TLS_SERVER_HASH_SHA256, 'tls.server.hash.sha256').

%% Distinguished name of [subject](https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.6) of the issuer of the x.509 certificate presented by the client
-define(TLS_SERVER_ISSUER, 'tls.server.issuer').

%% A hash that identifies servers based on how they perform an SSL/TLS handshake
-define(TLS_SERVER_JA3S, 'tls.server.ja3s').

%% Date/Time indicating when server certificate is no longer considered valid
-define(TLS_SERVER_NOT_AFTER, 'tls.server.not_after').

%% Date/Time indicating when server certificate is first considered valid
-define(TLS_SERVER_NOT_BEFORE, 'tls.server.not_before').

%% Distinguished name of subject of the x.509 certificate presented by the server
-define(TLS_SERVER_SUBJECT, 'tls.server.subject').

%% Domain extracted from the `url.full`, such as "opentelemetry.io"
%% In some cases a URL may refer to an IP and/or port directly, without a domain name. In this case, the IP address would go to the domain field. If the URL contains a [literal IPv6 address](https://www.rfc-editor.org/rfc/rfc2732#section-2) enclosed by `[` and `]`, the `[` and `]` characters should also be captured in the domain field
-define(URL_DOMAIN, 'url.domain').

%% The file extension extracted from the `url.full`, excluding the leading dot
%% The file extension is only set if it exists, as not every url has a file extension. When the file name has multiple extensions `example.tar.gz`, only the last one should be captured `gz`, not `tar.gz`
-define(URL_EXTENSION, 'url.extension').

%% The [URI fragment](https://www.rfc-editor.org/rfc/rfc3986#section-3.5) component
-define(URL_FRAGMENT, 'url.fragment').

%% Absolute URL describing a network resource according to [RFC3986](https://www.rfc-editor.org/rfc/rfc3986)
%% For network calls, URL usually has `scheme://host[:port][path][?query][#fragment]` format, where the fragment is not transmitted over HTTP, but if it is known, it SHOULD be included nevertheless.
%% `url.full` MUST NOT contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case username and password SHOULD be redacted and attribute's value SHOULD be `https://REDACTED:REDACTED@www.example.com/`.
%% `url.full` SHOULD capture the absolute URL when it is available (or can be reconstructed). Sensitive content provided in `url.full` SHOULD be scrubbed when instrumentations can identify it
-define(URL_FULL, 'url.full').

%% Unmodified original URL as seen in the event source
%% In network monitoring, the observed URL may be a full URL, whereas in access logs, the URL is often just represented as a path. This field is meant to represent the URL as it was observed, complete or not.
%% `url.original` might contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case password and username SHOULD NOT be redacted and attribute's value SHOULD remain the same
-define(URL_ORIGINAL, 'url.original').

%% The [URI path](https://www.rfc-editor.org/rfc/rfc3986#section-3.3) component
%% Sensitive content provided in `url.path` SHOULD be scrubbed when instrumentations can identify it
-define(URL_PATH, 'url.path').

%% Port extracted from the `url.full`
-define(URL_PORT, 'url.port').

%% The [URI query](https://www.rfc-editor.org/rfc/rfc3986#section-3.4) component
%% Sensitive content provided in `url.query` SHOULD be scrubbed when instrumentations can identify it
-define(URL_QUERY, 'url.query').

%% The highest registered url domain, stripped of the subdomain
%% This value can be determined precisely with the [public suffix list](http://publicsuffix.org). For example, the registered domain for `foo.example.com` is `example.com`. Trying to approximate this by simply taking the last two labels will not work well for TLDs such as `co.uk`
-define(URL_REGISTERED_DOMAIN, 'url.registered_domain').

%% The subdomain portion of a fully qualified domain name includes all of the names except the host name under the registered_domain. In a partially qualified domain, or if the qualification level of the full name cannot be determined, subdomain contains all of the names below the registered domain
%% The subdomain portion of `www.east.mydomain.co.uk` is `east`. If the domain has multiple levels of subdomain, such as `sub2.sub1.example.com`, the subdomain field should contain `sub2.sub1`, with no trailing period
-define(URL_SUBDOMAIN, 'url.subdomain').

%% The effective top level domain (eTLD), also known as the domain suffix, is the last part of the domain name. For example, the top level domain for example.com is `com`
%% This value can be determined precisely with the [public suffix list](http://publicsuffix.org)
-define(URL_TOP_LEVEL_DOMAIN, 'url.top_level_domain').

%% Name of the user-agent extracted from original. Usually refers to the browser's name
%% [Example](https://www.whatsmyua.info) of extracting browser's name from original string. In the case of using a user-agent for non-browser products, such as microservices with multiple names/versions inside the `user_agent.original`, the most significant name SHOULD be selected. In such a scenario it should align with `user_agent.version`
-define(USER_AGENT_NAME, 'user_agent.name').

%% Value of the [HTTP User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#field.user-agent) header sent by the client
-define(USER_AGENT_ORIGINAL, 'user_agent.original').

%% Version of the user-agent extracted from original. Usually refers to the browser's version
%% [Example](https://www.whatsmyua.info) of extracting browser's version from original string. In the case of using a user-agent for non-browser products, such as microservices with multiple names/versions inside the `user_agent.original`, the most significant version SHOULD be selected. In such a scenario it should align with `user_agent.name`
-define(USER_AGENT_VERSION, 'user_agent.version').
