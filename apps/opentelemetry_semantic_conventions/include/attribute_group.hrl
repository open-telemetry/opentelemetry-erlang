%% The schema url for telemetry resources
-define(ATTRIBUTE_GROUP_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.20.0">>).

%% HTTP request method
-define(HTTP_METHOD, 'http.method').

%% [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6)
-define(HTTP_STATUS_CODE, 'http.status_code').

%% Application layer protocol used. The value SHOULD be normalized to lowercase
-define(NET_PROTOCOL_NAME, 'net.protocol.name').

%% Version of the application layer protocol used. See note below
%% `net.protocol.version` refers to the version of the protocol used and might be different from the protocol client's version. If the HTTP client used has a version of `0.27.2`, but sends HTTP version `1.1`, this attribute should be set to `1.1`
-define(NET_PROTOCOL_VERSION, 'net.protocol.version').

%% Host identifier of the ["URI origin"](https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin) HTTP request is sent to
%% Determined by using the first of the following that applies
%% 
%% - Host identifier of the [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource)
%%   if it's sent in absolute-form
%% - Host identifier of the `Host` header
%% 
%% SHOULD NOT be set if capturing it would require an extra DNS lookup
-define(NET_PEER_NAME, 'net.peer.name').

%% Port identifier of the ["URI origin"](https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin) HTTP request is sent to
%% When [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource) is absolute URI, `net.peer.name` MUST match URI port identifier, otherwise it MUST match `Host` header port identifier
-define(NET_PEER_PORT, 'net.peer.port').

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

%% The name identifies the event
-define(EVENT_NAME, 'event.name').

%% The domain identifies the business context for the events
%% Events across different domains may have same `event.name`, yet be
%% unrelated events
-define(EVENT_DOMAIN, 'event.domain').

%% The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it
-define(EXCEPTION_TYPE, 'exception.type').

%% The exception message
-define(EXCEPTION_MESSAGE, 'exception.message').

%% A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG
-define(EXCEPTION_STACKTRACE, 'exception.stacktrace').

%% Transport protocol used. See note below
-define(NET_TRANSPORT, 'net.transport').

%% Remote socket peer name
-define(NET_SOCK_PEER_NAME, 'net.sock.peer.name').

%% Remote socket peer address: IPv4 or IPv6 for internet protocols, path for local communication, [etc](https://man7.org/linux/man-pages/man7/address_families.7.html)
-define(NET_SOCK_PEER_ADDR, 'net.sock.peer.addr').

%% Remote socket peer port
-define(NET_SOCK_PEER_PORT, 'net.sock.peer.port').

%% Protocol [address family](https://man7.org/linux/man-pages/man7/address_families.7.html) which is used for communication
-define(NET_SOCK_FAMILY, 'net.sock.family').

%% Local socket address. Useful in case of a multi-IP host
-define(NET_SOCK_HOST_ADDR, 'net.sock.host.addr').

%% Local socket port number
-define(NET_SOCK_HOST_PORT, 'net.sock.host.port').

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

%% The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size
-define(HTTP_REQUEST_CONTENT_LENGTH, 'http.request_content_length').

%% The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size
-define(HTTP_RESPONSE_CONTENT_LENGTH, 'http.response_content_length').

%% Value of the [HTTP User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#field.user-agent) header sent by the client
-define(USER_AGENT_ORIGINAL, 'user_agent.original').

%% The message destination name
%% Destination name SHOULD uniquely identify a specific queue, topic or other entity within the broker. If
%% the broker does not have such notion, the destination name SHOULD uniquely identify the broker
-define(MESSAGING_DESTINATION_NAME, 'messaging.destination.name').

%% The message source name
%% Source name SHOULD uniquely identify a specific queue, topic, or other entity within the broker. If
%% the broker does not have such notion, the source name SHOULD uniquely identify the broker
-define(MESSAGING_SOURCE_NAME, 'messaging.source.name').

%% A value used by the messaging system as an identifier for the message, represented as a string
-define(MESSAGING_MESSAGE_ID, 'messaging.message.id').

%% The [conversation ID](#conversations) identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID"
-define(MESSAGING_MESSAGE_CONVERSATION_ID, 'messaging.message.conversation_id').

%% The (uncompressed) size of the message payload in bytes. Also use this attribute if it is unknown whether the compressed or uncompressed payload size is reported
-define(MESSAGING_MESSAGE_PAYLOAD_SIZE_BYTES, 'messaging.message.payload_size_bytes').

%% The compressed size of the message payload in bytes
-define(MESSAGING_MESSAGE_PAYLOAD_COMPRESSED_SIZE_BYTES, 'messaging.message.payload_compressed_size_bytes').

%% Low cardinality representation of the messaging destination name
%% Destination names could be constructed from templates. An example would be a destination name involving a user name or product id. Although the destination name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation
-define(MESSAGING_DESTINATION_TEMPLATE, 'messaging.destination.template').

%% A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed
-define(MESSAGING_DESTINATION_TEMPORARY, 'messaging.destination.temporary').

%% A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name)
-define(MESSAGING_DESTINATION_ANONYMOUS, 'messaging.destination.anonymous').

%% Low cardinality representation of the messaging source name
%% Source names could be constructed from templates. An example would be a source name involving a user name or product id. Although the source name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation
-define(MESSAGING_SOURCE_TEMPLATE, 'messaging.source.template').

%% A boolean that is true if the message source is temporary and might not exist anymore after messages are processed
-define(MESSAGING_SOURCE_TEMPORARY, 'messaging.source.temporary').

%% A boolean that is true if the message source is anonymous (could be unnamed or have auto-generated name)
-define(MESSAGING_SOURCE_ANONYMOUS, 'messaging.source.anonymous').

%% A string identifying the messaging system
-define(MESSAGING_SYSTEM, 'messaging.system').

%% A string identifying the kind of messaging operation as defined in the [Operation names](#operation-names) section above
%% If a custom value is used, it MUST be of low cardinality
-define(MESSAGING_OPERATION, 'messaging.operation').

%% The number of messages sent, received, or processed in the scope of the batching operation
%% Instrumentations SHOULD NOT set `messaging.batch.message_count` on spans that operate with a single message. When a messaging client library supports both batch and single-message API for the same operation, instrumentations SHOULD use `messaging.batch.message_count` for batching APIs and SHOULD NOT use it for single-message APIs
-define(MESSAGING_BATCH_MESSAGE_COUNT, 'messaging.batch.message_count').

%% RabbitMQ message routing key
-define(MESSAGING_RABBITMQ_DESTINATION_ROUTING_KEY, 'messaging.rabbitmq.destination.routing_key').

%% Message keys in Kafka are used for grouping alike messages to ensure they're processed on the same partition. They differ from `messaging.message.id` in that they're not unique. If the key is `null`, the attribute MUST NOT be set
%% If the key type is not string, it's string representation has to be supplied for the attribute. If the key has no unambiguous, canonical string form, don't include its value
-define(MESSAGING_KAFKA_MESSAGE_KEY, 'messaging.kafka.message.key').

%% Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers
-define(MESSAGING_KAFKA_CONSUMER_GROUP, 'messaging.kafka.consumer.group').

%% Client Id for the Consumer or Producer that is handling the message
-define(MESSAGING_KAFKA_CLIENT_ID, 'messaging.kafka.client_id').

%% Partition the message is sent to
-define(MESSAGING_KAFKA_DESTINATION_PARTITION, 'messaging.kafka.destination.partition').

%% Partition the message is received from
-define(MESSAGING_KAFKA_SOURCE_PARTITION, 'messaging.kafka.source.partition').

%% The offset of a record in the corresponding Kafka partition
-define(MESSAGING_KAFKA_MESSAGE_OFFSET, 'messaging.kafka.message.offset').

%% A boolean that is true if the message is a tombstone
-define(MESSAGING_KAFKA_MESSAGE_TOMBSTONE, 'messaging.kafka.message.tombstone').

%% Namespace of RocketMQ resources, resources in different namespaces are individual
-define(MESSAGING_ROCKETMQ_NAMESPACE, 'messaging.rocketmq.namespace').

%% Name of the RocketMQ producer/consumer group that is handling the message. The client type is identified by the SpanKind
-define(MESSAGING_ROCKETMQ_CLIENT_GROUP, 'messaging.rocketmq.client_group').

%% The unique identifier for each client
-define(MESSAGING_ROCKETMQ_CLIENT_ID, 'messaging.rocketmq.client_id').

%% The timestamp in milliseconds that the delay message is expected to be delivered to consumer
-define(MESSAGING_ROCKETMQ_MESSAGE_DELIVERY_TIMESTAMP, 'messaging.rocketmq.message.delivery_timestamp').

%% The delay time level for delay message, which determines the message delay time
-define(MESSAGING_ROCKETMQ_MESSAGE_DELAY_TIME_LEVEL, 'messaging.rocketmq.message.delay_time_level').

%% It is essential for FIFO message. Messages that belong to the same message group are always processed one by one within the same consumer group
-define(MESSAGING_ROCKETMQ_MESSAGE_GROUP, 'messaging.rocketmq.message.group').

%% Type of message
-define(MESSAGING_ROCKETMQ_MESSAGE_TYPE, 'messaging.rocketmq.message.type').

%% The secondary classifier of message besides topic
-define(MESSAGING_ROCKETMQ_MESSAGE_TAG, 'messaging.rocketmq.message.tag').

%% Key(s) of message, another way to mark message besides message id
-define(MESSAGING_ROCKETMQ_MESSAGE_KEYS, 'messaging.rocketmq.message.keys').

%% Model of message consumption. This only applies to consumer spans
-define(MESSAGING_ROCKETMQ_CONSUMPTION_MODEL, 'messaging.rocketmq.consumption_model').
