defmodule OpenTelemetry.SemanticConventions.Attribute_group do
  @doc """
  The schema url for telemetry resources.

      iex> OpenTelemetry.SemanticConventions.Attribute_group.attribute_group_schema_url()
      "https://opentelemetry.io/schemas/1.20.0"
  """
  @spec attribute_group_schema_url :: String.t()
  def attribute_group_schema_url do
    "https://opentelemetry.io/schemas/1.20.0"
  end
  @doc """
  HTTP request method

      iex> OpenTelemetry.SemanticConventions.Attribute_group.http_method()
      :"http.method"
  """
  @spec http_method :: :"http.method"
  def http_method do
    :"http.method"
  end
  @doc """
  [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6)

      iex> OpenTelemetry.SemanticConventions.Attribute_group.http_status_code()
      :"http.status_code"
  """
  @spec http_status_code :: :"http.status_code"
  def http_status_code do
    :"http.status_code"
  end
  @doc """
  Application layer protocol used. The value SHOULD be normalized to lowercase

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_protocol_name()
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

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_protocol_version()
      :"net.protocol.version"
  """
  @spec net_protocol_version :: :"net.protocol.version"
  def net_protocol_version do
    :"net.protocol.version"
  end
  @doc """
  Host identifier of the ["URI origin"](https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin) HTTP request is sent to

  ### Notes

  Determined by using the first of the following that applies
  
  - Host identifier of the [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource)
    if it's sent in absolute-form
  - Host identifier of the `Host` header
  
  SHOULD NOT be set if capturing it would require an extra DNS lookup

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_peer_name()
      :"net.peer.name"
  """
  @spec net_peer_name :: :"net.peer.name"
  def net_peer_name do
    :"net.peer.name"
  end
  @doc """
  Port identifier of the ["URI origin"](https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin) HTTP request is sent to

  ### Notes

  When [request target](https://www.rfc-editor.org/rfc/rfc9110.html#target.resource) is absolute URI, `net.peer.name` MUST match URI port identifier, otherwise it MUST match `Host` header port identifier

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_peer_port()
      :"net.peer.port"
  """
  @spec net_peer_port :: :"net.peer.port"
  def net_peer_port do
    :"net.peer.port"
  end
  @doc """
  The URI scheme identifying the used protocol

      iex> OpenTelemetry.SemanticConventions.Attribute_group.http_scheme()
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

      iex> OpenTelemetry.SemanticConventions.Attribute_group.http_route()
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

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_host_name()
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

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_host_port()
      :"net.host.port"
  """
  @spec net_host_port :: :"net.host.port"
  def net_host_port do
    :"net.host.port"
  end
  @doc """
  The name identifies the event

      iex> OpenTelemetry.SemanticConventions.Attribute_group.event_name()
      :"event.name"
  """
  @spec event_name :: :"event.name"
  def event_name do
    :"event.name"
  end
  @doc """
  The domain identifies the business context for the events

  ### Notes

  Events across different domains may have same `event.name`, yet be
  unrelated events

      iex> OpenTelemetry.SemanticConventions.Attribute_group.event_domain()
      :"event.domain"
  """
  @spec event_domain :: :"event.domain"
  def event_domain do
    :"event.domain"
  end
  @doc """
  The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it

      iex> OpenTelemetry.SemanticConventions.Attribute_group.exception_type()
      :"exception.type"
  """
  @spec exception_type :: :"exception.type"
  def exception_type do
    :"exception.type"
  end
  @doc """
  The exception message

      iex> OpenTelemetry.SemanticConventions.Attribute_group.exception_message()
      :"exception.message"
  """
  @spec exception_message :: :"exception.message"
  def exception_message do
    :"exception.message"
  end
  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG

      iex> OpenTelemetry.SemanticConventions.Attribute_group.exception_stacktrace()
      :"exception.stacktrace"
  """
  @spec exception_stacktrace :: :"exception.stacktrace"
  def exception_stacktrace do
    :"exception.stacktrace"
  end
  @doc """
  Transport protocol used. See note below

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_transport()
      :"net.transport"
  """
  @spec net_transport :: :"net.transport"
  def net_transport do
    :"net.transport"
  end
  @doc """
  Remote socket peer name

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_sock_peer_name()
      :"net.sock.peer.name"
  """
  @spec net_sock_peer_name :: :"net.sock.peer.name"
  def net_sock_peer_name do
    :"net.sock.peer.name"
  end
  @doc """
  Remote socket peer address: IPv4 or IPv6 for internet protocols, path for local communication, [etc](https://man7.org/linux/man-pages/man7/address_families.7.html)

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_sock_peer_addr()
      :"net.sock.peer.addr"
  """
  @spec net_sock_peer_addr :: :"net.sock.peer.addr"
  def net_sock_peer_addr do
    :"net.sock.peer.addr"
  end
  @doc """
  Remote socket peer port

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_sock_peer_port()
      :"net.sock.peer.port"
  """
  @spec net_sock_peer_port :: :"net.sock.peer.port"
  def net_sock_peer_port do
    :"net.sock.peer.port"
  end
  @doc """
  Protocol [address family](https://man7.org/linux/man-pages/man7/address_families.7.html) which is used for communication

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_sock_family()
      :"net.sock.family"
  """
  @spec net_sock_family :: :"net.sock.family"
  def net_sock_family do
    :"net.sock.family"
  end
  @doc """
  Local socket address. Useful in case of a multi-IP host

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_sock_host_addr()
      :"net.sock.host.addr"
  """
  @spec net_sock_host_addr :: :"net.sock.host.addr"
  def net_sock_host_addr do
    :"net.sock.host.addr"
  end
  @doc """
  Local socket port number

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_sock_host_port()
      :"net.sock.host.port"
  """
  @spec net_sock_host_port :: :"net.sock.host.port"
  def net_sock_host_port do
    :"net.sock.host.port"
  end
  @doc """
  The internet connection type currently being used by the host

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_host_connection_type()
      :"net.host.connection.type"
  """
  @spec net_host_connection_type :: :"net.host.connection.type"
  def net_host_connection_type do
    :"net.host.connection.type"
  end
  @doc """
  This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_host_connection_subtype()
      :"net.host.connection.subtype"
  """
  @spec net_host_connection_subtype :: :"net.host.connection.subtype"
  def net_host_connection_subtype do
    :"net.host.connection.subtype"
  end
  @doc """
  The name of the mobile carrier

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_host_carrier_name()
      :"net.host.carrier.name"
  """
  @spec net_host_carrier_name :: :"net.host.carrier.name"
  def net_host_carrier_name do
    :"net.host.carrier.name"
  end
  @doc """
  The mobile carrier country code

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_host_carrier_mcc()
      :"net.host.carrier.mcc"
  """
  @spec net_host_carrier_mcc :: :"net.host.carrier.mcc"
  def net_host_carrier_mcc do
    :"net.host.carrier.mcc"
  end
  @doc """
  The mobile carrier network code

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_host_carrier_mnc()
      :"net.host.carrier.mnc"
  """
  @spec net_host_carrier_mnc :: :"net.host.carrier.mnc"
  def net_host_carrier_mnc do
    :"net.host.carrier.mnc"
  end
  @doc """
  The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network

      iex> OpenTelemetry.SemanticConventions.Attribute_group.net_host_carrier_icc()
      :"net.host.carrier.icc"
  """
  @spec net_host_carrier_icc :: :"net.host.carrier.icc"
  def net_host_carrier_icc do
    :"net.host.carrier.icc"
  end
  @doc """
  The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size

      iex> OpenTelemetry.SemanticConventions.Attribute_group.http_request_content_length()
      :"http.request_content_length"
  """
  @spec http_request_content_length :: :"http.request_content_length"
  def http_request_content_length do
    :"http.request_content_length"
  end
  @doc """
  The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size

      iex> OpenTelemetry.SemanticConventions.Attribute_group.http_response_content_length()
      :"http.response_content_length"
  """
  @spec http_response_content_length :: :"http.response_content_length"
  def http_response_content_length do
    :"http.response_content_length"
  end
  @doc """
  Value of the [HTTP User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#field.user-agent) header sent by the client

      iex> OpenTelemetry.SemanticConventions.Attribute_group.user_agent_original()
      :"user_agent.original"
  """
  @spec user_agent_original :: :"user_agent.original"
  def user_agent_original do
    :"user_agent.original"
  end
  @doc """
  The message destination name

  ### Notes

  Destination name SHOULD uniquely identify a specific queue, topic or other entity within the broker. If
  the broker does not have such notion, the destination name SHOULD uniquely identify the broker

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_destination_name()
      :"messaging.destination.name"
  """
  @spec messaging_destination_name :: :"messaging.destination.name"
  def messaging_destination_name do
    :"messaging.destination.name"
  end
  @doc """
  The message source name

  ### Notes

  Source name SHOULD uniquely identify a specific queue, topic, or other entity within the broker. If
  the broker does not have such notion, the source name SHOULD uniquely identify the broker

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_source_name()
      :"messaging.source.name"
  """
  @spec messaging_source_name :: :"messaging.source.name"
  def messaging_source_name do
    :"messaging.source.name"
  end
  @doc """
  A value used by the messaging system as an identifier for the message, represented as a string

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_message_id()
      :"messaging.message.id"
  """
  @spec messaging_message_id :: :"messaging.message.id"
  def messaging_message_id do
    :"messaging.message.id"
  end
  @doc """
  The [conversation ID](#conversations) identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID"

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_message_conversation_id()
      :"messaging.message.conversation_id"
  """
  @spec messaging_message_conversation_id :: :"messaging.message.conversation_id"
  def messaging_message_conversation_id do
    :"messaging.message.conversation_id"
  end
  @doc """
  The (uncompressed) size of the message payload in bytes. Also use this attribute if it is unknown whether the compressed or uncompressed payload size is reported

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_message_payload_size_bytes()
      :"messaging.message.payload_size_bytes"
  """
  @spec messaging_message_payload_size_bytes :: :"messaging.message.payload_size_bytes"
  def messaging_message_payload_size_bytes do
    :"messaging.message.payload_size_bytes"
  end
  @doc """
  The compressed size of the message payload in bytes

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_message_payload_compressed_size_bytes()
      :"messaging.message.payload_compressed_size_bytes"
  """
  @spec messaging_message_payload_compressed_size_bytes :: :"messaging.message.payload_compressed_size_bytes"
  def messaging_message_payload_compressed_size_bytes do
    :"messaging.message.payload_compressed_size_bytes"
  end
  @doc """
  Low cardinality representation of the messaging destination name

  ### Notes

  Destination names could be constructed from templates. An example would be a destination name involving a user name or product id. Although the destination name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_destination_template()
      :"messaging.destination.template"
  """
  @spec messaging_destination_template :: :"messaging.destination.template"
  def messaging_destination_template do
    :"messaging.destination.template"
  end
  @doc """
  A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_destination_temporary()
      :"messaging.destination.temporary"
  """
  @spec messaging_destination_temporary :: :"messaging.destination.temporary"
  def messaging_destination_temporary do
    :"messaging.destination.temporary"
  end
  @doc """
  A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name)

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_destination_anonymous()
      :"messaging.destination.anonymous"
  """
  @spec messaging_destination_anonymous :: :"messaging.destination.anonymous"
  def messaging_destination_anonymous do
    :"messaging.destination.anonymous"
  end
  @doc """
  Low cardinality representation of the messaging source name

  ### Notes

  Source names could be constructed from templates. An example would be a source name involving a user name or product id. Although the source name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_source_template()
      :"messaging.source.template"
  """
  @spec messaging_source_template :: :"messaging.source.template"
  def messaging_source_template do
    :"messaging.source.template"
  end
  @doc """
  A boolean that is true if the message source is temporary and might not exist anymore after messages are processed

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_source_temporary()
      :"messaging.source.temporary"
  """
  @spec messaging_source_temporary :: :"messaging.source.temporary"
  def messaging_source_temporary do
    :"messaging.source.temporary"
  end
  @doc """
  A boolean that is true if the message source is anonymous (could be unnamed or have auto-generated name)

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_source_anonymous()
      :"messaging.source.anonymous"
  """
  @spec messaging_source_anonymous :: :"messaging.source.anonymous"
  def messaging_source_anonymous do
    :"messaging.source.anonymous"
  end
  @doc """
  A string identifying the messaging system

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_system()
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

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_operation()
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

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_batch_message_count()
      :"messaging.batch.message_count"
  """
  @spec messaging_batch_message_count :: :"messaging.batch.message_count"
  def messaging_batch_message_count do
    :"messaging.batch.message_count"
  end
  @doc """
  RabbitMQ message routing key

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rabbitmq_destination_routing_key()
      :"messaging.rabbitmq.destination.routing_key"
  """
  @spec messaging_rabbitmq_destination_routing_key :: :"messaging.rabbitmq.destination.routing_key"
  def messaging_rabbitmq_destination_routing_key do
    :"messaging.rabbitmq.destination.routing_key"
  end
  @doc """
  Message keys in Kafka are used for grouping alike messages to ensure they're processed on the same partition. They differ from `messaging.message.id` in that they're not unique. If the key is `null`, the attribute MUST NOT be set

  ### Notes

  If the key type is not string, it's string representation has to be supplied for the attribute. If the key has no unambiguous, canonical string form, don't include its value

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_kafka_message_key()
      :"messaging.kafka.message.key"
  """
  @spec messaging_kafka_message_key :: :"messaging.kafka.message.key"
  def messaging_kafka_message_key do
    :"messaging.kafka.message.key"
  end
  @doc """
  Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_kafka_consumer_group()
      :"messaging.kafka.consumer.group"
  """
  @spec messaging_kafka_consumer_group :: :"messaging.kafka.consumer.group"
  def messaging_kafka_consumer_group do
    :"messaging.kafka.consumer.group"
  end
  @doc """
  Client Id for the Consumer or Producer that is handling the message

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_kafka_client_id()
      :"messaging.kafka.client_id"
  """
  @spec messaging_kafka_client_id :: :"messaging.kafka.client_id"
  def messaging_kafka_client_id do
    :"messaging.kafka.client_id"
  end
  @doc """
  Partition the message is sent to

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_kafka_destination_partition()
      :"messaging.kafka.destination.partition"
  """
  @spec messaging_kafka_destination_partition :: :"messaging.kafka.destination.partition"
  def messaging_kafka_destination_partition do
    :"messaging.kafka.destination.partition"
  end
  @doc """
  Partition the message is received from

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_kafka_source_partition()
      :"messaging.kafka.source.partition"
  """
  @spec messaging_kafka_source_partition :: :"messaging.kafka.source.partition"
  def messaging_kafka_source_partition do
    :"messaging.kafka.source.partition"
  end
  @doc """
  The offset of a record in the corresponding Kafka partition

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_kafka_message_offset()
      :"messaging.kafka.message.offset"
  """
  @spec messaging_kafka_message_offset :: :"messaging.kafka.message.offset"
  def messaging_kafka_message_offset do
    :"messaging.kafka.message.offset"
  end
  @doc """
  A boolean that is true if the message is a tombstone

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_kafka_message_tombstone()
      :"messaging.kafka.message.tombstone"
  """
  @spec messaging_kafka_message_tombstone :: :"messaging.kafka.message.tombstone"
  def messaging_kafka_message_tombstone do
    :"messaging.kafka.message.tombstone"
  end
  @doc """
  Namespace of RocketMQ resources, resources in different namespaces are individual

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rocketmq_namespace()
      :"messaging.rocketmq.namespace"
  """
  @spec messaging_rocketmq_namespace :: :"messaging.rocketmq.namespace"
  def messaging_rocketmq_namespace do
    :"messaging.rocketmq.namespace"
  end
  @doc """
  Name of the RocketMQ producer/consumer group that is handling the message. The client type is identified by the SpanKind

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rocketmq_client_group()
      :"messaging.rocketmq.client_group"
  """
  @spec messaging_rocketmq_client_group :: :"messaging.rocketmq.client_group"
  def messaging_rocketmq_client_group do
    :"messaging.rocketmq.client_group"
  end
  @doc """
  The unique identifier for each client

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rocketmq_client_id()
      :"messaging.rocketmq.client_id"
  """
  @spec messaging_rocketmq_client_id :: :"messaging.rocketmq.client_id"
  def messaging_rocketmq_client_id do
    :"messaging.rocketmq.client_id"
  end
  @doc """
  The timestamp in milliseconds that the delay message is expected to be delivered to consumer

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rocketmq_message_delivery_timestamp()
      :"messaging.rocketmq.message.delivery_timestamp"
  """
  @spec messaging_rocketmq_message_delivery_timestamp :: :"messaging.rocketmq.message.delivery_timestamp"
  def messaging_rocketmq_message_delivery_timestamp do
    :"messaging.rocketmq.message.delivery_timestamp"
  end
  @doc """
  The delay time level for delay message, which determines the message delay time

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rocketmq_message_delay_time_level()
      :"messaging.rocketmq.message.delay_time_level"
  """
  @spec messaging_rocketmq_message_delay_time_level :: :"messaging.rocketmq.message.delay_time_level"
  def messaging_rocketmq_message_delay_time_level do
    :"messaging.rocketmq.message.delay_time_level"
  end
  @doc """
  It is essential for FIFO message. Messages that belong to the same message group are always processed one by one within the same consumer group

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rocketmq_message_group()
      :"messaging.rocketmq.message.group"
  """
  @spec messaging_rocketmq_message_group :: :"messaging.rocketmq.message.group"
  def messaging_rocketmq_message_group do
    :"messaging.rocketmq.message.group"
  end
  @doc """
  Type of message

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rocketmq_message_type()
      :"messaging.rocketmq.message.type"
  """
  @spec messaging_rocketmq_message_type :: :"messaging.rocketmq.message.type"
  def messaging_rocketmq_message_type do
    :"messaging.rocketmq.message.type"
  end
  @doc """
  The secondary classifier of message besides topic

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rocketmq_message_tag()
      :"messaging.rocketmq.message.tag"
  """
  @spec messaging_rocketmq_message_tag :: :"messaging.rocketmq.message.tag"
  def messaging_rocketmq_message_tag do
    :"messaging.rocketmq.message.tag"
  end
  @doc """
  Key(s) of message, another way to mark message besides message id

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rocketmq_message_keys()
      :"messaging.rocketmq.message.keys"
  """
  @spec messaging_rocketmq_message_keys :: :"messaging.rocketmq.message.keys"
  def messaging_rocketmq_message_keys do
    :"messaging.rocketmq.message.keys"
  end
  @doc """
  Model of message consumption. This only applies to consumer spans

      iex> OpenTelemetry.SemanticConventions.Attribute_group.messaging_rocketmq_consumption_model()
      :"messaging.rocketmq.consumption_model"
  """
  @spec messaging_rocketmq_consumption_model :: :"messaging.rocketmq.consumption_model"
  def messaging_rocketmq_consumption_model do
    :"messaging.rocketmq.consumption_model"
  end
end