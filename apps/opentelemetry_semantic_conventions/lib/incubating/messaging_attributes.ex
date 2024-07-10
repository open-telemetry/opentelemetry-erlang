defmodule OpenTelemetry.SemConv.Incubating.MessagingAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Messaging attributes.
  """

  @doc """
  The number of messages sent, received, or processed in the scope of the batching operation.
  ### Value type

  Value must be of type `integer()`.
  ### Notes

  Instrumentations **SHOULD** **NOT** set `messaging.batch.message_count` on spans that operate with a single message. When a messaging client library supports both batch and single-message API for the same operation, instrumentations **SHOULD** use `messaging.batch.message_count` for batching APIs and **SHOULD** **NOT** use it for single-message APIs.

  ### Examples

  ```
  [0, 1, 2]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_batch_messagecount()
      :"messaging.batch.message_count"

  ### Erlang

  ```erlang
  ?MESSAGING_BATCH_MESSAGECOUNT.
  'messaging.batch.message_count'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_batch_messagecount :: :"messaging.batch.message_count"
  def messaging_batch_messagecount do
    :"messaging.batch.message_count"
  end

  @doc """
  A unique identifier for the client that consumes or produces a message.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["client-5", "myhost@8742@s8083jm"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_client_id()
      :"messaging.client.id"

  ### Erlang

  ```erlang
  ?MESSAGING_CLIENT_ID.
  'messaging.client.id'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_client_id :: :"messaging.client.id"
  def messaging_client_id do
    :"messaging.client.id"
  end

  @deprecated """
  Replaced by `messaging.client.id`.
  """
  @spec messaging_clientid :: :"messaging.client_id"
  def messaging_clientid do
    :"messaging.client_id"
  end

  @doc """
  A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name).
  ### Value type

  Value must be of type `boolean()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_destination_anonymous()
      :"messaging.destination.anonymous"

  ### Erlang

  ```erlang
  ?MESSAGING_DESTINATION_ANONYMOUS.
  'messaging.destination.anonymous'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_destination_anonymous :: :"messaging.destination.anonymous"
  def messaging_destination_anonymous do
    :"messaging.destination.anonymous"
  end

  @doc """
  The message destination name
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Destination name **SHOULD** uniquely identify a specific queue, topic or other entity within the broker. If
  the broker doesn't have such notion, the destination name **SHOULD** uniquely identify the broker.

  ### Examples

  ```
  ["MyQueue", "MyTopic"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_destination_name()
      :"messaging.destination.name"

  ### Erlang

  ```erlang
  ?MESSAGING_DESTINATION_NAME.
  'messaging.destination.name'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_destination_name :: :"messaging.destination.name"
  def messaging_destination_name do
    :"messaging.destination.name"
  end

  @doc """
  The identifier of the partition messages are sent to or received from, unique within the `messaging.destination.name`.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  1
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_destination_partition_id()
      :"messaging.destination.partition.id"

  ### Erlang

  ```erlang
  ?MESSAGING_DESTINATION_PARTITION_ID.
  'messaging.destination.partition.id'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_destination_partition_id :: :"messaging.destination.partition.id"
  def messaging_destination_partition_id do
    :"messaging.destination.partition.id"
  end

  @doc """
  Low cardinality representation of the messaging destination name
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Destination names could be constructed from templates. An example would be a destination name involving a user name or product id. Although the destination name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation.

  ### Examples

  ```
  ["/customers/{customerId}"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_destination_template()
      :"messaging.destination.template"

  ### Erlang

  ```erlang
  ?MESSAGING_DESTINATION_TEMPLATE.
  'messaging.destination.template'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_destination_template :: :"messaging.destination.template"
  def messaging_destination_template do
    :"messaging.destination.template"
  end

  @doc """
  A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed.
  ### Value type

  Value must be of type `boolean()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_destination_temporary()
      :"messaging.destination.temporary"

  ### Erlang

  ```erlang
  ?MESSAGING_DESTINATION_TEMPORARY.
  'messaging.destination.temporary'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_destination_temporary :: :"messaging.destination.temporary"
  def messaging_destination_temporary do
    :"messaging.destination.temporary"
  end

  @doc """
  A boolean that is true if the publish message destination is anonymous (could be unnamed or have auto-generated name).
  ### Value type

  Value must be of type `boolean()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_destinationpublish_anonymous()
      :"messaging.destination_publish.anonymous"

  ### Erlang

  ```erlang
  ?MESSAGING_DESTINATIONPUBLISH_ANONYMOUS.
  'messaging.destination_publish.anonymous'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_destinationpublish_anonymous :: :"messaging.destination_publish.anonymous"
  def messaging_destinationpublish_anonymous do
    :"messaging.destination_publish.anonymous"
  end

  @doc """
  The name of the original destination the message was published to
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The name **SHOULD** uniquely identify a specific queue, topic, or other entity within the broker. If
  the broker doesn't have such notion, the original destination name **SHOULD** uniquely identify the broker.

  ### Examples

  ```
  ["MyQueue", "MyTopic"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_destinationpublish_name()
      :"messaging.destination_publish.name"

  ### Erlang

  ```erlang
  ?MESSAGING_DESTINATIONPUBLISH_NAME.
  'messaging.destination_publish.name'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_destinationpublish_name :: :"messaging.destination_publish.name"
  def messaging_destinationpublish_name do
    :"messaging.destination_publish.name"
  end

  @doc """
  The name of the consumer group the event consumer is associated with.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  indexer
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_eventhubs_consumer_group()
      :"messaging.eventhubs.consumer.group"

  ### Erlang

  ```erlang
  ?MESSAGING_EVENTHUBS_CONSUMER_GROUP.
  'messaging.eventhubs.consumer.group'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_eventhubs_consumer_group :: :"messaging.eventhubs.consumer.group"
  def messaging_eventhubs_consumer_group do
    :"messaging.eventhubs.consumer.group"
  end

  @doc """
  The UTC epoch seconds at which the message has been accepted and stored in the entity.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  1701393730
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_eventhubs_message_enqueuedtime()
      :"messaging.eventhubs.message.enqueued_time"

  ### Erlang

  ```erlang
  ?MESSAGING_EVENTHUBS_MESSAGE_ENQUEUEDTIME.
  'messaging.eventhubs.message.enqueued_time'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_eventhubs_message_enqueuedtime :: :"messaging.eventhubs.message.enqueued_time"
  def messaging_eventhubs_message_enqueuedtime do
    :"messaging.eventhubs.message.enqueued_time"
  end

  @doc """
  The ack deadline in seconds set for the modify ack deadline request.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  10
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_gcppubsub_message_ackdeadline()
      :"messaging.gcp_pubsub.message.ack_deadline"

  ### Erlang

  ```erlang
  ?MESSAGING_GCPPUBSUB_MESSAGE_ACKDEADLINE.
  'messaging.gcp_pubsub.message.ack_deadline'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_gcppubsub_message_ackdeadline :: :"messaging.gcp_pubsub.message.ack_deadline"
  def messaging_gcppubsub_message_ackdeadline do
    :"messaging.gcp_pubsub.message.ack_deadline"
  end

  @doc """
  The ack id for a given message.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ack_id
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_gcppubsub_message_ackid()
      :"messaging.gcp_pubsub.message.ack_id"

  ### Erlang

  ```erlang
  ?MESSAGING_GCPPUBSUB_MESSAGE_ACKID.
  'messaging.gcp_pubsub.message.ack_id'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_gcppubsub_message_ackid :: :"messaging.gcp_pubsub.message.ack_id"
  def messaging_gcppubsub_message_ackid do
    :"messaging.gcp_pubsub.message.ack_id"
  end

  @doc """
  The delivery attempt for a given message.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  2
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_gcppubsub_message_deliveryattempt()
      :"messaging.gcp_pubsub.message.delivery_attempt"

  ### Erlang

  ```erlang
  ?MESSAGING_GCPPUBSUB_MESSAGE_DELIVERYATTEMPT.
  'messaging.gcp_pubsub.message.delivery_attempt'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_gcppubsub_message_deliveryattempt ::
          :"messaging.gcp_pubsub.message.delivery_attempt"
  def messaging_gcppubsub_message_deliveryattempt do
    :"messaging.gcp_pubsub.message.delivery_attempt"
  end

  @doc """
  The ordering key for a given message. If the attribute is not present, the message does not have an ordering key.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ordering_key
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_gcppubsub_message_orderingkey()
      :"messaging.gcp_pubsub.message.ordering_key"

  ### Erlang

  ```erlang
  ?MESSAGING_GCPPUBSUB_MESSAGE_ORDERINGKEY.
  'messaging.gcp_pubsub.message.ordering_key'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_gcppubsub_message_orderingkey :: :"messaging.gcp_pubsub.message.ordering_key"
  def messaging_gcppubsub_message_orderingkey do
    :"messaging.gcp_pubsub.message.ordering_key"
  end

  @doc """
  Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  my-group
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_kafka_consumer_group()
      :"messaging.kafka.consumer.group"

  ### Erlang

  ```erlang
  ?MESSAGING_KAFKA_CONSUMER_GROUP.
  'messaging.kafka.consumer.group'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_kafka_consumer_group :: :"messaging.kafka.consumer.group"
  def messaging_kafka_consumer_group do
    :"messaging.kafka.consumer.group"
  end

  @deprecated """
  Replaced by `messaging.destination.partition.id`.
  """
  @spec messaging_kafka_destination_partition :: :"messaging.kafka.destination.partition"
  def messaging_kafka_destination_partition do
    :"messaging.kafka.destination.partition"
  end

  @doc """
  Message keys in Kafka are used for grouping alike messages to ensure they're processed on the same partition. They differ from `messaging.message.id` in that they're not unique. If the key is `null`, the attribute **MUST** **NOT** be set.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  If the key type is not string, it's string representation has to be supplied for the attribute. If the key has no unambiguous, canonical string form, don't include its value.

  ### Examples

  ```
  myKey
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_kafka_message_key()
      :"messaging.kafka.message.key"

  ### Erlang

  ```erlang
  ?MESSAGING_KAFKA_MESSAGE_KEY.
  'messaging.kafka.message.key'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_kafka_message_key :: :"messaging.kafka.message.key"
  def messaging_kafka_message_key do
    :"messaging.kafka.message.key"
  end

  @doc """
  The offset of a record in the corresponding Kafka partition.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  42
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_kafka_message_offset()
      :"messaging.kafka.message.offset"

  ### Erlang

  ```erlang
  ?MESSAGING_KAFKA_MESSAGE_OFFSET.
  'messaging.kafka.message.offset'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_kafka_message_offset :: :"messaging.kafka.message.offset"
  def messaging_kafka_message_offset do
    :"messaging.kafka.message.offset"
  end

  @doc """
  A boolean that is true if the message is a tombstone.
  ### Value type

  Value must be of type `boolean()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_kafka_message_tombstone()
      :"messaging.kafka.message.tombstone"

  ### Erlang

  ```erlang
  ?MESSAGING_KAFKA_MESSAGE_TOMBSTONE.
  'messaging.kafka.message.tombstone'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_kafka_message_tombstone :: :"messaging.kafka.message.tombstone"
  def messaging_kafka_message_tombstone do
    :"messaging.kafka.message.tombstone"
  end

  @doc """
  The size of the message body in bytes.

  ### Value type

  Value must be of type `integer()`.
  ### Notes

  This can refer to both the compressed or uncompressed body size. If both sizes are known, the uncompressed
  body size should be used.

  ### Examples

  ```
  1439
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_message_body_size()
      :"messaging.message.body.size"

  ### Erlang

  ```erlang
  ?MESSAGING_MESSAGE_BODY_SIZE.
  'messaging.message.body.size'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_message_body_size :: :"messaging.message.body.size"
  def messaging_message_body_size do
    :"messaging.message.body.size"
  end

  @doc """
  The conversation ID identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID".

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  MyConversationId
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_message_conversationid()
      :"messaging.message.conversation_id"

  ### Erlang

  ```erlang
  ?MESSAGING_MESSAGE_CONVERSATIONID.
  'messaging.message.conversation_id'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_message_conversationid :: :"messaging.message.conversation_id"
  def messaging_message_conversationid do
    :"messaging.message.conversation_id"
  end

  @doc """
  The size of the message body and metadata in bytes.

  ### Value type

  Value must be of type `integer()`.
  ### Notes

  This can refer to both the compressed or uncompressed size. If both sizes are known, the uncompressed
  size should be used.

  ### Examples

  ```
  2738
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_message_envelope_size()
      :"messaging.message.envelope.size"

  ### Erlang

  ```erlang
  ?MESSAGING_MESSAGE_ENVELOPE_SIZE.
  'messaging.message.envelope.size'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_message_envelope_size :: :"messaging.message.envelope.size"
  def messaging_message_envelope_size do
    :"messaging.message.envelope.size"
  end

  @doc """
  A value used by the messaging system as an identifier for the message, represented as a string.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  452a7c7c7c7048c2f887f61572b18fc2
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_message_id()
      :"messaging.message.id"

  ### Erlang

  ```erlang
  ?MESSAGING_MESSAGE_ID.
  'messaging.message.id'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_message_id :: :"messaging.message.id"
  def messaging_message_id do
    :"messaging.message.id"
  end

  @deprecated """
  Replaced by `messaging.operation.type`.
  """
  @spec messaging_operation :: :"messaging.operation"
  def messaging_operation do
    :"messaging.operation"
  end

  @doc """
  The system-specific name of the messaging operation.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["ack", "nack", "send"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_operation_name()
      :"messaging.operation.name"

  ### Erlang

  ```erlang
  ?MESSAGING_OPERATION_NAME.
  'messaging.operation.name'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_operation_name :: :"messaging.operation.name"
  def messaging_operation_name do
    :"messaging.operation.name"
  end

  @typedoc """
  A string identifying the type of the messaging operation.


  ### Enum Values
  * `:publish` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - One or more messages are provided for publishing to an intermediary. If a single message is published, the context of the "Publish" span can be used as the creation context and no "Create" span needs to be created.

  * `:create` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A message is created. "Create" spans always refer to a single message and are used to provide a unique creation context for messages in batch publishing scenarios.

  * `:receive` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - One or more messages are requested by a consumer. This operation refers to pull-based scenarios, where consumers explicitly call methods of messaging SDKs to receive messages.

  * `:deliver` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - One or more messages are delivered to or processed by a consumer.

  * `:settle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - One or more messages are settled.

  """
  @type messaging_operation_type() :: %{
          :publish => :publish,
          :create => :create,
          :receive => :receive,
          :deliver => :process,
          :settle => :settle
        }
  @doc """
  A string identifying the type of the messaging operation.


  ### Notes

  If a custom value is used, it **MUST** be of low cardinality.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_operation_type().publish
      :publish
      
      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_operation_type(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'messaging_operation_type.publish'.
  publish

  ?messaging_operation_type.(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec messaging_operation_type() :: messaging_operation_type()
  def messaging_operation_type() do
    %{
      :publish => :publish,
      :create => :create,
      :receive => :receive,
      :deliver => :process,
      :settle => :settle
    }
  end

  @spec messaging_operation_type(atom() | String.t()) :: atom() | String.t()
  def messaging_operation_type(custom_value) do
    custom_value
  end

  @doc """
  RabbitMQ message routing key.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  myKey
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rabbitmq_destination_routingkey()
      :"messaging.rabbitmq.destination.routing_key"

  ### Erlang

  ```erlang
  ?MESSAGING_RABBITMQ_DESTINATION_ROUTINGKEY.
  'messaging.rabbitmq.destination.routing_key'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rabbitmq_destination_routingkey :: :"messaging.rabbitmq.destination.routing_key"
  def messaging_rabbitmq_destination_routingkey do
    :"messaging.rabbitmq.destination.routing_key"
  end

  @doc """
  RabbitMQ message delivery tag

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  123
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rabbitmq_message_deliverytag()
      :"messaging.rabbitmq.message.delivery_tag"

  ### Erlang

  ```erlang
  ?MESSAGING_RABBITMQ_MESSAGE_DELIVERYTAG.
  'messaging.rabbitmq.message.delivery_tag'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rabbitmq_message_deliverytag :: :"messaging.rabbitmq.message.delivery_tag"
  def messaging_rabbitmq_message_deliverytag do
    :"messaging.rabbitmq.message.delivery_tag"
  end

  @doc """
  Name of the RocketMQ producer/consumer group that is handling the message. The client type is identified by the SpanKind.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  myConsumerGroup
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_clientgroup()
      :"messaging.rocketmq.client_group"

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_CLIENTGROUP.
  'messaging.rocketmq.client_group'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_clientgroup :: :"messaging.rocketmq.client_group"
  def messaging_rocketmq_clientgroup do
    :"messaging.rocketmq.client_group"
  end

  @typedoc """
  Model of message consumption. This only applies to consumer spans.


  ### Enum Values
  * `:clustering` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Clustering consumption model
  * `:broadcasting` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Broadcasting consumption model
  """
  @type messaging_rocketmq_consumptionmodel() :: %{
          :clustering => :clustering,
          :broadcasting => :broadcasting
        }
  @doc """
  Model of message consumption. This only applies to consumer spans.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_consumptionmodel().clustering
      :clustering
      
      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_consumptionmodel(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'messaging_rocketmq_consumptionmodel.clustering'.
  clustering

  ?messaging_rocketmq_consumptionmodel.(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_consumptionmodel() :: messaging_rocketmq_consumptionmodel()
  def messaging_rocketmq_consumptionmodel() do
    %{
      :clustering => :clustering,
      :broadcasting => :broadcasting
    }
  end

  @spec messaging_rocketmq_consumptionmodel(atom() | String.t()) :: atom() | String.t()
  def messaging_rocketmq_consumptionmodel(custom_value) do
    custom_value
  end

  @doc """
  The delay time level for delay message, which determines the message delay time.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  3
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_delaytimelevel()
      :"messaging.rocketmq.message.delay_time_level"

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_MESSAGE_DELAYTIMELEVEL.
  'messaging.rocketmq.message.delay_time_level'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_message_delaytimelevel ::
          :"messaging.rocketmq.message.delay_time_level"
  def messaging_rocketmq_message_delaytimelevel do
    :"messaging.rocketmq.message.delay_time_level"
  end

  @doc """
  The timestamp in milliseconds that the delay message is expected to be delivered to consumer.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  1665987217045
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_deliverytimestamp()
      :"messaging.rocketmq.message.delivery_timestamp"

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_MESSAGE_DELIVERYTIMESTAMP.
  'messaging.rocketmq.message.delivery_timestamp'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_message_deliverytimestamp ::
          :"messaging.rocketmq.message.delivery_timestamp"
  def messaging_rocketmq_message_deliverytimestamp do
    :"messaging.rocketmq.message.delivery_timestamp"
  end

  @doc """
  It is essential for FIFO message. Messages that belong to the same message group are always processed one by one within the same consumer group.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  myMessageGroup
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_group()
      :"messaging.rocketmq.message.group"

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_MESSAGE_GROUP.
  'messaging.rocketmq.message.group'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_message_group :: :"messaging.rocketmq.message.group"
  def messaging_rocketmq_message_group do
    :"messaging.rocketmq.message.group"
  end

  @doc """
  Key(s) of message, another way to mark message besides message id.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["keyA", "keyB"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_keys()
      :"messaging.rocketmq.message.keys"

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_MESSAGE_KEYS.
  'messaging.rocketmq.message.keys'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_message_keys :: :"messaging.rocketmq.message.keys"
  def messaging_rocketmq_message_keys do
    :"messaging.rocketmq.message.keys"
  end

  @doc """
  The secondary classifier of message besides topic.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  tagA
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_tag()
      :"messaging.rocketmq.message.tag"

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_MESSAGE_TAG.
  'messaging.rocketmq.message.tag'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_message_tag :: :"messaging.rocketmq.message.tag"
  def messaging_rocketmq_message_tag do
    :"messaging.rocketmq.message.tag"
  end

  @typedoc """
  Type of message.


  ### Enum Values
  * `:normal` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Normal message
  * `:fifo` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - FIFO message
  * `:delay` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Delay message
  * `:transaction` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Transaction message
  """
  @type messaging_rocketmq_message_type() :: %{
          :normal => :normal,
          :fifo => :fifo,
          :delay => :delay,
          :transaction => :transaction
        }
  @doc """
  Type of message.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_type().normal
      :normal
      
      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_type(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'messaging_rocketmq_message_type.normal'.
  normal

  ?messaging_rocketmq_message_type.(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_message_type() :: messaging_rocketmq_message_type()
  def messaging_rocketmq_message_type() do
    %{
      :normal => :normal,
      :fifo => :fifo,
      :delay => :delay,
      :transaction => :transaction
    }
  end

  @spec messaging_rocketmq_message_type(atom() | String.t()) :: atom() | String.t()
  def messaging_rocketmq_message_type(custom_value) do
    custom_value
  end

  @doc """
  Namespace of RocketMQ resources, resources in different namespaces are individual.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  myNamespace
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_namespace()
      :"messaging.rocketmq.namespace"

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_NAMESPACE.
  'messaging.rocketmq.namespace'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_namespace :: :"messaging.rocketmq.namespace"
  def messaging_rocketmq_namespace do
    :"messaging.rocketmq.namespace"
  end

  @doc """
  The name of the subscription in the topic messages are received from.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  mySubscription
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_destination_subscriptionname()
      :"messaging.servicebus.destination.subscription_name"

  ### Erlang

  ```erlang
  ?MESSAGING_SERVICEBUS_DESTINATION_SUBSCRIPTIONNAME.
  'messaging.servicebus.destination.subscription_name'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_servicebus_destination_subscriptionname ::
          :"messaging.servicebus.destination.subscription_name"
  def messaging_servicebus_destination_subscriptionname do
    :"messaging.servicebus.destination.subscription_name"
  end

  @typedoc """
  Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock).


  ### Enum Values
  * `:complete` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Message is completed
  * `:abandon` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Message is abandoned
  * `:dead_letter` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Message is sent to dead letter queue
  * `:defer` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Message is deferred
  """
  @type messaging_servicebus_dispositionstatus() :: %{
          :complete => :complete,
          :abandon => :abandon,
          :dead_letter => :dead_letter,
          :defer => :defer
        }
  @doc """
  Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock).



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_dispositionstatus().complete
      :complete
      
      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_dispositionstatus(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'messaging_servicebus_dispositionstatus.complete'.
  complete

  ?messaging_servicebus_dispositionstatus.(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec messaging_servicebus_dispositionstatus() :: messaging_servicebus_dispositionstatus()
  def messaging_servicebus_dispositionstatus() do
    %{
      :complete => :complete,
      :abandon => :abandon,
      :dead_letter => :dead_letter,
      :defer => :defer
    }
  end

  @spec messaging_servicebus_dispositionstatus(atom() | String.t()) :: atom() | String.t()
  def messaging_servicebus_dispositionstatus(custom_value) do
    custom_value
  end

  @doc """
  Number of deliveries that have been attempted for this message.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  2
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_message_deliverycount()
      :"messaging.servicebus.message.delivery_count"

  ### Erlang

  ```erlang
  ?MESSAGING_SERVICEBUS_MESSAGE_DELIVERYCOUNT.
  'messaging.servicebus.message.delivery_count'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_servicebus_message_deliverycount ::
          :"messaging.servicebus.message.delivery_count"
  def messaging_servicebus_message_deliverycount do
    :"messaging.servicebus.message.delivery_count"
  end

  @doc """
  The UTC epoch seconds at which the message has been accepted and stored in the entity.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  1701393730
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_message_enqueuedtime()
      :"messaging.servicebus.message.enqueued_time"

  ### Erlang

  ```erlang
  ?MESSAGING_SERVICEBUS_MESSAGE_ENQUEUEDTIME.
  'messaging.servicebus.message.enqueued_time'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_servicebus_message_enqueuedtime :: :"messaging.servicebus.message.enqueued_time"
  def messaging_servicebus_message_enqueuedtime do
    :"messaging.servicebus.message.enqueued_time"
  end

  @typedoc """
  The messaging system as identified by the client instrumentation.

  ### Enum Values
  * `:activemq` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache ActiveMQ
  * `:aws_sqs` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Amazon Simple Queue Service (SQS)
  * `:eventgrid` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure Event Grid
  * `:eventhubs` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure Event Hubs
  * `:servicebus` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure Service Bus
  * `:gcp_pubsub` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Google Cloud Pub/Sub
  * `:jms` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Java Message Service
  * `:kafka` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Kafka
  * `:rabbitmq` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - RabbitMQ
  * `:rocketmq` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache RocketMQ
  """
  @type messaging_system() :: %{
          :activemq => :activemq,
          :aws_sqs => :aws_sqs,
          :eventgrid => :eventgrid,
          :eventhubs => :eventhubs,
          :servicebus => :servicebus,
          :gcp_pubsub => :gcp_pubsub,
          :jms => :jms,
          :kafka => :kafka,
          :rabbitmq => :rabbitmq,
          :rocketmq => :rocketmq
        }
  @doc """
  The messaging system as identified by the client instrumentation.

  ### Notes

  The actual messaging system may differ from the one known by the client. For example, when using Kafka client libraries to communicate with Azure Event Hubs, the `messaging.system` is set to `kafka` based on the instrumentation's best knowledge.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_system().activemq
      :activemq
      
      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_system(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'messaging_system.activemq'.
  activemq

  ?messaging_system.(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec messaging_system() :: messaging_system()
  def messaging_system() do
    %{
      :activemq => :activemq,
      :aws_sqs => :aws_sqs,
      :eventgrid => :eventgrid,
      :eventhubs => :eventhubs,
      :servicebus => :servicebus,
      :gcp_pubsub => :gcp_pubsub,
      :jms => :jms,
      :kafka => :kafka,
      :rabbitmq => :rabbitmq,
      :rocketmq => :rocketmq
    }
  end

  @spec messaging_system(atom() | String.t()) :: atom() | String.t()
  def messaging_system(custom_value) do
    custom_value
  end
end
