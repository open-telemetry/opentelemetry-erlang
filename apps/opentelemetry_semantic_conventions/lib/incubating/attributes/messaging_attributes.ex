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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_batch_message_count()
      :"messaging.batch.message_count"

  ### Erlang

  ```erlang
  ?MESSAGING_BATCH_MESSAGE_COUNT.
  'messaging.batch.message_count'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_batch_message_count :: :"messaging.batch.message_count"
  def messaging_batch_message_count do
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

  @doc """
  The name of the consumer group with which a consumer is associated.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Semantic conventions for individual messaging systems **SHOULD** document whether `messaging.consumer.group.name` is applicable and what it means in the context of that system.

  ### Examples

  ```
  ["my-group", "indexer"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_consumer_group_name()
      :"messaging.consumer.group.name"

  ### Erlang

  ```erlang
  ?MESSAGING_CONSUMER_GROUP_NAME.
  'messaging.consumer.group.name'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_consumer_group_name :: :"messaging.consumer.group.name"
  def messaging_consumer_group_name do
    :"messaging.consumer.group.name"
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
  The name of the destination subscription from which a message is consumed.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Semantic conventions for individual messaging systems **SHOULD** document whether `messaging.destination.subscription.name` is applicable and what it means in the context of that system.

  ### Examples

  ```
  ["subscription-a"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_destination_subscription_name()
      :"messaging.destination.subscription.name"

  ### Erlang

  ```erlang
  ?MESSAGING_DESTINATION_SUBSCRIPTION_NAME.
  'messaging.destination.subscription.name'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_destination_subscription_name :: :"messaging.destination.subscription.name"
  def messaging_destination_subscription_name do
    :"messaging.destination.subscription.name"
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

  @deprecated """
  No replacement at this time.
  """
  @spec messaging_destination_publish_anonymous :: :"messaging.destination_publish.anonymous"
  def messaging_destination_publish_anonymous do
    :"messaging.destination_publish.anonymous"
  end

  @deprecated """
  No replacement at this time.
  """
  @spec messaging_destination_publish_name :: :"messaging.destination_publish.name"
  def messaging_destination_publish_name do
    :"messaging.destination_publish.name"
  end

  @deprecated """
  Replaced by `messaging.consumer.group.name`.

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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_eventhubs_message_enqueued_time()
      :"messaging.eventhubs.message.enqueued_time"

  ### Erlang

  ```erlang
  ?MESSAGING_EVENTHUBS_MESSAGE_ENQUEUED_TIME.
  'messaging.eventhubs.message.enqueued_time'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_eventhubs_message_enqueued_time :: :"messaging.eventhubs.message.enqueued_time"
  def messaging_eventhubs_message_enqueued_time do
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_gcp_pubsub_message_ack_deadline()
      :"messaging.gcp_pubsub.message.ack_deadline"

  ### Erlang

  ```erlang
  ?MESSAGING_GCP_PUBSUB_MESSAGE_ACK_DEADLINE.
  'messaging.gcp_pubsub.message.ack_deadline'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_gcp_pubsub_message_ack_deadline :: :"messaging.gcp_pubsub.message.ack_deadline"
  def messaging_gcp_pubsub_message_ack_deadline do
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_gcp_pubsub_message_ack_id()
      :"messaging.gcp_pubsub.message.ack_id"

  ### Erlang

  ```erlang
  ?MESSAGING_GCP_PUBSUB_MESSAGE_ACK_ID.
  'messaging.gcp_pubsub.message.ack_id'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_gcp_pubsub_message_ack_id :: :"messaging.gcp_pubsub.message.ack_id"
  def messaging_gcp_pubsub_message_ack_id do
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_gcp_pubsub_message_delivery_attempt()
      :"messaging.gcp_pubsub.message.delivery_attempt"

  ### Erlang

  ```erlang
  ?MESSAGING_GCP_PUBSUB_MESSAGE_DELIVERY_ATTEMPT.
  'messaging.gcp_pubsub.message.delivery_attempt'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_gcp_pubsub_message_delivery_attempt ::
          :"messaging.gcp_pubsub.message.delivery_attempt"
  def messaging_gcp_pubsub_message_delivery_attempt do
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_gcp_pubsub_message_ordering_key()
      :"messaging.gcp_pubsub.message.ordering_key"

  ### Erlang

  ```erlang
  ?MESSAGING_GCP_PUBSUB_MESSAGE_ORDERING_KEY.
  'messaging.gcp_pubsub.message.ordering_key'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_gcp_pubsub_message_ordering_key :: :"messaging.gcp_pubsub.message.ordering_key"
  def messaging_gcp_pubsub_message_ordering_key do
    :"messaging.gcp_pubsub.message.ordering_key"
  end

  @deprecated """
  Replaced by `messaging.consumer.group.name`.

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

  @deprecated """
  Replaced by `messaging.kafka.offset`.

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
  The offset of a record in the corresponding Kafka partition.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  42
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_kafka_offset()
      :"messaging.kafka.offset"

  ### Erlang

  ```erlang
  ?MESSAGING_KAFKA_OFFSET.
  'messaging.kafka.offset'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_kafka_offset :: :"messaging.kafka.offset"
  def messaging_kafka_offset do
    :"messaging.kafka.offset"
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_message_conversation_id()
      :"messaging.message.conversation_id"

  ### Erlang

  ```erlang
  ?MESSAGING_MESSAGE_CONVERSATION_ID.
  'messaging.message.conversation_id'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_message_conversation_id :: :"messaging.message.conversation_id"
  def messaging_message_conversation_id do
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

  * `:process` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - One or more messages are processed by a consumer.

  * `:settle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - One or more messages are settled.

  * `:deliver` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Deprecated. Use `process` instead.~~
  """
  @type messaging_operation_type_values() :: %{
          :publish => :publish,
          :create => :create,
          :receive => :receive,
          :process => :process,
          :settle => :settle,
          :deliver => :deliver
        }
  @doc """
  A string identifying the type of the messaging operation.


  ### Notes

  If a custom value is used, it **MUST** be of low cardinality.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_operation_type()
      :"messaging.operation.type"

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_operation_type_values().publish
      :publish

      iex> %{OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_operation_type() => OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_operation_type_values().publish}
      %{:"messaging.operation.type" => :publish}

  ### Erlang

  ```erlang
  ?MESSAGING_OPERATION_TYPE.
  'messaging.operation.type'

  ?MESSAGING_OPERATION_TYPE_VALUES_PUBLISH.
  'publish'

  \#{?MESSAGING_OPERATION_TYPE => ?MESSAGING_OPERATION_TYPE_VALUES_PUBLISH}.
  \#{'messaging.operation.type' => 'publish'}
  ```

  <!-- tabs-close -->
  """
  @spec messaging_operation_type :: :"messaging.operation.type"
  def messaging_operation_type do
    :"messaging.operation.type"
  end

  @spec messaging_operation_type_values() :: messaging_operation_type_values()
  def messaging_operation_type_values() do
    %{
      :publish => :publish,
      :create => :create,
      :receive => :receive,
      :process => :process,
      :settle => :settle,
      :deliver => :deliver
    }
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rabbitmq_destination_routing_key()
      :"messaging.rabbitmq.destination.routing_key"

  ### Erlang

  ```erlang
  ?MESSAGING_RABBITMQ_DESTINATION_ROUTING_KEY.
  'messaging.rabbitmq.destination.routing_key'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rabbitmq_destination_routing_key ::
          :"messaging.rabbitmq.destination.routing_key"
  def messaging_rabbitmq_destination_routing_key do
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rabbitmq_message_delivery_tag()
      :"messaging.rabbitmq.message.delivery_tag"

  ### Erlang

  ```erlang
  ?MESSAGING_RABBITMQ_MESSAGE_DELIVERY_TAG.
  'messaging.rabbitmq.message.delivery_tag'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rabbitmq_message_delivery_tag :: :"messaging.rabbitmq.message.delivery_tag"
  def messaging_rabbitmq_message_delivery_tag do
    :"messaging.rabbitmq.message.delivery_tag"
  end

  @deprecated """
  Replaced by `messaging.consumer.group.name` on the consumer spans. No replacement for producer spans.

  """
  @spec messaging_rocketmq_client_group :: :"messaging.rocketmq.client_group"
  def messaging_rocketmq_client_group do
    :"messaging.rocketmq.client_group"
  end

  @typedoc """
  Model of message consumption. This only applies to consumer spans.


  ### Enum Values
  * `:clustering` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Clustering consumption model
  * `:broadcasting` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Broadcasting consumption model
  """
  @type messaging_rocketmq_consumption_model_values() :: %{
          :clustering => :clustering,
          :broadcasting => :broadcasting
        }
  @doc """
  Model of message consumption. This only applies to consumer spans.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_consumption_model()
      :"messaging.rocketmq.consumption_model"

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_consumption_model_values().clustering
      :clustering

      iex> %{OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_consumption_model() => OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_consumption_model_values().clustering}
      %{:"messaging.rocketmq.consumption_model" => :clustering}

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_CONSUMPTION_MODEL.
  'messaging.rocketmq.consumption_model'

  ?MESSAGING_ROCKETMQ_CONSUMPTION_MODEL_VALUES_CLUSTERING.
  'clustering'

  \#{?MESSAGING_ROCKETMQ_CONSUMPTION_MODEL => ?MESSAGING_ROCKETMQ_CONSUMPTION_MODEL_VALUES_CLUSTERING}.
  \#{'messaging.rocketmq.consumption_model' => 'clustering'}
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_consumption_model :: :"messaging.rocketmq.consumption_model"
  def messaging_rocketmq_consumption_model do
    :"messaging.rocketmq.consumption_model"
  end

  @spec messaging_rocketmq_consumption_model_values() ::
          messaging_rocketmq_consumption_model_values()
  def messaging_rocketmq_consumption_model_values() do
    %{
      :clustering => :clustering,
      :broadcasting => :broadcasting
    }
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_delay_time_level()
      :"messaging.rocketmq.message.delay_time_level"

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_MESSAGE_DELAY_TIME_LEVEL.
  'messaging.rocketmq.message.delay_time_level'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_message_delay_time_level ::
          :"messaging.rocketmq.message.delay_time_level"
  def messaging_rocketmq_message_delay_time_level do
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_delivery_timestamp()
      :"messaging.rocketmq.message.delivery_timestamp"

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_MESSAGE_DELIVERY_TIMESTAMP.
  'messaging.rocketmq.message.delivery_timestamp'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_message_delivery_timestamp ::
          :"messaging.rocketmq.message.delivery_timestamp"
  def messaging_rocketmq_message_delivery_timestamp do
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
  @type messaging_rocketmq_message_type_values() :: %{
          :normal => :normal,
          :fifo => :fifo,
          :delay => :delay,
          :transaction => :transaction
        }
  @doc """
  Type of message.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_type()
      :"messaging.rocketmq.message.type"

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_type_values().normal
      :normal

      iex> %{OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_type() => OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_rocketmq_message_type_values().normal}
      %{:"messaging.rocketmq.message.type" => :normal}

  ### Erlang

  ```erlang
  ?MESSAGING_ROCKETMQ_MESSAGE_TYPE.
  'messaging.rocketmq.message.type'

  ?MESSAGING_ROCKETMQ_MESSAGE_TYPE_VALUES_NORMAL.
  'normal'

  \#{?MESSAGING_ROCKETMQ_MESSAGE_TYPE => ?MESSAGING_ROCKETMQ_MESSAGE_TYPE_VALUES_NORMAL}.
  \#{'messaging.rocketmq.message.type' => 'normal'}
  ```

  <!-- tabs-close -->
  """
  @spec messaging_rocketmq_message_type :: :"messaging.rocketmq.message.type"
  def messaging_rocketmq_message_type do
    :"messaging.rocketmq.message.type"
  end

  @spec messaging_rocketmq_message_type_values() :: messaging_rocketmq_message_type_values()
  def messaging_rocketmq_message_type_values() do
    %{
      :normal => :normal,
      :fifo => :fifo,
      :delay => :delay,
      :transaction => :transaction
    }
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

  @deprecated """
  Replaced by `messaging.servicebus.destination.subscription_name`.

  """
  @spec messaging_servicebus_destination_subscription_name ::
          :"messaging.servicebus.destination.subscription_name"
  def messaging_servicebus_destination_subscription_name do
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
  @type messaging_servicebus_disposition_status_values() :: %{
          :complete => :complete,
          :abandon => :abandon,
          :dead_letter => :dead_letter,
          :defer => :defer
        }
  @doc """
  Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock).



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_disposition_status()
      :"messaging.servicebus.disposition_status"

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_disposition_status_values().complete
      :complete

      iex> %{OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_disposition_status() => OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_disposition_status_values().complete}
      %{:"messaging.servicebus.disposition_status" => :complete}

  ### Erlang

  ```erlang
  ?MESSAGING_SERVICEBUS_DISPOSITION_STATUS.
  'messaging.servicebus.disposition_status'

  ?MESSAGING_SERVICEBUS_DISPOSITION_STATUS_VALUES_COMPLETE.
  'complete'

  \#{?MESSAGING_SERVICEBUS_DISPOSITION_STATUS => ?MESSAGING_SERVICEBUS_DISPOSITION_STATUS_VALUES_COMPLETE}.
  \#{'messaging.servicebus.disposition_status' => 'complete'}
  ```

  <!-- tabs-close -->
  """
  @spec messaging_servicebus_disposition_status :: :"messaging.servicebus.disposition_status"
  def messaging_servicebus_disposition_status do
    :"messaging.servicebus.disposition_status"
  end

  @spec messaging_servicebus_disposition_status_values() ::
          messaging_servicebus_disposition_status_values()
  def messaging_servicebus_disposition_status_values() do
    %{
      :complete => :complete,
      :abandon => :abandon,
      :dead_letter => :dead_letter,
      :defer => :defer
    }
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_message_delivery_count()
      :"messaging.servicebus.message.delivery_count"

  ### Erlang

  ```erlang
  ?MESSAGING_SERVICEBUS_MESSAGE_DELIVERY_COUNT.
  'messaging.servicebus.message.delivery_count'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_servicebus_message_delivery_count ::
          :"messaging.servicebus.message.delivery_count"
  def messaging_servicebus_message_delivery_count do
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

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_servicebus_message_enqueued_time()
      :"messaging.servicebus.message.enqueued_time"

  ### Erlang

  ```erlang
  ?MESSAGING_SERVICEBUS_MESSAGE_ENQUEUED_TIME.
  'messaging.servicebus.message.enqueued_time'
  ```

  <!-- tabs-close -->
  """
  @spec messaging_servicebus_message_enqueued_time ::
          :"messaging.servicebus.message.enqueued_time"
  def messaging_servicebus_message_enqueued_time do
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
  * `:pulsar` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Pulsar
  """
  @type messaging_system_values() :: %{
          :activemq => :activemq,
          :aws_sqs => :aws_sqs,
          :eventgrid => :eventgrid,
          :eventhubs => :eventhubs,
          :servicebus => :servicebus,
          :gcp_pubsub => :gcp_pubsub,
          :jms => :jms,
          :kafka => :kafka,
          :rabbitmq => :rabbitmq,
          :rocketmq => :rocketmq,
          :pulsar => :pulsar
        }
  @doc """
  The messaging system as identified by the client instrumentation.

  ### Notes

  The actual messaging system may differ from the one known by the client. For example, when using Kafka client libraries to communicate with Azure Event Hubs, the `messaging.system` is set to `kafka` based on the instrumentation's best knowledge.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_system()
      :"messaging.system"

      iex> OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_system_values().activemq
      :activemq

      iex> %{OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_system() => OpenTelemetry.SemConv.Incubating.MessagingAttributes.messaging_system_values().activemq}
      %{:"messaging.system" => :activemq}

  ### Erlang

  ```erlang
  ?MESSAGING_SYSTEM.
  'messaging.system'

  ?MESSAGING_SYSTEM_VALUES_ACTIVEMQ.
  'activemq'

  \#{?MESSAGING_SYSTEM => ?MESSAGING_SYSTEM_VALUES_ACTIVEMQ}.
  \#{'messaging.system' => 'activemq'}
  ```

  <!-- tabs-close -->
  """
  @spec messaging_system :: :"messaging.system"
  def messaging_system do
    :"messaging.system"
  end

  @spec messaging_system_values() :: messaging_system_values()
  def messaging_system_values() do
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
      :rocketmq => :rocketmq,
      :pulsar => :pulsar
    }
  end
end
