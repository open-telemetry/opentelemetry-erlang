defmodule OpenTelemetry.SemanticConventions.MessagingAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Messaging attributes.
  """

  @doc """
  The number of messages sent, received, or processed in the scope of the batching operation.
  ### Notes

  Instrumentations **SHOULD** **NOT** set `messaging.batch.message_count` on spans that operate with a single message. When a messaging client library supports both batch and single-message API for the same operation, instrumentations **SHOULD** use `messaging.batch.message_count` for batching APIs and **SHOULD** **NOT** use it for single-message APIs.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_batch_messagecount()
      :"messaging.batch.message_count"
  """
  @spec messaging_batch_messagecount :: :"messaging.batch.message_count"
  def messaging_batch_messagecount do
    :"messaging.batch.message_count"
  end

  @doc """
  A unique identifier for the client that consumes or produces a message.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_client_id()
      :"messaging.client.id"
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


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_destination_anonymous()
      :"messaging.destination.anonymous"
  """
  @spec messaging_destination_anonymous :: :"messaging.destination.anonymous"
  def messaging_destination_anonymous do
    :"messaging.destination.anonymous"
  end

  @doc """
  The message destination name
  ### Notes

  Destination name **SHOULD** uniquely identify a specific queue, topic or other entity within the broker. If
  the broker doesn't have such notion, the destination name **SHOULD** uniquely identify the broker.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_destination_name()
      :"messaging.destination.name"
  """
  @spec messaging_destination_name :: :"messaging.destination.name"
  def messaging_destination_name do
    :"messaging.destination.name"
  end

  @doc """
  The identifier of the partition messages are sent to or received from, unique within the `messaging.destination.name`.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_destination_partition_id()
      :"messaging.destination.partition.id"
  """
  @spec messaging_destination_partition_id :: :"messaging.destination.partition.id"
  def messaging_destination_partition_id do
    :"messaging.destination.partition.id"
  end

  @doc """
  Low cardinality representation of the messaging destination name
  ### Notes

  Destination names could be constructed from templates. An example would be a destination name involving a user name or product id. Although the destination name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_destination_template()
      :"messaging.destination.template"
  """
  @spec messaging_destination_template :: :"messaging.destination.template"
  def messaging_destination_template do
    :"messaging.destination.template"
  end

  @doc """
  A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_destination_temporary()
      :"messaging.destination.temporary"
  """
  @spec messaging_destination_temporary :: :"messaging.destination.temporary"
  def messaging_destination_temporary do
    :"messaging.destination.temporary"
  end

  @doc """
  A boolean that is true if the publish message destination is anonymous (could be unnamed or have auto-generated name).


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_destinationpublish_anonymous()
      :"messaging.destination_publish.anonymous"
  """
  @spec messaging_destinationpublish_anonymous :: :"messaging.destination_publish.anonymous"
  def messaging_destinationpublish_anonymous do
    :"messaging.destination_publish.anonymous"
  end

  @doc """
  The name of the original destination the message was published to
  ### Notes

  The name **SHOULD** uniquely identify a specific queue, topic, or other entity within the broker. If
  the broker doesn't have such notion, the original destination name **SHOULD** uniquely identify the broker.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_destinationpublish_name()
      :"messaging.destination_publish.name"
  """
  @spec messaging_destinationpublish_name :: :"messaging.destination_publish.name"
  def messaging_destinationpublish_name do
    :"messaging.destination_publish.name"
  end

  @doc """
  The name of the consumer group the event consumer is associated with.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_eventhubs_consumer_group()
      :"messaging.eventhubs.consumer.group"
  """
  @spec messaging_eventhubs_consumer_group :: :"messaging.eventhubs.consumer.group"
  def messaging_eventhubs_consumer_group do
    :"messaging.eventhubs.consumer.group"
  end

  @doc """
  The UTC epoch seconds at which the message has been accepted and stored in the entity.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_eventhubs_message_enqueuedtime()
      :"messaging.eventhubs.message.enqueued_time"
  """
  @spec messaging_eventhubs_message_enqueuedtime :: :"messaging.eventhubs.message.enqueued_time"
  def messaging_eventhubs_message_enqueuedtime do
    :"messaging.eventhubs.message.enqueued_time"
  end

  @doc """
  The ack deadline in seconds set for the modify ack deadline request.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_gcppubsub_message_ackdeadline()
      :"messaging.gcp_pubsub.message.ack_deadline"
  """
  @spec messaging_gcppubsub_message_ackdeadline :: :"messaging.gcp_pubsub.message.ack_deadline"
  def messaging_gcppubsub_message_ackdeadline do
    :"messaging.gcp_pubsub.message.ack_deadline"
  end

  @doc """
  The ack id for a given message.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_gcppubsub_message_ackid()
      :"messaging.gcp_pubsub.message.ack_id"
  """
  @spec messaging_gcppubsub_message_ackid :: :"messaging.gcp_pubsub.message.ack_id"
  def messaging_gcppubsub_message_ackid do
    :"messaging.gcp_pubsub.message.ack_id"
  end

  @doc """
  The delivery attempt for a given message.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_gcppubsub_message_deliveryattempt()
      :"messaging.gcp_pubsub.message.delivery_attempt"
  """
  @spec messaging_gcppubsub_message_deliveryattempt ::
          :"messaging.gcp_pubsub.message.delivery_attempt"
  def messaging_gcppubsub_message_deliveryattempt do
    :"messaging.gcp_pubsub.message.delivery_attempt"
  end

  @doc """
  The ordering key for a given message. If the attribute is not present, the message does not have an ordering key.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_gcppubsub_message_orderingkey()
      :"messaging.gcp_pubsub.message.ordering_key"
  """
  @spec messaging_gcppubsub_message_orderingkey :: :"messaging.gcp_pubsub.message.ordering_key"
  def messaging_gcppubsub_message_orderingkey do
    :"messaging.gcp_pubsub.message.ordering_key"
  end

  @doc """
  Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_kafka_consumer_group()
      :"messaging.kafka.consumer.group"
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

  ### Notes

  If the key type is not string, it's string representation has to be supplied for the attribute. If the key has no unambiguous, canonical string form, don't include its value.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_kafka_message_key()
      :"messaging.kafka.message.key"
  """
  @spec messaging_kafka_message_key :: :"messaging.kafka.message.key"
  def messaging_kafka_message_key do
    :"messaging.kafka.message.key"
  end

  @doc """
  The offset of a record in the corresponding Kafka partition.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_kafka_message_offset()
      :"messaging.kafka.message.offset"
  """
  @spec messaging_kafka_message_offset :: :"messaging.kafka.message.offset"
  def messaging_kafka_message_offset do
    :"messaging.kafka.message.offset"
  end

  @doc """
  A boolean that is true if the message is a tombstone.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_kafka_message_tombstone()
      :"messaging.kafka.message.tombstone"
  """
  @spec messaging_kafka_message_tombstone :: :"messaging.kafka.message.tombstone"
  def messaging_kafka_message_tombstone do
    :"messaging.kafka.message.tombstone"
  end

  @doc """
  The size of the message body in bytes.

  ### Notes

  This can refer to both the compressed or uncompressed body size. If both sizes are known, the uncompressed
  body size should be used.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_message_body_size()
      :"messaging.message.body.size"
  """
  @spec messaging_message_body_size :: :"messaging.message.body.size"
  def messaging_message_body_size do
    :"messaging.message.body.size"
  end

  @doc """
  The conversation ID identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID".



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_message_conversationid()
      :"messaging.message.conversation_id"
  """
  @spec messaging_message_conversationid :: :"messaging.message.conversation_id"
  def messaging_message_conversationid do
    :"messaging.message.conversation_id"
  end

  @doc """
  The size of the message body and metadata in bytes.

  ### Notes

  This can refer to both the compressed or uncompressed size. If both sizes are known, the uncompressed
  size should be used.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_message_envelope_size()
      :"messaging.message.envelope.size"
  """
  @spec messaging_message_envelope_size :: :"messaging.message.envelope.size"
  def messaging_message_envelope_size do
    :"messaging.message.envelope.size"
  end

  @doc """
  A value used by the messaging system as an identifier for the message, represented as a string.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_message_id()
      :"messaging.message.id"
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



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_operation_name()
      :"messaging.operation.name"
  """
  @spec messaging_operation_name :: :"messaging.operation.name"
  def messaging_operation_name do
    :"messaging.operation.name"
  end

  @type deliver() :: :process

  @typedoc """
  A string identifying the type of the messaging operation.


  ### Options
  * `:publish` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - One or more messages are provided for publishing to an intermediary. If a single message is published, the context of the "Publish" span can be used as the creation context and no "Create" span needs to be created.

  * `:create` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - A message is created. "Create" spans always refer to a single message and are used to provide a unique creation context for messages in batch publishing scenarios.

  * `:receive` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - One or more messages are requested by a consumer. This operation refers to pull-based scenarios, where consumers explicitly call methods of messaging SDKs to receive messages.

  * `:deliver` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - One or more messages are delivered to or processed by a consumer.

  * `:settle` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - One or more messages are settled.


  """
  @type messaging_operation_type() :: :publish | :create | :receive | deliver() | :settle | atom()

  @doc """
  A string identifying the type of the messaging operation.

  ### Notes

  If a custom value is used, it **MUST** be of low cardinality.

  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_operation_type(:publish)
      :publish
      
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_operation_type(:custom_value)
      :custom_value
  """
  @spec messaging_operation_type(messaging_operation_type()) ::
          :publish | :create | :receive | deliver() | :settle | atom()
  def messaging_operation_type(option) do
    :"messaging.operation.type"

    case option do
      :publish -> :publish
      :create -> :create
      :receive -> :receive
      :deliver -> :process
      :settle -> :settle
      _ -> option
    end
  end

  @doc """
  RabbitMQ message routing key.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rabbitmq_destination_routingkey()
      :"messaging.rabbitmq.destination.routing_key"
  """
  @spec messaging_rabbitmq_destination_routingkey :: :"messaging.rabbitmq.destination.routing_key"
  def messaging_rabbitmq_destination_routingkey do
    :"messaging.rabbitmq.destination.routing_key"
  end

  @doc """
  RabbitMQ message delivery tag



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rabbitmq_message_deliverytag()
      :"messaging.rabbitmq.message.delivery_tag"
  """
  @spec messaging_rabbitmq_message_deliverytag :: :"messaging.rabbitmq.message.delivery_tag"
  def messaging_rabbitmq_message_deliverytag do
    :"messaging.rabbitmq.message.delivery_tag"
  end

  @doc """
  Name of the RocketMQ producer/consumer group that is handling the message. The client type is identified by the SpanKind.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_clientgroup()
      :"messaging.rocketmq.client_group"
  """
  @spec messaging_rocketmq_clientgroup :: :"messaging.rocketmq.client_group"
  def messaging_rocketmq_clientgroup do
    :"messaging.rocketmq.client_group"
  end

  @typedoc """
  Model of message consumption. This only applies to consumer spans.


  ### Options
  * `:clustering` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Clustering consumption model
  * `:broadcasting` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Broadcasting consumption model

  """
  @type messaging_rocketmq_consumptionmodel() :: :clustering | :broadcasting | atom()

  @doc """
  Model of message consumption. This only applies to consumer spans.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_consumptionmodel(:clustering)
      :clustering
      
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_consumptionmodel(:custom_value)
      :custom_value
  """
  @spec messaging_rocketmq_consumptionmodel(messaging_rocketmq_consumptionmodel()) ::
          :clustering | :broadcasting | atom()
  def messaging_rocketmq_consumptionmodel(option) do
    :"messaging.rocketmq.consumption_model"

    case option do
      :clustering -> :clustering
      :broadcasting -> :broadcasting
      _ -> option
    end
  end

  @doc """
  The delay time level for delay message, which determines the message delay time.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_message_delaytimelevel()
      :"messaging.rocketmq.message.delay_time_level"
  """
  @spec messaging_rocketmq_message_delaytimelevel ::
          :"messaging.rocketmq.message.delay_time_level"
  def messaging_rocketmq_message_delaytimelevel do
    :"messaging.rocketmq.message.delay_time_level"
  end

  @doc """
  The timestamp in milliseconds that the delay message is expected to be delivered to consumer.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_message_deliverytimestamp()
      :"messaging.rocketmq.message.delivery_timestamp"
  """
  @spec messaging_rocketmq_message_deliverytimestamp ::
          :"messaging.rocketmq.message.delivery_timestamp"
  def messaging_rocketmq_message_deliverytimestamp do
    :"messaging.rocketmq.message.delivery_timestamp"
  end

  @doc """
  It is essential for FIFO message. Messages that belong to the same message group are always processed one by one within the same consumer group.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_message_group()
      :"messaging.rocketmq.message.group"
  """
  @spec messaging_rocketmq_message_group :: :"messaging.rocketmq.message.group"
  def messaging_rocketmq_message_group do
    :"messaging.rocketmq.message.group"
  end

  @doc """
  Key(s) of message, another way to mark message besides message id.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_message_keys()
      :"messaging.rocketmq.message.keys"
  """
  @spec messaging_rocketmq_message_keys :: :"messaging.rocketmq.message.keys"
  def messaging_rocketmq_message_keys do
    :"messaging.rocketmq.message.keys"
  end

  @doc """
  The secondary classifier of message besides topic.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_message_tag()
      :"messaging.rocketmq.message.tag"
  """
  @spec messaging_rocketmq_message_tag :: :"messaging.rocketmq.message.tag"
  def messaging_rocketmq_message_tag do
    :"messaging.rocketmq.message.tag"
  end

  @typedoc """
  Type of message.


  ### Options
  * `:normal` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Normal message
  * `:fifo` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - FIFO message
  * `:delay` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Delay message
  * `:transaction` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Transaction message

  """
  @type messaging_rocketmq_message_type() :: :normal | :fifo | :delay | :transaction | atom()

  @doc """
  Type of message.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_message_type(:normal)
      :normal
      
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_message_type(:custom_value)
      :custom_value
  """
  @spec messaging_rocketmq_message_type(messaging_rocketmq_message_type()) ::
          :normal | :fifo | :delay | :transaction | atom()
  def messaging_rocketmq_message_type(option) do
    :"messaging.rocketmq.message.type"

    case option do
      :normal -> :normal
      :fifo -> :fifo
      :delay -> :delay
      :transaction -> :transaction
      _ -> option
    end
  end

  @doc """
  Namespace of RocketMQ resources, resources in different namespaces are individual.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_rocketmq_namespace()
      :"messaging.rocketmq.namespace"
  """
  @spec messaging_rocketmq_namespace :: :"messaging.rocketmq.namespace"
  def messaging_rocketmq_namespace do
    :"messaging.rocketmq.namespace"
  end

  @doc """
  The name of the subscription in the topic messages are received from.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_servicebus_destination_subscriptionname()
      :"messaging.servicebus.destination.subscription_name"
  """
  @spec messaging_servicebus_destination_subscriptionname ::
          :"messaging.servicebus.destination.subscription_name"
  def messaging_servicebus_destination_subscriptionname do
    :"messaging.servicebus.destination.subscription_name"
  end

  @typedoc """
  Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock).


  ### Options
  * `:complete` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Message is completed
  * `:abandon` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Message is abandoned
  * `:dead_letter` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Message is sent to dead letter queue
  * `:defer` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Message is deferred

  """
  @type messaging_servicebus_dispositionstatus() ::
          :complete | :abandon | :dead_letter | :defer | atom()

  @doc """
  Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock).



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_servicebus_dispositionstatus(:complete)
      :complete
      
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_servicebus_dispositionstatus(:custom_value)
      :custom_value
  """
  @spec messaging_servicebus_dispositionstatus(messaging_servicebus_dispositionstatus()) ::
          :complete | :abandon | :dead_letter | :defer | atom()
  def messaging_servicebus_dispositionstatus(option) do
    :"messaging.servicebus.disposition_status"

    case option do
      :complete -> :complete
      :abandon -> :abandon
      :dead_letter -> :dead_letter
      :defer -> :defer
      _ -> option
    end
  end

  @doc """
  Number of deliveries that have been attempted for this message.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_servicebus_message_deliverycount()
      :"messaging.servicebus.message.delivery_count"
  """
  @spec messaging_servicebus_message_deliverycount ::
          :"messaging.servicebus.message.delivery_count"
  def messaging_servicebus_message_deliverycount do
    :"messaging.servicebus.message.delivery_count"
  end

  @doc """
  The UTC epoch seconds at which the message has been accepted and stored in the entity.



  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_servicebus_message_enqueuedtime()
      :"messaging.servicebus.message.enqueued_time"
  """
  @spec messaging_servicebus_message_enqueuedtime :: :"messaging.servicebus.message.enqueued_time"
  def messaging_servicebus_message_enqueuedtime do
    :"messaging.servicebus.message.enqueued_time"
  end

  @typedoc """
  The messaging system as identified by the client instrumentation.

  ### Options
  * `:activemq` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache ActiveMQ
  * `:aws_sqs` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Amazon Simple Queue Service (SQS)
  * `:eventgrid` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Azure Event Grid
  * `:eventhubs` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Azure Event Hubs
  * `:servicebus` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Azure Service Bus
  * `:gcp_pubsub` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Google Cloud Pub/Sub
  * `:jms` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Java Message Service
  * `:kafka` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Kafka
  * `:rabbitmq` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - RabbitMQ
  * `:rocketmq` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache RocketMQ

  """
  @type messaging_system() ::
          :activemq
          | :aws_sqs
          | :eventgrid
          | :eventhubs
          | :servicebus
          | :gcp_pubsub
          | :jms
          | :kafka
          | :rabbitmq
          | :rocketmq
          | atom()

  @doc """
  The messaging system as identified by the client instrumentation.
  ### Notes

  The actual messaging system may differ from the one known by the client. For example, when using Kafka client libraries to communicate with Azure Event Hubs, the `messaging.system` is set to `kafka` based on the instrumentation's best knowledge.


  ### Example
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_system(:activemq)
      :activemq
      
      iex> OpenTelemetry.SemanticConventions.MessagingAttributes.messaging_system(:custom_value)
      :custom_value
  """
  @spec messaging_system(messaging_system()) ::
          :activemq
          | :aws_sqs
          | :eventgrid
          | :eventhubs
          | :servicebus
          | :gcp_pubsub
          | :jms
          | :kafka
          | :rabbitmq
          | :rocketmq
          | atom()
  def messaging_system(option) do
    :"messaging.system"

    case option do
      :activemq -> :activemq
      :aws_sqs -> :aws_sqs
      :eventgrid -> :eventgrid
      :eventhubs -> :eventhubs
      :servicebus -> :servicebus
      :gcp_pubsub -> :gcp_pubsub
      :jms -> :jms
      :kafka -> :kafka
      :rabbitmq -> :rabbitmq
      :rocketmq -> :rocketmq
      _ -> option
    end
  end
end
