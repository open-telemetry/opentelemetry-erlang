
%%%------------------------------------------------------------------------
%% Copyright The OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%-------------------------------------------------------------------------

%% The number of messages sent, received, or processed in the scope of the batching operation.
-define(MESSAGING_BATCH_MESSAGECOUNT, 'messaging.batch.message_count').


%% A unique identifier for the client that consumes or produces a message.
%%  
-define(MESSAGING_CLIENT_ID, 'messaging.client.id').

%% @deprecated Replaced by `messaging.client.id`.
%% Deprecated, use `messaging.client.id` instead.
%%  
-define(MESSAGING_CLIENTID, 'messaging.client_id').


%% A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name).
-define(MESSAGING_DESTINATION_ANONYMOUS, 'messaging.destination.anonymous').


%% The message destination name
-define(MESSAGING_DESTINATION_NAME, 'messaging.destination.name').


%% The identifier of the partition messages are sent to or received from, unique within the `messaging.destination.name`.
%%  
-define(MESSAGING_DESTINATION_PARTITION_ID, 'messaging.destination.partition.id').


%% Low cardinality representation of the messaging destination name
-define(MESSAGING_DESTINATION_TEMPLATE, 'messaging.destination.template').


%% A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed.
-define(MESSAGING_DESTINATION_TEMPORARY, 'messaging.destination.temporary').


%% A boolean that is true if the publish message destination is anonymous (could be unnamed or have auto-generated name).
-define(MESSAGING_DESTINATIONPUBLISH_ANONYMOUS, 'messaging.destination_publish.anonymous').


%% The name of the original destination the message was published to
-define(MESSAGING_DESTINATIONPUBLISH_NAME, 'messaging.destination_publish.name').


%% The name of the consumer group the event consumer is associated with.
%%  
-define(MESSAGING_EVENTHUBS_CONSUMER_GROUP, 'messaging.eventhubs.consumer.group').


%% The UTC epoch seconds at which the message has been accepted and stored in the entity.
%%  
-define(MESSAGING_EVENTHUBS_MESSAGE_ENQUEUEDTIME, 'messaging.eventhubs.message.enqueued_time').


%% The ack deadline in seconds set for the modify ack deadline request.
%%  
-define(MESSAGING_GCPPUBSUB_MESSAGE_ACKDEADLINE, 'messaging.gcp_pubsub.message.ack_deadline').


%% The ack id for a given message.
%%  
-define(MESSAGING_GCPPUBSUB_MESSAGE_ACKID, 'messaging.gcp_pubsub.message.ack_id').


%% The delivery attempt for a given message.
%%  
-define(MESSAGING_GCPPUBSUB_MESSAGE_DELIVERYATTEMPT, 'messaging.gcp_pubsub.message.delivery_attempt').


%% The ordering key for a given message. If the attribute is not present, the message does not have an ordering key.
%%  
-define(MESSAGING_GCPPUBSUB_MESSAGE_ORDERINGKEY, 'messaging.gcp_pubsub.message.ordering_key').


%% Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers.
%%  
-define(MESSAGING_KAFKA_CONSUMER_GROUP, 'messaging.kafka.consumer.group').

%% @deprecated Replaced by `messaging.destination.partition.id`.
%% Deprecated, use `messaging.destination.partition.id` instead.
%%  
-define(MESSAGING_KAFKA_DESTINATION_PARTITION, 'messaging.kafka.destination.partition').


%% Message keys in Kafka are used for grouping alike messages to ensure they're processed on the same partition. They differ from `messaging.message.id` in that they're not unique. If the key is `null`, the attribute MUST NOT be set.
%%  
-define(MESSAGING_KAFKA_MESSAGE_KEY, 'messaging.kafka.message.key').


%% The offset of a record in the corresponding Kafka partition.
%%  
-define(MESSAGING_KAFKA_MESSAGE_OFFSET, 'messaging.kafka.message.offset').


%% A boolean that is true if the message is a tombstone.
-define(MESSAGING_KAFKA_MESSAGE_TOMBSTONE, 'messaging.kafka.message.tombstone').


%% The size of the message body in bytes.
%%  
-define(MESSAGING_MESSAGE_BODY_SIZE, 'messaging.message.body.size').


%% The conversation ID identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID".
%%  
-define(MESSAGING_MESSAGE_CONVERSATIONID, 'messaging.message.conversation_id').


%% The size of the message body and metadata in bytes.
%%  
-define(MESSAGING_MESSAGE_ENVELOPE_SIZE, 'messaging.message.envelope.size').


%% A value used by the messaging system as an identifier for the message, represented as a string.
-define(MESSAGING_MESSAGE_ID, 'messaging.message.id').

%% @deprecated Replaced by `messaging.operation.type`.
%% Deprecated, use `messaging.operation.type` instead.
%%  
-define(MESSAGING_OPERATION, 'messaging.operation').


%% The system-specific name of the messaging operation.
%%  
-define(MESSAGING_OPERATION_NAME, 'messaging.operation.name').


%% A string identifying the type of the messaging operation.
%%  

-define('messaging_operation_type.publish', 'publish').

-define('messaging_operation_type.create', 'create').

-define('messaging_operation_type.receive', 'receive').

-define('messaging_operation_type.deliver', 'process').

-define('messaging_operation_type.settle', 'settle').

-define(messaging_operation_type(Custom), Custom).


%% RabbitMQ message routing key.
%%  
-define(MESSAGING_RABBITMQ_DESTINATION_ROUTINGKEY, 'messaging.rabbitmq.destination.routing_key').


%% RabbitMQ message delivery tag
%%  
-define(MESSAGING_RABBITMQ_MESSAGE_DELIVERYTAG, 'messaging.rabbitmq.message.delivery_tag').


%% Name of the RocketMQ producer/consumer group that is handling the message. The client type is identified by the SpanKind.
%%  
-define(MESSAGING_ROCKETMQ_CLIENTGROUP, 'messaging.rocketmq.client_group').


%% Model of message consumption. This only applies to consumer spans.
%%  

-define('messaging_rocketmq_consumptionmodel.clustering', 'clustering').

-define('messaging_rocketmq_consumptionmodel.broadcasting', 'broadcasting').

-define(messaging_rocketmq_consumptionmodel(Custom), Custom).


%% The delay time level for delay message, which determines the message delay time.
%%  
-define(MESSAGING_ROCKETMQ_MESSAGE_DELAYTIMELEVEL, 'messaging.rocketmq.message.delay_time_level').


%% The timestamp in milliseconds that the delay message is expected to be delivered to consumer.
%%  
-define(MESSAGING_ROCKETMQ_MESSAGE_DELIVERYTIMESTAMP, 'messaging.rocketmq.message.delivery_timestamp').


%% It is essential for FIFO message. Messages that belong to the same message group are always processed one by one within the same consumer group.
%%  
-define(MESSAGING_ROCKETMQ_MESSAGE_GROUP, 'messaging.rocketmq.message.group').


%% Key(s) of message, another way to mark message besides message id.
%%  
-define(MESSAGING_ROCKETMQ_MESSAGE_KEYS, 'messaging.rocketmq.message.keys').


%% The secondary classifier of message besides topic.
%%  
-define(MESSAGING_ROCKETMQ_MESSAGE_TAG, 'messaging.rocketmq.message.tag').


%% Type of message.
%%  

-define('messaging_rocketmq_message_type.normal', 'normal').

-define('messaging_rocketmq_message_type.fifo', 'fifo').

-define('messaging_rocketmq_message_type.delay', 'delay').

-define('messaging_rocketmq_message_type.transaction', 'transaction').

-define(messaging_rocketmq_message_type(Custom), Custom).


%% Namespace of RocketMQ resources, resources in different namespaces are individual.
%%  
-define(MESSAGING_ROCKETMQ_NAMESPACE, 'messaging.rocketmq.namespace').


%% The name of the subscription in the topic messages are received from.
%%  
-define(MESSAGING_SERVICEBUS_DESTINATION_SUBSCRIPTIONNAME, 'messaging.servicebus.destination.subscription_name').


%% Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock).
%%  

-define('messaging_servicebus_dispositionstatus.complete', 'complete').

-define('messaging_servicebus_dispositionstatus.abandon', 'abandon').

-define('messaging_servicebus_dispositionstatus.dead_letter', 'dead_letter').

-define('messaging_servicebus_dispositionstatus.defer', 'defer').

-define(messaging_servicebus_dispositionstatus(Custom), Custom).


%% Number of deliveries that have been attempted for this message.
%%  
-define(MESSAGING_SERVICEBUS_MESSAGE_DELIVERYCOUNT, 'messaging.servicebus.message.delivery_count').


%% The UTC epoch seconds at which the message has been accepted and stored in the entity.
%%  
-define(MESSAGING_SERVICEBUS_MESSAGE_ENQUEUEDTIME, 'messaging.servicebus.message.enqueued_time').


%% The messaging system as identified by the client instrumentation.

-define('messaging_system.activemq', 'activemq').

-define('messaging_system.aws_sqs', 'aws_sqs').

-define('messaging_system.eventgrid', 'eventgrid').

-define('messaging_system.eventhubs', 'eventhubs').

-define('messaging_system.servicebus', 'servicebus').

-define('messaging_system.gcp_pubsub', 'gcp_pubsub').

-define('messaging_system.jms', 'jms').

-define('messaging_system.kafka', 'kafka').

-define('messaging_system.rabbitmq', 'rabbitmq').

-define('messaging_system.rocketmq', 'rocketmq').

-define(messaging_system(Custom), Custom).
