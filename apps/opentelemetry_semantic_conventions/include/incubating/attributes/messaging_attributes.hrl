
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/messaging_attributes.hrl").


%% The number of messages sent, received, or processed in the scope of the batching operation.
-define(MESSAGING_BATCH_MESSAGE_COUNT, 'messaging.batch.message_count').


%% A unique identifier for the client that consumes or produces a message.
%%  
-define(MESSAGING_CLIENT_ID, 'messaging.client.id').


%% The name of the consumer group with which a consumer is associated.
%%  
-define(MESSAGING_CONSUMER_GROUP_NAME, 'messaging.consumer.group.name').


%% A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name).
-define(MESSAGING_DESTINATION_ANONYMOUS, 'messaging.destination.anonymous').


%% The message destination name
-define(MESSAGING_DESTINATION_NAME, 'messaging.destination.name').


%% The identifier of the partition messages are sent to or received from, unique within the `messaging.destination.name`.
%%  
-define(MESSAGING_DESTINATION_PARTITION_ID, 'messaging.destination.partition.id').


%% The name of the destination subscription from which a message is consumed.
-define(MESSAGING_DESTINATION_SUBSCRIPTION_NAME, 'messaging.destination.subscription.name').


%% Low cardinality representation of the messaging destination name
-define(MESSAGING_DESTINATION_TEMPLATE, 'messaging.destination.template').


%% A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed.
-define(MESSAGING_DESTINATION_TEMPORARY, 'messaging.destination.temporary').

%% @deprecated No replacement at this time.
%% Deprecated, no replacement at this time.
-define(MESSAGING_DESTINATION_PUBLISH_ANONYMOUS, 'messaging.destination_publish.anonymous').

%% @deprecated No replacement at this time.
%% Deprecated, no replacement at this time.
-define(MESSAGING_DESTINATION_PUBLISH_NAME, 'messaging.destination_publish.name').

%% @deprecated Replaced by `messaging.consumer.group.name`.
%%  
%% Deprecated, use `messaging.consumer.group.name` instead.
%%  
-define(MESSAGING_EVENTHUBS_CONSUMER_GROUP, 'messaging.eventhubs.consumer.group').


%% The UTC epoch seconds at which the message has been accepted and stored in the entity.
%%  
-define(MESSAGING_EVENTHUBS_MESSAGE_ENQUEUED_TIME, 'messaging.eventhubs.message.enqueued_time').


%% The ack deadline in seconds set for the modify ack deadline request.
%%  
-define(MESSAGING_GCP_PUBSUB_MESSAGE_ACK_DEADLINE, 'messaging.gcp_pubsub.message.ack_deadline').


%% The ack id for a given message.
%%  
-define(MESSAGING_GCP_PUBSUB_MESSAGE_ACK_ID, 'messaging.gcp_pubsub.message.ack_id').


%% The delivery attempt for a given message.
%%  
-define(MESSAGING_GCP_PUBSUB_MESSAGE_DELIVERY_ATTEMPT, 'messaging.gcp_pubsub.message.delivery_attempt').


%% The ordering key for a given message. If the attribute is not present, the message does not have an ordering key.
%%  
-define(MESSAGING_GCP_PUBSUB_MESSAGE_ORDERING_KEY, 'messaging.gcp_pubsub.message.ordering_key').

%% @deprecated Replaced by `messaging.consumer.group.name`.
%%  
%% Deprecated, use `messaging.consumer.group.name` instead.
%%  
-define(MESSAGING_KAFKA_CONSUMER_GROUP, 'messaging.kafka.consumer.group').

%% @deprecated Replaced by `messaging.destination.partition.id`.
%% Deprecated, use `messaging.destination.partition.id` instead.
%%  
-define(MESSAGING_KAFKA_DESTINATION_PARTITION, 'messaging.kafka.destination.partition').


%% Message keys in Kafka are used for grouping alike messages to ensure they're processed on the same partition. They differ from `messaging.message.id` in that they're not unique. If the key is `null`, the attribute MUST NOT be set.
%%  
-define(MESSAGING_KAFKA_MESSAGE_KEY, 'messaging.kafka.message.key').

%% @deprecated Replaced by `messaging.kafka.offset`.
%%  
%% Deprecated, use `messaging.kafka.offset` instead.
%%  
-define(MESSAGING_KAFKA_MESSAGE_OFFSET, 'messaging.kafka.message.offset').


%% A boolean that is true if the message is a tombstone.
-define(MESSAGING_KAFKA_MESSAGE_TOMBSTONE, 'messaging.kafka.message.tombstone').


%% The offset of a record in the corresponding Kafka partition.
%%  
-define(MESSAGING_KAFKA_OFFSET, 'messaging.kafka.offset').


%% The size of the message body in bytes.
%%  
-define(MESSAGING_MESSAGE_BODY_SIZE, 'messaging.message.body.size').


%% The conversation ID identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID".
%%  
-define(MESSAGING_MESSAGE_CONVERSATION_ID, 'messaging.message.conversation_id').


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
-define(MESSAGING_OPERATION_TYPE, 'messaging.operation.type').

-define(MESSAGING_OPERATION_TYPE_VALUES_PUBLISH, 'publish').

-define(MESSAGING_OPERATION_TYPE_VALUES_CREATE, 'create').

-define(MESSAGING_OPERATION_TYPE_VALUES_RECEIVE, 'receive').

-define(MESSAGING_OPERATION_TYPE_VALUES_PROCESS, 'process').

-define(MESSAGING_OPERATION_TYPE_VALUES_SETTLE, 'settle').

-define(MESSAGING_OPERATION_TYPE_VALUES_DELIVER, 'deliver').



%% RabbitMQ message routing key.
%%  
-define(MESSAGING_RABBITMQ_DESTINATION_ROUTING_KEY, 'messaging.rabbitmq.destination.routing_key').


%% RabbitMQ message delivery tag
%%  
-define(MESSAGING_RABBITMQ_MESSAGE_DELIVERY_TAG, 'messaging.rabbitmq.message.delivery_tag').

%% @deprecated Replaced by `messaging.consumer.group.name` on the consumer spans. No replacement for producer spans.
%%  
%% Deprecated, use `messaging.consumer.group.name` instead.
%%  
-define(MESSAGING_ROCKETMQ_CLIENT_GROUP, 'messaging.rocketmq.client_group').


%% Model of message consumption. This only applies to consumer spans.
%%  
-define(MESSAGING_ROCKETMQ_CONSUMPTION_MODEL, 'messaging.rocketmq.consumption_model').

-define(MESSAGING_ROCKETMQ_CONSUMPTION_MODEL_VALUES_CLUSTERING, 'clustering').

-define(MESSAGING_ROCKETMQ_CONSUMPTION_MODEL_VALUES_BROADCASTING, 'broadcasting').



%% The delay time level for delay message, which determines the message delay time.
%%  
-define(MESSAGING_ROCKETMQ_MESSAGE_DELAY_TIME_LEVEL, 'messaging.rocketmq.message.delay_time_level').


%% The timestamp in milliseconds that the delay message is expected to be delivered to consumer.
%%  
-define(MESSAGING_ROCKETMQ_MESSAGE_DELIVERY_TIMESTAMP, 'messaging.rocketmq.message.delivery_timestamp').


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
-define(MESSAGING_ROCKETMQ_MESSAGE_TYPE, 'messaging.rocketmq.message.type').

-define(MESSAGING_ROCKETMQ_MESSAGE_TYPE_VALUES_NORMAL, 'normal').

-define(MESSAGING_ROCKETMQ_MESSAGE_TYPE_VALUES_FIFO, 'fifo').

-define(MESSAGING_ROCKETMQ_MESSAGE_TYPE_VALUES_DELAY, 'delay').

-define(MESSAGING_ROCKETMQ_MESSAGE_TYPE_VALUES_TRANSACTION, 'transaction').



%% Namespace of RocketMQ resources, resources in different namespaces are individual.
%%  
-define(MESSAGING_ROCKETMQ_NAMESPACE, 'messaging.rocketmq.namespace').

%% @deprecated Replaced by `messaging.servicebus.destination.subscription_name`.
%%  
%% Deprecated, use `messaging.servicebus.destination.subscription_name` instead.
%%  
-define(MESSAGING_SERVICEBUS_DESTINATION_SUBSCRIPTION_NAME, 'messaging.servicebus.destination.subscription_name').


%% Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock).
%%  
-define(MESSAGING_SERVICEBUS_DISPOSITION_STATUS, 'messaging.servicebus.disposition_status').

-define(MESSAGING_SERVICEBUS_DISPOSITION_STATUS_VALUES_COMPLETE, 'complete').

-define(MESSAGING_SERVICEBUS_DISPOSITION_STATUS_VALUES_ABANDON, 'abandon').

-define(MESSAGING_SERVICEBUS_DISPOSITION_STATUS_VALUES_DEAD_LETTER, 'dead_letter').

-define(MESSAGING_SERVICEBUS_DISPOSITION_STATUS_VALUES_DEFER, 'defer').



%% Number of deliveries that have been attempted for this message.
%%  
-define(MESSAGING_SERVICEBUS_MESSAGE_DELIVERY_COUNT, 'messaging.servicebus.message.delivery_count').


%% The UTC epoch seconds at which the message has been accepted and stored in the entity.
%%  
-define(MESSAGING_SERVICEBUS_MESSAGE_ENQUEUED_TIME, 'messaging.servicebus.message.enqueued_time').


%% The messaging system as identified by the client instrumentation.
-define(MESSAGING_SYSTEM, 'messaging.system').

-define(MESSAGING_SYSTEM_VALUES_ACTIVEMQ, 'activemq').

-define(MESSAGING_SYSTEM_VALUES_AWS_SQS, 'aws_sqs').

-define(MESSAGING_SYSTEM_VALUES_EVENTGRID, 'eventgrid').

-define(MESSAGING_SYSTEM_VALUES_EVENTHUBS, 'eventhubs').

-define(MESSAGING_SYSTEM_VALUES_SERVICEBUS, 'servicebus').

-define(MESSAGING_SYSTEM_VALUES_GCP_PUBSUB, 'gcp_pubsub').

-define(MESSAGING_SYSTEM_VALUES_JMS, 'jms').

-define(MESSAGING_SYSTEM_VALUES_KAFKA, 'kafka').

-define(MESSAGING_SYSTEM_VALUES_RABBITMQ, 'rabbitmq').

-define(MESSAGING_SYSTEM_VALUES_ROCKETMQ, 'rocketmq').

-define(MESSAGING_SYSTEM_VALUES_PULSAR, 'pulsar').

