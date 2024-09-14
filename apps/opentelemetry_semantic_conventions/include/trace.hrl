%% WARNING: These macros are deprecated and will be removed in a future release.
%% Migrate to >= v1.27.0 semantic conventions

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(TRACE_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.13.0">>).

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_LAMBDA_INVOKED_ARN, 'aws.lambda.invoked_arn').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CLOUDEVENTS_EVENT_ID, 'cloudevents.event_id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CLOUDEVENTS_EVENT_SOURCE, 'cloudevents.event_source').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CLOUDEVENTS_EVENT_SPEC_VERSION, 'cloudevents.event_spec_version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CLOUDEVENTS_EVENT_TYPE, 'cloudevents.event_type').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CLOUDEVENTS_EVENT_SUBJECT, 'cloudevents.event_subject').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(OPENTRACING_REF_TYPE, 'opentracing.ref_type').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_SYSTEM, 'db.system').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_CONNECTION_STRING, 'db.connection_string').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_USER, 'db.user').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_JDBC_DRIVER_CLASSNAME, 'db.jdbc.driver_classname').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_NAME, 'db.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_STATEMENT, 'db.statement').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_OPERATION, 'db.operation').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_PEER_NAME, 'net.peer.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_PEER_PORT, 'net.peer.port').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_SOCK_PEER_ADDR, 'net.sock.peer.addr').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_SOCK_PEER_PORT, 'net.sock.peer.port').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_SOCK_FAMILY, 'net.sock.family').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_SOCK_PEER_NAME, 'net.sock.peer.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_TRANSPORT, 'net.transport').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_MSSQL_INSTANCE_NAME, 'db.mssql.instance_name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_CASSANDRA_PAGE_SIZE, 'db.cassandra.page_size').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_CASSANDRA_CONSISTENCY_LEVEL, 'db.cassandra.consistency_level').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_CASSANDRA_TABLE, 'db.cassandra.table').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_CASSANDRA_IDEMPOTENCE, 'db.cassandra.idempotence').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_CASSANDRA_SPECULATIVE_EXECUTION_COUNT, 'db.cassandra.speculative_execution_count').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_CASSANDRA_COORDINATOR_ID, 'db.cassandra.coordinator.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_CASSANDRA_COORDINATOR_DC, 'db.cassandra.coordinator.dc').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_REDIS_DATABASE_INDEX, 'db.redis.database_index').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_MONGODB_COLLECTION, 'db.mongodb.collection').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DB_SQL_TABLE, 'db.sql.table').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(EXCEPTION_TYPE, 'exception.type').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(EXCEPTION_MESSAGE, 'exception.message').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(EXCEPTION_STACKTRACE, 'exception.stacktrace').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(EXCEPTION_ESCAPED, 'exception.escaped').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_TRIGGER, 'faas.trigger').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_EXECUTION, 'faas.execution').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_DOCUMENT_COLLECTION, 'faas.document.collection').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_DOCUMENT_OPERATION, 'faas.document.operation').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_DOCUMENT_TIME, 'faas.document.time').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_DOCUMENT_NAME, 'faas.document.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_METHOD, 'http.method').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_STATUS_CODE, 'http.status_code').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_FLAVOR, 'http.flavor').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_USER_AGENT, 'http.user_agent').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_REQUEST_CONTENT_LENGTH, 'http.request_content_length').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_RESPONSE_CONTENT_LENGTH, 'http.response_content_length').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_SCHEME, 'http.scheme').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_TARGET, 'http.target').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_ROUTE, 'http.route').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_CLIENT_IP, 'http.client_ip').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_HOST_NAME, 'net.host.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_HOST_PORT, 'net.host.port').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_SOCK_HOST_ADDR, 'net.sock.host.addr').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_SOCK_HOST_PORT, 'net.sock.host.port').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_APP_PROTOCOL_NAME, 'net.app.protocol.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_APP_PROTOCOL_VERSION, 'net.app.protocol.version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_HOST_CONNECTION_TYPE, 'net.host.connection.type').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_HOST_CONNECTION_SUBTYPE, 'net.host.connection.subtype').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_HOST_CARRIER_NAME, 'net.host.carrier.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_HOST_CARRIER_MCC, 'net.host.carrier.mcc').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_HOST_CARRIER_MNC, 'net.host.carrier.mnc').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(NET_HOST_CARRIER_ICC, 'net.host.carrier.icc').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_SYSTEM, 'messaging.system').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_DESTINATION, 'messaging.destination').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_DESTINATION_KIND, 'messaging.destination_kind').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_TEMP_DESTINATION, 'messaging.temp_destination').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_PROTOCOL, 'messaging.protocol').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_PROTOCOL_VERSION, 'messaging.protocol_version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_URL, 'messaging.url').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_MESSAGE_ID, 'messaging.message_id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_CONVERSATION_ID, 'messaging.conversation_id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_MESSAGE_PAYLOAD_SIZE_BYTES, 'messaging.message_payload_size_bytes').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_MESSAGE_PAYLOAD_COMPRESSED_SIZE_BYTES, 'messaging.message_payload_compressed_size_bytes').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_TIME, 'faas.time').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_CRON, 'faas.cron').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_COLDSTART, 'faas.coldstart').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_INVOKED_NAME, 'faas.invoked_name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_INVOKED_PROVIDER, 'faas.invoked_provider').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_INVOKED_REGION, 'faas.invoked_region').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PEER_SERVICE, 'peer.service').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(ENDUSER_ID, 'enduser.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(ENDUSER_ROLE, 'enduser.role').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(ENDUSER_SCOPE, 'enduser.scope').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(THREAD_ID, 'thread.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(THREAD_NAME, 'thread.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CODE_FUNCTION, 'code.function').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CODE_NAMESPACE, 'code.namespace').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CODE_FILEPATH, 'code.filepath').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CODE_LINENO, 'code.lineno').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_URL, 'http.url').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HTTP_RETRY_COUNT, 'http.retry_count').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(RPC_SYSTEM, 'rpc.system').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(RPC_SERVICE, 'rpc.service').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(RPC_METHOD, 'rpc.method').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_TABLE_NAMES, 'aws.dynamodb.table_names').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_CONSUMED_CAPACITY, 'aws.dynamodb.consumed_capacity').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_ITEM_COLLECTION_METRICS, 'aws.dynamodb.item_collection_metrics').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_PROVISIONED_READ_CAPACITY, 'aws.dynamodb.provisioned_read_capacity').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_PROVISIONED_WRITE_CAPACITY, 'aws.dynamodb.provisioned_write_capacity').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_CONSISTENT_READ, 'aws.dynamodb.consistent_read').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_PROJECTION, 'aws.dynamodb.projection').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_LIMIT, 'aws.dynamodb.limit').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_ATTRIBUTES_TO_GET, 'aws.dynamodb.attributes_to_get').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_INDEX_NAME, 'aws.dynamodb.index_name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_SELECT, 'aws.dynamodb.select').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_GLOBAL_SECONDARY_INDEXES, 'aws.dynamodb.global_secondary_indexes').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_LOCAL_SECONDARY_INDEXES, 'aws.dynamodb.local_secondary_indexes').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_EXCLUSIVE_START_TABLE, 'aws.dynamodb.exclusive_start_table').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_TABLE_COUNT, 'aws.dynamodb.table_count').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_SCAN_FORWARD, 'aws.dynamodb.scan_forward').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_SEGMENT, 'aws.dynamodb.segment').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_TOTAL_SEGMENTS, 'aws.dynamodb.total_segments').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_COUNT, 'aws.dynamodb.count').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_SCANNED_COUNT, 'aws.dynamodb.scanned_count').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_ATTRIBUTE_DEFINITIONS, 'aws.dynamodb.attribute_definitions').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_DYNAMODB_GLOBAL_SECONDARY_INDEX_UPDATES, 'aws.dynamodb.global_secondary_index_updates').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(GRAPHQL_OPERATION_NAME, 'graphql.operation.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(GRAPHQL_OPERATION_TYPE, 'graphql.operation.type').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(GRAPHQL_DOCUMENT, 'graphql.document').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_OPERATION, 'messaging.operation').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_CONSUMER_ID, 'messaging.consumer_id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_RABBITMQ_ROUTING_KEY, 'messaging.rabbitmq.routing_key').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_KAFKA_MESSAGE_KEY, 'messaging.kafka.message_key').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_KAFKA_CONSUMER_GROUP, 'messaging.kafka.consumer_group').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_KAFKA_CLIENT_ID, 'messaging.kafka.client_id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_KAFKA_PARTITION, 'messaging.kafka.partition').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_KAFKA_TOMBSTONE, 'messaging.kafka.tombstone').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_ROCKETMQ_NAMESPACE, 'messaging.rocketmq.namespace').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_ROCKETMQ_CLIENT_GROUP, 'messaging.rocketmq.client_group').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_ROCKETMQ_CLIENT_ID, 'messaging.rocketmq.client_id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_ROCKETMQ_MESSAGE_TYPE, 'messaging.rocketmq.message_type').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_ROCKETMQ_MESSAGE_TAG, 'messaging.rocketmq.message_tag').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_ROCKETMQ_MESSAGE_KEYS, 'messaging.rocketmq.message_keys').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGING_ROCKETMQ_CONSUMPTION_MODEL, 'messaging.rocketmq.consumption_model').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(RPC_GRPC_STATUS_CODE, 'rpc.grpc.status_code').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(RPC_JSONRPC_VERSION, 'rpc.jsonrpc.version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(RPC_JSONRPC_REQUEST_ID, 'rpc.jsonrpc.request_id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(RPC_JSONRPC_ERROR_CODE, 'rpc.jsonrpc.error_code').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(RPC_JSONRPC_ERROR_MESSAGE, 'rpc.jsonrpc.error_message').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGE_TYPE, 'message.type').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGE_ID, 'message.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGE_COMPRESSED_SIZE, 'message.compressed_size').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(MESSAGE_UNCOMPRESSED_SIZE, 'message.uncompressed_size').
