defmodule OpenTelemetry.SemanticConventions.Trace do
  @moduledoc """
  WARNING: This module is deprecated and will be removed in a future release.
  Migrate to >= v1.27.0 semantic conventions.
  """

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec trace_schema_url :: String.t()
  def trace_schema_url do
    "https://opentelemetry.io/schemas/1.13.0"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_lambda_invoked_arn :: :"aws.lambda.invoked_arn"
  def aws_lambda_invoked_arn do
    :"aws.lambda.invoked_arn"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec cloudevents_event_id :: :"cloudevents.event_id"
  def cloudevents_event_id do
    :"cloudevents.event_id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec cloudevents_event_source :: :"cloudevents.event_source"
  def cloudevents_event_source do
    :"cloudevents.event_source"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec cloudevents_event_spec_version :: :"cloudevents.event_spec_version"
  def cloudevents_event_spec_version do
    :"cloudevents.event_spec_version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec cloudevents_event_type :: :"cloudevents.event_type"
  def cloudevents_event_type do
    :"cloudevents.event_type"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec cloudevents_event_subject :: :"cloudevents.event_subject"
  def cloudevents_event_subject do
    :"cloudevents.event_subject"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec opentracing_ref_type :: :"opentracing.ref_type"
  def opentracing_ref_type do
    :"opentracing.ref_type"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_system :: :"db.system"
  def db_system do
    :"db.system"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_connection_string :: :"db.connection_string"
  def db_connection_string do
    :"db.connection_string"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_user :: :"db.user"
  def db_user do
    :"db.user"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_jdbc_driver_classname :: :"db.jdbc.driver_classname"
  def db_jdbc_driver_classname do
    :"db.jdbc.driver_classname"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_name :: :"db.name"
  def db_name do
    :"db.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_statement :: :"db.statement"
  def db_statement do
    :"db.statement"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_operation :: :"db.operation"
  def db_operation do
    :"db.operation"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_peer_name :: :"net.peer.name"
  def net_peer_name do
    :"net.peer.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_peer_port :: :"net.peer.port"
  def net_peer_port do
    :"net.peer.port"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_sock_peer_addr :: :"net.sock.peer.addr"
  def net_sock_peer_addr do
    :"net.sock.peer.addr"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_sock_peer_port :: :"net.sock.peer.port"
  def net_sock_peer_port do
    :"net.sock.peer.port"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_sock_family :: :"net.sock.family"
  def net_sock_family do
    :"net.sock.family"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_sock_peer_name :: :"net.sock.peer.name"
  def net_sock_peer_name do
    :"net.sock.peer.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_transport :: :"net.transport"
  def net_transport do
    :"net.transport"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_mssql_instance_name :: :"db.mssql.instance_name"
  def db_mssql_instance_name do
    :"db.mssql.instance_name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_cassandra_page_size :: :"db.cassandra.page_size"
  def db_cassandra_page_size do
    :"db.cassandra.page_size"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_cassandra_consistency_level :: :"db.cassandra.consistency_level"
  def db_cassandra_consistency_level do
    :"db.cassandra.consistency_level"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_cassandra_table :: :"db.cassandra.table"
  def db_cassandra_table do
    :"db.cassandra.table"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_cassandra_idempotence :: :"db.cassandra.idempotence"
  def db_cassandra_idempotence do
    :"db.cassandra.idempotence"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_cassandra_speculative_execution_count :: :"db.cassandra.speculative_execution_count"
  def db_cassandra_speculative_execution_count do
    :"db.cassandra.speculative_execution_count"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_cassandra_coordinator_id :: :"db.cassandra.coordinator.id"
  def db_cassandra_coordinator_id do
    :"db.cassandra.coordinator.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_cassandra_coordinator_dc :: :"db.cassandra.coordinator.dc"
  def db_cassandra_coordinator_dc do
    :"db.cassandra.coordinator.dc"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_redis_database_index :: :"db.redis.database_index"
  def db_redis_database_index do
    :"db.redis.database_index"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_mongodb_collection :: :"db.mongodb.collection"
  def db_mongodb_collection do
    :"db.mongodb.collection"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec db_sql_table :: :"db.sql.table"
  def db_sql_table do
    :"db.sql.table"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec exception_type :: :"exception.type"
  def exception_type do
    :"exception.type"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec exception_message :: :"exception.message"
  def exception_message do
    :"exception.message"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec exception_stacktrace :: :"exception.stacktrace"
  def exception_stacktrace do
    :"exception.stacktrace"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec exception_escaped :: :"exception.escaped"
  def exception_escaped do
    :"exception.escaped"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_trigger :: :"faas.trigger"
  def faas_trigger do
    :"faas.trigger"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_execution :: :"faas.execution"
  def faas_execution do
    :"faas.execution"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_document_collection :: :"faas.document.collection"
  def faas_document_collection do
    :"faas.document.collection"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_document_operation :: :"faas.document.operation"
  def faas_document_operation do
    :"faas.document.operation"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_document_time :: :"faas.document.time"
  def faas_document_time do
    :"faas.document.time"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_document_name :: :"faas.document.name"
  def faas_document_name do
    :"faas.document.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_method :: :"http.method"
  def http_method do
    :"http.method"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_status_code :: :"http.status_code"
  def http_status_code do
    :"http.status_code"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_flavor :: :"http.flavor"
  def http_flavor do
    :"http.flavor"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_user_agent :: :"http.user_agent"
  def http_user_agent do
    :"http.user_agent"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_request_content_length :: :"http.request_content_length"
  def http_request_content_length do
    :"http.request_content_length"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_response_content_length :: :"http.response_content_length"
  def http_response_content_length do
    :"http.response_content_length"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_scheme :: :"http.scheme"
  def http_scheme do
    :"http.scheme"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_target :: :"http.target"
  def http_target do
    :"http.target"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_route :: :"http.route"
  def http_route do
    :"http.route"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_client_ip :: :"http.client_ip"
  def http_client_ip do
    :"http.client_ip"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_host_name :: :"net.host.name"
  def net_host_name do
    :"net.host.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_host_port :: :"net.host.port"
  def net_host_port do
    :"net.host.port"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_sock_host_addr :: :"net.sock.host.addr"
  def net_sock_host_addr do
    :"net.sock.host.addr"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_sock_host_port :: :"net.sock.host.port"
  def net_sock_host_port do
    :"net.sock.host.port"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_app_protocol_name :: :"net.app.protocol.name"
  def net_app_protocol_name do
    :"net.app.protocol.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_app_protocol_version :: :"net.app.protocol.version"
  def net_app_protocol_version do
    :"net.app.protocol.version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_host_connection_type :: :"net.host.connection.type"
  def net_host_connection_type do
    :"net.host.connection.type"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_host_connection_subtype :: :"net.host.connection.subtype"
  def net_host_connection_subtype do
    :"net.host.connection.subtype"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_host_carrier_name :: :"net.host.carrier.name"
  def net_host_carrier_name do
    :"net.host.carrier.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_host_carrier_mcc :: :"net.host.carrier.mcc"
  def net_host_carrier_mcc do
    :"net.host.carrier.mcc"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_host_carrier_mnc :: :"net.host.carrier.mnc"
  def net_host_carrier_mnc do
    :"net.host.carrier.mnc"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec net_host_carrier_icc :: :"net.host.carrier.icc"
  def net_host_carrier_icc do
    :"net.host.carrier.icc"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_system :: :"messaging.system"
  def messaging_system do
    :"messaging.system"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_destination :: :"messaging.destination"
  def messaging_destination do
    :"messaging.destination"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_destination_kind :: :"messaging.destination_kind"
  def messaging_destination_kind do
    :"messaging.destination_kind"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_temp_destination :: :"messaging.temp_destination"
  def messaging_temp_destination do
    :"messaging.temp_destination"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_protocol :: :"messaging.protocol"
  def messaging_protocol do
    :"messaging.protocol"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_protocol_version :: :"messaging.protocol_version"
  def messaging_protocol_version do
    :"messaging.protocol_version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_url :: :"messaging.url"
  def messaging_url do
    :"messaging.url"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_message_id :: :"messaging.message_id"
  def messaging_message_id do
    :"messaging.message_id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_conversation_id :: :"messaging.conversation_id"
  def messaging_conversation_id do
    :"messaging.conversation_id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_message_payload_size_bytes :: :"messaging.message_payload_size_bytes"
  def messaging_message_payload_size_bytes do
    :"messaging.message_payload_size_bytes"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_message_payload_compressed_size_bytes ::
          :"messaging.message_payload_compressed_size_bytes"
  def messaging_message_payload_compressed_size_bytes do
    :"messaging.message_payload_compressed_size_bytes"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_time :: :"faas.time"
  def faas_time do
    :"faas.time"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_cron :: :"faas.cron"
  def faas_cron do
    :"faas.cron"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_coldstart :: :"faas.coldstart"
  def faas_coldstart do
    :"faas.coldstart"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_invoked_name :: :"faas.invoked_name"
  def faas_invoked_name do
    :"faas.invoked_name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_invoked_provider :: :"faas.invoked_provider"
  def faas_invoked_provider do
    :"faas.invoked_provider"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_invoked_region :: :"faas.invoked_region"
  def faas_invoked_region do
    :"faas.invoked_region"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec peer_service :: :"peer.service"
  def peer_service do
    :"peer.service"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec enduser_id :: :"enduser.id"
  def enduser_id do
    :"enduser.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec enduser_role :: :"enduser.role"
  def enduser_role do
    :"enduser.role"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec enduser_scope :: :"enduser.scope"
  def enduser_scope do
    :"enduser.scope"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec thread_id :: :"thread.id"
  def thread_id do
    :"thread.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec thread_name :: :"thread.name"
  def thread_name do
    :"thread.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec code_function :: :"code.function"
  def code_function do
    :"code.function"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec code_namespace :: :"code.namespace"
  def code_namespace do
    :"code.namespace"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec code_filepath :: :"code.filepath"
  def code_filepath do
    :"code.filepath"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec code_lineno :: :"code.lineno"
  def code_lineno do
    :"code.lineno"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_url :: :"http.url"
  def http_url do
    :"http.url"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec http_retry_count :: :"http.retry_count"
  def http_retry_count do
    :"http.retry_count"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec rpc_system :: :"rpc.system"
  def rpc_system do
    :"rpc.system"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec rpc_service :: :"rpc.service"
  def rpc_service do
    :"rpc.service"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec rpc_method :: :"rpc.method"
  def rpc_method do
    :"rpc.method"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_table_names :: :"aws.dynamodb.table_names"
  def aws_dynamodb_table_names do
    :"aws.dynamodb.table_names"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_consumed_capacity :: :"aws.dynamodb.consumed_capacity"
  def aws_dynamodb_consumed_capacity do
    :"aws.dynamodb.consumed_capacity"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_item_collection_metrics :: :"aws.dynamodb.item_collection_metrics"
  def aws_dynamodb_item_collection_metrics do
    :"aws.dynamodb.item_collection_metrics"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_provisioned_read_capacity :: :"aws.dynamodb.provisioned_read_capacity"
  def aws_dynamodb_provisioned_read_capacity do
    :"aws.dynamodb.provisioned_read_capacity"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_provisioned_write_capacity :: :"aws.dynamodb.provisioned_write_capacity"
  def aws_dynamodb_provisioned_write_capacity do
    :"aws.dynamodb.provisioned_write_capacity"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_consistent_read :: :"aws.dynamodb.consistent_read"
  def aws_dynamodb_consistent_read do
    :"aws.dynamodb.consistent_read"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_projection :: :"aws.dynamodb.projection"
  def aws_dynamodb_projection do
    :"aws.dynamodb.projection"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_limit :: :"aws.dynamodb.limit"
  def aws_dynamodb_limit do
    :"aws.dynamodb.limit"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_attributes_to_get :: :"aws.dynamodb.attributes_to_get"
  def aws_dynamodb_attributes_to_get do
    :"aws.dynamodb.attributes_to_get"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_index_name :: :"aws.dynamodb.index_name"
  def aws_dynamodb_index_name do
    :"aws.dynamodb.index_name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_select :: :"aws.dynamodb.select"
  def aws_dynamodb_select do
    :"aws.dynamodb.select"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_global_secondary_indexes :: :"aws.dynamodb.global_secondary_indexes"
  def aws_dynamodb_global_secondary_indexes do
    :"aws.dynamodb.global_secondary_indexes"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_local_secondary_indexes :: :"aws.dynamodb.local_secondary_indexes"
  def aws_dynamodb_local_secondary_indexes do
    :"aws.dynamodb.local_secondary_indexes"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_exclusive_start_table :: :"aws.dynamodb.exclusive_start_table"
  def aws_dynamodb_exclusive_start_table do
    :"aws.dynamodb.exclusive_start_table"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_table_count :: :"aws.dynamodb.table_count"
  def aws_dynamodb_table_count do
    :"aws.dynamodb.table_count"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_scan_forward :: :"aws.dynamodb.scan_forward"
  def aws_dynamodb_scan_forward do
    :"aws.dynamodb.scan_forward"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_segment :: :"aws.dynamodb.segment"
  def aws_dynamodb_segment do
    :"aws.dynamodb.segment"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_total_segments :: :"aws.dynamodb.total_segments"
  def aws_dynamodb_total_segments do
    :"aws.dynamodb.total_segments"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_count :: :"aws.dynamodb.count"
  def aws_dynamodb_count do
    :"aws.dynamodb.count"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_scanned_count :: :"aws.dynamodb.scanned_count"
  def aws_dynamodb_scanned_count do
    :"aws.dynamodb.scanned_count"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_attribute_definitions :: :"aws.dynamodb.attribute_definitions"
  def aws_dynamodb_attribute_definitions do
    :"aws.dynamodb.attribute_definitions"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_dynamodb_global_secondary_index_updates ::
          :"aws.dynamodb.global_secondary_index_updates"
  def aws_dynamodb_global_secondary_index_updates do
    :"aws.dynamodb.global_secondary_index_updates"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec graphql_operation_name :: :"graphql.operation.name"
  def graphql_operation_name do
    :"graphql.operation.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec graphql_operation_type :: :"graphql.operation.type"
  def graphql_operation_type do
    :"graphql.operation.type"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec graphql_document :: :"graphql.document"
  def graphql_document do
    :"graphql.document"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_operation :: :"messaging.operation"
  def messaging_operation do
    :"messaging.operation"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_consumer_id :: :"messaging.consumer_id"
  def messaging_consumer_id do
    :"messaging.consumer_id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_rabbitmq_routing_key :: :"messaging.rabbitmq.routing_key"
  def messaging_rabbitmq_routing_key do
    :"messaging.rabbitmq.routing_key"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_kafka_message_key :: :"messaging.kafka.message_key"
  def messaging_kafka_message_key do
    :"messaging.kafka.message_key"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_kafka_consumer_group :: :"messaging.kafka.consumer_group"
  def messaging_kafka_consumer_group do
    :"messaging.kafka.consumer_group"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_kafka_client_id :: :"messaging.kafka.client_id"
  def messaging_kafka_client_id do
    :"messaging.kafka.client_id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_kafka_partition :: :"messaging.kafka.partition"
  def messaging_kafka_partition do
    :"messaging.kafka.partition"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_kafka_tombstone :: :"messaging.kafka.tombstone"
  def messaging_kafka_tombstone do
    :"messaging.kafka.tombstone"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_rocketmq_namespace :: :"messaging.rocketmq.namespace"
  def messaging_rocketmq_namespace do
    :"messaging.rocketmq.namespace"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_rocketmq_client_group :: :"messaging.rocketmq.client_group"
  def messaging_rocketmq_client_group do
    :"messaging.rocketmq.client_group"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_rocketmq_client_id :: :"messaging.rocketmq.client_id"
  def messaging_rocketmq_client_id do
    :"messaging.rocketmq.client_id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_rocketmq_message_type :: :"messaging.rocketmq.message_type"
  def messaging_rocketmq_message_type do
    :"messaging.rocketmq.message_type"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_rocketmq_message_tag :: :"messaging.rocketmq.message_tag"
  def messaging_rocketmq_message_tag do
    :"messaging.rocketmq.message_tag"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_rocketmq_message_keys :: :"messaging.rocketmq.message_keys"
  def messaging_rocketmq_message_keys do
    :"messaging.rocketmq.message_keys"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec messaging_rocketmq_consumption_model :: :"messaging.rocketmq.consumption_model"
  def messaging_rocketmq_consumption_model do
    :"messaging.rocketmq.consumption_model"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec rpc_grpc_status_code :: :"rpc.grpc.status_code"
  def rpc_grpc_status_code do
    :"rpc.grpc.status_code"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec rpc_jsonrpc_version :: :"rpc.jsonrpc.version"
  def rpc_jsonrpc_version do
    :"rpc.jsonrpc.version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec rpc_jsonrpc_request_id :: :"rpc.jsonrpc.request_id"
  def rpc_jsonrpc_request_id do
    :"rpc.jsonrpc.request_id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec rpc_jsonrpc_error_code :: :"rpc.jsonrpc.error_code"
  def rpc_jsonrpc_error_code do
    :"rpc.jsonrpc.error_code"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec rpc_jsonrpc_error_message :: :"rpc.jsonrpc.error_message"
  def rpc_jsonrpc_error_message do
    :"rpc.jsonrpc.error_message"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec message_type :: :"message.type"
  def message_type do
    :"message.type"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec message_id :: :"message.id"
  def message_id do
    :"message.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec message_compressed_size :: :"message.compressed_size"
  def message_compressed_size do
    :"message.compressed_size"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec message_uncompressed_size :: :"message.uncompressed_size"
  def message_uncompressed_size do
    :"message.uncompressed_size"
  end
end
