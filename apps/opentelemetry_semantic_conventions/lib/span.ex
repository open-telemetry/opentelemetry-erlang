defmodule OpenTelemetry.SemanticConventions.Span do
  @moduledoc """
  OpenTelemetry Semantic Conventions for Attributes.
  """

  @doc namespace: :opentracing
  @typedoc """
  Parent-child Reference type

  ### Options


  * `:child_of`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - The parent Span depends on the child Span in some capacity

  * `:follows_from`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - The parent Span doesn't depend in any way on the result of the child Span



  """
  @type opentracing_ref_type() :: :child_of | :follows_from

  @doc namespace: :db
  @typedoc """
  An identifier for the database management system (DBMS) product being used. See below for a list of well-known identifiers

  ### Options


  * `:other_sql`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Some other SQL database. Fallback only. See notes

  * `:mssql`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Microsoft SQL Server

  * `:mssqlcompact`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Microsoft SQL Server Compact

  * `:mysql`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - MySQL

  * `:oracle`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Oracle Database

  * `:db2`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IBM Db2

  * `:postgresql`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - PostgreSQL

  * `:redshift`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Amazon Redshift

  * `:hive`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Hive

  * `:cloudscape`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Cloudscape

  * `:hsqldb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HyperSQL DataBase

  * `:progress`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Progress Database

  * `:maxdb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - SAP MaxDB

  * `:hanadb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - SAP HANA

  * `:ingres`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Ingres

  * `:firstsql`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - FirstSQL

  * `:edb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EnterpriseDB

  * `:cache`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - InterSystems Caché

  * `:adabas`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Adabas (Adaptable Database System)

  * `:firebird`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Firebird

  * `:derby`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Derby

  * `:filemaker`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - FileMaker

  * `:informix`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Informix

  * `:instantdb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - InstantDB

  * `:interbase`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - InterBase

  * `:mariadb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - MariaDB

  * `:netezza`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Netezza

  * `:pervasive`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Pervasive PSQL

  * `:pointbase`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - PointBase

  * `:sqlite`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - SQLite

  * `:sybase`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Sybase

  * `:teradata`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Teradata

  * `:vertica`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Vertica

  * `:h2`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - H2

  * `:coldfusion`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ColdFusion IMQ

  * `:cassandra`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Cassandra

  * `:hbase`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache HBase

  * `:mongodb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - MongoDB

  * `:redis`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Redis

  * `:couchbase`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Couchbase

  * `:couchdb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - CouchDB

  * `:cosmosdb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Microsoft Azure Cosmos DB

  * `:dynamodb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Amazon DynamoDB

  * `:neo4j`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Neo4j

  * `:geode`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Geode

  * `:elasticsearch`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Elasticsearch

  * `:memcached`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Memcached

  * `:cockroachdb`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - CockroachDB

  * `:opensearch`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - OpenSearch

  * `:clickhouse`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ClickHouse

  * `:spanner`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Cloud Spanner

  * `:trino`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Trino



  """
  @type db_system() ::
          :other_sql
          | :mssql
          | :mssqlcompact
          | :mysql
          | :oracle
          | :db2
          | :postgresql
          | :redshift
          | :hive
          | :cloudscape
          | :hsqldb
          | :progress
          | :maxdb
          | :hanadb
          | :ingres
          | :firstsql
          | :edb
          | :cache
          | :adabas
          | :firebird
          | :derby
          | :filemaker
          | :informix
          | :instantdb
          | :interbase
          | :mariadb
          | :netezza
          | :pervasive
          | :pointbase
          | :sqlite
          | :sybase
          | :teradata
          | :vertica
          | :h2
          | :coldfusion
          | :cassandra
          | :hbase
          | :mongodb
          | :redis
          | :couchbase
          | :couchdb
          | :cosmosdb
          | :dynamodb
          | :neo4j
          | :geode
          | :elasticsearch
          | :memcached
          | :cockroachdb
          | :opensearch
          | :clickhouse
          | :spanner
          | :trino
          | atom()

  @doc namespace: :db
  @typedoc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html)

  ### Options


  * `:all`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - all

  * `:each_quorum`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - each_quorum

  * `:quorum`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - quorum

  * `:local_quorum`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - local_quorum

  * `:one`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - one

  * `:two`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - two

  * `:three`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - three

  * `:local_one`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - local_one

  * `:any`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - any

  * `:serial`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - serial

  * `:local_serial`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - local_serial



  """
  @type db_cassandra_consistency_level() ::
          :all
          | :each_quorum
          | :quorum
          | :local_quorum
          | :one
          | :two
          | :three
          | :local_one
          | :any
          | :serial
          | :local_serial

  @doc namespace: :http
  @typedoc """
  HTTP request method

  ### Options


  * `:CONNECT` - CONNECT method

  * `:DELETE` - DELETE method

  * `:GET` - GET method

  * `:HEAD` - HEAD method

  * `:OPTIONS` - OPTIONS method

  * `:PATCH` - PATCH method

  * `:POST` - POST method

  * `:PUT` - PUT method

  * `:TRACE` - TRACE method

  * `:_OTHER` - Any HTTP method that the instrumentation has no prior knowledge of



  """
  @type http_request_method() ::
          :CONNECT
          | :DELETE
          | :GET
          | :HEAD
          | :OPTIONS
          | :PATCH
          | :POST
          | :PUT
          | :TRACE
          | :_OTHER
          | atom()

  @doc namespace: :db
  @typedoc """
  Cosmos client connection mode

  ### Options


  * `:gateway`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Gateway (HTTP) connections mode

  * `:direct`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Direct connection



  """
  @type db_cosmosdb_connection_mode() :: :gateway | :direct

  @doc namespace: :db
  @typedoc """
  CosmosDB Operation Type

  ### Options


  * `:Invalid`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - invalid

  * `:Create`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - create

  * `:Patch`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - patch

  * `:Read`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - read

  * `:ReadFeed`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - read_feed

  * `:Delete`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - delete

  * `:Replace`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - replace

  * `:Execute`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - execute

  * `:Query`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - query

  * `:Head`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - head

  * `:HeadFeed`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - head_feed

  * `:Upsert`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - upsert

  * `:Batch`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - batch

  * `:QueryPlan`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - query_plan

  * `:ExecuteJavaScript`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - execute_javascript



  """
  @type db_cosmosdb_operation_type() ::
          :Invalid
          | :Create
          | :Patch
          | :Read
          | :ReadFeed
          | :Delete
          | :Replace
          | :Execute
          | :Query
          | :Head
          | :HeadFeed
          | :Upsert
          | :Batch
          | :QueryPlan
          | :ExecuteJavaScript
          | atom()

  @doc namespace: :otel
  @typedoc """
  Name of the code, either "OK" or "ERROR". MUST NOT be set if the status code is UNSET

  ### Options


  * `:OK`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - The operation has been validated by an Application developer or Operator to have completed successfully

  * `:ERROR`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - The operation contains an error



  """
  @type otel_status_code() :: :OK | :ERROR

  @doc namespace: :faas
  @typedoc """
  Type of the trigger which caused this function invocation

  ### Options


  * `:datasource`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - A response to some data source operation such as a database or filesystem read/write

  * `:http`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - To provide an answer to an inbound HTTP request

  * `:pubsub`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - A function is set to be executed when messages are sent to a messaging system

  * `:timer`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - A function is scheduled to be executed regularly

  * `:other`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - If none of the others apply



  """
  @type faas_trigger() :: :datasource | :http | :pubsub | :timer | :other

  @doc namespace: :faas
  @typedoc """
  Describes the type of the operation that was performed on the data

  ### Options


  * `:insert`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When a new object is created

  * `:edit`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When an object is modified

  * `:delete`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When an object is deleted



  """
  @type faas_document_operation() :: :insert | :edit | :delete | atom()

  @doc namespace: :faas
  @typedoc """
  The cloud provider of the invoked function

  ### Options


  * `:alibaba_cloud`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Alibaba Cloud

  * `:aws`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Amazon Web Services

  * `:azure`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Microsoft Azure

  * `:gcp`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Google Cloud Platform

  * `:tencent_cloud`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Tencent Cloud



  """
  @type faas_invoked_provider() :: :alibaba_cloud | :aws | :azure | :gcp | :tencent_cloud | atom()

  @doc namespace: :error
  @typedoc """
  Describes a class of error the operation ended with

  ### Options


  * `:_OTHER` - A fallback error value to be used when the instrumentation doesn't define a custom value



  """
  @type error_type() :: :_OTHER | atom()

  @doc namespace: :rpc
  @typedoc """
  The value `aws-api`

  ### Options


  * `:grpc`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - gRPC

  * `:java_rmi`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Java RMI

  * `:dotnet_wcf`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - .NET WCF

  * `:apache_dubbo`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Dubbo

  * `:connect_rpc`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Connect RPC



  """
  @type rpc_system() :: :grpc | :java_rmi | :dotnet_wcf | :apache_dubbo | :connect_rpc | atom()

  @doc namespace: :graphql
  @typedoc """
  The type of the operation being executed

  ### Options


  * `:query`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - GraphQL query

  * `:mutation`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - GraphQL mutation

  * `:subscription`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - GraphQL subscription



  """
  @type graphql_operation_type() :: :query | :mutation | :subscription

  @doc namespace: :messaging
  @typedoc """
  A string identifying the kind of messaging operation

  ### Options


  * `:publish`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - One or more messages are provided for publishing to an intermediary. If a single message is published, the context of the "Publish" span can be used as the creation context and no "Create" span needs to be created

  * `:create`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - A message is created. "Create" spans always refer to a single message and are used to provide a unique creation context for messages in batch publishing scenarios

  * `:receive`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - One or more messages are requested by a consumer. This operation refers to pull-based scenarios, where consumers explicitly call methods of messaging SDKs to receive messages

  * `:process`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - One or more messages are delivered to or processed by a consumer

  * `:settle`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - One or more messages are settled



  """
  @type messaging_operation() :: :publish | :create | :receive | :process | :settle | atom()

  @doc namespace: :messaging
  @typedoc """
  An identifier for the messaging system being used. See below for a list of well-known identifiers

  ### Options


  * `:activemq`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache ActiveMQ

  * `:aws_sqs`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Amazon Simple Queue Service (SQS)

  * `:eventgrid`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Azure Event Grid

  * `:eventhubs`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Azure Event Hubs

  * `:servicebus`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Azure Service Bus

  * `:gcp_pubsub`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Google Cloud Pub/Sub

  * `:jms`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Java Message Service

  * `:kafka`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Kafka

  * `:rabbitmq`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - RabbitMQ

  * `:rocketmq`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache RocketMQ



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

  @doc namespace: :network
  @typedoc """
  [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication)

  ### Options


  * `:tcp` - TCP

  * `:udp` - UDP

  * `:pipe` - Named or anonymous pipe

  * `:unix` - Unix domain socket



  """
  @type network_transport() :: :tcp | :udp | :pipe | :unix | atom()

  @doc namespace: :network
  @typedoc """
  [OSI network layer](https://osi-model.com/network-layer/) or non-OSI equivalent

  ### Options


  * `:ipv4` - IPv4

  * `:ipv6` - IPv6



  """
  @type network_type() :: :ipv4 | :ipv6 | atom()

  @doc namespace: :rpc
  @typedoc """
  The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request

  ### Options


  * `0`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - OK

  * `1`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - CANCELLED

  * `2`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - UNKNOWN

  * `3`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - INVALID_ARGUMENT

  * `4`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - DEADLINE_EXCEEDED

  * `5`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - NOT_FOUND

  * `6`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ALREADY_EXISTS

  * `7`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - PERMISSION_DENIED

  * `8`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - RESOURCE_EXHAUSTED

  * `9`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - FAILED_PRECONDITION

  * `10`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ABORTED

  * `11`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - OUT_OF_RANGE

  * `12`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - UNIMPLEMENTED

  * `13`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - INTERNAL

  * `14`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - UNAVAILABLE

  * `15`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - DATA_LOSS

  * `16`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - UNAUTHENTICATED



  """
  @type rpc_grpc_status_code() ::
          0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16

  @doc namespace: :rpc
  @typedoc """
  The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values

  ### Options


  * `:cancelled`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - cancelled

  * `:unknown`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - unknown

  * `:invalid_argument`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - invalid_argument

  * `:deadline_exceeded`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - deadline_exceeded

  * `:not_found`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - not_found

  * `:already_exists`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - already_exists

  * `:permission_denied`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - permission_denied

  * `:resource_exhausted`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - resource_exhausted

  * `:failed_precondition`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - failed_precondition

  * `:aborted`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - aborted

  * `:out_of_range`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - out_of_range

  * `:unimplemented`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - unimplemented

  * `:internal`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - internal

  * `:unavailable`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - unavailable

  * `:data_loss`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - data_loss

  * `:unauthenticated`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - unauthenticated



  """
  @type rpc_connect_rpc_error_code() ::
          :cancelled
          | :unknown
          | :invalid_argument
          | :deadline_exceeded
          | :not_found
          | :already_exists
          | :permission_denied
          | :resource_exhausted
          | :failed_precondition
          | :aborted
          | :out_of_range
          | :unimplemented
          | :internal
          | :unavailable
          | :data_loss
          | :unauthenticated

  @doc """
  The URL of the OpenTelemetry schema for these keys and values.

      iex> OpenTelemetry.SemanticConventions.Span.schema_url()
      "https://opentelemetry.io/schemas/1.25.0"
  """
  @spec schema_url :: String.t()
  def schema_url do
    "https://opentelemetry.io/schemas/1.25.0"
  end

  @doc namespace: :opentracing

  @doc """


  Parent-child Reference type

  ### Notes

  The causal relationship between a child Span and a parent Span

      iex> OpenTelemetry.SemanticConventions.Span.opentracing_ref_type()
      :"opentracing.ref_type"
  """

  @spec opentracing_ref_type :: :"opentracing.ref_type"
  def opentracing_ref_type do
    :"opentracing.ref_type"
  end

  @doc namespace: :db

  @doc """


  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html)

      iex> OpenTelemetry.SemanticConventions.Span.db_cassandra_consistency_level()
      :"db.cassandra.consistency_level"
  """

  @spec db_cassandra_consistency_level :: :"db.cassandra.consistency_level"
  def db_cassandra_consistency_level do
    :"db.cassandra.consistency_level"
  end

  @doc namespace: :http

  @doc """


  HTTP request method

  ### Notes

  HTTP request method value SHOULD be "known" to the instrumentation.
  By default, this convention defines "known" methods as the ones listed in [RFC9110](https://www.rfc-editor.org/rfc/rfc9110.html#name-methods)
  and the PATCH method defined in [RFC5789](https://www.rfc-editor.org/rfc/rfc5789.html).

  If the HTTP request method is not known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`.

  If the HTTP instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way to override
  the list of known HTTP methods. If this override is done via environment variable, then the environment variable MUST be named
  OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated list of case-sensitive known HTTP methods
  (this list MUST be a full override of the default known method, it is not a list of known methods in addition to the defaults).

  HTTP method names are case-sensitive and `http.request.method` attribute value MUST match a known HTTP method name exactly.
  Instrumentations for specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical equivalent.
  Tracing instrumentations that do so, MUST also set `http.request.method_original` to the original value

      iex> OpenTelemetry.SemanticConventions.Span.http_request_method()
      :"http.request.method"
  """

  @spec http_request_method :: :"http.request.method"
  def http_request_method do
    :"http.request.method"
  end

  @doc namespace: :db

  @doc """


  Cosmos client connection mode

      iex> OpenTelemetry.SemanticConventions.Span.db_cosmosdb_connection_mode()
      :"db.cosmosdb.connection_mode"
  """

  @spec db_cosmosdb_connection_mode :: :"db.cosmosdb.connection_mode"
  def db_cosmosdb_connection_mode do
    :"db.cosmosdb.connection_mode"
  end

  @doc namespace: :db

  @doc """


  CosmosDB Operation Type

      iex> OpenTelemetry.SemanticConventions.Span.db_cosmosdb_operation_type()
      :"db.cosmosdb.operation_type"
  """

  @spec db_cosmosdb_operation_type :: :"db.cosmosdb.operation_type"
  def db_cosmosdb_operation_type do
    :"db.cosmosdb.operation_type"
  end

  @doc namespace: :otel

  @doc """


  Name of the code, either "OK" or "ERROR". MUST NOT be set if the status code is UNSET

      iex> OpenTelemetry.SemanticConventions.Span.otel_status_code()
      :"otel.status_code"
  """

  @spec otel_status_code :: :"otel.status_code"
  def otel_status_code do
    :"otel.status_code"
  end

  @doc namespace: :faas

  @doc """


  Type of the trigger which caused this function invocation

  ### Notes

  For the server/consumer span on the incoming side,
  `faas.trigger` MUST be set.

  Clients invoking FaaS instances usually cannot set `faas.trigger`,
  since they would typically need to look in the payload to determine
  the event type. If clients set it, it should be the same as the
  trigger that corresponding incoming would have (i.e., this has
  nothing to do with the underlying transport used to make the API
  call to invoke the lambda, which is often HTTP)

      iex> OpenTelemetry.SemanticConventions.Span.faas_trigger()
      :"faas.trigger"
  """

  @spec faas_trigger :: :"faas.trigger"
  def faas_trigger do
    :"faas.trigger"
  end

  @doc namespace: :faas

  @doc """


  Describes the type of the operation that was performed on the data

      iex> OpenTelemetry.SemanticConventions.Span.faas_document_operation()
      :"faas.document.operation"
  """

  @spec faas_document_operation :: :"faas.document.operation"
  def faas_document_operation do
    :"faas.document.operation"
  end

  @doc namespace: :faas

  @doc """


  The cloud provider of the invoked function

  ### Notes

  SHOULD be equal to the `cloud.provider` resource attribute of the invoked function

      iex> OpenTelemetry.SemanticConventions.Span.faas_invoked_provider()
      :"faas.invoked_provider"
  """

  @spec faas_invoked_provider :: :"faas.invoked_provider"
  def faas_invoked_provider do
    :"faas.invoked_provider"
  end

  @doc namespace: :rpc

  @doc """


  The value `aws-api`

      iex> OpenTelemetry.SemanticConventions.Span.rpc_system()
      :"rpc.system"
  """

  @spec rpc_system :: :"rpc.system"
  def rpc_system do
    :"rpc.system"
  end

  @doc namespace: :graphql

  @doc """


  The type of the operation being executed

      iex> OpenTelemetry.SemanticConventions.Span.graphql_operation_type()
      :"graphql.operation.type"
  """

  @spec graphql_operation_type :: :"graphql.operation.type"
  def graphql_operation_type do
    :"graphql.operation.type"
  end

  @doc namespace: :messaging

  @doc """


  A string identifying the kind of messaging operation

  ### Notes

  If a custom value is used, it MUST be of low cardinality

      iex> OpenTelemetry.SemanticConventions.Span.messaging_operation()
      :"messaging.operation"
  """

  @spec messaging_operation :: :"messaging.operation"
  def messaging_operation do
    :"messaging.operation"
  end

  @doc namespace: :network

  @doc """


  [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication)

  ### Notes

  The value SHOULD be normalized to lowercase.

  Consider always setting the transport when setting a port number, since
  a port number is ambiguous without knowing the transport. For example
  different processes could be listening on TCP port 12345 and UDP port 12345

      iex> OpenTelemetry.SemanticConventions.Span.network_transport()
      :"network.transport"
  """

  @spec network_transport :: :"network.transport"
  def network_transport do
    :"network.transport"
  end

  @doc namespace: :network

  @doc """


  [OSI network layer](https://osi-model.com/network-layer/) or non-OSI equivalent

  ### Notes

  The value SHOULD be normalized to lowercase

      iex> OpenTelemetry.SemanticConventions.Span.network_type()
      :"network.type"
  """

  @spec network_type :: :"network.type"
  def network_type do
    :"network.type"
  end

  @doc namespace: :rpc

  @doc """


  The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request

      iex> OpenTelemetry.SemanticConventions.Span.rpc_grpc_status_code()
      :"rpc.grpc.status_code"
  """

  @spec rpc_grpc_status_code :: :"rpc.grpc.status_code"
  def rpc_grpc_status_code do
    :"rpc.grpc.status_code"
  end

  @doc namespace: :rpc

  @doc """


  The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values

      iex> OpenTelemetry.SemanticConventions.Span.rpc_connect_rpc_error_code()
      :"rpc.connect_rpc.error_code"
  """

  @spec rpc_connect_rpc_error_code :: :"rpc.connect_rpc.error_code"
  def rpc_connect_rpc_error_code do
    :"rpc.connect_rpc.error_code"
  end
end
