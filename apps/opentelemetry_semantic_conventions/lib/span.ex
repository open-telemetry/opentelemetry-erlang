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
  Name of the code, either "OK" or "ERROR". **MUST NOT** be set if the status code is UNSET

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

  @doc namespace: :peer
  @doc """
  The [`service.name`](/docs/resource/README.md#service) of the remote service. **SHOULD** be equal to the actual `service.name` resource attribute of the remote service if any

      iex> OpenTelemetry.SemanticConventions.Span.peer_service()
      :"peer.service"
  """
  @spec peer_service :: :"peer.service"
  def peer_service do
    :"peer.service"
  end

  @doc namespace: :enduser
  @doc """
  Username or client_id extracted from the access token or [Authorization](https://tools.ietf.org/html/rfc7235#section-4.2) header in the inbound request from outside the system

      iex> OpenTelemetry.SemanticConventions.Span.enduser_id()
      :"enduser.id"
  """
  @spec enduser_id :: :"enduser.id"
  def enduser_id do
    :"enduser.id"
  end

  @doc namespace: :enduser
  @doc """
  Actual/assumed role the client is making the request under extracted from token or application security context

      iex> OpenTelemetry.SemanticConventions.Span.enduser_role()
      :"enduser.role"
  """
  @spec enduser_role :: :"enduser.role"
  def enduser_role do
    :"enduser.role"
  end

  @doc namespace: :enduser
  @doc """
  Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an [OAuth 2.0 Access Token](https://tools.ietf.org/html/rfc6749#section-3.3) or an attribute value in a [SAML 2.0 Assertion](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html)

      iex> OpenTelemetry.SemanticConventions.Span.enduser_scope()
      :"enduser.scope"
  """
  @spec enduser_scope :: :"enduser.scope"
  def enduser_scope do
    :"enduser.scope"
  end

  @doc namespace: :thread
  @doc """
  Current "managed" thread ID (as opposed to OS thread ID)

      iex> OpenTelemetry.SemanticConventions.Span.thread_id()
      :"thread.id"
  """
  @spec thread_id :: :"thread.id"
  def thread_id do
    :"thread.id"
  end

  @doc namespace: :thread
  @doc """
  Current thread name

      iex> OpenTelemetry.SemanticConventions.Span.thread_name()
      :"thread.name"
  """
  @spec thread_name :: :"thread.name"
  def thread_name do
    :"thread.name"
  end

  @doc namespace: :code
  @doc """
  The column number in `code.filepath` best representing the operation. It **SHOULD** point within the code unit named in `code.function`

      iex> OpenTelemetry.SemanticConventions.Span.code_column()
      :"code.column"
  """
  @spec code_column :: :"code.column"
  def code_column do
    :"code.column"
  end

  @doc namespace: :code
  @doc """
  The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path)

      iex> OpenTelemetry.SemanticConventions.Span.code_filepath()
      :"code.filepath"
  """
  @spec code_filepath :: :"code.filepath"
  def code_filepath do
    :"code.filepath"
  end

  @doc namespace: :code
  @doc """
  The method or function name, or equivalent (usually rightmost part of the code unit's name)

      iex> OpenTelemetry.SemanticConventions.Span.code_function()
      :"code.function"
  """
  @spec code_function :: :"code.function"
  def code_function do
    :"code.function"
  end

  @doc namespace: :code
  @doc """
  The line number in `code.filepath` best representing the operation. It **SHOULD** point within the code unit named in `code.function`

      iex> OpenTelemetry.SemanticConventions.Span.code_lineno()
      :"code.lineno"
  """
  @spec code_lineno :: :"code.lineno"
  def code_lineno do
    :"code.lineno"
  end

  @doc namespace: :code
  @doc """
  The "namespace" within which `code.function` is defined. Usually the qualified class or module name, such that `code.namespace` + some separator + `code.function` form a unique identifier for the code unit

      iex> OpenTelemetry.SemanticConventions.Span.code_namespace()
      :"code.namespace"
  """
  @spec code_namespace :: :"code.namespace"
  def code_namespace do
    :"code.namespace"
  end

  @doc namespace: :code
  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG

      iex> OpenTelemetry.SemanticConventions.Span.code_stacktrace()
      :"code.stacktrace"
  """
  @spec code_stacktrace :: :"code.stacktrace"
  def code_stacktrace do
    :"code.stacktrace"
  end

  @doc namespace: :aws
  @doc """
  The full invoked ARN as provided on the `Context` passed to the function (`Lambda-Runtime-Invoked-Function-Arn` header on the `/runtime/invocation/next` applicable)

  ### Notes

  This may be different from `cloud.resource_id` if an alias is involved

      iex> OpenTelemetry.SemanticConventions.Span.aws_lambda_invoked_arn()
      :"aws.lambda.invoked_arn"
  """
  @spec aws_lambda_invoked_arn :: :"aws.lambda.invoked_arn"
  def aws_lambda_invoked_arn do
    :"aws.lambda.invoked_arn"
  end

  @doc namespace: :cloudevents
  @doc """
  The [event_id](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#id) uniquely identifies the event

      iex> OpenTelemetry.SemanticConventions.Span.cloudevents_event_id()
      :"cloudevents.event_id"
  """
  @spec cloudevents_event_id :: :"cloudevents.event_id"
  def cloudevents_event_id do
    :"cloudevents.event_id"
  end

  @doc namespace: :cloudevents
  @doc """
  The [source](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#source-1) identifies the context in which an event happened

      iex> OpenTelemetry.SemanticConventions.Span.cloudevents_event_source()
      :"cloudevents.event_source"
  """
  @spec cloudevents_event_source :: :"cloudevents.event_source"
  def cloudevents_event_source do
    :"cloudevents.event_source"
  end

  @doc namespace: :cloudevents
  @doc """
  The [version of the CloudEvents specification](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#specversion) which the event uses

      iex> OpenTelemetry.SemanticConventions.Span.cloudevents_event_spec_version()
      :"cloudevents.event_spec_version"
  """
  @spec cloudevents_event_spec_version :: :"cloudevents.event_spec_version"
  def cloudevents_event_spec_version do
    :"cloudevents.event_spec_version"
  end

  @doc namespace: :cloudevents
  @doc """
  The [subject](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#subject) of the event in the context of the event producer (identified by source)

      iex> OpenTelemetry.SemanticConventions.Span.cloudevents_event_subject()
      :"cloudevents.event_subject"
  """
  @spec cloudevents_event_subject :: :"cloudevents.event_subject"
  def cloudevents_event_subject do
    :"cloudevents.event_subject"
  end

  @doc namespace: :cloudevents
  @doc """
  The [event_type](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#type) contains a value describing the type of event related to the originating occurrence

      iex> OpenTelemetry.SemanticConventions.Span.cloudevents_event_type()
      :"cloudevents.event_type"
  """
  @spec cloudevents_event_type :: :"cloudevents.event_type"
  def cloudevents_event_type do
    :"cloudevents.event_type"
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
  The Microsoft SQL Server [instance name](https://docs.microsoft.com/sql/connect/jdbc/building-the-connection-url?view=sql-server-ver15) connecting to. This name is used to determine the port of a named instance

  ### Notes

  If setting a `db.mssql.instance_name`, `server.port` is no longer required (but still recommended if non-standard)

      iex> OpenTelemetry.SemanticConventions.Span.db_mssql_instance_name()
      :"db.mssql.instance_name"
  """
  @spec db_mssql_instance_name :: :"db.mssql.instance_name"
  def db_mssql_instance_name do
    :"db.mssql.instance_name"
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

  @doc namespace: :db
  @doc """
  The data center of the coordinating node for a query

      iex> OpenTelemetry.SemanticConventions.Span.db_cassandra_coordinator_dc()
      :"db.cassandra.coordinator.dc"
  """
  @spec db_cassandra_coordinator_dc :: :"db.cassandra.coordinator.dc"
  def db_cassandra_coordinator_dc do
    :"db.cassandra.coordinator.dc"
  end

  @doc namespace: :db
  @doc """
  The ID of the coordinating node for a query

      iex> OpenTelemetry.SemanticConventions.Span.db_cassandra_coordinator_id()
      :"db.cassandra.coordinator.id"
  """
  @spec db_cassandra_coordinator_id :: :"db.cassandra.coordinator.id"
  def db_cassandra_coordinator_id do
    :"db.cassandra.coordinator.id"
  end

  @doc namespace: :db
  @doc """
  Whether or not the query is idempotent

      iex> OpenTelemetry.SemanticConventions.Span.db_cassandra_idempotence()
      :"db.cassandra.idempotence"
  """
  @spec db_cassandra_idempotence :: :"db.cassandra.idempotence"
  def db_cassandra_idempotence do
    :"db.cassandra.idempotence"
  end

  @doc namespace: :db
  @doc """
  The fetch size used for paging, i.e. how many rows will be returned at once

      iex> OpenTelemetry.SemanticConventions.Span.db_cassandra_page_size()
      :"db.cassandra.page_size"
  """
  @spec db_cassandra_page_size :: :"db.cassandra.page_size"
  def db_cassandra_page_size do
    :"db.cassandra.page_size"
  end

  @doc namespace: :db
  @doc """
  The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively

      iex> OpenTelemetry.SemanticConventions.Span.db_cassandra_speculative_execution_count()
      :"db.cassandra.speculative_execution_count"
  """
  @spec db_cassandra_speculative_execution_count :: :"db.cassandra.speculative_execution_count"
  def db_cassandra_speculative_execution_count do
    :"db.cassandra.speculative_execution_count"
  end

  @doc namespace: :db
  @doc """
  The name of the primary Cassandra table that the operation is acting upon, including the keyspace name (if applicable)

  ### Notes

  This mirrors the db.sql.table attribute but references cassandra rather than sql. It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value **MUST NOT** be set

      iex> OpenTelemetry.SemanticConventions.Span.db_cassandra_table()
      :"db.cassandra.table"
  """
  @spec db_cassandra_table :: :"db.cassandra.table"
  def db_cassandra_table do
    :"db.cassandra.table"
  end

  @doc namespace: :db
  @doc """
  The index of the database being accessed as used in the [`SELECT` command](https://redis.io/commands/select), provided as an integer. To be used instead of the generic `db.name` attribute

      iex> OpenTelemetry.SemanticConventions.Span.db_redis_database_index()
      :"db.redis.database_index"
  """
  @spec db_redis_database_index :: :"db.redis.database_index"
  def db_redis_database_index do
    :"db.redis.database_index"
  end

  @doc namespace: :db
  @doc """
  The MongoDB collection being accessed within the database stated in `db.name`

      iex> OpenTelemetry.SemanticConventions.Span.db_mongodb_collection()
      :"db.mongodb.collection"
  """
  @spec db_mongodb_collection :: :"db.mongodb.collection"
  def db_mongodb_collection do
    :"db.mongodb.collection"
  end

  @doc namespace: :http
  @doc """
  HTTP request method

  ### Notes

  HTTP request method value **SHOULD** be "known" to the instrumentation.
  By default, this convention defines "known" methods as the ones listed in [RFC9110](https://www.rfc-editor.org/rfc/rfc9110.html#name-methods)
  and the PATCH method defined in [RFC5789](https://www.rfc-editor.org/rfc/rfc5789.html).

  If the HTTP request method is not known to instrumentation, it **MUST** set the `http.request.method` attribute to `_OTHER`.

  If the HTTP instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it **MUST** provide a way to override
  the list of known HTTP methods. If this override is done via environment variable, then the environment variable **MUST** be named
  OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated list of case-sensitive known HTTP methods
  (this list **MUST** be a full override of the default known method, it is not a list of known methods in addition to the defaults).

  HTTP method names are case-sensitive and `http.request.method` attribute value **MUST** match a known HTTP method name exactly.
  Instrumentations for specific web frameworks that consider HTTP methods to be case insensitive, **SHOULD** populate a canonical equivalent.
  Tracing instrumentations that do so, **MUST** also set `http.request.method_original` to the original value

      iex> OpenTelemetry.SemanticConventions.Span.http_request_method()
      :"http.request.method"
  """
  @spec http_request_method :: :"http.request.method"
  def http_request_method do
    :"http.request.method"
  end

  @doc namespace: :url
  @doc """
  Absolute URL describing a network resource according to [RFC3986](https://www.rfc-editor.org/rfc/rfc3986)

  ### Notes

  For network calls, URL usually has `scheme://host[:port][path][?query][#fragment]` format, where the fragment is not transmitted over HTTP, but if it is known, it **SHOULD** be included nevertheless.
  `url.full` **MUST NOT** contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case username and password **SHOULD** be redacted and attribute's value **SHOULD** be `https://REDACTED:REDACTED@www.example.com/`.
  `url.full` **SHOULD** capture the absolute URL when it is available (or can be reconstructed). Sensitive content provided in `url.full` **SHOULD** be scrubbed when instrumentations can identify it

      iex> OpenTelemetry.SemanticConventions.Span.url_full()
      :"url.full"
  """
  @spec url_full :: :"url.full"
  def url_full do
    :"url.full"
  end

  @doc namespace: :db
  @doc """
  Represents the identifier of an Elasticsearch cluster

      iex> OpenTelemetry.SemanticConventions.Span.db_elasticsearch_cluster_name()
      :"db.elasticsearch.cluster.name"
  """
  @spec db_elasticsearch_cluster_name :: :"db.elasticsearch.cluster.name"
  def db_elasticsearch_cluster_name do
    :"db.elasticsearch.cluster.name"
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
  Cosmos DB container name

      iex> OpenTelemetry.SemanticConventions.Span.db_cosmosdb_container()
      :"db.cosmosdb.container"
  """
  @spec db_cosmosdb_container :: :"db.cosmosdb.container"
  def db_cosmosdb_container do
    :"db.cosmosdb.container"
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

  @doc namespace: :db
  @doc """
  RU consumed for that operation

      iex> OpenTelemetry.SemanticConventions.Span.db_cosmosdb_request_charge()
      :"db.cosmosdb.request_charge"
  """
  @spec db_cosmosdb_request_charge :: :"db.cosmosdb.request_charge"
  def db_cosmosdb_request_charge do
    :"db.cosmosdb.request_charge"
  end

  @doc namespace: :db
  @doc """
  Cosmos DB status code

      iex> OpenTelemetry.SemanticConventions.Span.db_cosmosdb_status_code()
      :"db.cosmosdb.status_code"
  """
  @spec db_cosmosdb_status_code :: :"db.cosmosdb.status_code"
  def db_cosmosdb_status_code do
    :"db.cosmosdb.status_code"
  end

  @doc namespace: :db
  @doc """
  Cosmos DB sub status code

      iex> OpenTelemetry.SemanticConventions.Span.db_cosmosdb_sub_status_code()
      :"db.cosmosdb.sub_status_code"
  """
  @spec db_cosmosdb_sub_status_code :: :"db.cosmosdb.sub_status_code"
  def db_cosmosdb_sub_status_code do
    :"db.cosmosdb.sub_status_code"
  end

  @doc namespace: :db
  @doc """
  Unique Cosmos client instance id

      iex> OpenTelemetry.SemanticConventions.Span.db_cosmosdb_client_id()
      :"db.cosmosdb.client_id"
  """
  @spec db_cosmosdb_client_id :: :"db.cosmosdb.client_id"
  def db_cosmosdb_client_id do
    :"db.cosmosdb.client_id"
  end

  @doc namespace: :db
  @doc """
  Request payload size in bytes

      iex> OpenTelemetry.SemanticConventions.Span.db_cosmosdb_request_content_length()
      :"db.cosmosdb.request_content_length"
  """
  @spec db_cosmosdb_request_content_length :: :"db.cosmosdb.request_content_length"
  def db_cosmosdb_request_content_length do
    :"db.cosmosdb.request_content_length"
  end

  @doc namespace: :user_agent
  @doc """
  Full user-agent string is generated by Cosmos DB SDK

  ### Notes

  The user-agent value is generated by SDK which is a combination of<br> `sdk_version` : Current version of SDK. e.g. 'cosmos-netstandard-sdk/3.23.0'<br> `direct_pkg_version` : Direct package version used by Cosmos DB SDK. e.g. '3.23.1'<br> `number_of_client_instances` : Number of cosmos client instances created by the application. e.g. '1'<br> `type_of_machine_architecture` : Machine architecture. e.g. 'X64'<br> `operating_system` : Operating System. e.g. 'Linux 5.4.0-1098-azure 104 18'<br> `runtime_framework` : Runtime Framework. e.g. '.NET Core 3.1.32'<br> `failover_information` : Generated key to determine if region failover enabled.
     Format Reg-{D (Disabled discovery)}-S(application region)|L(List of preferred regions)|N(None, user did not configure it).
     Default value is "NS"

      iex> OpenTelemetry.SemanticConventions.Span.user_agent_original()
      :"user_agent.original"
  """
  @spec user_agent_original :: :"user_agent.original"
  def user_agent_original do
    :"user_agent.original"
  end

  @doc namespace: :otel
  @doc """
  Name of the code, either "OK" or "ERROR". **MUST NOT** be set if the status code is UNSET

      iex> OpenTelemetry.SemanticConventions.Span.otel_status_code()
      :"otel.status_code"
  """
  @spec otel_status_code :: :"otel.status_code"
  def otel_status_code do
    :"otel.status_code"
  end

  @doc namespace: :otel
  @doc """
  Description of the Status if it has a value, otherwise not set

      iex> OpenTelemetry.SemanticConventions.Span.otel_status_description()
      :"otel.status_description"
  """
  @spec otel_status_description :: :"otel.status_description"
  def otel_status_description do
    :"otel.status_description"
  end

  @doc namespace: :faas
  @doc """
  The invocation ID of the current function invocation

      iex> OpenTelemetry.SemanticConventions.Span.faas_invocation_id()
      :"faas.invocation_id"
  """
  @spec faas_invocation_id :: :"faas.invocation_id"
  def faas_invocation_id do
    :"faas.invocation_id"
  end

  @doc namespace: :faas
  @doc """
  Type of the trigger which caused this function invocation

  ### Notes

  For the server/consumer span on the incoming side,
  `faas.trigger` **MUST** be set.

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
  The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name

      iex> OpenTelemetry.SemanticConventions.Span.faas_document_collection()
      :"faas.document.collection"
  """
  @spec faas_document_collection :: :"faas.document.collection"
  def faas_document_collection do
    :"faas.document.collection"
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
  The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name

      iex> OpenTelemetry.SemanticConventions.Span.faas_document_name()
      :"faas.document.name"
  """
  @spec faas_document_name :: :"faas.document.name"
  def faas_document_name do
    :"faas.document.name"
  end

  @doc namespace: :faas
  @doc """
  A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)

      iex> OpenTelemetry.SemanticConventions.Span.faas_document_time()
      :"faas.document.time"
  """
  @spec faas_document_time :: :"faas.document.time"
  def faas_document_time do
    :"faas.document.time"
  end

  @doc namespace: :faas
  @doc """
  A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm)

      iex> OpenTelemetry.SemanticConventions.Span.faas_cron()
      :"faas.cron"
  """
  @spec faas_cron :: :"faas.cron"
  def faas_cron do
    :"faas.cron"
  end

  @doc namespace: :faas
  @doc """
  A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)

      iex> OpenTelemetry.SemanticConventions.Span.faas_time()
      :"faas.time"
  """
  @spec faas_time :: :"faas.time"
  def faas_time do
    :"faas.time"
  end

  @doc namespace: :faas
  @doc """
  A boolean that is true if the serverless function is executed for the first time (aka cold-start)

      iex> OpenTelemetry.SemanticConventions.Span.faas_coldstart()
      :"faas.coldstart"
  """
  @spec faas_coldstart :: :"faas.coldstart"
  def faas_coldstart do
    :"faas.coldstart"
  end

  @doc namespace: :faas
  @doc """
  The name of the invoked function

  ### Notes

  **SHOULD** be equal to the `faas.name` resource attribute of the invoked function

      iex> OpenTelemetry.SemanticConventions.Span.faas_invoked_name()
      :"faas.invoked_name"
  """
  @spec faas_invoked_name :: :"faas.invoked_name"
  def faas_invoked_name do
    :"faas.invoked_name"
  end

  @doc namespace: :faas
  @doc """
  The cloud provider of the invoked function

  ### Notes

  **SHOULD** be equal to the `cloud.provider` resource attribute of the invoked function

      iex> OpenTelemetry.SemanticConventions.Span.faas_invoked_provider()
      :"faas.invoked_provider"
  """
  @spec faas_invoked_provider :: :"faas.invoked_provider"
  def faas_invoked_provider do
    :"faas.invoked_provider"
  end

  @doc namespace: :faas
  @doc """
  The cloud region of the invoked function

  ### Notes

  **SHOULD** be equal to the `cloud.region` resource attribute of the invoked function

      iex> OpenTelemetry.SemanticConventions.Span.faas_invoked_region()
      :"faas.invoked_region"
  """
  @spec faas_invoked_region :: :"faas.invoked_region"
  def faas_invoked_region do
    :"faas.invoked_region"
  end

  @doc namespace: :http
  @doc """
  The ordinal number of request resending attempt (for any reason, including redirects)

  ### Notes

  The resend count **SHOULD** be updated each time an HTTP request gets resent by the client, regardless of what was the cause of the resending (e.g. redirection, authorization failure, 503 Server Unavailable, network issues, or any other)

      iex> OpenTelemetry.SemanticConventions.Span.http_request_resend_count()
      :"http.request.resend_count"
  """
  @spec http_request_resend_count :: :"http.request.resend_count"
  def http_request_resend_count do
    :"http.request.resend_count"
  end

  @doc namespace: :url
  @doc """
  The [URI scheme](https://www.rfc-editor.org/rfc/rfc3986#section-3.1) component identifying the used protocol

      iex> OpenTelemetry.SemanticConventions.Span.url_scheme()
      :"url.scheme"
  """
  @spec url_scheme :: :"url.scheme"
  def url_scheme do
    :"url.scheme"
  end

  @doc namespace: :url
  @doc """
  The [URI path](https://www.rfc-editor.org/rfc/rfc3986#section-3.3) component

  ### Notes

  Sensitive content provided in `url.path` **SHOULD** be scrubbed when instrumentations can identify it

      iex> OpenTelemetry.SemanticConventions.Span.url_path()
      :"url.path"
  """
  @spec url_path :: :"url.path"
  def url_path do
    :"url.path"
  end

  @doc namespace: :http
  @doc """
  The matched route, that is, the path template in the format used by the respective server framework

  ### Notes

  **MUST NOT** be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can **NOT** substitute it.
  **SHOULD** include the [application root](/docs/http/http-spans.md#http-server-definitions) if there is one

      iex> OpenTelemetry.SemanticConventions.Span.http_route()
      :"http.route"
  """
  @spec http_route :: :"http.route"
  def http_route do
    :"http.route"
  end

  @doc namespace: :url
  @doc """
  The [URI query](https://www.rfc-editor.org/rfc/rfc3986#section-3.4) component

  ### Notes

  Sensitive content provided in `url.query` **SHOULD** be scrubbed when instrumentations can identify it

      iex> OpenTelemetry.SemanticConventions.Span.url_query()
      :"url.query"
  """
  @spec url_query :: :"url.query"
  def url_query do
    :"url.query"
  end

  @doc namespace: :client
  @doc """
  Client address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name

  ### Notes

  The IP address of the original client behind all proxies, if known (e.g. from [Forwarded#for](https://developer.mozilla.org/docs/Web/HTTP/Headers/Forwarded#for), [X-Forwarded-For](https://developer.mozilla.org/docs/Web/HTTP/Headers/X-Forwarded-For), or a similar header). Otherwise, the immediate client peer address

      iex> OpenTelemetry.SemanticConventions.Span.client_address()
      :"client.address"
  """
  @spec client_address :: :"client.address"
  def client_address do
    :"client.address"
  end

  @doc namespace: :client
  @doc """
  The port of whichever client was captured in `client.address`

  ### Notes

  When observed from the server side, and when communicating through an intermediary, `client.port` **SHOULD** represent the client port behind any intermediaries,  for example proxies, if it's available

      iex> OpenTelemetry.SemanticConventions.Span.client_port()
      :"client.port"
  """
  @spec client_port :: :"client.port"
  def client_port do
    :"client.port"
  end

  @doc namespace: :network
  @doc """
  Local socket address. Useful in case of a multi-IP host

      iex> OpenTelemetry.SemanticConventions.Span.network_local_address()
      :"network.local.address"
  """
  @spec network_local_address :: :"network.local.address"
  def network_local_address do
    :"network.local.address"
  end

  @doc namespace: :network
  @doc """
  Local socket port. Useful in case of a multi-port host

      iex> OpenTelemetry.SemanticConventions.Span.network_local_port()
      :"network.local.port"
  """
  @spec network_local_port :: :"network.local.port"
  def network_local_port do
    :"network.local.port"
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

  @doc namespace: :aws
  @doc """
  The AWS request ID as returned in the response headers `x-amz-request-id` or `x-amz-requestid`

      iex> OpenTelemetry.SemanticConventions.Span.aws_request_id()
      :"aws.request_id"
  """
  @spec aws_request_id :: :"aws.request_id"
  def aws_request_id do
    :"aws.request_id"
  end

  @doc namespace: :rpc
  @doc """
  The name of the operation corresponding to the request, as returned by the AWS SDK

  ### Notes

  This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side)

      iex> OpenTelemetry.SemanticConventions.Span.rpc_method()
      :"rpc.method"
  """
  @spec rpc_method :: :"rpc.method"
  def rpc_method do
    :"rpc.method"
  end

  @doc namespace: :rpc
  @doc """
  The name of the service to which a request is made, as returned by the AWS SDK

  ### Notes

  This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side)

      iex> OpenTelemetry.SemanticConventions.Span.rpc_service()
      :"rpc.service"
  """
  @spec rpc_service :: :"rpc.service"
  def rpc_service do
    :"rpc.service"
  end

  @doc namespace: :aws
  @doc """
  The value of the `AttributesToGet` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_attributes_to_get()
      :"aws.dynamodb.attributes_to_get"
  """
  @spec aws_dynamodb_attributes_to_get :: :"aws.dynamodb.attributes_to_get"
  def aws_dynamodb_attributes_to_get do
    :"aws.dynamodb.attributes_to_get"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ConsistentRead` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_consistent_read()
      :"aws.dynamodb.consistent_read"
  """
  @spec aws_dynamodb_consistent_read :: :"aws.dynamodb.consistent_read"
  def aws_dynamodb_consistent_read do
    :"aws.dynamodb.consistent_read"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of each item in the `ConsumedCapacity` response field

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_consumed_capacity()
      :"aws.dynamodb.consumed_capacity"
  """
  @spec aws_dynamodb_consumed_capacity :: :"aws.dynamodb.consumed_capacity"
  def aws_dynamodb_consumed_capacity do
    :"aws.dynamodb.consumed_capacity"
  end

  @doc namespace: :aws
  @doc """
  The value of the `IndexName` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_index_name()
      :"aws.dynamodb.index_name"
  """
  @spec aws_dynamodb_index_name :: :"aws.dynamodb.index_name"
  def aws_dynamodb_index_name do
    :"aws.dynamodb.index_name"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of the `ItemCollectionMetrics` response field

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_item_collection_metrics()
      :"aws.dynamodb.item_collection_metrics"
  """
  @spec aws_dynamodb_item_collection_metrics :: :"aws.dynamodb.item_collection_metrics"
  def aws_dynamodb_item_collection_metrics do
    :"aws.dynamodb.item_collection_metrics"
  end

  @doc namespace: :aws
  @doc """
  The value of the `Limit` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_limit()
      :"aws.dynamodb.limit"
  """
  @spec aws_dynamodb_limit :: :"aws.dynamodb.limit"
  def aws_dynamodb_limit do
    :"aws.dynamodb.limit"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ProjectionExpression` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_projection()
      :"aws.dynamodb.projection"
  """
  @spec aws_dynamodb_projection :: :"aws.dynamodb.projection"
  def aws_dynamodb_projection do
    :"aws.dynamodb.projection"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_provisioned_read_capacity()
      :"aws.dynamodb.provisioned_read_capacity"
  """
  @spec aws_dynamodb_provisioned_read_capacity :: :"aws.dynamodb.provisioned_read_capacity"
  def aws_dynamodb_provisioned_read_capacity do
    :"aws.dynamodb.provisioned_read_capacity"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_provisioned_write_capacity()
      :"aws.dynamodb.provisioned_write_capacity"
  """
  @spec aws_dynamodb_provisioned_write_capacity :: :"aws.dynamodb.provisioned_write_capacity"
  def aws_dynamodb_provisioned_write_capacity do
    :"aws.dynamodb.provisioned_write_capacity"
  end

  @doc namespace: :aws
  @doc """
  The value of the `Select` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_select()
      :"aws.dynamodb.select"
  """
  @spec aws_dynamodb_select :: :"aws.dynamodb.select"
  def aws_dynamodb_select do
    :"aws.dynamodb.select"
  end

  @doc namespace: :aws
  @doc """
  The keys in the `RequestItems` object field

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_table_names()
      :"aws.dynamodb.table_names"
  """
  @spec aws_dynamodb_table_names :: :"aws.dynamodb.table_names"
  def aws_dynamodb_table_names do
    :"aws.dynamodb.table_names"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_global_secondary_indexes()
      :"aws.dynamodb.global_secondary_indexes"
  """
  @spec aws_dynamodb_global_secondary_indexes :: :"aws.dynamodb.global_secondary_indexes"
  def aws_dynamodb_global_secondary_indexes do
    :"aws.dynamodb.global_secondary_indexes"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_local_secondary_indexes()
      :"aws.dynamodb.local_secondary_indexes"
  """
  @spec aws_dynamodb_local_secondary_indexes :: :"aws.dynamodb.local_secondary_indexes"
  def aws_dynamodb_local_secondary_indexes do
    :"aws.dynamodb.local_secondary_indexes"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ExclusiveStartTableName` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_exclusive_start_table()
      :"aws.dynamodb.exclusive_start_table"
  """
  @spec aws_dynamodb_exclusive_start_table :: :"aws.dynamodb.exclusive_start_table"
  def aws_dynamodb_exclusive_start_table do
    :"aws.dynamodb.exclusive_start_table"
  end

  @doc namespace: :aws
  @doc """
  The number of items in the `TableNames` response parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_table_count()
      :"aws.dynamodb.table_count"
  """
  @spec aws_dynamodb_table_count :: :"aws.dynamodb.table_count"
  def aws_dynamodb_table_count do
    :"aws.dynamodb.table_count"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ScanIndexForward` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_scan_forward()
      :"aws.dynamodb.scan_forward"
  """
  @spec aws_dynamodb_scan_forward :: :"aws.dynamodb.scan_forward"
  def aws_dynamodb_scan_forward do
    :"aws.dynamodb.scan_forward"
  end

  @doc namespace: :aws
  @doc """
  The value of the `Count` response parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_count()
      :"aws.dynamodb.count"
  """
  @spec aws_dynamodb_count :: :"aws.dynamodb.count"
  def aws_dynamodb_count do
    :"aws.dynamodb.count"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ScannedCount` response parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_scanned_count()
      :"aws.dynamodb.scanned_count"
  """
  @spec aws_dynamodb_scanned_count :: :"aws.dynamodb.scanned_count"
  def aws_dynamodb_scanned_count do
    :"aws.dynamodb.scanned_count"
  end

  @doc namespace: :aws
  @doc """
  The value of the `Segment` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_segment()
      :"aws.dynamodb.segment"
  """
  @spec aws_dynamodb_segment :: :"aws.dynamodb.segment"
  def aws_dynamodb_segment do
    :"aws.dynamodb.segment"
  end

  @doc namespace: :aws
  @doc """
  The value of the `TotalSegments` request parameter

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_total_segments()
      :"aws.dynamodb.total_segments"
  """
  @spec aws_dynamodb_total_segments :: :"aws.dynamodb.total_segments"
  def aws_dynamodb_total_segments do
    :"aws.dynamodb.total_segments"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of each item in the `AttributeDefinitions` request field

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_attribute_definitions()
      :"aws.dynamodb.attribute_definitions"
  """
  @spec aws_dynamodb_attribute_definitions :: :"aws.dynamodb.attribute_definitions"
  def aws_dynamodb_attribute_definitions do
    :"aws.dynamodb.attribute_definitions"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of each item in the `GlobalSecondaryIndexUpdates` request field

      iex> OpenTelemetry.SemanticConventions.Span.aws_dynamodb_global_secondary_index_updates()
      :"aws.dynamodb.global_secondary_index_updates"
  """
  @spec aws_dynamodb_global_secondary_index_updates ::
          :"aws.dynamodb.global_secondary_index_updates"
  def aws_dynamodb_global_secondary_index_updates do
    :"aws.dynamodb.global_secondary_index_updates"
  end

  @doc namespace: :aws
  @doc """
  The S3 bucket name the request refers to. Corresponds to the `--bucket` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations

  ### Notes

  The `bucket` attribute is applicable to all S3 operations that reference a bucket, i.e. that require the bucket name as a mandatory parameter.
  This applies to almost all S3 operations except `list-buckets`

      iex> OpenTelemetry.SemanticConventions.Span.aws_s3_bucket()
      :"aws.s3.bucket"
  """
  @spec aws_s3_bucket :: :"aws.s3.bucket"
  def aws_s3_bucket do
    :"aws.s3.bucket"
  end

  @doc namespace: :aws
  @doc """
  The source object (in the form `bucket`/`key`) for the copy operation

  ### Notes

  The `copy_source` attribute applies to S3 copy operations and corresponds to the `--copy-source` parameter
  of the [copy-object operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html).
  This applies in particular to the following operations:

  - [copy-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)

      iex> OpenTelemetry.SemanticConventions.Span.aws_s3_copy_source()
      :"aws.s3.copy_source"
  """
  @spec aws_s3_copy_source :: :"aws.s3.copy_source"
  def aws_s3_copy_source do
    :"aws.s3.copy_source"
  end

  @doc namespace: :aws
  @doc """
  The delete request container that specifies the objects to be deleted

  ### Notes

  The `delete` attribute is only applicable to the [delete-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-object.html) operation.
  The `delete` attribute corresponds to the `--delete` parameter of the
  [delete-objects operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-objects.html)

      iex> OpenTelemetry.SemanticConventions.Span.aws_s3_delete()
      :"aws.s3.delete"
  """
  @spec aws_s3_delete :: :"aws.s3.delete"
  def aws_s3_delete do
    :"aws.s3.delete"
  end

  @doc namespace: :aws
  @doc """
  The S3 object key the request refers to. Corresponds to the `--key` parameter of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) operations

  ### Notes

  The `key` attribute is applicable to all object-related S3 operations, i.e. that require the object key as a mandatory parameter.
  This applies in particular to the following operations:

  - [copy-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/copy-object.html)
  - [delete-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/delete-object.html)
  - [get-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/get-object.html)
  - [head-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/head-object.html)
  - [put-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/put-object.html)
  - [restore-object](https://docs.aws.amazon.com/cli/latest/reference/s3api/restore-object.html)
  - [select-object-content](https://docs.aws.amazon.com/cli/latest/reference/s3api/select-object-content.html)
  - [abort-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/abort-multipart-upload.html)
  - [complete-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/complete-multipart-upload.html)
  - [create-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/create-multipart-upload.html)
  - [list-parts](https://docs.aws.amazon.com/cli/latest/reference/s3api/list-parts.html)
  - [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)

      iex> OpenTelemetry.SemanticConventions.Span.aws_s3_key()
      :"aws.s3.key"
  """
  @spec aws_s3_key :: :"aws.s3.key"
  def aws_s3_key do
    :"aws.s3.key"
  end

  @doc namespace: :aws
  @doc """
  The part number of the part being uploaded in a multipart-upload operation. This is a positive integer between 1 and 10,000

  ### Notes

  The `part_number` attribute is only applicable to the [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  and [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html) operations.
  The `part_number` attribute corresponds to the `--part-number` parameter of the
  [upload-part operation within the S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)

      iex> OpenTelemetry.SemanticConventions.Span.aws_s3_part_number()
      :"aws.s3.part_number"
  """
  @spec aws_s3_part_number :: :"aws.s3.part_number"
  def aws_s3_part_number do
    :"aws.s3.part_number"
  end

  @doc namespace: :aws
  @doc """
  Upload ID that identifies the multipart upload

  ### Notes

  The `upload_id` attribute applies to S3 multipart-upload operations and corresponds to the `--upload-id` parameter
  of the [S3 API](https://docs.aws.amazon.com/cli/latest/reference/s3api/index.html) multipart operations.
  This applies in particular to the following operations:

  - [abort-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/abort-multipart-upload.html)
  - [complete-multipart-upload](https://docs.aws.amazon.com/cli/latest/reference/s3api/complete-multipart-upload.html)
  - [list-parts](https://docs.aws.amazon.com/cli/latest/reference/s3api/list-parts.html)
  - [upload-part](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part.html)
  - [upload-part-copy](https://docs.aws.amazon.com/cli/latest/reference/s3api/upload-part-copy.html)

      iex> OpenTelemetry.SemanticConventions.Span.aws_s3_upload_id()
      :"aws.s3.upload_id"
  """
  @spec aws_s3_upload_id :: :"aws.s3.upload_id"
  def aws_s3_upload_id do
    :"aws.s3.upload_id"
  end

  @doc namespace: :graphql
  @doc """
  The GraphQL document being executed

  ### Notes

  The value may be sanitized to exclude sensitive information

      iex> OpenTelemetry.SemanticConventions.Span.graphql_document()
      :"graphql.document"
  """
  @spec graphql_document :: :"graphql.document"
  def graphql_document do
    :"graphql.document"
  end

  @doc namespace: :graphql
  @doc """
  The name of the operation being executed

      iex> OpenTelemetry.SemanticConventions.Span.graphql_operation_name()
      :"graphql.operation.name"
  """
  @spec graphql_operation_name :: :"graphql.operation.name"
  def graphql_operation_name do
    :"graphql.operation.name"
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

  If a custom value is used, it **MUST** be of low cardinality

      iex> OpenTelemetry.SemanticConventions.Span.messaging_operation()
      :"messaging.operation"
  """
  @spec messaging_operation :: :"messaging.operation"
  def messaging_operation do
    :"messaging.operation"
  end

  @doc namespace: :messaging
  @doc """
  The number of messages sent, received, or processed in the scope of the batching operation

  ### Notes

  Instrumentations **SHOULD NOT** set `messaging.batch.message_count` on spans that operate with a single message. When a messaging client library supports both batch and single-message API for the same operation, instrumentations **SHOULD** use `messaging.batch.message_count` for batching APIs and **SHOULD NOT** use it for single-message APIs

      iex> OpenTelemetry.SemanticConventions.Span.messaging_batch_message_count()
      :"messaging.batch.message_count"
  """
  @spec messaging_batch_message_count :: :"messaging.batch.message_count"
  def messaging_batch_message_count do
    :"messaging.batch.message_count"
  end

  @doc namespace: :messaging
  @doc """
  A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name)

      iex> OpenTelemetry.SemanticConventions.Span.messaging_destination_anonymous()
      :"messaging.destination.anonymous"
  """
  @spec messaging_destination_anonymous :: :"messaging.destination.anonymous"
  def messaging_destination_anonymous do
    :"messaging.destination.anonymous"
  end

  @doc namespace: :messaging
  @doc """
  The message destination name

  ### Notes

  Destination name **SHOULD** uniquely identify a specific queue, topic or other entity within the broker. If
  the broker doesn't have such notion, the destination name **SHOULD** uniquely identify the broker

      iex> OpenTelemetry.SemanticConventions.Span.messaging_destination_name()
      :"messaging.destination.name"
  """
  @spec messaging_destination_name :: :"messaging.destination.name"
  def messaging_destination_name do
    :"messaging.destination.name"
  end

  @doc namespace: :messaging
  @doc """
  Low cardinality representation of the messaging destination name

  ### Notes

  Destination names could be constructed from templates. An example would be a destination name involving a user name or product id. Although the destination name in this case is of high cardinality, the underlying template is of low cardinality and can be effectively used for grouping and aggregation

      iex> OpenTelemetry.SemanticConventions.Span.messaging_destination_template()
      :"messaging.destination.template"
  """
  @spec messaging_destination_template :: :"messaging.destination.template"
  def messaging_destination_template do
    :"messaging.destination.template"
  end

  @doc namespace: :messaging
  @doc """
  A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed

      iex> OpenTelemetry.SemanticConventions.Span.messaging_destination_temporary()
      :"messaging.destination.temporary"
  """
  @spec messaging_destination_temporary :: :"messaging.destination.temporary"
  def messaging_destination_temporary do
    :"messaging.destination.temporary"
  end

  @doc namespace: :messaging
  @doc """
  A unique identifier for the client that consumes or produces a message

      iex> OpenTelemetry.SemanticConventions.Span.messaging_client_id()
      :"messaging.client_id"
  """
  @spec messaging_client_id :: :"messaging.client_id"
  def messaging_client_id do
    :"messaging.client_id"
  end

  @doc namespace: :messaging
  @doc """
  The identifier of the partition messages are sent to or received from, unique within the `messaging.destination.name`

      iex> OpenTelemetry.SemanticConventions.Span.messaging_destination_partition_id()
      :"messaging.destination.partition.id"
  """
  @spec messaging_destination_partition_id :: :"messaging.destination.partition.id"
  def messaging_destination_partition_id do
    :"messaging.destination.partition.id"
  end

  @doc namespace: :messaging
  @doc """
  The size of the message body in bytes

  ### Notes

  This can refer to both the compressed or uncompressed body size. If both sizes are known, the uncompressed
  body size should be used

      iex> OpenTelemetry.SemanticConventions.Span.messaging_message_body_size()
      :"messaging.message.body.size"
  """
  @spec messaging_message_body_size :: :"messaging.message.body.size"
  def messaging_message_body_size do
    :"messaging.message.body.size"
  end

  @doc namespace: :messaging
  @doc """
  The conversation ID identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID"

      iex> OpenTelemetry.SemanticConventions.Span.messaging_message_conversation_id()
      :"messaging.message.conversation_id"
  """
  @spec messaging_message_conversation_id :: :"messaging.message.conversation_id"
  def messaging_message_conversation_id do
    :"messaging.message.conversation_id"
  end

  @doc namespace: :messaging
  @doc """
  The size of the message body and metadata in bytes

  ### Notes

  This can refer to both the compressed or uncompressed size. If both sizes are known, the uncompressed
  size should be used

      iex> OpenTelemetry.SemanticConventions.Span.messaging_message_envelope_size()
      :"messaging.message.envelope.size"
  """
  @spec messaging_message_envelope_size :: :"messaging.message.envelope.size"
  def messaging_message_envelope_size do
    :"messaging.message.envelope.size"
  end

  @doc namespace: :messaging
  @doc """
  A value used by the messaging system as an identifier for the message, represented as a string

      iex> OpenTelemetry.SemanticConventions.Span.messaging_message_id()
      :"messaging.message.id"
  """
  @spec messaging_message_id :: :"messaging.message.id"
  def messaging_message_id do
    :"messaging.message.id"
  end

  @doc namespace: :network
  @doc """
  [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication)

  ### Notes

  The value **SHOULD** be normalized to lowercase.

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

  The value **SHOULD** be normalized to lowercase

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
  `error.code` property of response if it is an error response

      iex> OpenTelemetry.SemanticConventions.Span.rpc_jsonrpc_error_code()
      :"rpc.jsonrpc.error_code"
  """
  @spec rpc_jsonrpc_error_code :: :"rpc.jsonrpc.error_code"
  def rpc_jsonrpc_error_code do
    :"rpc.jsonrpc.error_code"
  end

  @doc namespace: :rpc
  @doc """
  Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 doesn't specify this, the value can be omitted

      iex> OpenTelemetry.SemanticConventions.Span.rpc_jsonrpc_version()
      :"rpc.jsonrpc.version"
  """
  @spec rpc_jsonrpc_version :: :"rpc.jsonrpc.version"
  def rpc_jsonrpc_version do
    :"rpc.jsonrpc.version"
  end

  @doc namespace: :rpc
  @doc """
  `error.message` property of response if it is an error response

      iex> OpenTelemetry.SemanticConventions.Span.rpc_jsonrpc_error_message()
      :"rpc.jsonrpc.error_message"
  """
  @spec rpc_jsonrpc_error_message :: :"rpc.jsonrpc.error_message"
  def rpc_jsonrpc_error_message do
    :"rpc.jsonrpc.error_message"
  end

  @doc namespace: :rpc
  @doc """
  `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification

      iex> OpenTelemetry.SemanticConventions.Span.rpc_jsonrpc_request_id()
      :"rpc.jsonrpc.request_id"
  """
  @spec rpc_jsonrpc_request_id :: :"rpc.jsonrpc.request_id"
  def rpc_jsonrpc_request_id do
    :"rpc.jsonrpc.request_id"
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
