defmodule OpenTelemetry.SemanticConventions.Common do
  @moduledoc """
  OpenTelemetry Semantic Conventions for Attributes.
  """

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

  @doc namespace: :error
  @typedoc """
  Describes a class of error the operation ended with

  ### Options

  * `:_OTHER` - A fallback error value to be used when the instrumentation doesn't define a custom value

  """
  @type error_type() :: :_OTHER | atom()

  @doc namespace: :log
  @typedoc """
  The stream associated with the log. See below for a list of well-known values

  ### Options

  * `:stdout`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Logs from stdout stream

  * `:stderr`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Events from stderr stream

  """
  @type log_iostream() :: :stdout | :stderr

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

  @doc namespace: :process
  @typedoc """
  The CPU state for this data point. A process **SHOULD** be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels

  ### Options

  * `:system`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - system

  * `:user`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - user

  * `:wait`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - wait

  """
  @type process_cpu_state() :: :system | :user | :wait | atom()

  @doc namespace: :rpc
  @typedoc """
  A string identifying the remoting system. See below for a list of well-known identifiers

  ### Options

  * `:grpc`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - gRPC

  * `:java_rmi`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Java RMI

  * `:dotnet_wcf`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - .NET WCF

  * `:apache_dubbo`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Dubbo

  * `:connect_rpc`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Connect RPC

  """
  @type rpc_system() :: :grpc | :java_rmi | :dotnet_wcf | :apache_dubbo | :connect_rpc | atom()

  @doc namespace: :system
  @typedoc """
  The CPU state for this data point. A system's CPU **SHOULD** be characterized *either* by data points with no `state` labels, *or only* data points with `state` labels

  ### Options

  * `:user`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - user

  * `:system`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - system

  * `:nice`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - nice

  * `:idle`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - idle

  * `:iowait`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - iowait

  * `:interrupt`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - interrupt

  * `:steal`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - steal

  """
  @type system_cpu_state() ::
          :user | :system | :nice | :idle | :iowait | :interrupt | :steal | atom()

  @doc namespace: :system
  @typedoc """
  The memory state

  ### Options

  * `:used`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - used

  * `:free`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - free

  * `:shared`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - shared

  * `:buffers`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - buffers

  * `:cached`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - cached

  """
  @type system_memory_state() :: :used | :free | :shared | :buffers | :cached | atom()

  @doc namespace: :system
  @typedoc """
  The paging access direction

  ### Options

  * `:in`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - in

  * `:out`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - out

  """
  @type system_paging_direction() :: :in | :out

  @doc namespace: :system
  @typedoc """
  The memory paging state

  ### Options

  * `:used`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - used

  * `:free`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - free

  """
  @type system_paging_state() :: :used | :free

  @doc namespace: :system
  @typedoc """
  The memory paging type

  ### Options

  * `:major`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - major

  * `:minor`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - minor

  """
  @type system_paging_type() :: :major | :minor

  @doc namespace: :system
  @typedoc """
  The filesystem state

  ### Options

  * `:used`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - used

  * `:free`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - free

  * `:reserved`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - reserved

  """
  @type system_filesystem_state() :: :used | :free | :reserved

  @doc namespace: :system
  @typedoc """
  The filesystem type

  ### Options

  * `:fat32`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - fat32

  * `:exfat`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - exfat

  * `:ntfs`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ntfs

  * `:refs`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - refs

  * `:hfsplus`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - hfsplus

  * `:ext4`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ext4

  """
  @type system_filesystem_type() :: :fat32 | :exfat | :ntfs | :refs | :hfsplus | :ext4 | atom()

  @doc namespace: :system
  @typedoc """
  A stateless protocol **MUST NOT** set this attribute

  ### Options

  * `:close`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - close

  * `:close_wait`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - close_wait

  * `:closing`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - closing

  * `:delete`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - delete

  * `:established`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - established

  * `:fin_wait_1`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - fin_wait_1

  * `:fin_wait_2`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - fin_wait_2

  * `:last_ack`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - last_ack

  * `:listen`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - listen

  * `:syn_recv`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - syn_recv

  * `:syn_sent`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - syn_sent

  * `:time_wait`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - time_wait

  """
  @type system_network_state() ::
          :close
          | :close_wait
          | :closing
          | :delete
          | :established
          | :fin_wait_1
          | :fin_wait_2
          | :last_ack
          | :listen
          | :syn_recv
          | :syn_sent
          | :time_wait

  @doc namespace: :system
  @typedoc """
  The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)

  ### Options

  * `:running`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - running

  * `:sleeping`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - sleeping

  * `:stopped`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - stopped

  * `:defunct`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - defunct

  """
  @type system_process_status() :: :running | :sleeping | :stopped | :defunct | atom()

  @doc namespace: :network
  @typedoc """
  This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection

  ### Options

  * `:gprs`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - GPRS

  * `:edge`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EDGE

  * `:umts`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - UMTS

  * `:cdma`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - CDMA

  * `:evdo_0`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EVDO Rel. 0

  * `:evdo_a`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EVDO Rev. A

  * `:cdma2000_1xrtt`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - CDMA2000 1XRTT

  * `:hsdpa`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HSDPA

  * `:hsupa`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HSUPA

  * `:hspa`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HSPA

  * `:iden`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IDEN

  * `:evdo_b`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EVDO Rev. B

  * `:lte`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - LTE

  * `:ehrpd`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - EHRPD

  * `:hspap`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HSPAP

  * `:gsm`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - GSM

  * `:td_scdma`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - TD-SCDMA

  * `:iwlan`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IWLAN

  * `:nr`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 5G NR (New Radio)

  * `:nrnsa`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 5G NRNSA (New Radio Non-Standalone)

  * `:lte_ca`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - LTE CA

  """
  @type network_connection_subtype() ::
          :gprs
          | :edge
          | :umts
          | :cdma
          | :evdo_0
          | :evdo_a
          | :cdma2000_1xrtt
          | :hsdpa
          | :hsupa
          | :hspa
          | :iden
          | :evdo_b
          | :lte
          | :ehrpd
          | :hspap
          | :gsm
          | :td_scdma
          | :iwlan
          | :nr
          | :nrnsa
          | :lte_ca
          | atom()

  @doc namespace: :network
  @typedoc """
  The internet connection type

  ### Options

  * `:wifi`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - wifi

  * `:wired`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - wired

  * `:cell`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - cell

  * `:unavailable`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - unavailable

  * `:unknown`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - unknown

  """
  @type network_connection_type() :: :wifi | :wired | :cell | :unavailable | :unknown | atom()

  @doc namespace: :container
  @typedoc """
  The CPU state for this data point

  ### Options

  * `:user`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When tasks of the cgroup are in user mode (Linux). When all container processes are in user mode (Windows)

  * `:system`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When CPU is used by the system (host OS)

  * `:kernel`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When tasks of the cgroup are in kernel mode (Linux). When all container processes are in kernel mode (Windows)

  """
  @type container_cpu_state() :: :user | :system | :kernel | atom()

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

  @doc namespace: :http
  @typedoc """
  Deprecated, use `network.protocol.name` instead

  ### Options

  * `:"1.0"`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HTTP/1.0

  * `:"1.1"`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HTTP/1.1

  * `:"2.0"`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HTTP/2

  * `:"3.0"`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HTTP/3

  * `:SPDY`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - SPDY protocol

  * `:QUIC`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - QUIC protocol

  """
  @type http_flavor() :: :"1.0" | :"1.1" | :"2.0" | :"3.0" | :SPDY | :QUIC | atom()

  @doc namespace: :system
  @typedoc """
  Deprecated, use `system.process.status` instead

  ### Options

  * `:running`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - running

  * `:sleeping`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - sleeping

  * `:stopped`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - stopped

  * `:defunct`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - defunct

  """
  @type system_processes_status() :: :running | :sleeping | :stopped | :defunct | atom()

  @doc namespace: :disk
  @typedoc """
  The disk IO operation direction

  ### Options

  * `:read`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - read

  * `:write`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - write

  """
  @type disk_io_direction() :: :read | :write

  @doc namespace: :faas
  @typedoc """
  Describes the type of the operation that was performed on the data

  ### Options

  * `:insert`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When a new object is created

  * `:edit`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When an object is modified

  * `:delete`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When an object is deleted

  """
  @type faas_document_operation() :: :insert | :edit | :delete | atom()

  @doc namespace: :host
  @typedoc """
  The CPU architecture the host system is running on

  ### Options

  * `:amd64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - AMD64

  * `:arm32`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ARM32

  * `:arm64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ARM64

  * `:ia64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Itanium

  * `:ppc32`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 32-bit PowerPC

  * `:ppc64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 64-bit PowerPC

  * `:s390x`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IBM z/Architecture

  * `:x86`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 32-bit x86

  """
  @type host_arch() :: :amd64 | :arm32 | :arm64 | :ia64 | :ppc32 | :ppc64 | :s390x | :x86 | atom()

  @doc namespace: :http
  @typedoc """
  State of the HTTP connection in the HTTP connection pool

  ### Options

  * `:active`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - active state

  * `:idle`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - idle state

  """
  @type http_connection_state() :: :active | :idle | atom()

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
  Model of message consumption. This only applies to consumer spans

  ### Options

  * `:clustering`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Clustering consumption model

  * `:broadcasting`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Broadcasting consumption model

  """
  @type messaging_rocketmq_consumption_model() :: :clustering | :broadcasting

  @doc namespace: :messaging
  @typedoc """
  Type of message

  ### Options

  * `:normal`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Normal message

  * `:fifo`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - FIFO message

  * `:delay`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Delay message

  * `:transaction`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Transaction message

  """
  @type messaging_rocketmq_message_type() :: :normal | :fifo | :delay | :transaction

  @doc namespace: :messaging
  @typedoc """
  Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock)

  ### Options

  * `:complete`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Message is completed

  * `:abandon`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Message is abandoned

  * `:dead_letter`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Message is sent to dead letter queue

  * `:defer`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Message is deferred

  """
  @type messaging_servicebus_disposition_status() ::
          :complete | :abandon | :dead_letter | :defer | atom()

  @doc namespace: :network
  @typedoc """
  The network IO operation direction

  ### Options

  * `:transmit`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - transmit

  * `:receive`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - receive

  """
  @type network_io_direction() :: :transmit | :receive

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

  @doc namespace: :telemetry
  @typedoc """
  The language of the telemetry SDK

  ### Options

  * `:cpp` - cpp

  * `:dotnet` - dotnet

  * `:erlang` - erlang

  * `:go` - go

  * `:java` - java

  * `:nodejs` - nodejs

  * `:php` - php

  * `:python` - python

  * `:ruby` - ruby

  * `:rust` - rust

  * `:swift` - swift

  * `:webjs` - webjs

  """
  @type telemetry_sdk_language() ::
          :cpp
          | :dotnet
          | :erlang
          | :go
          | :java
          | :nodejs
          | :php
          | :python
          | :ruby
          | :rust
          | :swift
          | :webjs
          | atom()

  @doc namespace: :tls
  @typedoc """
  Normalized lowercase protocol name parsed from original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)

  ### Options

  * `:ssl`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ssl

  * `:tls`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - tls

  """
  @type tls_protocol_name() :: :ssl | :tls | atom()

  @doc """
  The URL of the OpenTelemetry schema for these keys and values.

      iex> OpenTelemetry.SemanticConventions.Common.schema_url()
      "https://opentelemetry.io/schemas/1.25.0"
  """
  @spec schema_url :: String.t()
  def schema_url do
    "https://opentelemetry.io/schemas/1.25.0"
  end

  @doc namespace: :faas
  @doc """
  The name of the invoked function

  ### Notes

  **SHOULD** be equal to the `faas.name` resource attribute of the invoked function

      iex> OpenTelemetry.SemanticConventions.Common.faas_invoked_name()
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

      iex> OpenTelemetry.SemanticConventions.Common.faas_invoked_provider()
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

      iex> OpenTelemetry.SemanticConventions.Common.faas_invoked_region()
      :"faas.invoked_region"
  """
  @spec faas_invoked_region :: :"faas.invoked_region"
  def faas_invoked_region do
    :"faas.invoked_region"
  end

  @doc namespace: :faas
  @doc """
  Type of the trigger which caused this function invocation

      iex> OpenTelemetry.SemanticConventions.Common.faas_trigger()
      :"faas.trigger"
  """
  @spec faas_trigger :: :"faas.trigger"
  def faas_trigger do
    :"faas.trigger"
  end

  @doc namespace: :client
  @doc """
  Client address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name

  ### Notes

  When observed from the server side, and when communicating through an intermediary, `client.address` **SHOULD** represent the client address behind any intermediaries,  for example proxies, if it's available

      iex> OpenTelemetry.SemanticConventions.Common.client_address()
      :"client.address"
  """
  @spec client_address :: :"client.address"
  def client_address do
    :"client.address"
  end

  @doc namespace: :client
  @doc """
  Client port number

  ### Notes

  When observed from the server side, and when communicating through an intermediary, `client.port` **SHOULD** represent the client port behind any intermediaries,  for example proxies, if it's available

      iex> OpenTelemetry.SemanticConventions.Common.client_port()
      :"client.port"
  """
  @spec client_port :: :"client.port"
  def client_port do
    :"client.port"
  end

  @doc namespace: :server
  @doc """
  Server domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name

  ### Notes

  When observed from the client side, and when communicating through an intermediary, `server.address` **SHOULD** represent the server address behind any intermediaries, for example proxies, if it's available

      iex> OpenTelemetry.SemanticConventions.Common.server_address()
      :"server.address"
  """
  @spec server_address :: :"server.address"
  def server_address do
    :"server.address"
  end

  @doc namespace: :server
  @doc """
  Server port number

  ### Notes

  When observed from the client side, and when communicating through an intermediary, `server.port` **SHOULD** represent the server port behind any intermediaries, for example proxies, if it's available

      iex> OpenTelemetry.SemanticConventions.Common.server_port()
      :"server.port"
  """
  @spec server_port :: :"server.port"
  def server_port do
    :"server.port"
  end

  @doc namespace: :source
  @doc """
  Source address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name

  ### Notes

  When observed from the destination side, and when communicating through an intermediary, `source.address` **SHOULD** represent the source address behind any intermediaries, for example proxies, if it's available

      iex> OpenTelemetry.SemanticConventions.Common.source_address()
      :"source.address"
  """
  @spec source_address :: :"source.address"
  def source_address do
    :"source.address"
  end

  @doc namespace: :source
  @doc """
  Source port number

      iex> OpenTelemetry.SemanticConventions.Common.source_port()
      :"source.port"
  """
  @spec source_port :: :"source.port"
  def source_port do
    :"source.port"
  end

  @doc namespace: :destination
  @doc """
  Destination address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name

  ### Notes

  When observed from the source side, and when communicating through an intermediary, `destination.address` **SHOULD** represent the destination address behind any intermediaries, for example proxies, if it's available

      iex> OpenTelemetry.SemanticConventions.Common.destination_address()
      :"destination.address"
  """
  @spec destination_address :: :"destination.address"
  def destination_address do
    :"destination.address"
  end

  @doc namespace: :destination
  @doc """
  Destination port number

      iex> OpenTelemetry.SemanticConventions.Common.destination_port()
      :"destination.port"
  """
  @spec destination_port :: :"destination.port"
  def destination_port do
    :"destination.port"
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

      iex> OpenTelemetry.SemanticConventions.Common.http_request_method()
      :"http.request.method"
  """
  @spec http_request_method :: :"http.request.method"
  def http_request_method do
    :"http.request.method"
  end

  @doc namespace: :error
  @doc """
  Describes a class of error the operation ended with

  ### Notes

  If the request fails with an error before response status code was sent or received,
  `error.type` **SHOULD** be set to exception type (its fully-qualified class name, if applicable)
  or a component-specific low cardinality error identifier.

  If response status code was sent or received and status indicates an error according to [HTTP span status definition](/docs/http/http-spans.md),
  `error.type` **SHOULD** be set to the status code number (represented as a string), an exception type (if thrown) or a component-specific error identifier.

  The `error.type` value **SHOULD** be predictable and **SHOULD** have low cardinality.
  Instrumentations **SHOULD** document the list of errors they report.

  The cardinality of `error.type` within one instrumentation library **SHOULD** be low, but
  telemetry consumers that aggregate data from multiple instrumentation libraries and applications
  should be prepared for `error.type` to have high cardinality at query time, when no
  additional filters are applied.

  If the request has completed successfully, instrumentations **SHOULD NOT** set `error.type`

      iex> OpenTelemetry.SemanticConventions.Common.error_type()
      :"error.type"
  """
  @spec error_type :: :"error.type"
  def error_type do
    :"error.type"
  end

  @doc namespace: :http
  @doc """
  [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6)

      iex> OpenTelemetry.SemanticConventions.Common.http_response_status_code()
      :"http.response.status_code"
  """
  @spec http_response_status_code :: :"http.response.status_code"
  def http_response_status_code do
    :"http.response.status_code"
  end

  @doc namespace: :network
  @doc """
  [OSI application layer](https://osi-model.com/application-layer/) or non-OSI equivalent

  ### Notes

  The value **SHOULD** be normalized to lowercase

      iex> OpenTelemetry.SemanticConventions.Common.network_protocol_name()
      :"network.protocol.name"
  """
  @spec network_protocol_name :: :"network.protocol.name"
  def network_protocol_name do
    :"network.protocol.name"
  end

  @doc namespace: :network
  @doc """
  The actual version of the protocol used for network communication

  ### Notes

  If protocol version is subject to negotiation (for example using [ALPN](https://www.rfc-editor.org/rfc/rfc7301.html)), this attribute **SHOULD** be set to the negotiated version. If the actual protocol version is not known, this attribute **SHOULD NOT** be set

      iex> OpenTelemetry.SemanticConventions.Common.network_protocol_version()
      :"network.protocol.version"
  """
  @spec network_protocol_version :: :"network.protocol.version"
  def network_protocol_version do
    :"network.protocol.version"
  end

  @doc namespace: :url
  @doc """
  The [URI scheme](https://www.rfc-editor.org/rfc/rfc3986#section-3.1) component identifying the used protocol

      iex> OpenTelemetry.SemanticConventions.Common.url_scheme()
      :"url.scheme"
  """
  @spec url_scheme :: :"url.scheme"
  def url_scheme do
    :"url.scheme"
  end

  @doc namespace: :http
  @doc """
  The matched route, that is, the path template in the format used by the respective server framework

  ### Notes

  **MUST NOT** be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can **NOT** substitute it.
  **SHOULD** include the [application root](/docs/http/http-spans.md#http-server-definitions) if there is one

      iex> OpenTelemetry.SemanticConventions.Common.http_route()
      :"http.route"
  """
  @spec http_route :: :"http.route"
  def http_route do
    :"http.route"
  end

  @doc namespace: :event
  @doc """
  Identifies the class / type of event

  ### Notes

  Event names are subject to the same rules as [attribute names](https://github.com/open-telemetry/opentelemetry-specification/tree/v1.31.0/specification/common/attribute-naming.md). Notably, event names are namespaced to avoid collisions and provide a clean separation of semantics for events in separate domains like browser, mobile, and kubernetes

      iex> OpenTelemetry.SemanticConventions.Common.event_name()
      :"event.name"
  """
  @spec event_name :: :"event.name"
  def event_name do
    :"event.name"
  end

  @doc namespace: :log
  @doc """
  A unique identifier for the Log Record

  ### Notes

  If an id is provided, other log records with the same id will be considered duplicates and can be removed safely. This means, that two distinguishable log records **MUST** have different values.
  The id **MAY** be an [Universally Unique Lexicographically Sortable Identifier (ULID)](https://github.com/ulid/spec), but other identifiers (e.g. UUID) may be used as needed

      iex> OpenTelemetry.SemanticConventions.Common.log_record_uid()
      :"log.record.uid"
  """
  @spec log_record_uid :: :"log.record.uid"
  def log_record_uid do
    :"log.record.uid"
  end

  @doc namespace: :exception
  @doc """
  The exception message

      iex> OpenTelemetry.SemanticConventions.Common.exception_message()
      :"exception.message"
  """
  @spec exception_message :: :"exception.message"
  def exception_message do
    :"exception.message"
  end

  @doc namespace: :exception
  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG

      iex> OpenTelemetry.SemanticConventions.Common.exception_stacktrace()
      :"exception.stacktrace"
  """
  @spec exception_stacktrace :: :"exception.stacktrace"
  def exception_stacktrace do
    :"exception.stacktrace"
  end

  @doc namespace: :exception
  @doc """
  The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it

      iex> OpenTelemetry.SemanticConventions.Common.exception_type()
      :"exception.type"
  """
  @spec exception_type :: :"exception.type"
  def exception_type do
    :"exception.type"
  end

  @doc namespace: :log
  @doc """
  The stream associated with the log. See below for a list of well-known values

      iex> OpenTelemetry.SemanticConventions.Common.log_iostream()
      :"log.iostream"
  """
  @spec log_iostream :: :"log.iostream"
  def log_iostream do
    :"log.iostream"
  end

  @doc namespace: :log
  @doc """
  The basename of the file

      iex> OpenTelemetry.SemanticConventions.Common.log_file_name()
      :"log.file.name"
  """
  @spec log_file_name :: :"log.file.name"
  def log_file_name do
    :"log.file.name"
  end

  @doc namespace: :log
  @doc """
  The basename of the file, with symlinks resolved

      iex> OpenTelemetry.SemanticConventions.Common.log_file_name_resolved()
      :"log.file.name_resolved"
  """
  @spec log_file_name_resolved :: :"log.file.name_resolved"
  def log_file_name_resolved do
    :"log.file.name_resolved"
  end

  @doc namespace: :log
  @doc """
  The full path to the file

      iex> OpenTelemetry.SemanticConventions.Common.log_file_path()
      :"log.file.path"
  """
  @spec log_file_path :: :"log.file.path"
  def log_file_path do
    :"log.file.path"
  end

  @doc namespace: :log
  @doc """
  The full path to the file, with symlinks resolved

      iex> OpenTelemetry.SemanticConventions.Common.log_file_path_resolved()
      :"log.file.path_resolved"
  """
  @spec log_file_path_resolved :: :"log.file.path_resolved"
  def log_file_path_resolved do
    :"log.file.path_resolved"
  end

  @doc namespace: :messaging
  @doc """
  An identifier for the messaging system being used. See below for a list of well-known identifiers

      iex> OpenTelemetry.SemanticConventions.Common.messaging_system()
      :"messaging.system"
  """
  @spec messaging_system :: :"messaging.system"
  def messaging_system do
    :"messaging.system"
  end

  @doc namespace: :pool
  @doc """
  The name of the connection pool; unique within the instrumented application. In case the connection pool implementation doesn't provide a name, instrumentation should use a combination of `server.address` and `server.port` attributes formatted as `server.address:server.port`

      iex> OpenTelemetry.SemanticConventions.Common.pool_name()
      :"pool.name"
  """
  @spec pool_name :: :"pool.name"
  def pool_name do
    :"pool.name"
  end

  @doc namespace: :network
  @doc """
  [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication)

  ### Notes

  The value **SHOULD** be normalized to lowercase.

  Consider always setting the transport when setting a port number, since
  a port number is ambiguous without knowing the transport. For example
  different processes could be listening on TCP port 12345 and UDP port 12345

      iex> OpenTelemetry.SemanticConventions.Common.network_transport()
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

      iex> OpenTelemetry.SemanticConventions.Common.network_type()
      :"network.type"
  """
  @spec network_type :: :"network.type"
  def network_type do
    :"network.type"
  end

  @doc namespace: :messaging
  @doc """
  The message destination name

  ### Notes

  Destination name **SHOULD** uniquely identify a specific queue, topic or other entity within the broker. If
  the broker doesn't have such notion, the destination name **SHOULD** uniquely identify the broker

      iex> OpenTelemetry.SemanticConventions.Common.messaging_destination_name()
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

      iex> OpenTelemetry.SemanticConventions.Common.messaging_destination_template()
      :"messaging.destination.template"
  """
  @spec messaging_destination_template :: :"messaging.destination.template"
  def messaging_destination_template do
    :"messaging.destination.template"
  end

  @doc namespace: :process
  @doc """
  The CPU state for this data point. A process **SHOULD** be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels

      iex> OpenTelemetry.SemanticConventions.Common.process_cpu_state()
      :"process.cpu.state"
  """
  @spec process_cpu_state :: :"process.cpu.state"
  def process_cpu_state do
    :"process.cpu.state"
  end

  @doc namespace: :rpc
  @doc """
  A string identifying the remoting system. See below for a list of well-known identifiers

      iex> OpenTelemetry.SemanticConventions.Common.rpc_system()
      :"rpc.system"
  """
  @spec rpc_system :: :"rpc.system"
  def rpc_system do
    :"rpc.system"
  end

  @doc namespace: :rpc
  @doc """
  The name of the (logical) method being called, must be equal to the $method part in the span name

  ### Notes

  This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side)

      iex> OpenTelemetry.SemanticConventions.Common.rpc_method()
      :"rpc.method"
  """
  @spec rpc_method :: :"rpc.method"
  def rpc_method do
    :"rpc.method"
  end

  @doc namespace: :rpc
  @doc """
  The full (logical) name of the service being called, including its package name, if applicable

  ### Notes

  This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side)

      iex> OpenTelemetry.SemanticConventions.Common.rpc_service()
      :"rpc.service"
  """
  @spec rpc_service :: :"rpc.service"
  def rpc_service do
    :"rpc.service"
  end

  @doc namespace: :system
  @doc """
  The device identifier

      iex> OpenTelemetry.SemanticConventions.Common.system_device()
      :"system.device"
  """
  @spec system_device :: :"system.device"
  def system_device do
    :"system.device"
  end

  @doc namespace: :system
  @doc """
  The logical CPU number [0..n-1]

      iex> OpenTelemetry.SemanticConventions.Common.system_cpu_logical_number()
      :"system.cpu.logical_number"
  """
  @spec system_cpu_logical_number :: :"system.cpu.logical_number"
  def system_cpu_logical_number do
    :"system.cpu.logical_number"
  end

  @doc namespace: :system
  @doc """
  The CPU state for this data point. A system's CPU **SHOULD** be characterized *either* by data points with no `state` labels, *or only* data points with `state` labels

      iex> OpenTelemetry.SemanticConventions.Common.system_cpu_state()
      :"system.cpu.state"
  """
  @spec system_cpu_state :: :"system.cpu.state"
  def system_cpu_state do
    :"system.cpu.state"
  end

  @doc namespace: :system
  @doc """
  The memory state

      iex> OpenTelemetry.SemanticConventions.Common.system_memory_state()
      :"system.memory.state"
  """
  @spec system_memory_state :: :"system.memory.state"
  def system_memory_state do
    :"system.memory.state"
  end

  @doc namespace: :system
  @doc """
  The paging access direction

      iex> OpenTelemetry.SemanticConventions.Common.system_paging_direction()
      :"system.paging.direction"
  """
  @spec system_paging_direction :: :"system.paging.direction"
  def system_paging_direction do
    :"system.paging.direction"
  end

  @doc namespace: :system
  @doc """
  The memory paging state

      iex> OpenTelemetry.SemanticConventions.Common.system_paging_state()
      :"system.paging.state"
  """
  @spec system_paging_state :: :"system.paging.state"
  def system_paging_state do
    :"system.paging.state"
  end

  @doc namespace: :system
  @doc """
  The memory paging type

      iex> OpenTelemetry.SemanticConventions.Common.system_paging_type()
      :"system.paging.type"
  """
  @spec system_paging_type :: :"system.paging.type"
  def system_paging_type do
    :"system.paging.type"
  end

  @doc namespace: :system
  @doc """
  The filesystem mode

      iex> OpenTelemetry.SemanticConventions.Common.system_filesystem_mode()
      :"system.filesystem.mode"
  """
  @spec system_filesystem_mode :: :"system.filesystem.mode"
  def system_filesystem_mode do
    :"system.filesystem.mode"
  end

  @doc namespace: :system
  @doc """
  The filesystem mount path

      iex> OpenTelemetry.SemanticConventions.Common.system_filesystem_mountpoint()
      :"system.filesystem.mountpoint"
  """
  @spec system_filesystem_mountpoint :: :"system.filesystem.mountpoint"
  def system_filesystem_mountpoint do
    :"system.filesystem.mountpoint"
  end

  @doc namespace: :system
  @doc """
  The filesystem state

      iex> OpenTelemetry.SemanticConventions.Common.system_filesystem_state()
      :"system.filesystem.state"
  """
  @spec system_filesystem_state :: :"system.filesystem.state"
  def system_filesystem_state do
    :"system.filesystem.state"
  end

  @doc namespace: :system
  @doc """
  The filesystem type

      iex> OpenTelemetry.SemanticConventions.Common.system_filesystem_type()
      :"system.filesystem.type"
  """
  @spec system_filesystem_type :: :"system.filesystem.type"
  def system_filesystem_type do
    :"system.filesystem.type"
  end

  @doc namespace: :system
  @doc """
  A stateless protocol **MUST NOT** set this attribute

      iex> OpenTelemetry.SemanticConventions.Common.system_network_state()
      :"system.network.state"
  """
  @spec system_network_state :: :"system.network.state"
  def system_network_state do
    :"system.network.state"
  end

  @doc namespace: :system
  @doc """
  The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)

      iex> OpenTelemetry.SemanticConventions.Common.system_process_status()
      :"system.process.status"
  """
  @spec system_process_status :: :"system.process.status"
  def system_process_status do
    :"system.process.status"
  end

  @doc namespace: :network
  @doc """
  Local address of the network connection - IP address or Unix domain socket name

      iex> OpenTelemetry.SemanticConventions.Common.network_local_address()
      :"network.local.address"
  """
  @spec network_local_address :: :"network.local.address"
  def network_local_address do
    :"network.local.address"
  end

  @doc namespace: :network
  @doc """
  Local port number of the network connection

      iex> OpenTelemetry.SemanticConventions.Common.network_local_port()
      :"network.local.port"
  """
  @spec network_local_port :: :"network.local.port"
  def network_local_port do
    :"network.local.port"
  end

  @doc namespace: :network
  @doc """
  Peer address of the network connection - IP address or Unix domain socket name

      iex> OpenTelemetry.SemanticConventions.Common.network_peer_address()
      :"network.peer.address"
  """
  @spec network_peer_address :: :"network.peer.address"
  def network_peer_address do
    :"network.peer.address"
  end

  @doc namespace: :network
  @doc """
  Peer port number of the network connection

      iex> OpenTelemetry.SemanticConventions.Common.network_peer_port()
      :"network.peer.port"
  """
  @spec network_peer_port :: :"network.peer.port"
  def network_peer_port do
    :"network.peer.port"
  end

  @doc namespace: :network
  @doc """
  The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network

      iex> OpenTelemetry.SemanticConventions.Common.network_carrier_icc()
      :"network.carrier.icc"
  """
  @spec network_carrier_icc :: :"network.carrier.icc"
  def network_carrier_icc do
    :"network.carrier.icc"
  end

  @doc namespace: :network
  @doc """
  The mobile carrier country code

      iex> OpenTelemetry.SemanticConventions.Common.network_carrier_mcc()
      :"network.carrier.mcc"
  """
  @spec network_carrier_mcc :: :"network.carrier.mcc"
  def network_carrier_mcc do
    :"network.carrier.mcc"
  end

  @doc namespace: :network
  @doc """
  The mobile carrier network code

      iex> OpenTelemetry.SemanticConventions.Common.network_carrier_mnc()
      :"network.carrier.mnc"
  """
  @spec network_carrier_mnc :: :"network.carrier.mnc"
  def network_carrier_mnc do
    :"network.carrier.mnc"
  end

  @doc namespace: :network
  @doc """
  The name of the mobile carrier

      iex> OpenTelemetry.SemanticConventions.Common.network_carrier_name()
      :"network.carrier.name"
  """
  @spec network_carrier_name :: :"network.carrier.name"
  def network_carrier_name do
    :"network.carrier.name"
  end

  @doc namespace: :network
  @doc """
  This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection

      iex> OpenTelemetry.SemanticConventions.Common.network_connection_subtype()
      :"network.connection.subtype"
  """
  @spec network_connection_subtype :: :"network.connection.subtype"
  def network_connection_subtype do
    :"network.connection.subtype"
  end

  @doc namespace: :network
  @doc """
  The internet connection type

      iex> OpenTelemetry.SemanticConventions.Common.network_connection_type()
      :"network.connection.type"
  """
  @spec network_connection_type :: :"network.connection.type"
  def network_connection_type do
    :"network.connection.type"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of each item in the `AttributeDefinitions` request field

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_attribute_definitions()
      :"aws.dynamodb.attribute_definitions"
  """
  @spec aws_dynamodb_attribute_definitions :: :"aws.dynamodb.attribute_definitions"
  def aws_dynamodb_attribute_definitions do
    :"aws.dynamodb.attribute_definitions"
  end

  @doc namespace: :aws
  @doc """
  The value of the `AttributesToGet` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_attributes_to_get()
      :"aws.dynamodb.attributes_to_get"
  """
  @spec aws_dynamodb_attributes_to_get :: :"aws.dynamodb.attributes_to_get"
  def aws_dynamodb_attributes_to_get do
    :"aws.dynamodb.attributes_to_get"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ConsistentRead` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_consistent_read()
      :"aws.dynamodb.consistent_read"
  """
  @spec aws_dynamodb_consistent_read :: :"aws.dynamodb.consistent_read"
  def aws_dynamodb_consistent_read do
    :"aws.dynamodb.consistent_read"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of each item in the `ConsumedCapacity` response field

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_consumed_capacity()
      :"aws.dynamodb.consumed_capacity"
  """
  @spec aws_dynamodb_consumed_capacity :: :"aws.dynamodb.consumed_capacity"
  def aws_dynamodb_consumed_capacity do
    :"aws.dynamodb.consumed_capacity"
  end

  @doc namespace: :aws
  @doc """
  The value of the `Count` response parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_count()
      :"aws.dynamodb.count"
  """
  @spec aws_dynamodb_count :: :"aws.dynamodb.count"
  def aws_dynamodb_count do
    :"aws.dynamodb.count"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ExclusiveStartTableName` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_exclusive_start_table()
      :"aws.dynamodb.exclusive_start_table"
  """
  @spec aws_dynamodb_exclusive_start_table :: :"aws.dynamodb.exclusive_start_table"
  def aws_dynamodb_exclusive_start_table do
    :"aws.dynamodb.exclusive_start_table"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of each item in the `GlobalSecondaryIndexUpdates` request field

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_global_secondary_index_updates()
      :"aws.dynamodb.global_secondary_index_updates"
  """
  @spec aws_dynamodb_global_secondary_index_updates ::
          :"aws.dynamodb.global_secondary_index_updates"
  def aws_dynamodb_global_secondary_index_updates do
    :"aws.dynamodb.global_secondary_index_updates"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of each item of the `GlobalSecondaryIndexes` request field

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_global_secondary_indexes()
      :"aws.dynamodb.global_secondary_indexes"
  """
  @spec aws_dynamodb_global_secondary_indexes :: :"aws.dynamodb.global_secondary_indexes"
  def aws_dynamodb_global_secondary_indexes do
    :"aws.dynamodb.global_secondary_indexes"
  end

  @doc namespace: :aws
  @doc """
  The value of the `IndexName` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_index_name()
      :"aws.dynamodb.index_name"
  """
  @spec aws_dynamodb_index_name :: :"aws.dynamodb.index_name"
  def aws_dynamodb_index_name do
    :"aws.dynamodb.index_name"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of the `ItemCollectionMetrics` response field

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_item_collection_metrics()
      :"aws.dynamodb.item_collection_metrics"
  """
  @spec aws_dynamodb_item_collection_metrics :: :"aws.dynamodb.item_collection_metrics"
  def aws_dynamodb_item_collection_metrics do
    :"aws.dynamodb.item_collection_metrics"
  end

  @doc namespace: :aws
  @doc """
  The value of the `Limit` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_limit()
      :"aws.dynamodb.limit"
  """
  @spec aws_dynamodb_limit :: :"aws.dynamodb.limit"
  def aws_dynamodb_limit do
    :"aws.dynamodb.limit"
  end

  @doc namespace: :aws
  @doc """
  The JSON-serialized value of each item of the `LocalSecondaryIndexes` request field

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_local_secondary_indexes()
      :"aws.dynamodb.local_secondary_indexes"
  """
  @spec aws_dynamodb_local_secondary_indexes :: :"aws.dynamodb.local_secondary_indexes"
  def aws_dynamodb_local_secondary_indexes do
    :"aws.dynamodb.local_secondary_indexes"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ProjectionExpression` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_projection()
      :"aws.dynamodb.projection"
  """
  @spec aws_dynamodb_projection :: :"aws.dynamodb.projection"
  def aws_dynamodb_projection do
    :"aws.dynamodb.projection"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ProvisionedThroughput.ReadCapacityUnits` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_provisioned_read_capacity()
      :"aws.dynamodb.provisioned_read_capacity"
  """
  @spec aws_dynamodb_provisioned_read_capacity :: :"aws.dynamodb.provisioned_read_capacity"
  def aws_dynamodb_provisioned_read_capacity do
    :"aws.dynamodb.provisioned_read_capacity"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ProvisionedThroughput.WriteCapacityUnits` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_provisioned_write_capacity()
      :"aws.dynamodb.provisioned_write_capacity"
  """
  @spec aws_dynamodb_provisioned_write_capacity :: :"aws.dynamodb.provisioned_write_capacity"
  def aws_dynamodb_provisioned_write_capacity do
    :"aws.dynamodb.provisioned_write_capacity"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ScanIndexForward` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_scan_forward()
      :"aws.dynamodb.scan_forward"
  """
  @spec aws_dynamodb_scan_forward :: :"aws.dynamodb.scan_forward"
  def aws_dynamodb_scan_forward do
    :"aws.dynamodb.scan_forward"
  end

  @doc namespace: :aws
  @doc """
  The value of the `ScannedCount` response parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_scanned_count()
      :"aws.dynamodb.scanned_count"
  """
  @spec aws_dynamodb_scanned_count :: :"aws.dynamodb.scanned_count"
  def aws_dynamodb_scanned_count do
    :"aws.dynamodb.scanned_count"
  end

  @doc namespace: :aws
  @doc """
  The value of the `Segment` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_segment()
      :"aws.dynamodb.segment"
  """
  @spec aws_dynamodb_segment :: :"aws.dynamodb.segment"
  def aws_dynamodb_segment do
    :"aws.dynamodb.segment"
  end

  @doc namespace: :aws
  @doc """
  The value of the `Select` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_select()
      :"aws.dynamodb.select"
  """
  @spec aws_dynamodb_select :: :"aws.dynamodb.select"
  def aws_dynamodb_select do
    :"aws.dynamodb.select"
  end

  @doc namespace: :aws
  @doc """
  The number of items in the `TableNames` response parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_table_count()
      :"aws.dynamodb.table_count"
  """
  @spec aws_dynamodb_table_count :: :"aws.dynamodb.table_count"
  def aws_dynamodb_table_count do
    :"aws.dynamodb.table_count"
  end

  @doc namespace: :aws
  @doc """
  The keys in the `RequestItems` object field

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_table_names()
      :"aws.dynamodb.table_names"
  """
  @spec aws_dynamodb_table_names :: :"aws.dynamodb.table_names"
  def aws_dynamodb_table_names do
    :"aws.dynamodb.table_names"
  end

  @doc namespace: :aws
  @doc """
  The value of the `TotalSegments` request parameter

      iex> OpenTelemetry.SemanticConventions.Common.aws_dynamodb_total_segments()
      :"aws.dynamodb.total_segments"
  """
  @spec aws_dynamodb_total_segments :: :"aws.dynamodb.total_segments"
  def aws_dynamodb_total_segments do
    :"aws.dynamodb.total_segments"
  end

  @doc namespace: :browser
  @doc """
  Array of brand name and version separated by a space

  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.brands`)

      iex> OpenTelemetry.SemanticConventions.Common.browser_brands()
      :"browser.brands"
  """
  @spec browser_brands :: :"browser.brands"
  def browser_brands do
    :"browser.brands"
  end

  @doc namespace: :browser
  @doc """
  Preferred language of the user using the browser

  ### Notes

  This value is intended to be taken from the Navigator API `navigator.language`

      iex> OpenTelemetry.SemanticConventions.Common.browser_language()
      :"browser.language"
  """
  @spec browser_language :: :"browser.language"
  def browser_language do
    :"browser.language"
  end

  @doc namespace: :browser
  @doc """
  A boolean that is true if the browser is running on a mobile device

  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.mobile`). If unavailable, this attribute **SHOULD** be left unset

      iex> OpenTelemetry.SemanticConventions.Common.browser_mobile()
      :"browser.mobile"
  """
  @spec browser_mobile :: :"browser.mobile"
  def browser_mobile do
    :"browser.mobile"
  end

  @doc namespace: :browser
  @doc """
  The platform on which the browser is running

  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.platform`). If unavailable, the legacy `navigator.platform` API **SHOULD NOT** be used instead and this attribute **SHOULD** be left unset in order for the values to be consistent.
  The list of possible values is defined in the [W3C User-Agent Client Hints specification](https://wicg.github.io/ua-client-hints/#sec-ch-ua-platform). Note that some (but not all) of these values can overlap with values in the [`os.type` and `os.name` attributes](./os.md). However, for consistency, the values in the `browser.platform` attribute should capture the exact value that the user agent provides

      iex> OpenTelemetry.SemanticConventions.Common.browser_platform()
      :"browser.platform"
  """
  @spec browser_platform :: :"browser.platform"
  def browser_platform do
    :"browser.platform"
  end

  @doc namespace: :cloudevents
  @doc """
  The [event_id](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#id) uniquely identifies the event

      iex> OpenTelemetry.SemanticConventions.Common.cloudevents_event_id()
      :"cloudevents.event_id"
  """
  @spec cloudevents_event_id :: :"cloudevents.event_id"
  def cloudevents_event_id do
    :"cloudevents.event_id"
  end

  @doc namespace: :cloudevents
  @doc """
  The [source](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#source-1) identifies the context in which an event happened

      iex> OpenTelemetry.SemanticConventions.Common.cloudevents_event_source()
      :"cloudevents.event_source"
  """
  @spec cloudevents_event_source :: :"cloudevents.event_source"
  def cloudevents_event_source do
    :"cloudevents.event_source"
  end

  @doc namespace: :cloudevents
  @doc """
  The [version of the CloudEvents specification](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#specversion) which the event uses

      iex> OpenTelemetry.SemanticConventions.Common.cloudevents_event_spec_version()
      :"cloudevents.event_spec_version"
  """
  @spec cloudevents_event_spec_version :: :"cloudevents.event_spec_version"
  def cloudevents_event_spec_version do
    :"cloudevents.event_spec_version"
  end

  @doc namespace: :cloudevents
  @doc """
  The [subject](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#subject) of the event in the context of the event producer (identified by source)

      iex> OpenTelemetry.SemanticConventions.Common.cloudevents_event_subject()
      :"cloudevents.event_subject"
  """
  @spec cloudevents_event_subject :: :"cloudevents.event_subject"
  def cloudevents_event_subject do
    :"cloudevents.event_subject"
  end

  @doc namespace: :cloudevents
  @doc """
  The [event_type](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#type) contains a value describing the type of event related to the originating occurrence

      iex> OpenTelemetry.SemanticConventions.Common.cloudevents_event_type()
      :"cloudevents.event_type"
  """
  @spec cloudevents_event_type :: :"cloudevents.event_type"
  def cloudevents_event_type do
    :"cloudevents.event_type"
  end

  @doc namespace: :code
  @doc """
  The column number in `code.filepath` best representing the operation. It **SHOULD** point within the code unit named in `code.function`

      iex> OpenTelemetry.SemanticConventions.Common.code_column()
      :"code.column"
  """
  @spec code_column :: :"code.column"
  def code_column do
    :"code.column"
  end

  @doc namespace: :code
  @doc """
  The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path)

      iex> OpenTelemetry.SemanticConventions.Common.code_filepath()
      :"code.filepath"
  """
  @spec code_filepath :: :"code.filepath"
  def code_filepath do
    :"code.filepath"
  end

  @doc namespace: :code
  @doc """
  The method or function name, or equivalent (usually rightmost part of the code unit's name)

      iex> OpenTelemetry.SemanticConventions.Common.code_function()
      :"code.function"
  """
  @spec code_function :: :"code.function"
  def code_function do
    :"code.function"
  end

  @doc namespace: :code
  @doc """
  The line number in `code.filepath` best representing the operation. It **SHOULD** point within the code unit named in `code.function`

      iex> OpenTelemetry.SemanticConventions.Common.code_lineno()
      :"code.lineno"
  """
  @spec code_lineno :: :"code.lineno"
  def code_lineno do
    :"code.lineno"
  end

  @doc namespace: :code
  @doc """
  The "namespace" within which `code.function` is defined. Usually the qualified class or module name, such that `code.namespace` + some separator + `code.function` form a unique identifier for the code unit

      iex> OpenTelemetry.SemanticConventions.Common.code_namespace()
      :"code.namespace"
  """
  @spec code_namespace :: :"code.namespace"
  def code_namespace do
    :"code.namespace"
  end

  @doc namespace: :code
  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG

      iex> OpenTelemetry.SemanticConventions.Common.code_stacktrace()
      :"code.stacktrace"
  """
  @spec code_stacktrace :: :"code.stacktrace"
  def code_stacktrace do
    :"code.stacktrace"
  end

  @doc namespace: :container
  @doc """
  The command used to run the container (i.e. the command name)

  ### Notes

  If using embedded credentials or sensitive data, it is recommended to remove them to prevent potential leakage

      iex> OpenTelemetry.SemanticConventions.Common.container_command()
      :"container.command"
  """
  @spec container_command :: :"container.command"
  def container_command do
    :"container.command"
  end

  @doc namespace: :container
  @doc """
  All the command arguments (including the command/executable itself) run by the container. [2]

      iex> OpenTelemetry.SemanticConventions.Common.container_command_args()
      :"container.command_args"
  """
  @spec container_command_args :: :"container.command_args"
  def container_command_args do
    :"container.command_args"
  end

  @doc namespace: :container
  @doc """
  The full command run by the container as a single string representing the full command. [2]

      iex> OpenTelemetry.SemanticConventions.Common.container_command_line()
      :"container.command_line"
  """
  @spec container_command_line :: :"container.command_line"
  def container_command_line do
    :"container.command_line"
  end

  @doc namespace: :container
  @doc """
  The CPU state for this data point

      iex> OpenTelemetry.SemanticConventions.Common.container_cpu_state()
      :"container.cpu.state"
  """
  @spec container_cpu_state :: :"container.cpu.state"
  def container_cpu_state do
    :"container.cpu.state"
  end

  @doc namespace: :container
  @doc """
  Container ID. Usually a UUID, as for example used to [identify Docker containers](https://docs.docker.com/engine/reference/run/#container-identification). The UUID might be abbreviated

      iex> OpenTelemetry.SemanticConventions.Common.container_id()
      :"container.id"
  """
  @spec container_id :: :"container.id"
  def container_id do
    :"container.id"
  end

  @doc namespace: :container
  @doc """
  Runtime specific image identifier. Usually a hash algorithm followed by a UUID

  ### Notes

  Docker defines a sha256 of the image id; `container.image.id` corresponds to the `Image` field from the Docker container inspect [API](https://docs.docker.com/engine/api/v1.43/#tag/Container/operation/ContainerInspect) endpoint.
  K8s defines a link to the container registry repository with digest `"imageID": "registry.azurecr.io /namespace/service/dockerfile@sha256:bdeabd40c3a8a492eaf9e8e44d0ebbb84bac7ee25ac0cf8a7159d25f62555625"`.
  The ID is assinged by the container runtime and can vary in different environments. Consider using `oci.manifest.digest` if it is important to identify the same image in different environments/runtimes

      iex> OpenTelemetry.SemanticConventions.Common.container_image_id()
      :"container.image.id"
  """
  @spec container_image_id :: :"container.image.id"
  def container_image_id do
    :"container.image.id"
  end

  @doc namespace: :container
  @doc """
  Name of the image the container was built on

      iex> OpenTelemetry.SemanticConventions.Common.container_image_name()
      :"container.image.name"
  """
  @spec container_image_name :: :"container.image.name"
  def container_image_name do
    :"container.image.name"
  end

  @doc namespace: :container
  @doc """
  Repo digests of the container image as provided by the container runtime

  ### Notes

  [Docker](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect) and [CRI](https://github.com/kubernetes/cri-api/blob/c75ef5b473bbe2d0a4fc92f82235efd665ea8e9f/pkg/apis/runtime/v1/api.proto#L1237-L1238) report those under the `RepoDigests` field

      iex> OpenTelemetry.SemanticConventions.Common.container_image_repo_digests()
      :"container.image.repo_digests"
  """
  @spec container_image_repo_digests :: :"container.image.repo_digests"
  def container_image_repo_digests do
    :"container.image.repo_digests"
  end

  @doc namespace: :container
  @doc """
  Container image tags. An example can be found in [Docker Image Inspect](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect). Should be only the `<tag>` section of the full name for example from `registry.example.com/my-org/my-image:<tag>`

      iex> OpenTelemetry.SemanticConventions.Common.container_image_tags()
      :"container.image.tags"
  """
  @spec container_image_tags :: :"container.image.tags"
  def container_image_tags do
    :"container.image.tags"
  end

  @doc namespace: :container
  @doc """
  Container name used by container runtime

      iex> OpenTelemetry.SemanticConventions.Common.container_name()
      :"container.name"
  """
  @spec container_name :: :"container.name"
  def container_name do
    :"container.name"
  end

  @doc namespace: :container
  @doc """
  The container runtime managing this container

      iex> OpenTelemetry.SemanticConventions.Common.container_runtime()
      :"container.runtime"
  """
  @spec container_runtime :: :"container.runtime"
  def container_runtime do
    :"container.runtime"
  end

  @doc namespace: :db
  @doc """
  The consistency level of the query. Based on consistency values from [CQL](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html)

      iex> OpenTelemetry.SemanticConventions.Common.db_cassandra_consistency_level()
      :"db.cassandra.consistency_level"
  """
  @spec db_cassandra_consistency_level :: :"db.cassandra.consistency_level"
  def db_cassandra_consistency_level do
    :"db.cassandra.consistency_level"
  end

  @doc namespace: :db
  @doc """
  The data center of the coordinating node for a query

      iex> OpenTelemetry.SemanticConventions.Common.db_cassandra_coordinator_dc()
      :"db.cassandra.coordinator.dc"
  """
  @spec db_cassandra_coordinator_dc :: :"db.cassandra.coordinator.dc"
  def db_cassandra_coordinator_dc do
    :"db.cassandra.coordinator.dc"
  end

  @doc namespace: :db
  @doc """
  The ID of the coordinating node for a query

      iex> OpenTelemetry.SemanticConventions.Common.db_cassandra_coordinator_id()
      :"db.cassandra.coordinator.id"
  """
  @spec db_cassandra_coordinator_id :: :"db.cassandra.coordinator.id"
  def db_cassandra_coordinator_id do
    :"db.cassandra.coordinator.id"
  end

  @doc namespace: :db
  @doc """
  Whether or not the query is idempotent

      iex> OpenTelemetry.SemanticConventions.Common.db_cassandra_idempotence()
      :"db.cassandra.idempotence"
  """
  @spec db_cassandra_idempotence :: :"db.cassandra.idempotence"
  def db_cassandra_idempotence do
    :"db.cassandra.idempotence"
  end

  @doc namespace: :db
  @doc """
  The fetch size used for paging, i.e. how many rows will be returned at once

      iex> OpenTelemetry.SemanticConventions.Common.db_cassandra_page_size()
      :"db.cassandra.page_size"
  """
  @spec db_cassandra_page_size :: :"db.cassandra.page_size"
  def db_cassandra_page_size do
    :"db.cassandra.page_size"
  end

  @doc namespace: :db
  @doc """
  The number of times a query was speculatively executed. Not set or `0` if the query was not executed speculatively

      iex> OpenTelemetry.SemanticConventions.Common.db_cassandra_speculative_execution_count()
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

      iex> OpenTelemetry.SemanticConventions.Common.db_cassandra_table()
      :"db.cassandra.table"
  """
  @spec db_cassandra_table :: :"db.cassandra.table"
  def db_cassandra_table do
    :"db.cassandra.table"
  end

  @doc namespace: :db
  @doc """
  Unique Cosmos client instance id

      iex> OpenTelemetry.SemanticConventions.Common.db_cosmosdb_client_id()
      :"db.cosmosdb.client_id"
  """
  @spec db_cosmosdb_client_id :: :"db.cosmosdb.client_id"
  def db_cosmosdb_client_id do
    :"db.cosmosdb.client_id"
  end

  @doc namespace: :db
  @doc """
  Cosmos client connection mode

      iex> OpenTelemetry.SemanticConventions.Common.db_cosmosdb_connection_mode()
      :"db.cosmosdb.connection_mode"
  """
  @spec db_cosmosdb_connection_mode :: :"db.cosmosdb.connection_mode"
  def db_cosmosdb_connection_mode do
    :"db.cosmosdb.connection_mode"
  end

  @doc namespace: :db
  @doc """
  Cosmos DB container name

      iex> OpenTelemetry.SemanticConventions.Common.db_cosmosdb_container()
      :"db.cosmosdb.container"
  """
  @spec db_cosmosdb_container :: :"db.cosmosdb.container"
  def db_cosmosdb_container do
    :"db.cosmosdb.container"
  end

  @doc namespace: :db
  @doc """
  CosmosDB Operation Type

      iex> OpenTelemetry.SemanticConventions.Common.db_cosmosdb_operation_type()
      :"db.cosmosdb.operation_type"
  """
  @spec db_cosmosdb_operation_type :: :"db.cosmosdb.operation_type"
  def db_cosmosdb_operation_type do
    :"db.cosmosdb.operation_type"
  end

  @doc namespace: :db
  @doc """
  RU consumed for that operation

      iex> OpenTelemetry.SemanticConventions.Common.db_cosmosdb_request_charge()
      :"db.cosmosdb.request_charge"
  """
  @spec db_cosmosdb_request_charge :: :"db.cosmosdb.request_charge"
  def db_cosmosdb_request_charge do
    :"db.cosmosdb.request_charge"
  end

  @doc namespace: :db
  @doc """
  Request payload size in bytes

      iex> OpenTelemetry.SemanticConventions.Common.db_cosmosdb_request_content_length()
      :"db.cosmosdb.request_content_length"
  """
  @spec db_cosmosdb_request_content_length :: :"db.cosmosdb.request_content_length"
  def db_cosmosdb_request_content_length do
    :"db.cosmosdb.request_content_length"
  end

  @doc namespace: :db
  @doc """
  Cosmos DB status code

      iex> OpenTelemetry.SemanticConventions.Common.db_cosmosdb_status_code()
      :"db.cosmosdb.status_code"
  """
  @spec db_cosmosdb_status_code :: :"db.cosmosdb.status_code"
  def db_cosmosdb_status_code do
    :"db.cosmosdb.status_code"
  end

  @doc namespace: :db
  @doc """
  Cosmos DB sub status code

      iex> OpenTelemetry.SemanticConventions.Common.db_cosmosdb_sub_status_code()
      :"db.cosmosdb.sub_status_code"
  """
  @spec db_cosmosdb_sub_status_code :: :"db.cosmosdb.sub_status_code"
  def db_cosmosdb_sub_status_code do
    :"db.cosmosdb.sub_status_code"
  end

  @doc namespace: :db
  @doc """
  Represents the identifier of an Elasticsearch cluster

      iex> OpenTelemetry.SemanticConventions.Common.db_elasticsearch_cluster_name()
      :"db.elasticsearch.cluster.name"
  """
  @spec db_elasticsearch_cluster_name :: :"db.elasticsearch.cluster.name"
  def db_elasticsearch_cluster_name do
    :"db.elasticsearch.cluster.name"
  end

  @doc namespace: :db
  @doc """
  An identifier (address, unique name, or any other identifier) of the database instance that is executing queries or mutations on the current connection. This is useful in cases where the database is running in a clustered environment and the instrumentation is able to record the node executing the query. The client may obtain this value in databases like MySQL using queries like `select @@hostname`

      iex> OpenTelemetry.SemanticConventions.Common.db_instance_id()
      :"db.instance.id"
  """
  @spec db_instance_id :: :"db.instance.id"
  def db_instance_id do
    :"db.instance.id"
  end

  @doc namespace: :db
  @doc """
  The MongoDB collection being accessed within the database stated in `db.name`

      iex> OpenTelemetry.SemanticConventions.Common.db_mongodb_collection()
      :"db.mongodb.collection"
  """
  @spec db_mongodb_collection :: :"db.mongodb.collection"
  def db_mongodb_collection do
    :"db.mongodb.collection"
  end

  @doc namespace: :db
  @doc """
  The Microsoft SQL Server [instance name](https://docs.microsoft.com/sql/connect/jdbc/building-the-connection-url?view=sql-server-ver15) connecting to. This name is used to determine the port of a named instance

  ### Notes

  If setting a `db.mssql.instance_name`, `server.port` is no longer required (but still recommended if non-standard)

      iex> OpenTelemetry.SemanticConventions.Common.db_mssql_instance_name()
      :"db.mssql.instance_name"
  """
  @spec db_mssql_instance_name :: :"db.mssql.instance_name"
  def db_mssql_instance_name do
    :"db.mssql.instance_name"
  end

  @doc namespace: :db
  @doc """
  This attribute is used to report the name of the database being accessed. For commands that switch the database, this should be set to the target database (even if the command fails)

  ### Notes

  In some SQL databases, the database name to be used is called "schema name". In case there are multiple layers that could be considered for database name (e.g. Oracle instance name and schema name), the database name to be used is the more specific layer (e.g. Oracle schema name)

      iex> OpenTelemetry.SemanticConventions.Common.db_name()
      :"db.name"
  """
  @spec db_name :: :"db.name"
  def db_name do
    :"db.name"
  end

  @doc namespace: :db
  @doc """
  The name of the operation being executed, e.g. the [MongoDB command name](https://docs.mongodb.com/manual/reference/command/#database-operations) such as `findAndModify`, or the SQL keyword

  ### Notes

  When setting this to an SQL keyword, it is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if the operation name is provided by the library being instrumented. If the SQL statement has an ambiguous operation, or performs more than one operation, this value may be omitted

      iex> OpenTelemetry.SemanticConventions.Common.db_operation()
      :"db.operation"
  """
  @spec db_operation :: :"db.operation"
  def db_operation do
    :"db.operation"
  end

  @doc namespace: :db
  @doc """
  The index of the database being accessed as used in the [`SELECT` command](https://redis.io/commands/select), provided as an integer. To be used instead of the generic `db.name` attribute

      iex> OpenTelemetry.SemanticConventions.Common.db_redis_database_index()
      :"db.redis.database_index"
  """
  @spec db_redis_database_index :: :"db.redis.database_index"
  def db_redis_database_index do
    :"db.redis.database_index"
  end

  @doc namespace: :db
  @doc """
  The name of the primary table that the operation is acting upon, including the database name (if applicable)

  ### Notes

  It is not recommended to attempt any client-side parsing of `db.statement` just to get this property, but it should be set if it is provided by the library being instrumented. If the operation is acting upon an anonymous table, or more than one table, this value **MUST NOT** be set

      iex> OpenTelemetry.SemanticConventions.Common.db_sql_table()
      :"db.sql.table"
  """
  @spec db_sql_table :: :"db.sql.table"
  def db_sql_table do
    :"db.sql.table"
  end

  @doc namespace: :db
  @doc """
  The database statement being executed

      iex> OpenTelemetry.SemanticConventions.Common.db_statement()
      :"db.statement"
  """
  @spec db_statement :: :"db.statement"
  def db_statement do
    :"db.statement"
  end

  @doc namespace: :db
  @doc """
  An identifier for the database management system (DBMS) product being used. See below for a list of well-known identifiers

      iex> OpenTelemetry.SemanticConventions.Common.db_system()
      :"db.system"
  """
  @spec db_system :: :"db.system"
  def db_system do
    :"db.system"
  end

  @doc namespace: :db
  @doc """
  Username for accessing the database

      iex> OpenTelemetry.SemanticConventions.Common.db_user()
      :"db.user"
  """
  @spec db_user :: :"db.user"
  def db_user do
    :"db.user"
  end

  @doc namespace: :deployment
  @doc """
  Name of the [deployment environment](https://wikipedia.org/wiki/Deployment_environment) (aka deployment tier)

  ### Notes

  `deployment.environment` does not affect the uniqueness constraints defined through
  the `service.namespace`, `service.name` and `service.instance.id` resource attributes.
  This implies that resources carrying the following attribute combinations **MUST** be
  considered to be identifying the same service:

  * `service.name=frontend`, `deployment.environment=production`
  * `service.name=frontend`, `deployment.environment=staging`

      iex> OpenTelemetry.SemanticConventions.Common.deployment_environment()
      :"deployment.environment"
  """
  @spec deployment_environment :: :"deployment.environment"
  def deployment_environment do
    :"deployment.environment"
  end

  @doc namespace: :db

  @deprecated """
  "Replaced by `server.address` and `server.port`."
  """
  @spec db_connection_string :: :"db.connection_string"
  def db_connection_string do
    :"db.connection_string"
  end

  @doc namespace: :db

  @deprecated """
  Replaced by `db.instance.id`
  """
  @spec db_elasticsearch_node_name :: :"db.elasticsearch.node.name"
  def db_elasticsearch_node_name do
    :"db.elasticsearch.node.name"
  end

  @doc namespace: :db

  @deprecated """
  Removed as not used
  """
  @spec db_jdbc_driver_classname :: :"db.jdbc.driver_classname"
  def db_jdbc_driver_classname do
    :"db.jdbc.driver_classname"
  end

  @doc namespace: :http

  @deprecated """
  Replaced by `network.protocol.name`
  """
  @spec http_flavor :: :"http.flavor"
  def http_flavor do
    :"http.flavor"
  end

  @doc namespace: :http

  @deprecated """
  Replaced by `http.request.method`
  """
  @spec http_method :: :"http.method"
  def http_method do
    :"http.method"
  end

  @doc namespace: :http

  @deprecated """
  Replaced by `http.request.header.content-length`
  """
  @spec http_request_content_length :: :"http.request_content_length"
  def http_request_content_length do
    :"http.request_content_length"
  end

  @doc namespace: :http

  @deprecated """
  Replaced by `http.response.header.content-length`
  """
  @spec http_response_content_length :: :"http.response_content_length"
  def http_response_content_length do
    :"http.response_content_length"
  end

  @doc namespace: :http

  @deprecated """
  Replaced by `url.scheme` instead
  """
  @spec http_scheme :: :"http.scheme"
  def http_scheme do
    :"http.scheme"
  end

  @doc namespace: :http

  @deprecated """
  Replaced by `http.response.status_code`
  """
  @spec http_status_code :: :"http.status_code"
  def http_status_code do
    :"http.status_code"
  end

  @doc namespace: :http

  @deprecated """
  Split to `url.path` and `url.query
  """
  @spec http_target :: :"http.target"
  def http_target do
    :"http.target"
  end

  @doc namespace: :http

  @deprecated """
  Replaced by `url.full`
  """
  @spec http_url :: :"http.url"
  def http_url do
    :"http.url"
  end

  @doc namespace: :http

  @deprecated """
  Replaced by `user_agent.original`
  """
  @spec http_user_agent :: :"http.user_agent"
  def http_user_agent do
    :"http.user_agent"
  end

  @doc namespace: :messaging

  @deprecated """
  Replaced by `messaging.destination.partition.id`
  """
  @spec messaging_kafka_destination_partition :: :"messaging.kafka.destination.partition"
  def messaging_kafka_destination_partition do
    :"messaging.kafka.destination.partition"
  end

  @doc namespace: :system

  @deprecated """
  Replaced by `system.process.status`
  """
  @spec system_processes_status :: :"system.processes.status"
  def system_processes_status do
    :"system.processes.status"
  end

  @doc namespace: :device
  @doc """
  A unique identifier representing the device

  ### Notes

  The device identifier **MUST** only be defined using the values outlined below. This value is not an advertising identifier and **MUST NOT** be used as such. On iOS (Swift or Objective-C), this value **MUST** be equal to the [vendor identifier](https://developer.apple.com/documentation/uikit/uidevice/1620059-identifierforvendor). On Android (Java or Kotlin), this value **MUST** be equal to the Firebase Installation ID or a globally unique UUID which is persisted across sessions in your application. More information can be found [here](https://developer.android.com/training/articles/user-data-ids) on best practices and exact implementation details. Caution should be taken when storing personal data or anything which can identify a user. GDPR and data protection laws may apply, ensure you do your own due diligence

      iex> OpenTelemetry.SemanticConventions.Common.device_id()
      :"device.id"
  """
  @spec device_id :: :"device.id"
  def device_id do
    :"device.id"
  end

  @doc namespace: :device
  @doc """
  The name of the device manufacturer

  ### Notes

  The Android OS provides this field via [Build](https://developer.android.com/reference/android/os/Build#MANUFACTURER). iOS apps **SHOULD** hardcode the value `Apple`

      iex> OpenTelemetry.SemanticConventions.Common.device_manufacturer()
      :"device.manufacturer"
  """
  @spec device_manufacturer :: :"device.manufacturer"
  def device_manufacturer do
    :"device.manufacturer"
  end

  @doc namespace: :device
  @doc """
  The model identifier for the device

  ### Notes

  It's recommended this value represents a machine-readable version of the model identifier rather than the market or consumer-friendly name of the device

      iex> OpenTelemetry.SemanticConventions.Common.device_model_identifier()
      :"device.model.identifier"
  """
  @spec device_model_identifier :: :"device.model.identifier"
  def device_model_identifier do
    :"device.model.identifier"
  end

  @doc namespace: :device
  @doc """
  The marketing name for the device model

  ### Notes

  It's recommended this value represents a human-readable version of the device model rather than a machine-readable alternative

      iex> OpenTelemetry.SemanticConventions.Common.device_model_name()
      :"device.model.name"
  """
  @spec device_model_name :: :"device.model.name"
  def device_model_name do
    :"device.model.name"
  end

  @doc namespace: :disk
  @doc """
  The disk IO operation direction

      iex> OpenTelemetry.SemanticConventions.Common.disk_io_direction()
      :"disk.io.direction"
  """
  @spec disk_io_direction :: :"disk.io.direction"
  def disk_io_direction do
    :"disk.io.direction"
  end

  @doc namespace: :dns
  @doc """
  The name being queried

  ### Notes

  If the name field contains non-printable characters (below 32 or above 126), those characters should be represented as escaped base 10 integers (\DDD). Back slashes and quotes should be escaped. Tabs, carriage returns, and line feeds should be converted to \t, \r, and \n respectively

      iex> OpenTelemetry.SemanticConventions.Common.dns_question_name()
      :"dns.question.name"
  """
  @spec dns_question_name :: :"dns.question.name"
  def dns_question_name do
    :"dns.question.name"
  end

  @doc namespace: :enduser
  @doc """
  Username or client_id extracted from the access token or [Authorization](https://tools.ietf.org/html/rfc7235#section-4.2) header in the inbound request from outside the system

      iex> OpenTelemetry.SemanticConventions.Common.enduser_id()
      :"enduser.id"
  """
  @spec enduser_id :: :"enduser.id"
  def enduser_id do
    :"enduser.id"
  end

  @doc namespace: :enduser
  @doc """
  Actual/assumed role the client is making the request under extracted from token or application security context

      iex> OpenTelemetry.SemanticConventions.Common.enduser_role()
      :"enduser.role"
  """
  @spec enduser_role :: :"enduser.role"
  def enduser_role do
    :"enduser.role"
  end

  @doc namespace: :enduser
  @doc """
  Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an [OAuth 2.0 Access Token](https://tools.ietf.org/html/rfc6749#section-3.3) or an attribute value in a [SAML 2.0 Assertion](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html)

      iex> OpenTelemetry.SemanticConventions.Common.enduser_scope()
      :"enduser.scope"
  """
  @spec enduser_scope :: :"enduser.scope"
  def enduser_scope do
    :"enduser.scope"
  end

  @doc namespace: :exception
  @doc """
  **SHOULD** be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span

  ### Notes

  An exception is considered to have escaped (or left) the scope of a span,
  if that span is ended while the exception is still logically "in flight".
  This may be actually "in flight" in some languages (e.g. if the exception
  is passed to a Context manager's `__exit__` method in Python) but will
  usually be caught at the point of recording the exception in most languages.

  It is usually not possible to determine at the point where an exception is thrown
  whether it will escape the scope of a span.
  However, it is trivial to know that an exception
  will escape, if one checks for an active exception just before ending the span,
  as done in the [example for recording span exceptions](#recording-an-exception).

  It follows that an exception may still escape the scope of the span
  even if the `exception.escaped` attribute was not set or set to false,
  since the event might have been recorded at a time where it was not
  clear whether the exception will escape

      iex> OpenTelemetry.SemanticConventions.Common.exception_escaped()
      :"exception.escaped"
  """
  @spec exception_escaped :: :"exception.escaped"
  def exception_escaped do
    :"exception.escaped"
  end

  @doc namespace: :faas
  @doc """
  A boolean that is true if the serverless function is executed for the first time (aka cold-start)

      iex> OpenTelemetry.SemanticConventions.Common.faas_coldstart()
      :"faas.coldstart"
  """
  @spec faas_coldstart :: :"faas.coldstart"
  def faas_coldstart do
    :"faas.coldstart"
  end

  @doc namespace: :faas
  @doc """
  A string containing the schedule period as [Cron Expression](https://docs.oracle.com/cd/E12058_01/doc/doc.1014/e12030/cron_expressions.htm)

      iex> OpenTelemetry.SemanticConventions.Common.faas_cron()
      :"faas.cron"
  """
  @spec faas_cron :: :"faas.cron"
  def faas_cron do
    :"faas.cron"
  end

  @doc namespace: :faas
  @doc """
  The name of the source on which the triggering operation was performed. For example, in Cloud Storage or S3 corresponds to the bucket name, and in Cosmos DB to the database name

      iex> OpenTelemetry.SemanticConventions.Common.faas_document_collection()
      :"faas.document.collection"
  """
  @spec faas_document_collection :: :"faas.document.collection"
  def faas_document_collection do
    :"faas.document.collection"
  end

  @doc namespace: :faas
  @doc """
  The document name/table subjected to the operation. For example, in Cloud Storage or S3 is the name of the file, and in Cosmos DB the table name

      iex> OpenTelemetry.SemanticConventions.Common.faas_document_name()
      :"faas.document.name"
  """
  @spec faas_document_name :: :"faas.document.name"
  def faas_document_name do
    :"faas.document.name"
  end

  @doc namespace: :faas
  @doc """
  Describes the type of the operation that was performed on the data

      iex> OpenTelemetry.SemanticConventions.Common.faas_document_operation()
      :"faas.document.operation"
  """
  @spec faas_document_operation :: :"faas.document.operation"
  def faas_document_operation do
    :"faas.document.operation"
  end

  @doc namespace: :faas
  @doc """
  A string containing the time when the data was accessed in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)

      iex> OpenTelemetry.SemanticConventions.Common.faas_document_time()
      :"faas.document.time"
  """
  @spec faas_document_time :: :"faas.document.time"
  def faas_document_time do
    :"faas.document.time"
  end

  @doc namespace: :faas
  @doc """
  The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version

  ### Notes

  * **AWS Lambda:** Use the (full) log stream name

      iex> OpenTelemetry.SemanticConventions.Common.faas_instance()
      :"faas.instance"
  """
  @spec faas_instance :: :"faas.instance"
  def faas_instance do
    :"faas.instance"
  end

  @doc namespace: :faas
  @doc """
  The invocation ID of the current function invocation

      iex> OpenTelemetry.SemanticConventions.Common.faas_invocation_id()
      :"faas.invocation_id"
  """
  @spec faas_invocation_id :: :"faas.invocation_id"
  def faas_invocation_id do
    :"faas.invocation_id"
  end

  @doc namespace: :faas
  @doc """
  The amount of memory available to the serverless function converted to Bytes

  ### Notes

  It's recommended to set this attribute since e.g. too little memory can easily stop a Java AWS Lambda function from working correctly. On AWS Lambda, the environment variable `AWS_LAMBDA_FUNCTION_MEMORY_SIZE` provides this information (which must be multiplied by 1,048,576)

      iex> OpenTelemetry.SemanticConventions.Common.faas_max_memory()
      :"faas.max_memory"
  """
  @spec faas_max_memory :: :"faas.max_memory"
  def faas_max_memory do
    :"faas.max_memory"
  end

  @doc namespace: :faas
  @doc """
  The name of the single function that this runtime instance executes

  ### Notes

  This is the name of the function as configured/deployed on the FaaS
  platform and is usually different from the name of the callback
  function (which may be stored in the
  [`code.namespace`/`code.function`](/docs/general/attributes.md#source-code-attributes)
  span attributes).

  For some cloud providers, the above definition is ambiguous. The following
  definition of function name **MUST** be used for this attribute
  (and consequently the span name) for the listed cloud providers/products:

  * **Azure:**  The full name `<FUNCAPP>/<FUNC>`, i.e., function app name
    followed by a forward slash followed by the function name (this form
    can also be seen in the resource JSON for the function).
    This means that a span attribute **MUST** be used, as an Azure function
    app can host multiple functions that would usually share
    a TracerProvider (see also the `cloud.resource_id` attribute)

      iex> OpenTelemetry.SemanticConventions.Common.faas_name()
      :"faas.name"
  """
  @spec faas_name :: :"faas.name"
  def faas_name do
    :"faas.name"
  end

  @doc namespace: :faas
  @doc """
  A string containing the function invocation time in the [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) format expressed in [UTC](https://www.w3.org/TR/NOTE-datetime)

      iex> OpenTelemetry.SemanticConventions.Common.faas_time()
      :"faas.time"
  """
  @spec faas_time :: :"faas.time"
  def faas_time do
    :"faas.time"
  end

  @doc namespace: :faas
  @doc """
  The immutable version of the function being executed

  ### Notes

  Depending on the cloud provider and platform, use:

  * **AWS Lambda:** The [function version](https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html)
    (an integer represented as a decimal string).
  * **Google Cloud Run (Services):** The [revision](https://cloud.google.com/run/docs/managing/revisions)
    (i.e., the function name plus the revision suffix).
  * **Google Cloud Functions:** The value of the
    [`K_REVISION` environment variable](https://cloud.google.com/functions/docs/env-var#runtime_environment_variables_set_automatically).
  * **Azure Functions:** Not applicable. Do not set this attribute

      iex> OpenTelemetry.SemanticConventions.Common.faas_version()
      :"faas.version"
  """
  @spec faas_version :: :"faas.version"
  def faas_version do
    :"faas.version"
  end

  @doc namespace: :feature_flag
  @doc """
  The unique identifier of the feature flag

      iex> OpenTelemetry.SemanticConventions.Common.feature_flag_key()
      :"feature_flag.key"
  """
  @spec feature_flag_key :: :"feature_flag.key"
  def feature_flag_key do
    :"feature_flag.key"
  end

  @doc namespace: :feature_flag
  @doc """
  The name of the service provider that performs the flag evaluation

      iex> OpenTelemetry.SemanticConventions.Common.feature_flag_provider_name()
      :"feature_flag.provider_name"
  """
  @spec feature_flag_provider_name :: :"feature_flag.provider_name"
  def feature_flag_provider_name do
    :"feature_flag.provider_name"
  end

  @doc namespace: :feature_flag
  @doc """
  **SHOULD** be a semantic identifier for a value. If one is unavailable, a stringified version of the value can be used

  ### Notes

  A semantic identifier, commonly referred to as a variant, provides a means
  for referring to a value without including the value itself. This can
  provide additional context for understanding the meaning behind a value.
  For example, the variant `red` maybe be used for the value `#c05543`.

  A stringified version of the value can be used in situations where a
  semantic identifier is unavailable. String representation of the value
  should be determined by the implementer

      iex> OpenTelemetry.SemanticConventions.Common.feature_flag_variant()
      :"feature_flag.variant"
  """
  @spec feature_flag_variant :: :"feature_flag.variant"
  def feature_flag_variant do
    :"feature_flag.variant"
  end

  @doc namespace: :file
  @doc """
  Directory where the file is located. It should include the drive letter, when appropriate

      iex> OpenTelemetry.SemanticConventions.Common.file_directory()
      :"file.directory"
  """
  @spec file_directory :: :"file.directory"
  def file_directory do
    :"file.directory"
  end

  @doc namespace: :file
  @doc """
  File extension, excluding the leading dot

  ### Notes

  When the file name has multiple extensions (example.tar.gz), only the last one should be captured ("gz", not "tar.gz")

      iex> OpenTelemetry.SemanticConventions.Common.file_extension()
      :"file.extension"
  """
  @spec file_extension :: :"file.extension"
  def file_extension do
    :"file.extension"
  end

  @doc namespace: :file
  @doc """
  Name of the file including the extension, without the directory

      iex> OpenTelemetry.SemanticConventions.Common.file_name()
      :"file.name"
  """
  @spec file_name :: :"file.name"
  def file_name do
    :"file.name"
  end

  @doc namespace: :file
  @doc """
  Full path to the file, including the file name. It should include the drive letter, when appropriate

      iex> OpenTelemetry.SemanticConventions.Common.file_path()
      :"file.path"
  """
  @spec file_path :: :"file.path"
  def file_path do
    :"file.path"
  end

  @doc namespace: :file
  @doc """
  File size in bytes

      iex> OpenTelemetry.SemanticConventions.Common.file_size()
      :"file.size"
  """
  @spec file_size :: :"file.size"
  def file_size do
    :"file.size"
  end

  @doc namespace: :gcp
  @doc """
  The name of the Cloud Run [execution](https://cloud.google.com/run/docs/managing/job-executions) being run for the Job, as set by the [`CLOUD_RUN_EXECUTION`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable

      iex> OpenTelemetry.SemanticConventions.Common.gcp_cloud_run_job_execution()
      :"gcp.cloud_run.job.execution"
  """
  @spec gcp_cloud_run_job_execution :: :"gcp.cloud_run.job.execution"
  def gcp_cloud_run_job_execution do
    :"gcp.cloud_run.job.execution"
  end

  @doc namespace: :gcp
  @doc """
  The index for a task within an execution as provided by the [`CLOUD_RUN_TASK_INDEX`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable

      iex> OpenTelemetry.SemanticConventions.Common.gcp_cloud_run_job_task_index()
      :"gcp.cloud_run.job.task_index"
  """
  @spec gcp_cloud_run_job_task_index :: :"gcp.cloud_run.job.task_index"
  def gcp_cloud_run_job_task_index do
    :"gcp.cloud_run.job.task_index"
  end

  @doc namespace: :gcp
  @doc """
  The hostname of a GCE instance. This is the full value of the default or [custom hostname](https://cloud.google.com/compute/docs/instances/custom-hostname-vm)

      iex> OpenTelemetry.SemanticConventions.Common.gcp_gce_instance_hostname()
      :"gcp.gce.instance.hostname"
  """
  @spec gcp_gce_instance_hostname :: :"gcp.gce.instance.hostname"
  def gcp_gce_instance_hostname do
    :"gcp.gce.instance.hostname"
  end

  @doc namespace: :gcp
  @doc """
  The instance name of a GCE instance. This is the value provided by `host.name`, the visible name of the instance in the Cloud Console UI, and the prefix for the default hostname of the instance as defined by the [default internal DNS name](https://cloud.google.com/compute/docs/internal-dns#instance-fully-qualified-domain-names)

      iex> OpenTelemetry.SemanticConventions.Common.gcp_gce_instance_name()
      :"gcp.gce.instance.name"
  """
  @spec gcp_gce_instance_name :: :"gcp.gce.instance.name"
  def gcp_gce_instance_name do
    :"gcp.gce.instance.name"
  end

  @doc namespace: :host
  @doc """
  The CPU architecture the host system is running on

      iex> OpenTelemetry.SemanticConventions.Common.host_arch()
      :"host.arch"
  """
  @spec host_arch :: :"host.arch"
  def host_arch do
    :"host.arch"
  end

  @doc namespace: :host
  @doc """
  The amount of level 2 memory cache available to the processor (in Bytes)

      iex> OpenTelemetry.SemanticConventions.Common.host_cpu_cache_l2_size()
      :"host.cpu.cache.l2.size"
  """
  @spec host_cpu_cache_l2_size :: :"host.cpu.cache.l2.size"
  def host_cpu_cache_l2_size do
    :"host.cpu.cache.l2.size"
  end

  @doc namespace: :host
  @doc """
  Family or generation of the CPU

      iex> OpenTelemetry.SemanticConventions.Common.host_cpu_family()
      :"host.cpu.family"
  """
  @spec host_cpu_family :: :"host.cpu.family"
  def host_cpu_family do
    :"host.cpu.family"
  end

  @doc namespace: :host
  @doc """
  Model identifier. It provides more granular information about the CPU, distinguishing it from other CPUs within the same family

      iex> OpenTelemetry.SemanticConventions.Common.host_cpu_model_id()
      :"host.cpu.model.id"
  """
  @spec host_cpu_model_id :: :"host.cpu.model.id"
  def host_cpu_model_id do
    :"host.cpu.model.id"
  end

  @doc namespace: :host
  @doc """
  Model designation of the processor

      iex> OpenTelemetry.SemanticConventions.Common.host_cpu_model_name()
      :"host.cpu.model.name"
  """
  @spec host_cpu_model_name :: :"host.cpu.model.name"
  def host_cpu_model_name do
    :"host.cpu.model.name"
  end

  @doc namespace: :host
  @doc """
  Stepping or core revisions

      iex> OpenTelemetry.SemanticConventions.Common.host_cpu_stepping()
      :"host.cpu.stepping"
  """
  @spec host_cpu_stepping :: :"host.cpu.stepping"
  def host_cpu_stepping do
    :"host.cpu.stepping"
  end

  @doc namespace: :host
  @doc """
  Processor manufacturer identifier. A maximum 12-character string

  ### Notes

  [CPUID](https://wiki.osdev.org/CPUID) command returns the vendor ID string in EBX, EDX and ECX registers. Writing these to memory in this order results in a 12-character string

      iex> OpenTelemetry.SemanticConventions.Common.host_cpu_vendor_id()
      :"host.cpu.vendor.id"
  """
  @spec host_cpu_vendor_id :: :"host.cpu.vendor.id"
  def host_cpu_vendor_id do
    :"host.cpu.vendor.id"
  end

  @doc namespace: :host
  @doc """
  Unique host ID. For Cloud, this must be the instance_id assigned by the cloud provider. For non-containerized systems, this should be the `machine-id`. See the table below for the sources to use to determine the `machine-id` based on operating system

      iex> OpenTelemetry.SemanticConventions.Common.host_id()
      :"host.id"
  """
  @spec host_id :: :"host.id"
  def host_id do
    :"host.id"
  end

  @doc namespace: :host
  @doc """
  VM image ID or host OS image ID. For Cloud, this value is from the provider

      iex> OpenTelemetry.SemanticConventions.Common.host_image_id()
      :"host.image.id"
  """
  @spec host_image_id :: :"host.image.id"
  def host_image_id do
    :"host.image.id"
  end

  @doc namespace: :host
  @doc """
  Name of the VM image or OS install the host was instantiated from

      iex> OpenTelemetry.SemanticConventions.Common.host_image_name()
      :"host.image.name"
  """
  @spec host_image_name :: :"host.image.name"
  def host_image_name do
    :"host.image.name"
  end

  @doc namespace: :host
  @doc """
  The version string of the VM image or host OS as defined in [Version Attributes](/docs/resource/README.md#version-attributes)

      iex> OpenTelemetry.SemanticConventions.Common.host_image_version()
      :"host.image.version"
  """
  @spec host_image_version :: :"host.image.version"
  def host_image_version do
    :"host.image.version"
  end

  @doc namespace: :host
  @doc """
  Available IP addresses of the host, excluding loopback interfaces

  ### Notes

  IPv4 Addresses **MUST** be specified in dotted-quad notation. IPv6 addresses **MUST** be specified in the [RFC 5952](https://www.rfc-editor.org/rfc/rfc5952.html) format

      iex> OpenTelemetry.SemanticConventions.Common.host_ip()
      :"host.ip"
  """
  @spec host_ip :: :"host.ip"
  def host_ip do
    :"host.ip"
  end

  @doc namespace: :host
  @doc """
  Available MAC addresses of the host, excluding loopback interfaces

  ### Notes

  MAC Addresses **MUST** be represented in [IEEE RA hexadecimal form](https://standards.ieee.org/wp-content/uploads/import/documents/tutorials/eui.pdf): as hyphen-separated octets in uppercase hexadecimal form from most to least significant

      iex> OpenTelemetry.SemanticConventions.Common.host_mac()
      :"host.mac"
  """
  @spec host_mac :: :"host.mac"
  def host_mac do
    :"host.mac"
  end

  @doc namespace: :host
  @doc """
  Name of the host. On Unix systems, it may contain what the hostname command returns, or the fully qualified hostname, or another name specified by the user

      iex> OpenTelemetry.SemanticConventions.Common.host_name()
      :"host.name"
  """
  @spec host_name :: :"host.name"
  def host_name do
    :"host.name"
  end

  @doc namespace: :host
  @doc """
  Type of host. For Cloud, this must be the machine type

      iex> OpenTelemetry.SemanticConventions.Common.host_type()
      :"host.type"
  """
  @spec host_type :: :"host.type"
  def host_type do
    :"host.type"
  end

  @doc namespace: :http
  @doc """
  State of the HTTP connection in the HTTP connection pool

      iex> OpenTelemetry.SemanticConventions.Common.http_connection_state()
      :"http.connection.state"
  """
  @spec http_connection_state :: :"http.connection.state"
  def http_connection_state do
    :"http.connection.state"
  end

  @doc namespace: :http
  @doc """
  The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size

      iex> OpenTelemetry.SemanticConventions.Common.http_request_body_size()
      :"http.request.body.size"
  """
  @spec http_request_body_size :: :"http.request.body.size"
  def http_request_body_size do
    :"http.request.body.size"
  end

  @doc namespace: :http
  @doc """
  Original HTTP method sent by the client in the request line

      iex> OpenTelemetry.SemanticConventions.Common.http_request_method_original()
      :"http.request.method_original"
  """
  @spec http_request_method_original :: :"http.request.method_original"
  def http_request_method_original do
    :"http.request.method_original"
  end

  @doc namespace: :http
  @doc """
  The ordinal number of request resending attempt (for any reason, including redirects)

  ### Notes

  The resend count **SHOULD** be updated each time an HTTP request gets resent by the client, regardless of what was the cause of the resending (e.g. redirection, authorization failure, 503 Server Unavailable, network issues, or any other)

      iex> OpenTelemetry.SemanticConventions.Common.http_request_resend_count()
      :"http.request.resend_count"
  """
  @spec http_request_resend_count :: :"http.request.resend_count"
  def http_request_resend_count do
    :"http.request.resend_count"
  end

  @doc namespace: :http
  @doc """
  The total size of the request in bytes. This should be the total number of bytes sent over the wire, including the request line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and request body if any

      iex> OpenTelemetry.SemanticConventions.Common.http_request_size()
      :"http.request.size"
  """
  @spec http_request_size :: :"http.request.size"
  def http_request_size do
    :"http.request.size"
  end

  @doc namespace: :http
  @doc """
  The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size

      iex> OpenTelemetry.SemanticConventions.Common.http_response_body_size()
      :"http.response.body.size"
  """
  @spec http_response_body_size :: :"http.response.body.size"
  def http_response_body_size do
    :"http.response.body.size"
  end

  @doc namespace: :http
  @doc """
  The total size of the response in bytes. This should be the total number of bytes sent over the wire, including the status line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and response body and trailers if any

      iex> OpenTelemetry.SemanticConventions.Common.http_response_size()
      :"http.response.size"
  """
  @spec http_response_size :: :"http.response.size"
  def http_response_size do
    :"http.response.size"
  end

  @doc namespace: :k8s
  @doc """
  The name of the cluster

      iex> OpenTelemetry.SemanticConventions.Common.k8s_cluster_name()
      :"k8s.cluster.name"
  """
  @spec k8s_cluster_name :: :"k8s.cluster.name"
  def k8s_cluster_name do
    :"k8s.cluster.name"
  end

  @doc namespace: :k8s
  @doc """
  A pseudo-ID for the cluster, set to the UID of the `kube-system` namespace

  ### Notes

  K8s doesn't have support for obtaining a cluster ID. If this is ever
  added, we will recommend collecting the `k8s.cluster.uid` through the
  official APIs. In the meantime, we are able to use the `uid` of the
  `kube-system` namespace as a proxy for cluster ID. Read on for the
  rationale.

  Every object created in a K8s cluster is assigned a distinct UID. The
  `kube-system` namespace is used by Kubernetes itself and will exist
  for the lifetime of the cluster. Using the `uid` of the `kube-system`
  namespace is a reasonable proxy for the K8s ClusterID as it will only
  change if the cluster is rebuilt. Furthermore, Kubernetes UIDs are
  UUIDs as standardized by
  [ISO/IEC 9834-8 and ITU-T X.667](https://www.itu.int/ITU-T/studygroups/com17/oid.html).
  Which states:

  > If generated according to one of the mechanisms defined in Rec.
    ITU-T X.667 | ISO/IEC 9834-8, a UUID is either guaranteed to be
    different from all other UUIDs generated before 3603 A.D., or is
    extremely likely to be different (depending on the mechanism chosen).

  Therefore, UIDs between clusters should be extremely unlikely to
  conflict

      iex> OpenTelemetry.SemanticConventions.Common.k8s_cluster_uid()
      :"k8s.cluster.uid"
  """
  @spec k8s_cluster_uid :: :"k8s.cluster.uid"
  def k8s_cluster_uid do
    :"k8s.cluster.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the Container from Pod specification, must be unique within a Pod. Container runtime usually uses different globally unique name (`container.name`)

      iex> OpenTelemetry.SemanticConventions.Common.k8s_container_name()
      :"k8s.container.name"
  """
  @spec k8s_container_name :: :"k8s.container.name"
  def k8s_container_name do
    :"k8s.container.name"
  end

  @doc namespace: :k8s
  @doc """
  Number of times the container was restarted. This attribute can be used to identify a particular container (running or stopped) within a container spec

      iex> OpenTelemetry.SemanticConventions.Common.k8s_container_restart_count()
      :"k8s.container.restart_count"
  """
  @spec k8s_container_restart_count :: :"k8s.container.restart_count"
  def k8s_container_restart_count do
    :"k8s.container.restart_count"
  end

  @doc namespace: :k8s
  @doc """
  The name of the CronJob

      iex> OpenTelemetry.SemanticConventions.Common.k8s_cronjob_name()
      :"k8s.cronjob.name"
  """
  @spec k8s_cronjob_name :: :"k8s.cronjob.name"
  def k8s_cronjob_name do
    :"k8s.cronjob.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the CronJob

      iex> OpenTelemetry.SemanticConventions.Common.k8s_cronjob_uid()
      :"k8s.cronjob.uid"
  """
  @spec k8s_cronjob_uid :: :"k8s.cronjob.uid"
  def k8s_cronjob_uid do
    :"k8s.cronjob.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the DaemonSet

      iex> OpenTelemetry.SemanticConventions.Common.k8s_daemonset_name()
      :"k8s.daemonset.name"
  """
  @spec k8s_daemonset_name :: :"k8s.daemonset.name"
  def k8s_daemonset_name do
    :"k8s.daemonset.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the DaemonSet

      iex> OpenTelemetry.SemanticConventions.Common.k8s_daemonset_uid()
      :"k8s.daemonset.uid"
  """
  @spec k8s_daemonset_uid :: :"k8s.daemonset.uid"
  def k8s_daemonset_uid do
    :"k8s.daemonset.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the Deployment

      iex> OpenTelemetry.SemanticConventions.Common.k8s_deployment_name()
      :"k8s.deployment.name"
  """
  @spec k8s_deployment_name :: :"k8s.deployment.name"
  def k8s_deployment_name do
    :"k8s.deployment.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the Deployment

      iex> OpenTelemetry.SemanticConventions.Common.k8s_deployment_uid()
      :"k8s.deployment.uid"
  """
  @spec k8s_deployment_uid :: :"k8s.deployment.uid"
  def k8s_deployment_uid do
    :"k8s.deployment.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the Job

      iex> OpenTelemetry.SemanticConventions.Common.k8s_job_name()
      :"k8s.job.name"
  """
  @spec k8s_job_name :: :"k8s.job.name"
  def k8s_job_name do
    :"k8s.job.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the Job

      iex> OpenTelemetry.SemanticConventions.Common.k8s_job_uid()
      :"k8s.job.uid"
  """
  @spec k8s_job_uid :: :"k8s.job.uid"
  def k8s_job_uid do
    :"k8s.job.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the namespace that the pod is running in

      iex> OpenTelemetry.SemanticConventions.Common.k8s_namespace_name()
      :"k8s.namespace.name"
  """
  @spec k8s_namespace_name :: :"k8s.namespace.name"
  def k8s_namespace_name do
    :"k8s.namespace.name"
  end

  @doc namespace: :k8s
  @doc """
  The name of the Node

      iex> OpenTelemetry.SemanticConventions.Common.k8s_node_name()
      :"k8s.node.name"
  """
  @spec k8s_node_name :: :"k8s.node.name"
  def k8s_node_name do
    :"k8s.node.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the Node

      iex> OpenTelemetry.SemanticConventions.Common.k8s_node_uid()
      :"k8s.node.uid"
  """
  @spec k8s_node_uid :: :"k8s.node.uid"
  def k8s_node_uid do
    :"k8s.node.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the Pod

      iex> OpenTelemetry.SemanticConventions.Common.k8s_pod_name()
      :"k8s.pod.name"
  """
  @spec k8s_pod_name :: :"k8s.pod.name"
  def k8s_pod_name do
    :"k8s.pod.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the Pod

      iex> OpenTelemetry.SemanticConventions.Common.k8s_pod_uid()
      :"k8s.pod.uid"
  """
  @spec k8s_pod_uid :: :"k8s.pod.uid"
  def k8s_pod_uid do
    :"k8s.pod.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the ReplicaSet

      iex> OpenTelemetry.SemanticConventions.Common.k8s_replicaset_name()
      :"k8s.replicaset.name"
  """
  @spec k8s_replicaset_name :: :"k8s.replicaset.name"
  def k8s_replicaset_name do
    :"k8s.replicaset.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the ReplicaSet

      iex> OpenTelemetry.SemanticConventions.Common.k8s_replicaset_uid()
      :"k8s.replicaset.uid"
  """
  @spec k8s_replicaset_uid :: :"k8s.replicaset.uid"
  def k8s_replicaset_uid do
    :"k8s.replicaset.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the StatefulSet

      iex> OpenTelemetry.SemanticConventions.Common.k8s_statefulset_name()
      :"k8s.statefulset.name"
  """
  @spec k8s_statefulset_name :: :"k8s.statefulset.name"
  def k8s_statefulset_name do
    :"k8s.statefulset.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the StatefulSet

      iex> OpenTelemetry.SemanticConventions.Common.k8s_statefulset_uid()
      :"k8s.statefulset.uid"
  """
  @spec k8s_statefulset_uid :: :"k8s.statefulset.uid"
  def k8s_statefulset_uid do
    :"k8s.statefulset.uid"
  end

  @doc namespace: :messaging
  @doc """
  The number of messages sent, received, or processed in the scope of the batching operation

  ### Notes

  Instrumentations **SHOULD NOT** set `messaging.batch.message_count` on spans that operate with a single message. When a messaging client library supports both batch and single-message API for the same operation, instrumentations **SHOULD** use `messaging.batch.message_count` for batching APIs and **SHOULD NOT** use it for single-message APIs

      iex> OpenTelemetry.SemanticConventions.Common.messaging_batch_message_count()
      :"messaging.batch.message_count"
  """
  @spec messaging_batch_message_count :: :"messaging.batch.message_count"
  def messaging_batch_message_count do
    :"messaging.batch.message_count"
  end

  @doc namespace: :messaging
  @doc """
  A unique identifier for the client that consumes or produces a message

      iex> OpenTelemetry.SemanticConventions.Common.messaging_client_id()
      :"messaging.client_id"
  """
  @spec messaging_client_id :: :"messaging.client_id"
  def messaging_client_id do
    :"messaging.client_id"
  end

  @doc namespace: :messaging
  @doc """
  A boolean that is true if the message destination is anonymous (could be unnamed or have auto-generated name)

      iex> OpenTelemetry.SemanticConventions.Common.messaging_destination_anonymous()
      :"messaging.destination.anonymous"
  """
  @spec messaging_destination_anonymous :: :"messaging.destination.anonymous"
  def messaging_destination_anonymous do
    :"messaging.destination.anonymous"
  end

  @doc namespace: :messaging
  @doc """
  The identifier of the partition messages are sent to or received from, unique within the `messaging.destination.name`

      iex> OpenTelemetry.SemanticConventions.Common.messaging_destination_partition_id()
      :"messaging.destination.partition.id"
  """
  @spec messaging_destination_partition_id :: :"messaging.destination.partition.id"
  def messaging_destination_partition_id do
    :"messaging.destination.partition.id"
  end

  @doc namespace: :messaging
  @doc """
  A boolean that is true if the message destination is temporary and might not exist anymore after messages are processed

      iex> OpenTelemetry.SemanticConventions.Common.messaging_destination_temporary()
      :"messaging.destination.temporary"
  """
  @spec messaging_destination_temporary :: :"messaging.destination.temporary"
  def messaging_destination_temporary do
    :"messaging.destination.temporary"
  end

  @doc namespace: :messaging
  @doc """
  A boolean that is true if the publish message destination is anonymous (could be unnamed or have auto-generated name)

      iex> OpenTelemetry.SemanticConventions.Common.messaging_destination_publish_anonymous()
      :"messaging.destination_publish.anonymous"
  """
  @spec messaging_destination_publish_anonymous :: :"messaging.destination_publish.anonymous"
  def messaging_destination_publish_anonymous do
    :"messaging.destination_publish.anonymous"
  end

  @doc namespace: :messaging
  @doc """
  The name of the original destination the message was published to

  ### Notes

  The name **SHOULD** uniquely identify a specific queue, topic, or other entity within the broker. If
  the broker doesn't have such notion, the original destination name **SHOULD** uniquely identify the broker

      iex> OpenTelemetry.SemanticConventions.Common.messaging_destination_publish_name()
      :"messaging.destination_publish.name"
  """
  @spec messaging_destination_publish_name :: :"messaging.destination_publish.name"
  def messaging_destination_publish_name do
    :"messaging.destination_publish.name"
  end

  @doc namespace: :messaging
  @doc """
  The name of the consumer group the event consumer is associated with

      iex> OpenTelemetry.SemanticConventions.Common.messaging_eventhubs_consumer_group()
      :"messaging.eventhubs.consumer.group"
  """
  @spec messaging_eventhubs_consumer_group :: :"messaging.eventhubs.consumer.group"
  def messaging_eventhubs_consumer_group do
    :"messaging.eventhubs.consumer.group"
  end

  @doc namespace: :messaging
  @doc """
  The UTC epoch seconds at which the message has been accepted and stored in the entity

      iex> OpenTelemetry.SemanticConventions.Common.messaging_eventhubs_message_enqueued_time()
      :"messaging.eventhubs.message.enqueued_time"
  """
  @spec messaging_eventhubs_message_enqueued_time :: :"messaging.eventhubs.message.enqueued_time"
  def messaging_eventhubs_message_enqueued_time do
    :"messaging.eventhubs.message.enqueued_time"
  end

  @doc namespace: :messaging
  @doc """
  The ordering key for a given message. If the attribute is not present, the message does not have an ordering key

      iex> OpenTelemetry.SemanticConventions.Common.messaging_gcp_pubsub_message_ordering_key()
      :"messaging.gcp_pubsub.message.ordering_key"
  """
  @spec messaging_gcp_pubsub_message_ordering_key :: :"messaging.gcp_pubsub.message.ordering_key"
  def messaging_gcp_pubsub_message_ordering_key do
    :"messaging.gcp_pubsub.message.ordering_key"
  end

  @doc namespace: :messaging
  @doc """
  Name of the Kafka Consumer Group that is handling the message. Only applies to consumers, not producers

      iex> OpenTelemetry.SemanticConventions.Common.messaging_kafka_consumer_group()
      :"messaging.kafka.consumer.group"
  """
  @spec messaging_kafka_consumer_group :: :"messaging.kafka.consumer.group"
  def messaging_kafka_consumer_group do
    :"messaging.kafka.consumer.group"
  end

  @doc namespace: :messaging
  @doc """
  Message keys in Kafka are used for grouping alike messages to ensure they're processed on the same partition. They differ from `messaging.message.id` in that they're not unique. If the key is `null`, the attribute **MUST NOT** be set

  ### Notes

  If the key type is not string, it's string representation has to be supplied for the attribute. If the key has no unambiguous, canonical string form, don't include its value

      iex> OpenTelemetry.SemanticConventions.Common.messaging_kafka_message_key()
      :"messaging.kafka.message.key"
  """
  @spec messaging_kafka_message_key :: :"messaging.kafka.message.key"
  def messaging_kafka_message_key do
    :"messaging.kafka.message.key"
  end

  @doc namespace: :messaging
  @doc """
  The offset of a record in the corresponding Kafka partition

      iex> OpenTelemetry.SemanticConventions.Common.messaging_kafka_message_offset()
      :"messaging.kafka.message.offset"
  """
  @spec messaging_kafka_message_offset :: :"messaging.kafka.message.offset"
  def messaging_kafka_message_offset do
    :"messaging.kafka.message.offset"
  end

  @doc namespace: :messaging
  @doc """
  A boolean that is true if the message is a tombstone

      iex> OpenTelemetry.SemanticConventions.Common.messaging_kafka_message_tombstone()
      :"messaging.kafka.message.tombstone"
  """
  @spec messaging_kafka_message_tombstone :: :"messaging.kafka.message.tombstone"
  def messaging_kafka_message_tombstone do
    :"messaging.kafka.message.tombstone"
  end

  @doc namespace: :messaging
  @doc """
  The size of the message body in bytes

  ### Notes

  This can refer to both the compressed or uncompressed body size. If both sizes are known, the uncompressed
  body size should be used

      iex> OpenTelemetry.SemanticConventions.Common.messaging_message_body_size()
      :"messaging.message.body.size"
  """
  @spec messaging_message_body_size :: :"messaging.message.body.size"
  def messaging_message_body_size do
    :"messaging.message.body.size"
  end

  @doc namespace: :messaging
  @doc """
  The conversation ID identifying the conversation to which the message belongs, represented as a string. Sometimes called "Correlation ID"

      iex> OpenTelemetry.SemanticConventions.Common.messaging_message_conversation_id()
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

      iex> OpenTelemetry.SemanticConventions.Common.messaging_message_envelope_size()
      :"messaging.message.envelope.size"
  """
  @spec messaging_message_envelope_size :: :"messaging.message.envelope.size"
  def messaging_message_envelope_size do
    :"messaging.message.envelope.size"
  end

  @doc namespace: :messaging
  @doc """
  A value used by the messaging system as an identifier for the message, represented as a string

      iex> OpenTelemetry.SemanticConventions.Common.messaging_message_id()
      :"messaging.message.id"
  """
  @spec messaging_message_id :: :"messaging.message.id"
  def messaging_message_id do
    :"messaging.message.id"
  end

  @doc namespace: :messaging
  @doc """
  A string identifying the kind of messaging operation

  ### Notes

  If a custom value is used, it **MUST** be of low cardinality

      iex> OpenTelemetry.SemanticConventions.Common.messaging_operation()
      :"messaging.operation"
  """
  @spec messaging_operation :: :"messaging.operation"
  def messaging_operation do
    :"messaging.operation"
  end

  @doc namespace: :messaging
  @doc """
  RabbitMQ message routing key

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rabbitmq_destination_routing_key()
      :"messaging.rabbitmq.destination.routing_key"
  """
  @spec messaging_rabbitmq_destination_routing_key ::
          :"messaging.rabbitmq.destination.routing_key"
  def messaging_rabbitmq_destination_routing_key do
    :"messaging.rabbitmq.destination.routing_key"
  end

  @doc namespace: :messaging
  @doc """
  RabbitMQ message delivery tag

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rabbitmq_message_delivery_tag()
      :"messaging.rabbitmq.message.delivery_tag"
  """
  @spec messaging_rabbitmq_message_delivery_tag :: :"messaging.rabbitmq.message.delivery_tag"
  def messaging_rabbitmq_message_delivery_tag do
    :"messaging.rabbitmq.message.delivery_tag"
  end

  @doc namespace: :messaging
  @doc """
  Name of the RocketMQ producer/consumer group that is handling the message. The client type is identified by the SpanKind

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rocketmq_client_group()
      :"messaging.rocketmq.client_group"
  """
  @spec messaging_rocketmq_client_group :: :"messaging.rocketmq.client_group"
  def messaging_rocketmq_client_group do
    :"messaging.rocketmq.client_group"
  end

  @doc namespace: :messaging
  @doc """
  Model of message consumption. This only applies to consumer spans

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rocketmq_consumption_model()
      :"messaging.rocketmq.consumption_model"
  """
  @spec messaging_rocketmq_consumption_model :: :"messaging.rocketmq.consumption_model"
  def messaging_rocketmq_consumption_model do
    :"messaging.rocketmq.consumption_model"
  end

  @doc namespace: :messaging
  @doc """
  The delay time level for delay message, which determines the message delay time

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rocketmq_message_delay_time_level()
      :"messaging.rocketmq.message.delay_time_level"
  """
  @spec messaging_rocketmq_message_delay_time_level ::
          :"messaging.rocketmq.message.delay_time_level"
  def messaging_rocketmq_message_delay_time_level do
    :"messaging.rocketmq.message.delay_time_level"
  end

  @doc namespace: :messaging
  @doc """
  The timestamp in milliseconds that the delay message is expected to be delivered to consumer

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rocketmq_message_delivery_timestamp()
      :"messaging.rocketmq.message.delivery_timestamp"
  """
  @spec messaging_rocketmq_message_delivery_timestamp ::
          :"messaging.rocketmq.message.delivery_timestamp"
  def messaging_rocketmq_message_delivery_timestamp do
    :"messaging.rocketmq.message.delivery_timestamp"
  end

  @doc namespace: :messaging
  @doc """
  It is essential for FIFO message. Messages that belong to the same message group are always processed one by one within the same consumer group

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rocketmq_message_group()
      :"messaging.rocketmq.message.group"
  """
  @spec messaging_rocketmq_message_group :: :"messaging.rocketmq.message.group"
  def messaging_rocketmq_message_group do
    :"messaging.rocketmq.message.group"
  end

  @doc namespace: :messaging
  @doc """
  Key(s) of message, another way to mark message besides message id

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rocketmq_message_keys()
      :"messaging.rocketmq.message.keys"
  """
  @spec messaging_rocketmq_message_keys :: :"messaging.rocketmq.message.keys"
  def messaging_rocketmq_message_keys do
    :"messaging.rocketmq.message.keys"
  end

  @doc namespace: :messaging
  @doc """
  The secondary classifier of message besides topic

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rocketmq_message_tag()
      :"messaging.rocketmq.message.tag"
  """
  @spec messaging_rocketmq_message_tag :: :"messaging.rocketmq.message.tag"
  def messaging_rocketmq_message_tag do
    :"messaging.rocketmq.message.tag"
  end

  @doc namespace: :messaging
  @doc """
  Type of message

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rocketmq_message_type()
      :"messaging.rocketmq.message.type"
  """
  @spec messaging_rocketmq_message_type :: :"messaging.rocketmq.message.type"
  def messaging_rocketmq_message_type do
    :"messaging.rocketmq.message.type"
  end

  @doc namespace: :messaging
  @doc """
  Namespace of RocketMQ resources, resources in different namespaces are individual

      iex> OpenTelemetry.SemanticConventions.Common.messaging_rocketmq_namespace()
      :"messaging.rocketmq.namespace"
  """
  @spec messaging_rocketmq_namespace :: :"messaging.rocketmq.namespace"
  def messaging_rocketmq_namespace do
    :"messaging.rocketmq.namespace"
  end

  @doc namespace: :messaging
  @doc """
  The name of the subscription in the topic messages are received from

      iex> OpenTelemetry.SemanticConventions.Common.messaging_servicebus_destination_subscription_name()
      :"messaging.servicebus.destination.subscription_name"
  """
  @spec messaging_servicebus_destination_subscription_name ::
          :"messaging.servicebus.destination.subscription_name"
  def messaging_servicebus_destination_subscription_name do
    :"messaging.servicebus.destination.subscription_name"
  end

  @doc namespace: :messaging
  @doc """
  Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock)

      iex> OpenTelemetry.SemanticConventions.Common.messaging_servicebus_disposition_status()
      :"messaging.servicebus.disposition_status"
  """
  @spec messaging_servicebus_disposition_status :: :"messaging.servicebus.disposition_status"
  def messaging_servicebus_disposition_status do
    :"messaging.servicebus.disposition_status"
  end

  @doc namespace: :messaging
  @doc """
  Number of deliveries that have been attempted for this message

      iex> OpenTelemetry.SemanticConventions.Common.messaging_servicebus_message_delivery_count()
      :"messaging.servicebus.message.delivery_count"
  """
  @spec messaging_servicebus_message_delivery_count ::
          :"messaging.servicebus.message.delivery_count"
  def messaging_servicebus_message_delivery_count do
    :"messaging.servicebus.message.delivery_count"
  end

  @doc namespace: :messaging
  @doc """
  The UTC epoch seconds at which the message has been accepted and stored in the entity

      iex> OpenTelemetry.SemanticConventions.Common.messaging_servicebus_message_enqueued_time()
      :"messaging.servicebus.message.enqueued_time"
  """
  @spec messaging_servicebus_message_enqueued_time ::
          :"messaging.servicebus.message.enqueued_time"
  def messaging_servicebus_message_enqueued_time do
    :"messaging.servicebus.message.enqueued_time"
  end

  @doc namespace: :network
  @doc """
  The network IO operation direction

      iex> OpenTelemetry.SemanticConventions.Common.network_io_direction()
      :"network.io.direction"
  """
  @spec network_io_direction :: :"network.io.direction"
  def network_io_direction do
    :"network.io.direction"
  end

  @doc namespace: :oci
  @doc """
  The digest of the OCI image manifest. For container images specifically is the digest by which the container image is known

  ### Notes

  Follows [OCI Image Manifest Specification](https://github.com/opencontainers/image-spec/blob/main/manifest.md), and specifically the [Digest property](https://github.com/opencontainers/image-spec/blob/main/descriptor.md#digests).
  An example can be found in [Example Image Manifest](https://docs.docker.com/registry/spec/manifest-v2-2/#example-image-manifest)

      iex> OpenTelemetry.SemanticConventions.Common.oci_manifest_digest()
      :"oci.manifest.digest"
  """
  @spec oci_manifest_digest :: :"oci.manifest.digest"
  def oci_manifest_digest do
    :"oci.manifest.digest"
  end

  @doc namespace: :process
  @doc """
  The command used to launch the process (i.e. the command name). On Linux based systems, can be set to the zeroth string in `proc/[pid]/cmdline`. On Windows, can be set to the first parameter extracted from `GetCommandLineW`

      iex> OpenTelemetry.SemanticConventions.Common.process_command()
      :"process.command"
  """
  @spec process_command :: :"process.command"
  def process_command do
    :"process.command"
  end

  @doc namespace: :process
  @doc """
  All the command arguments (including the command/executable itself) as received by the process. On Linux-based systems (and some other Unixoid systems supporting procfs), can be set according to the list of null-delimited strings extracted from `proc/[pid]/cmdline`. For libc-based executables, this would be the full argv vector passed to `main`

      iex> OpenTelemetry.SemanticConventions.Common.process_command_args()
      :"process.command_args"
  """
  @spec process_command_args :: :"process.command_args"
  def process_command_args do
    :"process.command_args"
  end

  @doc namespace: :process
  @doc """
  The full command used to launch the process as a single string representing the full command. On Windows, can be set to the result of `GetCommandLineW`. Do not set this if you have to assemble it just for monitoring; use `process.command_args` instead

      iex> OpenTelemetry.SemanticConventions.Common.process_command_line()
      :"process.command_line"
  """
  @spec process_command_line :: :"process.command_line"
  def process_command_line do
    :"process.command_line"
  end

  @doc namespace: :process
  @doc """
  The name of the process executable. On Linux based systems, can be set to the `Name` in `proc/[pid]/status`. On Windows, can be set to the base name of `GetProcessImageFileNameW`

      iex> OpenTelemetry.SemanticConventions.Common.process_executable_name()
      :"process.executable.name"
  """
  @spec process_executable_name :: :"process.executable.name"
  def process_executable_name do
    :"process.executable.name"
  end

  @doc namespace: :process
  @doc """
  The full path to the process executable. On Linux based systems, can be set to the target of `proc/[pid]/exe`. On Windows, can be set to the result of `GetProcessImageFileNameW`

      iex> OpenTelemetry.SemanticConventions.Common.process_executable_path()
      :"process.executable.path"
  """
  @spec process_executable_path :: :"process.executable.path"
  def process_executable_path do
    :"process.executable.path"
  end

  @doc namespace: :process
  @doc """
  The username of the user that owns the process

      iex> OpenTelemetry.SemanticConventions.Common.process_owner()
      :"process.owner"
  """
  @spec process_owner :: :"process.owner"
  def process_owner do
    :"process.owner"
  end

  @doc namespace: :process
  @doc """
  Parent Process identifier (PPID)

      iex> OpenTelemetry.SemanticConventions.Common.process_parent_pid()
      :"process.parent_pid"
  """
  @spec process_parent_pid :: :"process.parent_pid"
  def process_parent_pid do
    :"process.parent_pid"
  end

  @doc namespace: :process
  @doc """
  Process identifier (PID)

      iex> OpenTelemetry.SemanticConventions.Common.process_pid()
      :"process.pid"
  """
  @spec process_pid :: :"process.pid"
  def process_pid do
    :"process.pid"
  end

  @doc namespace: :process
  @doc """
  An additional description about the runtime of the process, for example a specific vendor customization of the runtime environment

      iex> OpenTelemetry.SemanticConventions.Common.process_runtime_description()
      :"process.runtime.description"
  """
  @spec process_runtime_description :: :"process.runtime.description"
  def process_runtime_description do
    :"process.runtime.description"
  end

  @doc namespace: :process
  @doc """
  The name of the runtime of this process. For compiled native binaries, this **SHOULD** be the name of the compiler

      iex> OpenTelemetry.SemanticConventions.Common.process_runtime_name()
      :"process.runtime.name"
  """
  @spec process_runtime_name :: :"process.runtime.name"
  def process_runtime_name do
    :"process.runtime.name"
  end

  @doc namespace: :process
  @doc """
  The version of the runtime of this process, as returned by the runtime without modification

      iex> OpenTelemetry.SemanticConventions.Common.process_runtime_version()
      :"process.runtime.version"
  """
  @spec process_runtime_version :: :"process.runtime.version"
  def process_runtime_version do
    :"process.runtime.version"
  end

  @doc namespace: :rpc
  @doc """
  The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values

      iex> OpenTelemetry.SemanticConventions.Common.rpc_connect_rpc_error_code()
      :"rpc.connect_rpc.error_code"
  """
  @spec rpc_connect_rpc_error_code :: :"rpc.connect_rpc.error_code"
  def rpc_connect_rpc_error_code do
    :"rpc.connect_rpc.error_code"
  end

  @doc namespace: :rpc
  @doc """
  The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request

      iex> OpenTelemetry.SemanticConventions.Common.rpc_grpc_status_code()
      :"rpc.grpc.status_code"
  """
  @spec rpc_grpc_status_code :: :"rpc.grpc.status_code"
  def rpc_grpc_status_code do
    :"rpc.grpc.status_code"
  end

  @doc namespace: :rpc
  @doc """
  `error.code` property of response if it is an error response

      iex> OpenTelemetry.SemanticConventions.Common.rpc_jsonrpc_error_code()
      :"rpc.jsonrpc.error_code"
  """
  @spec rpc_jsonrpc_error_code :: :"rpc.jsonrpc.error_code"
  def rpc_jsonrpc_error_code do
    :"rpc.jsonrpc.error_code"
  end

  @doc namespace: :rpc
  @doc """
  `error.message` property of response if it is an error response

      iex> OpenTelemetry.SemanticConventions.Common.rpc_jsonrpc_error_message()
      :"rpc.jsonrpc.error_message"
  """
  @spec rpc_jsonrpc_error_message :: :"rpc.jsonrpc.error_message"
  def rpc_jsonrpc_error_message do
    :"rpc.jsonrpc.error_message"
  end

  @doc namespace: :rpc
  @doc """
  `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification

      iex> OpenTelemetry.SemanticConventions.Common.rpc_jsonrpc_request_id()
      :"rpc.jsonrpc.request_id"
  """
  @spec rpc_jsonrpc_request_id :: :"rpc.jsonrpc.request_id"
  def rpc_jsonrpc_request_id do
    :"rpc.jsonrpc.request_id"
  end

  @doc namespace: :rpc
  @doc """
  Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 doesn't specify this, the value can be omitted

      iex> OpenTelemetry.SemanticConventions.Common.rpc_jsonrpc_version()
      :"rpc.jsonrpc.version"
  """
  @spec rpc_jsonrpc_version :: :"rpc.jsonrpc.version"
  def rpc_jsonrpc_version do
    :"rpc.jsonrpc.version"
  end

  @doc namespace: :service
  @doc """
  The string ID of the service instance

  ### Notes

  **MUST** be unique for each instance of the same `service.namespace,service.name` pair (in other words
  `service.namespace,service.name,service.instance.id` triplet **MUST** be globally unique). The ID helps to
  distinguish instances of the same service that exist at the same time (e.g. instances of a horizontally scaled
  service).

  Implementations, such as SDKs, are recommended to generate a random Version 1 or Version 4 [RFC
  4122](https://www.ietf.org/rfc/rfc4122.txt) UUID, but are free to use an inherent unique ID as the source of
  this value if stability is desirable. In that case, the ID **SHOULD** be used as source of a UUID Version 5 and
  **SHOULD** use the following UUID as the namespace: `4d63009a-8d0f-11ee-aad7-4c796ed8e320`.

  UUIDs are typically recommended, as only an opaque value for the purposes of identifying a service instance is
  needed. Similar to what can be seen in the man page for the
  [`/etc/machine-id`](https://www.freedesktop.org/software/systemd/man/machine-id.html) file, the underlying
  data, such as pod name and namespace should be treated as confidential, being the user's choice to expose it
  or not via another resource attribute.

  For applications running behind an application server (like unicorn), we do not recommend using one identifier
  for all processes participating in the application. Instead, it's recommended each division (e.g. a worker
  thread in unicorn) to have its own instance.id.

  It's not recommended for a Collector to set `service.instance.id` if it can't unambiguously determine the
  service instance that is generating that telemetry. For instance, creating an UUID based on `pod.name` will
  likely be wrong, as the Collector might not know from which container within that pod the telemetry originated.
  However, Collectors can set the `service.instance.id` if they can unambiguously determine the service instance
  for that telemetry. This is typically the case for scraping receivers, as they know the target address and
  port

      iex> OpenTelemetry.SemanticConventions.Common.service_instance_id()
      :"service.instance.id"
  """
  @spec service_instance_id :: :"service.instance.id"
  def service_instance_id do
    :"service.instance.id"
  end

  @doc namespace: :service
  @doc """
  Logical name of the service

  ### Notes

  **MUST** be the same for all instances of horizontally scaled services. If the value was not specified, SDKs **MUST** fallback to `unknown_service:` concatenated with [`process.executable.name`](process.md#process), e.g. `unknown_service:bash`. If `process.executable.name` is not available, the value **MUST** be set to `unknown_service`

      iex> OpenTelemetry.SemanticConventions.Common.service_name()
      :"service.name"
  """
  @spec service_name :: :"service.name"
  def service_name do
    :"service.name"
  end

  @doc namespace: :service
  @doc """
  A namespace for `service.name`

  ### Notes

  A string value having a meaning that helps to distinguish a group of services, for example the team name that owns a group of services. `service.name` is expected to be unique within the same namespace. If `service.namespace` is not specified in the Resource then `service.name` is expected to be unique for all services that have no explicit namespace defined (so the empty/unspecified namespace is simply one more valid namespace). Zero-length namespace string is assumed equal to unspecified namespace

      iex> OpenTelemetry.SemanticConventions.Common.service_namespace()
      :"service.namespace"
  """
  @spec service_namespace :: :"service.namespace"
  def service_namespace do
    :"service.namespace"
  end

  @doc namespace: :service
  @doc """
  The version string of the service API or implementation. The format is not defined by these conventions

      iex> OpenTelemetry.SemanticConventions.Common.service_version()
      :"service.version"
  """
  @spec service_version :: :"service.version"
  def service_version do
    :"service.version"
  end

  @doc namespace: :session
  @doc """
  A unique id to identify a session

      iex> OpenTelemetry.SemanticConventions.Common.session_id()
      :"session.id"
  """
  @spec session_id :: :"session.id"
  def session_id do
    :"session.id"
  end

  @doc namespace: :session
  @doc """
  The previous `session.id` for this user, when known

      iex> OpenTelemetry.SemanticConventions.Common.session_previous_id()
      :"session.previous_id"
  """
  @spec session_previous_id :: :"session.previous_id"
  def session_previous_id do
    :"session.previous_id"
  end

  @doc namespace: :telemetry
  @doc """
  The language of the telemetry SDK

      iex> OpenTelemetry.SemanticConventions.Common.telemetry_sdk_language()
      :"telemetry.sdk.language"
  """
  @spec telemetry_sdk_language :: :"telemetry.sdk.language"
  def telemetry_sdk_language do
    :"telemetry.sdk.language"
  end

  @doc namespace: :telemetry
  @doc """
  The name of the telemetry SDK as defined above

  ### Notes

  The OpenTelemetry SDK **MUST** set the `telemetry.sdk.name` attribute to `opentelemetry`.
  If another SDK, like a fork or a vendor-provided implementation, is used, this SDK **MUST** set the
  `telemetry.sdk.name` attribute to the fully-qualified class or module name of this SDK's main entry point
  or another suitable identifier depending on the language.
  The identifier `opentelemetry` is reserved and **MUST NOT** be used in this case.
  All custom identifiers **SHOULD** be stable across different versions of an implementation

      iex> OpenTelemetry.SemanticConventions.Common.telemetry_sdk_name()
      :"telemetry.sdk.name"
  """
  @spec telemetry_sdk_name :: :"telemetry.sdk.name"
  def telemetry_sdk_name do
    :"telemetry.sdk.name"
  end

  @doc namespace: :telemetry
  @doc """
  The version string of the telemetry SDK

      iex> OpenTelemetry.SemanticConventions.Common.telemetry_sdk_version()
      :"telemetry.sdk.version"
  """
  @spec telemetry_sdk_version :: :"telemetry.sdk.version"
  def telemetry_sdk_version do
    :"telemetry.sdk.version"
  end

  @doc namespace: :telemetry
  @doc """
  The name of the auto instrumentation agent or distribution, if used

  ### Notes

  Official auto instrumentation agents and distributions **SHOULD** set the `telemetry.distro.name` attribute to
  a string starting with `opentelemetry-`, e.g. `opentelemetry-java-instrumentation`

      iex> OpenTelemetry.SemanticConventions.Common.telemetry_distro_name()
      :"telemetry.distro.name"
  """
  @spec telemetry_distro_name :: :"telemetry.distro.name"
  def telemetry_distro_name do
    :"telemetry.distro.name"
  end

  @doc namespace: :telemetry
  @doc """
  The version string of the auto instrumentation agent or distribution, if used

      iex> OpenTelemetry.SemanticConventions.Common.telemetry_distro_version()
      :"telemetry.distro.version"
  """
  @spec telemetry_distro_version :: :"telemetry.distro.version"
  def telemetry_distro_version do
    :"telemetry.distro.version"
  end

  @doc namespace: :thread
  @doc """
  Current "managed" thread ID (as opposed to OS thread ID)

      iex> OpenTelemetry.SemanticConventions.Common.thread_id()
      :"thread.id"
  """
  @spec thread_id :: :"thread.id"
  def thread_id do
    :"thread.id"
  end

  @doc namespace: :thread
  @doc """
  Current thread name

      iex> OpenTelemetry.SemanticConventions.Common.thread_name()
      :"thread.name"
  """
  @spec thread_name :: :"thread.name"
  def thread_name do
    :"thread.name"
  end

  @doc namespace: :tls
  @doc """
  String indicating the [cipher](https://datatracker.ietf.org/doc/html/rfc5246#appendix-A.5) used during the current connection

  ### Notes

  The values allowed for `tls.cipher` **MUST** be one of the `Descriptions` of the [registered TLS Cipher Suits](https://www.iana.org/assignments/tls-parameters/tls-parameters.xhtml#table-tls-parameters-4)

      iex> OpenTelemetry.SemanticConventions.Common.tls_cipher()
      :"tls.cipher"
  """
  @spec tls_cipher :: :"tls.cipher"
  def tls_cipher do
    :"tls.cipher"
  end

  @doc namespace: :tls
  @doc """
  PEM-encoded stand-alone certificate offered by the client. This is usually mutually-exclusive of `client.certificate_chain` since this value also exists in that list

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_certificate()
      :"tls.client.certificate"
  """
  @spec tls_client_certificate :: :"tls.client.certificate"
  def tls_client_certificate do
    :"tls.client.certificate"
  end

  @doc namespace: :tls
  @doc """
  Array of PEM-encoded certificates that make up the certificate chain offered by the client. This is usually mutually-exclusive of `client.certificate` since that value should be the first certificate in the chain

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_certificate_chain()
      :"tls.client.certificate_chain"
  """
  @spec tls_client_certificate_chain :: :"tls.client.certificate_chain"
  def tls_client_certificate_chain do
    :"tls.client.certificate_chain"
  end

  @doc namespace: :tls
  @doc """
  Certificate fingerprint using the MD5 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_hash_md5()
      :"tls.client.hash.md5"
  """
  @spec tls_client_hash_md5 :: :"tls.client.hash.md5"
  def tls_client_hash_md5 do
    :"tls.client.hash.md5"
  end

  @doc namespace: :tls
  @doc """
  Certificate fingerprint using the SHA1 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_hash_sha1()
      :"tls.client.hash.sha1"
  """
  @spec tls_client_hash_sha1 :: :"tls.client.hash.sha1"
  def tls_client_hash_sha1 do
    :"tls.client.hash.sha1"
  end

  @doc namespace: :tls
  @doc """
  Certificate fingerprint using the SHA256 digest of DER-encoded version of certificate offered by the client. For consistency with other hash values, this value should be formatted as an uppercase hash

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_hash_sha256()
      :"tls.client.hash.sha256"
  """
  @spec tls_client_hash_sha256 :: :"tls.client.hash.sha256"
  def tls_client_hash_sha256 do
    :"tls.client.hash.sha256"
  end

  @doc namespace: :tls
  @doc """
  Distinguished name of [subject](https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.6) of the issuer of the x.509 certificate presented by the client

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_issuer()
      :"tls.client.issuer"
  """
  @spec tls_client_issuer :: :"tls.client.issuer"
  def tls_client_issuer do
    :"tls.client.issuer"
  end

  @doc namespace: :tls
  @doc """
  A hash that identifies clients based on how they perform an SSL/TLS handshake

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_ja3()
      :"tls.client.ja3"
  """
  @spec tls_client_ja3 :: :"tls.client.ja3"
  def tls_client_ja3 do
    :"tls.client.ja3"
  end

  @doc namespace: :tls
  @doc """
  Date/Time indicating when client certificate is no longer considered valid

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_not_after()
      :"tls.client.not_after"
  """
  @spec tls_client_not_after :: :"tls.client.not_after"
  def tls_client_not_after do
    :"tls.client.not_after"
  end

  @doc namespace: :tls
  @doc """
  Date/Time indicating when client certificate is first considered valid

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_not_before()
      :"tls.client.not_before"
  """
  @spec tls_client_not_before :: :"tls.client.not_before"
  def tls_client_not_before do
    :"tls.client.not_before"
  end

  @doc namespace: :tls
  @doc """
  Also called an SNI, this tells the server which hostname to which the client is attempting to connect to

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_server_name()
      :"tls.client.server_name"
  """
  @spec tls_client_server_name :: :"tls.client.server_name"
  def tls_client_server_name do
    :"tls.client.server_name"
  end

  @doc namespace: :tls
  @doc """
  Distinguished name of subject of the x.509 certificate presented by the client

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_subject()
      :"tls.client.subject"
  """
  @spec tls_client_subject :: :"tls.client.subject"
  def tls_client_subject do
    :"tls.client.subject"
  end

  @doc namespace: :tls
  @doc """
  Array of ciphers offered by the client during the client hello

      iex> OpenTelemetry.SemanticConventions.Common.tls_client_supported_ciphers()
      :"tls.client.supported_ciphers"
  """
  @spec tls_client_supported_ciphers :: :"tls.client.supported_ciphers"
  def tls_client_supported_ciphers do
    :"tls.client.supported_ciphers"
  end

  @doc namespace: :tls
  @doc """
  String indicating the curve used for the given cipher, when applicable

      iex> OpenTelemetry.SemanticConventions.Common.tls_curve()
      :"tls.curve"
  """
  @spec tls_curve :: :"tls.curve"
  def tls_curve do
    :"tls.curve"
  end

  @doc namespace: :tls
  @doc """
  Boolean flag indicating if the TLS negotiation was successful and transitioned to an encrypted tunnel

      iex> OpenTelemetry.SemanticConventions.Common.tls_established()
      :"tls.established"
  """
  @spec tls_established :: :"tls.established"
  def tls_established do
    :"tls.established"
  end

  @doc namespace: :tls
  @doc """
  String indicating the protocol being tunneled. Per the values in the [IANA registry](https://www.iana.org/assignments/tls-extensiontype-values/tls-extensiontype-values.xhtml#alpn-protocol-ids), this string should be lower case

      iex> OpenTelemetry.SemanticConventions.Common.tls_next_protocol()
      :"tls.next_protocol"
  """
  @spec tls_next_protocol :: :"tls.next_protocol"
  def tls_next_protocol do
    :"tls.next_protocol"
  end

  @doc namespace: :tls
  @doc """
  Normalized lowercase protocol name parsed from original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)

      iex> OpenTelemetry.SemanticConventions.Common.tls_protocol_name()
      :"tls.protocol.name"
  """
  @spec tls_protocol_name :: :"tls.protocol.name"
  def tls_protocol_name do
    :"tls.protocol.name"
  end

  @doc namespace: :tls
  @doc """
  Numeric part of the version parsed from the original string of the negotiated [SSL/TLS protocol version](https://www.openssl.org/docs/man1.1.1/man3/SSL_get_version.html#RETURN-VALUES)

      iex> OpenTelemetry.SemanticConventions.Common.tls_protocol_version()
      :"tls.protocol.version"
  """
  @spec tls_protocol_version :: :"tls.protocol.version"
  def tls_protocol_version do
    :"tls.protocol.version"
  end

  @doc namespace: :tls
  @doc """
  Boolean flag indicating if this TLS connection was resumed from an existing TLS negotiation

      iex> OpenTelemetry.SemanticConventions.Common.tls_resumed()
      :"tls.resumed"
  """
  @spec tls_resumed :: :"tls.resumed"
  def tls_resumed do
    :"tls.resumed"
  end

  @doc namespace: :tls
  @doc """
  PEM-encoded stand-alone certificate offered by the server. This is usually mutually-exclusive of `server.certificate_chain` since this value also exists in that list

      iex> OpenTelemetry.SemanticConventions.Common.tls_server_certificate()
      :"tls.server.certificate"
  """
  @spec tls_server_certificate :: :"tls.server.certificate"
  def tls_server_certificate do
    :"tls.server.certificate"
  end

  @doc namespace: :tls
  @doc """
  Array of PEM-encoded certificates that make up the certificate chain offered by the server. This is usually mutually-exclusive of `server.certificate` since that value should be the first certificate in the chain

      iex> OpenTelemetry.SemanticConventions.Common.tls_server_certificate_chain()
      :"tls.server.certificate_chain"
  """
  @spec tls_server_certificate_chain :: :"tls.server.certificate_chain"
  def tls_server_certificate_chain do
    :"tls.server.certificate_chain"
  end

  @doc namespace: :tls
  @doc """
  Certificate fingerprint using the MD5 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash

      iex> OpenTelemetry.SemanticConventions.Common.tls_server_hash_md5()
      :"tls.server.hash.md5"
  """
  @spec tls_server_hash_md5 :: :"tls.server.hash.md5"
  def tls_server_hash_md5 do
    :"tls.server.hash.md5"
  end

  @doc namespace: :tls
  @doc """
  Certificate fingerprint using the SHA1 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash

      iex> OpenTelemetry.SemanticConventions.Common.tls_server_hash_sha1()
      :"tls.server.hash.sha1"
  """
  @spec tls_server_hash_sha1 :: :"tls.server.hash.sha1"
  def tls_server_hash_sha1 do
    :"tls.server.hash.sha1"
  end

  @doc namespace: :tls
  @doc """
  Certificate fingerprint using the SHA256 digest of DER-encoded version of certificate offered by the server. For consistency with other hash values, this value should be formatted as an uppercase hash

      iex> OpenTelemetry.SemanticConventions.Common.tls_server_hash_sha256()
      :"tls.server.hash.sha256"
  """
  @spec tls_server_hash_sha256 :: :"tls.server.hash.sha256"
  def tls_server_hash_sha256 do
    :"tls.server.hash.sha256"
  end

  @doc namespace: :tls
  @doc """
  Distinguished name of [subject](https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.6) of the issuer of the x.509 certificate presented by the client

      iex> OpenTelemetry.SemanticConventions.Common.tls_server_issuer()
      :"tls.server.issuer"
  """
  @spec tls_server_issuer :: :"tls.server.issuer"
  def tls_server_issuer do
    :"tls.server.issuer"
  end

  @doc namespace: :tls
  @doc """
  A hash that identifies servers based on how they perform an SSL/TLS handshake

      iex> OpenTelemetry.SemanticConventions.Common.tls_server_ja3s()
      :"tls.server.ja3s"
  """
  @spec tls_server_ja3s :: :"tls.server.ja3s"
  def tls_server_ja3s do
    :"tls.server.ja3s"
  end

  @doc namespace: :tls
  @doc """
  Date/Time indicating when server certificate is no longer considered valid

      iex> OpenTelemetry.SemanticConventions.Common.tls_server_not_after()
      :"tls.server.not_after"
  """
  @spec tls_server_not_after :: :"tls.server.not_after"
  def tls_server_not_after do
    :"tls.server.not_after"
  end

  @doc namespace: :tls
  @doc """
  Date/Time indicating when server certificate is first considered valid

      iex> OpenTelemetry.SemanticConventions.Common.tls_server_not_before()
      :"tls.server.not_before"
  """
  @spec tls_server_not_before :: :"tls.server.not_before"
  def tls_server_not_before do
    :"tls.server.not_before"
  end

  @doc namespace: :tls
  @doc """
  Distinguished name of subject of the x.509 certificate presented by the server

      iex> OpenTelemetry.SemanticConventions.Common.tls_server_subject()
      :"tls.server.subject"
  """
  @spec tls_server_subject :: :"tls.server.subject"
  def tls_server_subject do
    :"tls.server.subject"
  end

  @doc namespace: :url
  @doc """
  Domain extracted from the `url.full`, such as "opentelemetry.io"

  ### Notes

  In some cases a URL may refer to an IP and/or port directly, without a domain name. In this case, the IP address would go to the domain field. If the URL contains a [literal IPv6 address](https://www.rfc-editor.org/rfc/rfc2732#section-2) enclosed by `[` and `]`, the `[` and `]` characters should also be captured in the domain field

      iex> OpenTelemetry.SemanticConventions.Common.url_domain()
      :"url.domain"
  """
  @spec url_domain :: :"url.domain"
  def url_domain do
    :"url.domain"
  end

  @doc namespace: :url
  @doc """
  The file extension extracted from the `url.full`, excluding the leading dot

  ### Notes

  The file extension is only set if it exists, as not every url has a file extension. When the file name has multiple extensions `example.tar.gz`, only the last one should be captured `gz`, not `tar.gz`

      iex> OpenTelemetry.SemanticConventions.Common.url_extension()
      :"url.extension"
  """
  @spec url_extension :: :"url.extension"
  def url_extension do
    :"url.extension"
  end

  @doc namespace: :url
  @doc """
  The [URI fragment](https://www.rfc-editor.org/rfc/rfc3986#section-3.5) component

      iex> OpenTelemetry.SemanticConventions.Common.url_fragment()
      :"url.fragment"
  """
  @spec url_fragment :: :"url.fragment"
  def url_fragment do
    :"url.fragment"
  end

  @doc namespace: :url
  @doc """
  Absolute URL describing a network resource according to [RFC3986](https://www.rfc-editor.org/rfc/rfc3986)

  ### Notes

  For network calls, URL usually has `scheme://host[:port][path][?query][#fragment]` format, where the fragment is not transmitted over HTTP, but if it is known, it **SHOULD** be included nevertheless.
  `url.full` **MUST NOT** contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case username and password **SHOULD** be redacted and attribute's value **SHOULD** be `https://REDACTED:REDACTED@www.example.com/`.
  `url.full` **SHOULD** capture the absolute URL when it is available (or can be reconstructed). Sensitive content provided in `url.full` **SHOULD** be scrubbed when instrumentations can identify it

      iex> OpenTelemetry.SemanticConventions.Common.url_full()
      :"url.full"
  """
  @spec url_full :: :"url.full"
  def url_full do
    :"url.full"
  end

  @doc namespace: :url
  @doc """
  Unmodified original URL as seen in the event source

  ### Notes

  In network monitoring, the observed URL may be a full URL, whereas in access logs, the URL is often just represented as a path. This field is meant to represent the URL as it was observed, complete or not.
  `url.original` might contain credentials passed via URL in form of `https://username:password@www.example.com/`. In such case password and username **SHOULD NOT** be redacted and attribute's value **SHOULD** remain the same

      iex> OpenTelemetry.SemanticConventions.Common.url_original()
      :"url.original"
  """
  @spec url_original :: :"url.original"
  def url_original do
    :"url.original"
  end

  @doc namespace: :url
  @doc """
  The [URI path](https://www.rfc-editor.org/rfc/rfc3986#section-3.3) component

  ### Notes

  Sensitive content provided in `url.path` **SHOULD** be scrubbed when instrumentations can identify it

      iex> OpenTelemetry.SemanticConventions.Common.url_path()
      :"url.path"
  """
  @spec url_path :: :"url.path"
  def url_path do
    :"url.path"
  end

  @doc namespace: :url
  @doc """
  Port extracted from the `url.full`

      iex> OpenTelemetry.SemanticConventions.Common.url_port()
      :"url.port"
  """
  @spec url_port :: :"url.port"
  def url_port do
    :"url.port"
  end

  @doc namespace: :url
  @doc """
  The [URI query](https://www.rfc-editor.org/rfc/rfc3986#section-3.4) component

  ### Notes

  Sensitive content provided in `url.query` **SHOULD** be scrubbed when instrumentations can identify it

      iex> OpenTelemetry.SemanticConventions.Common.url_query()
      :"url.query"
  """
  @spec url_query :: :"url.query"
  def url_query do
    :"url.query"
  end

  @doc namespace: :url
  @doc """
  The highest registered url domain, stripped of the subdomain

  ### Notes

  This value can be determined precisely with the [public suffix list](http://publicsuffix.org). For example, the registered domain for `foo.example.com` is `example.com`. Trying to approximate this by simply taking the last two labels will not work well for TLDs such as `co.uk`

      iex> OpenTelemetry.SemanticConventions.Common.url_registered_domain()
      :"url.registered_domain"
  """
  @spec url_registered_domain :: :"url.registered_domain"
  def url_registered_domain do
    :"url.registered_domain"
  end

  @doc namespace: :url
  @doc """
  The subdomain portion of a fully qualified domain name includes all of the names except the host name under the registered_domain. In a partially qualified domain, or if the qualification level of the full name cannot be determined, subdomain contains all of the names below the registered domain

  ### Notes

  The subdomain portion of `www.east.mydomain.co.uk` is `east`. If the domain has multiple levels of subdomain, such as `sub2.sub1.example.com`, the subdomain field should contain `sub2.sub1`, with no trailing period

      iex> OpenTelemetry.SemanticConventions.Common.url_subdomain()
      :"url.subdomain"
  """
  @spec url_subdomain :: :"url.subdomain"
  def url_subdomain do
    :"url.subdomain"
  end

  @doc namespace: :url
  @doc """
  The effective top level domain (eTLD), also known as the domain suffix, is the last part of the domain name. For example, the top level domain for example.com is `com`

  ### Notes

  This value can be determined precisely with the [public suffix list](http://publicsuffix.org)

      iex> OpenTelemetry.SemanticConventions.Common.url_top_level_domain()
      :"url.top_level_domain"
  """
  @spec url_top_level_domain :: :"url.top_level_domain"
  def url_top_level_domain do
    :"url.top_level_domain"
  end

  @doc namespace: :user_agent
  @doc """
  Name of the user-agent extracted from original. Usually refers to the browser's name

  ### Notes

  [Example](https://www.whatsmyua.info) of extracting browser's name from original string. In the case of using a user-agent for non-browser products, such as microservices with multiple names/versions inside the `user_agent.original`, the most significant name **SHOULD** be selected. In such a scenario it should align with `user_agent.version`

      iex> OpenTelemetry.SemanticConventions.Common.user_agent_name()
      :"user_agent.name"
  """
  @spec user_agent_name :: :"user_agent.name"
  def user_agent_name do
    :"user_agent.name"
  end

  @doc namespace: :user_agent
  @doc """
  Value of the [HTTP User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#field.user-agent) header sent by the client

      iex> OpenTelemetry.SemanticConventions.Common.user_agent_original()
      :"user_agent.original"
  """
  @spec user_agent_original :: :"user_agent.original"
  def user_agent_original do
    :"user_agent.original"
  end

  @doc namespace: :user_agent
  @doc """
  Version of the user-agent extracted from original. Usually refers to the browser's version

  ### Notes

  [Example](https://www.whatsmyua.info) of extracting browser's version from original string. In the case of using a user-agent for non-browser products, such as microservices with multiple names/versions inside the `user_agent.original`, the most significant version **SHOULD** be selected. In such a scenario it should align with `user_agent.name`

      iex> OpenTelemetry.SemanticConventions.Common.user_agent_version()
      :"user_agent.version"
  """
  @spec user_agent_version :: :"user_agent.version"
  def user_agent_version do
    :"user_agent.version"
  end
end
