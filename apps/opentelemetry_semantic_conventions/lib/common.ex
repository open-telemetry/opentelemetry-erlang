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
  @type http_request_method() :: :CONNECT | :DELETE | :GET | :HEAD | :OPTIONS | :PATCH | :POST | :PUT | :TRACE | :_OTHER | atom()
  
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
  @type messaging_system() :: :activemq | :aws_sqs | :eventgrid | :eventhubs | :servicebus | :gcp_pubsub | :jms | :kafka | :rabbitmq | :rocketmq | atom()
  
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
  The CPU state for this data point. A process SHOULD be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels

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
  The CPU state for this data point. A system's CPU SHOULD be characterized *either* by data points with no `state` labels, *or only* data points with `state` labels

  ### Options

  
  * `:user`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - user
  
  * `:system`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - system
  
  * `:nice`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - nice
  
  * `:idle`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - idle
  
  * `:iowait`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - iowait
  
  * `:interrupt`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - interrupt
  
  * `:steal`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - steal
  

  
  """
  @type system_cpu_state() :: :user | :system | :nice | :idle | :iowait | :interrupt | :steal | atom()
  
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
  A stateless protocol MUST NOT set this attribute

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
  @type system_network_state() :: :close | :close_wait | :closing | :delete | :established | :fin_wait_1 | :fin_wait_2 | :last_ack | :listen | :syn_recv | :syn_sent | :time_wait
  
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
  @type network_connection_subtype() :: :gprs | :edge | :umts | :cdma | :evdo_0 | :evdo_a | :cdma2000_1xrtt | :hsdpa | :hsupa | :hspa | :iden | :evdo_b | :lte | :ehrpd | :hspap | :gsm | :td_scdma | :iwlan | :nr | :nrnsa | :lte_ca | atom()
  
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
  @type db_cassandra_consistency_level() :: :all | :each_quorum | :quorum | :local_quorum | :one | :two | :three | :local_one | :any | :serial | :local_serial
  
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
  @type db_cosmosdb_operation_type() :: :Invalid | :Create | :Patch | :Read | :ReadFeed | :Delete | :Replace | :Execute | :Query | :Head | :HeadFeed | :Upsert | :Batch | :QueryPlan | :ExecuteJavaScript | atom()
  
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
  @type db_system() :: :other_sql | :mssql | :mssqlcompact | :mysql | :oracle | :db2 | :postgresql | :redshift | :hive | :cloudscape | :hsqldb | :progress | :maxdb | :hanadb | :ingres | :firstsql | :edb | :cache | :adabas | :firebird | :derby | :filemaker | :informix | :instantdb | :interbase | :mariadb | :netezza | :pervasive | :pointbase | :sqlite | :sybase | :teradata | :vertica | :h2 | :coldfusion | :cassandra | :hbase | :mongodb | :redis | :couchbase | :couchdb | :cosmosdb | :dynamodb | :neo4j | :geode | :elasticsearch | :memcached | :cockroachdb | :opensearch | :clickhouse | :spanner | :trino | atom()
  
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
  @type messaging_servicebus_disposition_status() :: :complete | :abandon | :dead_letter | :defer | atom()
  
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
  @type rpc_connect_rpc_error_code() :: :cancelled | :unknown | :invalid_argument | :deadline_exceeded | :not_found | :already_exists | :permission_denied | :resource_exhausted | :failed_precondition | :aborted | :out_of_range | :unimplemented | :internal | :unavailable | :data_loss | :unauthenticated
  
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
  @type rpc_grpc_status_code() :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16
  
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
  @type telemetry_sdk_language() :: :cpp | :dotnet | :erlang | :go | :java | :nodejs | :php | :python | :ruby | :rust | :swift | :webjs | atom()
  
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
  

  The cloud provider of the invoked function

  ### Notes

  SHOULD be equal to the `cloud.provider` resource attribute of the invoked function

      iex> OpenTelemetry.SemanticConventions.Common.faas_invoked_provider()
      :"faas.invoked_provider"
  """
  
  @spec faas_invoked_provider :: :"faas.invoked_provider"
  def faas_invoked_provider do
    :"faas.invoked_provider"
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
  `error.type` SHOULD be set to exception type (its fully-qualified class name, if applicable)
  or a component-specific low cardinality error identifier.
  
  If response status code was sent or received and status indicates an error according to [HTTP span status definition](/docs/http/http-spans.md),
  `error.type` SHOULD be set to the status code number (represented as a string), an exception type (if thrown) or a component-specific error identifier.
  
  The `error.type` value SHOULD be predictable and SHOULD have low cardinality.
  Instrumentations SHOULD document the list of errors they report.
  
  The cardinality of `error.type` within one instrumentation library SHOULD be low, but
  telemetry consumers that aggregate data from multiple instrumentation libraries and applications
  should be prepared for `error.type` to have high cardinality at query time, when no
  additional filters are applied.
  
  If the request has completed successfully, instrumentations SHOULD NOT set `error.type`

      iex> OpenTelemetry.SemanticConventions.Common.error_type()
      :"error.type"
  """
  
  @spec error_type :: :"error.type"
  def error_type do
    :"error.type"
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
  
  @doc namespace: :network
  
  @doc """
  

  [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication)

  ### Notes

  The value SHOULD be normalized to lowercase.
  
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

  The value SHOULD be normalized to lowercase

      iex> OpenTelemetry.SemanticConventions.Common.network_type()
      :"network.type"
  """
  
  @spec network_type :: :"network.type"
  def network_type do
    :"network.type"
  end
  
  @doc namespace: :process
  
  @doc """
  

  The CPU state for this data point. A process SHOULD be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels

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
  
  @doc namespace: :system
  
  @doc """
  

  The CPU state for this data point. A system's CPU SHOULD be characterized *either* by data points with no `state` labels, *or only* data points with `state` labels

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
  

  A stateless protocol MUST NOT set this attribute

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
  

  An identifier for the database management system (DBMS) product being used. See below for a list of well-known identifiers

      iex> OpenTelemetry.SemanticConventions.Common.db_system()
      :"db.system"
  """
  
  @spec db_system :: :"db.system"
  def db_system do
    :"db.system"
  end
  
  @doc namespace: :http
  
  @deprecated """
  Replaced by `network.protocol.name`
  """
  
  @spec http_flavor :: :"http.flavor"
  def http_flavor do
    :"http.flavor"
  end
  
  @doc namespace: :system
  
  @deprecated """
  Replaced by `system.process.status`
  """
  
  @spec system_processes_status :: :"system.processes.status"
  def system_processes_status do
    :"system.processes.status"
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
  
  @doc namespace: :messaging
  
  @doc """
  

  A string identifying the kind of messaging operation

  ### Notes

  If a custom value is used, it MUST be of low cardinality

      iex> OpenTelemetry.SemanticConventions.Common.messaging_operation()
      :"messaging.operation"
  """
  
  @spec messaging_operation :: :"messaging.operation"
  def messaging_operation do
    :"messaging.operation"
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
  

  Describes the [settlement type](https://learn.microsoft.com/azure/service-bus-messaging/message-transfers-locks-settlement#peeklock)

      iex> OpenTelemetry.SemanticConventions.Common.messaging_servicebus_disposition_status()
      :"messaging.servicebus.disposition_status"
  """
  
  @spec messaging_servicebus_disposition_status :: :"messaging.servicebus.disposition_status"
  def messaging_servicebus_disposition_status do
    :"messaging.servicebus.disposition_status"
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
end