defmodule OpenTelemetry.SemanticConventions.Metric do
  @moduledoc """
  OpenTelemetry Semantic Conventions for Attributes.
  """

  @doc namespace: :container
  @typedoc """
  The CPU state for this data point. A container SHOULD be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels

  ### Options


  * `:user`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When tasks of the cgroup are in user mode (Linux). When all container processes are in user mode (Windows)

  * `:system`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When CPU is used by the system (host OS)

  * `:kernel`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - When tasks of the cgroup are in kernel mode (Linux). When all container processes are in kernel mode (Windows)



  """
  @type container_cpu_state() :: :user | :system | :kernel | atom()

  @doc namespace: :disk
  @typedoc """
  The disk IO operation direction

  ### Options


  * `:read`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - read

  * `:write`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - write



  """
  @type disk_io_direction() :: :read | :write

  @doc namespace: :network
  @typedoc """
  The network IO operation direction

  ### Options


  * `:transmit`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - transmit

  * `:receive`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - receive



  """
  @type network_io_direction() :: :transmit | :receive

  @doc namespace: :error
  @typedoc """
  Describes the error the DNS lookup failed with

  ### Options


  * `:_OTHER` - A fallback error value to be used when the instrumentation doesn't define a custom value



  """
  @type error_type() :: :_OTHER | atom()

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

  @doc namespace: :process
  @typedoc """
  The CPU state for this data point. A process SHOULD be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels

  ### Options


  * `:system`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - system

  * `:user`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - user

  * `:wait`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - wait



  """
  @type process_cpu_state() :: :system | :user | :wait | atom()

  @doc namespace: :process
  @typedoc """
  Specifies whether the context switches for this data point were voluntary or involuntary

  ### Options


  * `:voluntary`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - voluntary

  * `:involuntary`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - involuntary



  """
  @type process_context_switch_type() :: :voluntary | :involuntary | atom()

  @doc namespace: :process
  @typedoc """
  The type of page fault for this data point. Type `major` is for major/hard page faults, and `minor` is for minor/soft page faults

  ### Options


  * `:major`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - major

  * `:minor`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - minor



  """
  @type process_paging_fault_type() :: :major | :minor | atom()

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
  The paging access direction

  ### Options


  * `:in`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - in

  * `:out`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - out



  """
  @type system_paging_direction() :: :in | :out

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

  @doc """
  The URL of the OpenTelemetry schema for these keys and values.

      iex> OpenTelemetry.SemanticConventions.Metric.schema_url()
      "https://opentelemetry.io/schemas/1.25.0"
  """
  @spec schema_url :: String.t()
  def schema_url do
    "https://opentelemetry.io/schemas/1.25.0"
  end

  @doc namespace: :container

  @doc """


  The CPU state for this data point. A container SHOULD be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels

      iex> OpenTelemetry.SemanticConventions.Metric.container_cpu_state()
      :"container.cpu.state"
  """

  @spec container_cpu_state :: :"container.cpu.state"
  def container_cpu_state do
    :"container.cpu.state"
  end

  @doc namespace: :disk

  @doc """


  The disk IO operation direction

      iex> OpenTelemetry.SemanticConventions.Metric.disk_io_direction()
      :"disk.io.direction"
  """

  @spec disk_io_direction :: :"disk.io.direction"
  def disk_io_direction do
    :"disk.io.direction"
  end

  @doc namespace: :network

  @doc """


  The network IO operation direction

      iex> OpenTelemetry.SemanticConventions.Metric.network_io_direction()
      :"network.io.direction"
  """

  @spec network_io_direction :: :"network.io.direction"
  def network_io_direction do
    :"network.io.direction"
  end

  @doc namespace: :error

  @doc """


  Describes the error the DNS lookup failed with

  ### Notes

  Instrumentations SHOULD use error code such as one of errors reported by `getaddrinfo`([Linux or other POSIX systems](https://man7.org/linux/man-pages/man3/getaddrinfo.3.html) / [Windows](https://learn.microsoft.com/windows/win32/api/ws2tcpip/nf-ws2tcpip-getaddrinfo)) or one reported by the runtime or client library. If error code is not available, the full name of exception type SHOULD be used

      iex> OpenTelemetry.SemanticConventions.Metric.error_type()
      :"error.type"
  """

  @spec error_type :: :"error.type"
  def error_type do
    :"error.type"
  end

  @doc namespace: :faas

  @doc """


  Type of the trigger which caused this function invocation

      iex> OpenTelemetry.SemanticConventions.Metric.faas_trigger()
      :"faas.trigger"
  """

  @spec faas_trigger :: :"faas.trigger"
  def faas_trigger do
    :"faas.trigger"
  end

  @doc namespace: :http

  @doc """


  State of the HTTP connection in the HTTP connection pool

      iex> OpenTelemetry.SemanticConventions.Metric.http_connection_state()
      :"http.connection.state"
  """

  @spec http_connection_state :: :"http.connection.state"
  def http_connection_state do
    :"http.connection.state"
  end

  @doc namespace: :process

  @doc """


  The CPU state for this data point. A process SHOULD be characterized _either_ by data points with no `state` labels, _or only_ data points with `state` labels

      iex> OpenTelemetry.SemanticConventions.Metric.process_cpu_state()
      :"process.cpu.state"
  """

  @spec process_cpu_state :: :"process.cpu.state"
  def process_cpu_state do
    :"process.cpu.state"
  end

  @doc namespace: :process

  @doc """


  Specifies whether the context switches for this data point were voluntary or involuntary

      iex> OpenTelemetry.SemanticConventions.Metric.process_context_switch_type()
      :"process.context_switch_type"
  """

  @spec process_context_switch_type :: :"process.context_switch_type"
  def process_context_switch_type do
    :"process.context_switch_type"
  end

  @doc namespace: :process

  @doc """


  The type of page fault for this data point. Type `major` is for major/hard page faults, and `minor` is for minor/soft page faults

      iex> OpenTelemetry.SemanticConventions.Metric.process_paging_fault_type()
      :"process.paging.fault_type"
  """

  @spec process_paging_fault_type :: :"process.paging.fault_type"
  def process_paging_fault_type do
    :"process.paging.fault_type"
  end

  @doc namespace: :system

  @doc """


  The CPU state for this data point. A system's CPU SHOULD be characterized *either* by data points with no `state` labels, *or only* data points with `state` labels

      iex> OpenTelemetry.SemanticConventions.Metric.system_cpu_state()
      :"system.cpu.state"
  """

  @spec system_cpu_state :: :"system.cpu.state"
  def system_cpu_state do
    :"system.cpu.state"
  end

  @doc namespace: :system

  @doc """


  The memory state

      iex> OpenTelemetry.SemanticConventions.Metric.system_memory_state()
      :"system.memory.state"
  """

  @spec system_memory_state :: :"system.memory.state"
  def system_memory_state do
    :"system.memory.state"
  end

  @doc namespace: :system

  @doc """


  The memory paging state

      iex> OpenTelemetry.SemanticConventions.Metric.system_paging_state()
      :"system.paging.state"
  """

  @spec system_paging_state :: :"system.paging.state"
  def system_paging_state do
    :"system.paging.state"
  end

  @doc namespace: :system

  @doc """


  The memory paging type

      iex> OpenTelemetry.SemanticConventions.Metric.system_paging_type()
      :"system.paging.type"
  """

  @spec system_paging_type :: :"system.paging.type"
  def system_paging_type do
    :"system.paging.type"
  end

  @doc namespace: :system

  @doc """


  The paging access direction

      iex> OpenTelemetry.SemanticConventions.Metric.system_paging_direction()
      :"system.paging.direction"
  """

  @spec system_paging_direction :: :"system.paging.direction"
  def system_paging_direction do
    :"system.paging.direction"
  end

  @doc namespace: :system

  @doc """


  The filesystem state

      iex> OpenTelemetry.SemanticConventions.Metric.system_filesystem_state()
      :"system.filesystem.state"
  """

  @spec system_filesystem_state :: :"system.filesystem.state"
  def system_filesystem_state do
    :"system.filesystem.state"
  end

  @doc namespace: :system

  @doc """


  The filesystem type

      iex> OpenTelemetry.SemanticConventions.Metric.system_filesystem_type()
      :"system.filesystem.type"
  """

  @spec system_filesystem_type :: :"system.filesystem.type"
  def system_filesystem_type do
    :"system.filesystem.type"
  end

  @doc namespace: :system

  @doc """


  A stateless protocol MUST NOT set this attribute

      iex> OpenTelemetry.SemanticConventions.Metric.system_network_state()
      :"system.network.state"
  """

  @spec system_network_state :: :"system.network.state"
  def system_network_state do
    :"system.network.state"
  end

  @doc namespace: :system

  @doc """


  The process state, e.g., [Linux Process State Codes](https://man7.org/linux/man-pages/man1/ps.1.html#PROCESS_STATE_CODES)

      iex> OpenTelemetry.SemanticConventions.Metric.system_process_status()
      :"system.process.status"
  """

  @spec system_process_status :: :"system.process.status"
  def system_process_status do
    :"system.process.status"
  end
end
