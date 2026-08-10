defmodule OpenTelemetry.SemConv.Incubating.RPCAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for RPC attributes.
  """

  @deprecated """
  Deprecated, no replacement at this time.
  """
  @spec message_compressed_size :: :"message.compressed_size"
  def message_compressed_size do
    :"message.compressed_size"
  end

  @deprecated """
  Deprecated, no replacement at this time.
  """
  @spec message_id :: :"message.id"
  def message_id do
    :"message.id"
  end

  @typedoc """
  Deprecated, no replacement at this time.

  ### Enum Values
  * `:sent` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:received` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  """
  @type message_type_values() :: %{
          :sent => :SENT,
          :received => :RECEIVED
        }
  @deprecated """
  Deprecated, no replacement at this time.
  """
  @spec message_type :: :"message.type"
  def message_type do
    :"message.type"
  end

  @spec message_type_values() :: message_type_values()
  def message_type_values() do
    %{
      :sent => :SENT,
      :received => :RECEIVED
    }
  end

  @deprecated """
  Deprecated, no replacement at this time.
  """
  @spec message_uncompressed_size :: :"message.uncompressed_size"
  def message_uncompressed_size do
    :"message.uncompressed_size"
  end

  @typedoc """
  Deprecated, use `rpc.response.status_code` attribute instead.

  ### Enum Values
  * `:cancelled` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:unknown` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:invalid_argument` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:deadline_exceeded` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:not_found` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:already_exists` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:permission_denied` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:resource_exhausted` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:failed_precondition` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:aborted` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:out_of_range` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:unimplemented` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:internal` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:unavailable` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:data_loss` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:unauthenticated` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  """
  @type rpc_connect_rpc_error_code_values() :: %{
          :cancelled => :cancelled,
          :unknown => :unknown,
          :invalid_argument => :invalid_argument,
          :deadline_exceeded => :deadline_exceeded,
          :not_found => :not_found,
          :already_exists => :already_exists,
          :permission_denied => :permission_denied,
          :resource_exhausted => :resource_exhausted,
          :failed_precondition => :failed_precondition,
          :aborted => :aborted,
          :out_of_range => :out_of_range,
          :unimplemented => :unimplemented,
          :internal => :internal,
          :unavailable => :unavailable,
          :data_loss => :data_loss,
          :unauthenticated => :unauthenticated
        }
  @deprecated """
  Replaced by `rpc.response.status_code`.
  """
  @spec rpc_connect_rpc_error_code :: :"rpc.connect_rpc.error_code"
  def rpc_connect_rpc_error_code do
    :"rpc.connect_rpc.error_code"
  end

  @spec rpc_connect_rpc_error_code_values() :: rpc_connect_rpc_error_code_values()
  def rpc_connect_rpc_error_code_values() do
    %{
      :cancelled => :cancelled,
      :unknown => :unknown,
      :invalid_argument => :invalid_argument,
      :deadline_exceeded => :deadline_exceeded,
      :not_found => :not_found,
      :already_exists => :already_exists,
      :permission_denied => :permission_denied,
      :resource_exhausted => :resource_exhausted,
      :failed_precondition => :failed_precondition,
      :aborted => :aborted,
      :out_of_range => :out_of_range,
      :unimplemented => :unimplemented,
      :internal => :internal,
      :unavailable => :unavailable,
      :data_loss => :data_loss,
      :unauthenticated => :unauthenticated
    }
  end

  @deprecated """
  Replaced by `rpc.request.metadata`.
  """
  @spec rpc_connect_rpc_request_metadata :: :"rpc.connect_rpc.request.metadata"
  def rpc_connect_rpc_request_metadata do
    :"rpc.connect_rpc.request.metadata"
  end

  @deprecated """
  Replaced by `rpc.response.metadata`.
  """
  @spec rpc_connect_rpc_response_metadata :: :"rpc.connect_rpc.response.metadata"
  def rpc_connect_rpc_response_metadata do
    :"rpc.connect_rpc.response.metadata"
  end

  @deprecated """
  Replaced by `rpc.request.metadata`.
  """
  @spec rpc_grpc_request_metadata :: :"rpc.grpc.request.metadata"
  def rpc_grpc_request_metadata do
    :"rpc.grpc.request.metadata"
  end

  @deprecated """
  Replaced by `rpc.response.metadata`.
  """
  @spec rpc_grpc_response_metadata :: :"rpc.grpc.response.metadata"
  def rpc_grpc_response_metadata do
    :"rpc.grpc.response.metadata"
  end

  @typedoc """
  Deprecated, use string representation on the `rpc.response.status_code` attribute instead.

  ### Enum Values
  * `:ok` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OK
  * `:cancelled` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - CANCELLED
  * `:unknown` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - UNKNOWN
  * `:invalid_argument` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - INVALID_ARGUMENT
  * `:deadline_exceeded` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - DEADLINE_EXCEEDED
  * `:not_found` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - NOT_FOUND
  * `:already_exists` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - ALREADY_EXISTS
  * `:permission_denied` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - PERMISSION_DENIED
  * `:resource_exhausted` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - RESOURCE_EXHAUSTED
  * `:failed_precondition` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - FAILED_PRECONDITION
  * `:aborted` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - ABORTED
  * `:out_of_range` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OUT_OF_RANGE
  * `:unimplemented` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - UNIMPLEMENTED
  * `:internal` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - INTERNAL
  * `:unavailable` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - UNAVAILABLE
  * `:data_loss` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - DATA_LOSS
  * `:unauthenticated` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - UNAUTHENTICATED
  """
  @type rpc_grpc_status_code_values() :: %{
          :ok => 0,
          :cancelled => 1,
          :unknown => 2,
          :invalid_argument => 3,
          :deadline_exceeded => 4,
          :not_found => 5,
          :already_exists => 6,
          :permission_denied => 7,
          :resource_exhausted => 8,
          :failed_precondition => 9,
          :aborted => 10,
          :out_of_range => 11,
          :unimplemented => 12,
          :internal => 13,
          :unavailable => 14,
          :data_loss => 15,
          :unauthenticated => 16
        }
  @deprecated """
  Use string representation of the gRPC status code on the `rpc.response.status_code` attribute.
  """
  @spec rpc_grpc_status_code :: :"rpc.grpc.status_code"
  def rpc_grpc_status_code do
    :"rpc.grpc.status_code"
  end

  @spec rpc_grpc_status_code_values() :: rpc_grpc_status_code_values()
  def rpc_grpc_status_code_values() do
    %{
      :ok => 0,
      :cancelled => 1,
      :unknown => 2,
      :invalid_argument => 3,
      :deadline_exceeded => 4,
      :not_found => 5,
      :already_exists => 6,
      :permission_denied => 7,
      :resource_exhausted => 8,
      :failed_precondition => 9,
      :aborted => 10,
      :out_of_range => 11,
      :unimplemented => 12,
      :internal => 13,
      :unavailable => 14,
      :data_loss => 15,
      :unauthenticated => 16
    }
  end

  @deprecated """
  Use string representation of the error code on the `rpc.response.status_code` attribute.
  """
  @spec rpc_jsonrpc_error_code :: :"rpc.jsonrpc.error_code"
  def rpc_jsonrpc_error_code do
    :"rpc.jsonrpc.error_code"
  end

  @deprecated """
  Use the span status description when reporting JSON-RPC spans.
  """
  @spec rpc_jsonrpc_error_message :: :"rpc.jsonrpc.error_message"
  def rpc_jsonrpc_error_message do
    :"rpc.jsonrpc.error_message"
  end

  @deprecated """
  Replaced by `jsonrpc.request.id`.
  """
  @spec rpc_jsonrpc_request_id :: :"rpc.jsonrpc.request_id"
  def rpc_jsonrpc_request_id do
    :"rpc.jsonrpc.request_id"
  end

  @deprecated """
  Replaced by `jsonrpc.protocol.version`.
  """
  @spec rpc_jsonrpc_version :: :"rpc.jsonrpc.version"
  def rpc_jsonrpc_version do
    :"rpc.jsonrpc.version"
  end

  @deprecated """
  Deprecated, no replacement at this time.
  """
  @spec rpc_message_compressed_size :: :"rpc.message.compressed_size"
  def rpc_message_compressed_size do
    :"rpc.message.compressed_size"
  end

  @deprecated """
  Deprecated, no replacement at this time.
  """
  @spec rpc_message_id :: :"rpc.message.id"
  def rpc_message_id do
    :"rpc.message.id"
  end

  @typedoc """
  Whether this is a received or sent message.

  ### Enum Values
  * `:sent` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:received` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  """
  @type rpc_message_type_values() :: %{
          :sent => :SENT,
          :received => :RECEIVED
        }
  @deprecated """
  Deprecated, no replacement at this time.
  """
  @spec rpc_message_type :: :"rpc.message.type"
  def rpc_message_type do
    :"rpc.message.type"
  end

  @spec rpc_message_type_values() :: rpc_message_type_values()
  def rpc_message_type_values() do
    %{
      :sent => :SENT,
      :received => :RECEIVED
    }
  end

  @deprecated """
  Deprecated, no replacement at this time.
  """
  @spec rpc_message_uncompressed_size :: :"rpc.message.uncompressed_size"
  def rpc_message_uncompressed_size do
    :"rpc.message.uncompressed_size"
  end

  @doc """
  The fully-qualified logical name of the method from the RPC interface perspective.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The method name **MAY** have unbounded cardinality in edge or error cases.

  Some RPC frameworks or libraries provide a fixed set of recognized methods
  for client stubs and server implementations. Instrumentations for such
  frameworks **MUST** set this attribute to the original method name only
  when the method is recognized by the framework or library.

  When the method is not recognized, for example, when the server receives
  a request for a method that is not predefined on the server, or when
  instrumentation is not able to reliably detect if the method is predefined,
  the attribute **MUST** be set to `_OTHER`. In such cases, tracing
  instrumentations **MUST** also set `rpc.method_original` attribute to
  the original method value.

  If the RPC instrumentation could end up converting valid RPC methods to
  `_OTHER`, then it **SHOULD** provide a way to configure the list of recognized
  RPC methods.

  The `rpc.method` can be different from the name of any implementing
  method/function.
  The `code.function.name` attribute may be used to record the fully-qualified
  method actually executing the call on the server side, or the
  RPC client stub method on the client side.

  ### Examples

  ```
  ["com.example.ExampleService/exampleMethod", "EchoService/Echo", "_OTHER"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_method()
      :"rpc.method"

  ### Erlang

  ```erlang
  ?RPC_METHOD.
  'rpc.method'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_method :: :"rpc.method"
  def rpc_method do
    :"rpc.method"
  end

  @doc """
  The original name of the method used by the client.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["com.myservice.EchoService/catchAll", "com.myservice.EchoService/unknownMethod", "InvalidMethod"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_method_original()
      :"rpc.method_original"

  ### Erlang

  ```erlang
  ?RPC_METHOD_ORIGINAL.
  'rpc.method_original'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_method_original :: :"rpc.method_original"
  def rpc_method_original do
    :"rpc.method_original"
  end

  @doc """
  RPC request metadata, `<key>` being the normalized RPC metadata key (lowercase), the value being the metadata values.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which metadata values are to be captured.
  Including all request metadata values can be a security risk - explicit configuration helps avoid leaking sensitive information.

  For example, a property `my-custom-key` with value `["1.2.3.4", "1.2.3.5"]` **SHOULD** be recorded as
  `rpc.request.metadata.my-custom-key` attribute with value `["1.2.3.4", "1.2.3.5"]`

  ### Examples

  ```
  [["1.2.3.4", "1.2.3.5"]]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_request_metadata()
      :"rpc.request.metadata"

  ### Erlang

  ```erlang
  ?RPC_REQUEST_METADATA.
  'rpc.request.metadata'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_request_metadata :: :"rpc.request.metadata"
  def rpc_request_metadata do
    :"rpc.request.metadata"
  end

  @doc """
  RPC response metadata, `<key>` being the normalized RPC metadata key (lowercase), the value being the metadata values.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which metadata values are to be captured.
  Including all response metadata values can be a security risk - explicit configuration helps avoid leaking sensitive information.

  For example, a property `my-custom-key` with value `["attribute_value"]` **SHOULD** be recorded as
  the `rpc.response.metadata.my-custom-key` attribute with value `["attribute_value"]`

  ### Examples

  ```
  [["attribute_value"]]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_response_metadata()
      :"rpc.response.metadata"

  ### Erlang

  ```erlang
  ?RPC_RESPONSE_METADATA.
  'rpc.response.metadata'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_response_metadata :: :"rpc.response.metadata"
  def rpc_response_metadata do
    :"rpc.response.metadata"
  end

  @doc """
  Status code of the RPC returned by the RPC server or generated by the client
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Usually it represents an error code, but may also represent partial success, warning, or differentiate between various types of successful outcomes.
  Semantic conventions for individual RPC frameworks **SHOULD** document what `rpc.response.status_code` means in the context of that system and which values are considered to represent errors.

  ### Examples

  ```
  ["OK", "DEADLINE_EXCEEDED", "-32602"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_response_status_code()
      :"rpc.response.status_code"

  ### Erlang

  ```erlang
  ?RPC_RESPONSE_STATUS_CODE.
  'rpc.response.status_code'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_response_status_code :: :"rpc.response.status_code"
  def rpc_response_status_code do
    :"rpc.response.status_code"
  end

  @deprecated """
  Value should be included in `rpc.method` which is expected to be a fully-qualified name.
  """
  @spec rpc_service :: :"rpc.service"
  def rpc_service do
    :"rpc.service"
  end

  @typedoc """
  Deprecated, use `rpc.system.name` attribute instead.

  ### Enum Values
  * `:grpc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - gRPC
  * `:java_rmi` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Java RMI
  * `:dotnet_wcf` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - .NET WCF
  * `:apache_dubbo` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Dubbo
  * `:connect_rpc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Connect RPC
  * `:onc_rpc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [ONC RPC (Sun RPC)](https://datatracker.ietf.org/doc/html/rfc5531)
  * `:jsonrpc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - JSON-RPC
  """
  @type rpc_system_values() :: %{
          :grpc => :grpc,
          :java_rmi => :java_rmi,
          :dotnet_wcf => :dotnet_wcf,
          :apache_dubbo => :apache_dubbo,
          :connect_rpc => :connect_rpc,
          :onc_rpc => :onc_rpc,
          :jsonrpc => :jsonrpc
        }
  @deprecated """
  Replaced by `rpc.system.name`.
  """
  @spec rpc_system :: :"rpc.system"
  def rpc_system do
    :"rpc.system"
  end

  @spec rpc_system_values() :: rpc_system_values()
  def rpc_system_values() do
    %{
      :grpc => :grpc,
      :java_rmi => :java_rmi,
      :dotnet_wcf => :dotnet_wcf,
      :apache_dubbo => :apache_dubbo,
      :connect_rpc => :connect_rpc,
      :onc_rpc => :onc_rpc,
      :jsonrpc => :jsonrpc
    }
  end

  @typedoc """
  The Remote Procedure Call (RPC) system.

  ### Enum Values
  * `:grpc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [gRPC](https://grpc.io/)
  * `:dubbo` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Apache Dubbo](https://dubbo.apache.org/)
  * `:connectrpc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Connect RPC](https://connectrpc.com/)
  * `:jsonrpc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [JSON-RPC](https://www.jsonrpc.org/)
  """
  @type rpc_system_name_values() :: %{
          :grpc => :grpc,
          :dubbo => :dubbo,
          :connectrpc => :connectrpc,
          :jsonrpc => :jsonrpc
        }
  @doc """
  The Remote Procedure Call (RPC) system.

  ### Notes

  The client and server RPC systems may differ for the same RPC interaction. For example, a client may use Apache Dubbo or Connect RPC to communicate with a server that uses gRPC since both protocols provide compatibility with gRPC.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_system_name()
      :"rpc.system.name"

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_system_name_values().grpc
      :grpc

      iex> %{OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_system_name() => OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_system_name_values().grpc}
      %{:"rpc.system.name" => :grpc}

  ### Erlang

  ```erlang
  ?RPC_SYSTEM_NAME.
  'rpc.system.name'

  ?RPC_SYSTEM_NAME_VALUES_GRPC.
  'grpc'

  \#{?RPC_SYSTEM_NAME => ?RPC_SYSTEM_NAME_VALUES_GRPC}.
  \#{'rpc.system.name' => 'grpc'}
  ```

  <!-- tabs-close -->
  """
  @spec rpc_system_name :: :"rpc.system.name"
  def rpc_system_name do
    :"rpc.system.name"
  end

  @spec rpc_system_name_values() :: rpc_system_name_values()
  def rpc_system_name_values() do
    %{
      :grpc => :grpc,
      :dubbo => :dubbo,
      :connectrpc => :connectrpc,
      :jsonrpc => :jsonrpc
    }
  end
end
