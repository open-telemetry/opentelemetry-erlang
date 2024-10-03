defmodule OpenTelemetry.SemConv.Incubating.RPCAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for RPC attributes.
  """

  @deprecated """
  Replaced by `rpc.message.compressed_size`.
  """
  @spec message_compressed_size :: :"message.compressed_size"
  def message_compressed_size do
    :"message.compressed_size"
  end

  @deprecated """
  Replaced by `rpc.message.id`.
  """
  @spec message_id :: :"message.id"
  def message_id do
    :"message.id"
  end

  @typedoc """
  Deprecated, use `rpc.message.type` instead.

  ### Enum Values
  * `:sent` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:received` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type message_type_values() :: %{
          :sent => :SENT,
          :received => :RECEIVED
        }
  @deprecated """
  Replaced by `rpc.message.type`.
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
  Replaced by `rpc.message.uncompressed_size`.
  """
  @spec message_uncompressed_size :: :"message.uncompressed_size"
  def message_uncompressed_size do
    :"message.uncompressed_size"
  end

  @typedoc """
  The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values.

  ### Enum Values
  * `:cancelled` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:unknown` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:invalid_argument` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:deadline_exceeded` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:not_found` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:already_exists` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:permission_denied` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:resource_exhausted` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:failed_precondition` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:aborted` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:out_of_range` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:unimplemented` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:internal` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:unavailable` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:data_loss` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:unauthenticated` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
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
  @doc """
  The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_connect_rpc_error_code()
      :"rpc.connect_rpc.error_code"

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_connect_rpc_error_code_values().cancelled
      :cancelled

      iex> %{OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_connect_rpc_error_code() => OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_connect_rpc_error_code_values().cancelled}
      %{:"rpc.connect_rpc.error_code" => :cancelled}

  ### Erlang

  ```erlang
  ?RPC_CONNECT_RPC_ERROR_CODE.
  'rpc.connect_rpc.error_code'

  ?RPC_CONNECT_RPC_ERROR_CODE_VALUES_CANCELLED.
  'cancelled'

  \#{?RPC_CONNECT_RPC_ERROR_CODE => ?RPC_CONNECT_RPC_ERROR_CODE_VALUES_CANCELLED}.
  \#{'rpc.connect_rpc.error_code' => 'cancelled'}
  ```

  <!-- tabs-close -->
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

  @doc """
  Connect request metadata, `<key>` being the normalized Connect Metadata key (lowercase), the value being the metadata values.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which metadata values are to be captured. Including all request metadata values can be a security risk - explicit configuration helps avoid leaking sensitive information.

  ### Examples

  ```
  ["rpc.request.metadata.my-custom-metadata-attribute=[\"1.2.3.4\", \"1.2.3.5\"]"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_connect_rpc_request_metadata()
      :"rpc.connect_rpc.request.metadata"

  ### Erlang

  ```erlang
  ?RPC_CONNECT_RPC_REQUEST_METADATA.
  'rpc.connect_rpc.request.metadata'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_connect_rpc_request_metadata :: :"rpc.connect_rpc.request.metadata"
  def rpc_connect_rpc_request_metadata do
    :"rpc.connect_rpc.request.metadata"
  end

  @doc """
  Connect response metadata, `<key>` being the normalized Connect Metadata key (lowercase), the value being the metadata values.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which metadata values are to be captured. Including all response metadata values can be a security risk - explicit configuration helps avoid leaking sensitive information.

  ### Examples

  ```
  ["rpc.response.metadata.my-custom-metadata-attribute=[\"attribute_value\"]"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_connect_rpc_response_metadata()
      :"rpc.connect_rpc.response.metadata"

  ### Erlang

  ```erlang
  ?RPC_CONNECT_RPC_RESPONSE_METADATA.
  'rpc.connect_rpc.response.metadata'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_connect_rpc_response_metadata :: :"rpc.connect_rpc.response.metadata"
  def rpc_connect_rpc_response_metadata do
    :"rpc.connect_rpc.response.metadata"
  end

  @doc """
  gRPC request metadata, `<key>` being the normalized gRPC Metadata key (lowercase), the value being the metadata values.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which metadata values are to be captured. Including all request metadata values can be a security risk - explicit configuration helps avoid leaking sensitive information.

  ### Examples

  ```
  ["rpc.grpc.request.metadata.my-custom-metadata-attribute=[\"1.2.3.4\", \"1.2.3.5\"]"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_grpc_request_metadata()
      :"rpc.grpc.request.metadata"

  ### Erlang

  ```erlang
  ?RPC_GRPC_REQUEST_METADATA.
  'rpc.grpc.request.metadata'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_grpc_request_metadata :: :"rpc.grpc.request.metadata"
  def rpc_grpc_request_metadata do
    :"rpc.grpc.request.metadata"
  end

  @doc """
  gRPC response metadata, `<key>` being the normalized gRPC Metadata key (lowercase), the value being the metadata values.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which metadata values are to be captured. Including all response metadata values can be a security risk - explicit configuration helps avoid leaking sensitive information.

  ### Examples

  ```
  ["rpc.grpc.response.metadata.my-custom-metadata-attribute=[\"attribute_value\"]"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_grpc_response_metadata()
      :"rpc.grpc.response.metadata"

  ### Erlang

  ```erlang
  ?RPC_GRPC_RESPONSE_METADATA.
  'rpc.grpc.response.metadata'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_grpc_response_metadata :: :"rpc.grpc.response.metadata"
  def rpc_grpc_response_metadata do
    :"rpc.grpc.response.metadata"
  end

  @typedoc """
  The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request.

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
  @doc """
  The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_grpc_status_code()
      :"rpc.grpc.status_code"

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_grpc_status_code_values().ok
      0

      iex> %{OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_grpc_status_code() => OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_grpc_status_code_values().ok}
      %{:"rpc.grpc.status_code" => 0}

  ### Erlang

  ```erlang
  ?RPC_GRPC_STATUS_CODE.
  'rpc.grpc.status_code'

  ?RPC_GRPC_STATUS_CODE_VALUES_OK.
  '0'

  \#{?RPC_GRPC_STATUS_CODE => ?RPC_GRPC_STATUS_CODE_VALUES_OK}.
  \#{'rpc.grpc.status_code' => '0'}
  ```

  <!-- tabs-close -->
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

  @doc """
  `error.code` property of response if it is an error response.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [-32700, 100]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_jsonrpc_error_code()
      :"rpc.jsonrpc.error_code"

  ### Erlang

  ```erlang
  ?RPC_JSONRPC_ERROR_CODE.
  'rpc.jsonrpc.error_code'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_jsonrpc_error_code :: :"rpc.jsonrpc.error_code"
  def rpc_jsonrpc_error_code do
    :"rpc.jsonrpc.error_code"
  end

  @doc """
  `error.message` property of response if it is an error response.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Parse error", "User already exists"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_jsonrpc_error_message()
      :"rpc.jsonrpc.error_message"

  ### Erlang

  ```erlang
  ?RPC_JSONRPC_ERROR_MESSAGE.
  'rpc.jsonrpc.error_message'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_jsonrpc_error_message :: :"rpc.jsonrpc.error_message"
  def rpc_jsonrpc_error_message do
    :"rpc.jsonrpc.error_message"
  end

  @doc """
  `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["10", "request-7", ""]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_jsonrpc_request_id()
      :"rpc.jsonrpc.request_id"

  ### Erlang

  ```erlang
  ?RPC_JSONRPC_REQUEST_ID.
  'rpc.jsonrpc.request_id'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_jsonrpc_request_id :: :"rpc.jsonrpc.request_id"
  def rpc_jsonrpc_request_id do
    :"rpc.jsonrpc.request_id"
  end

  @doc """
  Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 doesn't specify this, the value can be omitted.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2.0", "1.0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_jsonrpc_version()
      :"rpc.jsonrpc.version"

  ### Erlang

  ```erlang
  ?RPC_JSONRPC_VERSION.
  'rpc.jsonrpc.version'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_jsonrpc_version :: :"rpc.jsonrpc.version"
  def rpc_jsonrpc_version do
    :"rpc.jsonrpc.version"
  end

  @doc """
  Compressed size of the message in bytes.
  ### Value type

  Value must be of type `integer()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_message_compressed_size()
      :"rpc.message.compressed_size"

  ### Erlang

  ```erlang
  ?RPC_MESSAGE_COMPRESSED_SIZE.
  'rpc.message.compressed_size'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_message_compressed_size :: :"rpc.message.compressed_size"
  def rpc_message_compressed_size do
    :"rpc.message.compressed_size"
  end

  @doc """
  MUST be calculated as two different counters starting from `1` one for sent messages and one for received message.
  ### Value type

  Value must be of type `integer()`.
  ### Notes

  This way we guarantee that the values will be consistent between different implementations.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_message_id()
      :"rpc.message.id"

  ### Erlang

  ```erlang
  ?RPC_MESSAGE_ID.
  'rpc.message.id'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_message_id :: :"rpc.message.id"
  def rpc_message_id do
    :"rpc.message.id"
  end

  @typedoc """
  Whether this is a received or sent message.

  ### Enum Values
  * `:sent` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:received` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type rpc_message_type_values() :: %{
          :sent => :SENT,
          :received => :RECEIVED
        }
  @doc """
  Whether this is a received or sent message.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_message_type()
      :"rpc.message.type"

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_message_type_values().sent
      :SENT

      iex> %{OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_message_type() => OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_message_type_values().sent}
      %{:"rpc.message.type" => :SENT}

  ### Erlang

  ```erlang
  ?RPC_MESSAGE_TYPE.
  'rpc.message.type'

  ?RPC_MESSAGE_TYPE_VALUES_SENT.
  'SENT'

  \#{?RPC_MESSAGE_TYPE => ?RPC_MESSAGE_TYPE_VALUES_SENT}.
  \#{'rpc.message.type' => 'SENT'}
  ```

  <!-- tabs-close -->
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

  @doc """
  Uncompressed size of the message in bytes.
  ### Value type

  Value must be of type `integer()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_message_uncompressed_size()
      :"rpc.message.uncompressed_size"

  ### Erlang

  ```erlang
  ?RPC_MESSAGE_UNCOMPRESSED_SIZE.
  'rpc.message.uncompressed_size'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_message_uncompressed_size :: :"rpc.message.uncompressed_size"
  def rpc_message_uncompressed_size do
    :"rpc.message.uncompressed_size"
  end

  @doc """
  The name of the (logical) method being called, must be equal to the $method part in the span name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side).

  ### Examples

  ```
  exampleMethod
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
  The full (logical) name of the service being called, including its package name, if applicable.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side).

  ### Examples

  ```
  myservice.EchoService
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_service()
      :"rpc.service"

  ### Erlang

  ```erlang
  ?RPC_SERVICE.
  'rpc.service'
  ```

  <!-- tabs-close -->
  """
  @spec rpc_service :: :"rpc.service"
  def rpc_service do
    :"rpc.service"
  end

  @typedoc """
  A string identifying the remoting system. See below for a list of well-known identifiers.

  ### Enum Values
  * `:grpc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - gRPC
  * `:java_rmi` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Java RMI
  * `:dotnet_wcf` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - .NET WCF
  * `:apache_dubbo` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Apache Dubbo
  * `:connect_rpc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Connect RPC
  """
  @type rpc_system_values() :: %{
          :grpc => :grpc,
          :java_rmi => :java_rmi,
          :dotnet_wcf => :dotnet_wcf,
          :apache_dubbo => :apache_dubbo,
          :connect_rpc => :connect_rpc
        }
  @doc """
  A string identifying the remoting system. See below for a list of well-known identifiers.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_system()
      :"rpc.system"

      iex> OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_system_values().grpc
      :grpc

      iex> %{OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_system() => OpenTelemetry.SemConv.Incubating.RPCAttributes.rpc_system_values().grpc}
      %{:"rpc.system" => :grpc}

  ### Erlang

  ```erlang
  ?RPC_SYSTEM.
  'rpc.system'

  ?RPC_SYSTEM_VALUES_GRPC.
  'grpc'

  \#{?RPC_SYSTEM => ?RPC_SYSTEM_VALUES_GRPC}.
  \#{'rpc.system' => 'grpc'}
  ```

  <!-- tabs-close -->
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
      :connect_rpc => :connect_rpc
    }
  end
end
