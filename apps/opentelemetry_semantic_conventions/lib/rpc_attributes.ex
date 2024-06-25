defmodule OpenTelemetry.SemanticConventions.RpcAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Rpc attributes.
  """

  @deprecated """
  Replaced by `rpc.message.compressed_size`.
  """
  @spec message_compressedsize :: :"message.compressed_size"
  def message_compressedsize do
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
  * `:sent` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:received` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  """
  @type message_type() :: %{
          :sent => :SENT,
          :received => :RECEIVED
        }
  @deprecated """
  Replaced by `rpc.message.type`.
  """
  @spec message_type() :: message_type()
  def message_type() do
    %{
      :sent => :SENT,
      :received => :RECEIVED
    }
  end

  @spec message_type(atom() | String.t()) :: atom() | String.t()
  def message_type(custom_value) do
    custom_value
  end

  @deprecated """
  Replaced by `rpc.message.uncompressed_size`.
  """
  @spec message_uncompressedsize :: :"message.uncompressed_size"
  def message_uncompressedsize do
    :"message.uncompressed_size"
  end

  @typedoc """
  The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values.

  ### Enum Values
  * `:cancelled` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:unknown` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:invalid_argument` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:deadline_exceeded` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:not_found` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:already_exists` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:permission_denied` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:resource_exhausted` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:failed_precondition` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:aborted` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:out_of_range` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:unimplemented` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:internal` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:unavailable` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:data_loss` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:unauthenticated` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  """
  @type rpc_connectrpc_errorcode() :: %{
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


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_connectrpc_errorcode().cancelled
      :cancelled
      
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_connectrpc_errorcode(:custom_value)
      :custom_value
  """
  @spec rpc_connectrpc_errorcode() :: rpc_connectrpc_errorcode()
  def rpc_connectrpc_errorcode() do
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

  @spec rpc_connectrpc_errorcode(atom() | String.t()) :: atom() | String.t()
  def rpc_connectrpc_errorcode(custom_value) do
    custom_value
  end

  @doc """
  Connect request metadata, `<key>` being the normalized Connect Metadata key (lowercase), the value being the metadata values.

  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which metadata values are to be captured. Including all request metadata values can be a security risk - explicit configuration helps avoid leaking sensitive information.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_connectrpc_request_metadata()
      :"rpc.connect_rpc.request.metadata"
  """
  @spec rpc_connectrpc_request_metadata :: :"rpc.connect_rpc.request.metadata"
  def rpc_connectrpc_request_metadata do
    :"rpc.connect_rpc.request.metadata"
  end

  @doc """
  Connect response metadata, `<key>` being the normalized Connect Metadata key (lowercase), the value being the metadata values.

  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which metadata values are to be captured. Including all response metadata values can be a security risk - explicit configuration helps avoid leaking sensitive information.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_connectrpc_response_metadata()
      :"rpc.connect_rpc.response.metadata"
  """
  @spec rpc_connectrpc_response_metadata :: :"rpc.connect_rpc.response.metadata"
  def rpc_connectrpc_response_metadata do
    :"rpc.connect_rpc.response.metadata"
  end

  @doc """
  gRPC request metadata, `<key>` being the normalized gRPC Metadata key (lowercase), the value being the metadata values.

  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which metadata values are to be captured. Including all request metadata values can be a security risk - explicit configuration helps avoid leaking sensitive information.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_grpc_request_metadata()
      :"rpc.grpc.request.metadata"
  """
  @spec rpc_grpc_request_metadata :: :"rpc.grpc.request.metadata"
  def rpc_grpc_request_metadata do
    :"rpc.grpc.request.metadata"
  end

  @doc """
  gRPC response metadata, `<key>` being the normalized gRPC Metadata key (lowercase), the value being the metadata values.

  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which metadata values are to be captured. Including all response metadata values can be a security risk - explicit configuration helps avoid leaking sensitive information.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_grpc_response_metadata()
      :"rpc.grpc.response.metadata"
  """
  @spec rpc_grpc_response_metadata :: :"rpc.grpc.response.metadata"
  def rpc_grpc_response_metadata do
    :"rpc.grpc.response.metadata"
  end

  @typedoc """
  The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request.

  ### Enum Values
  * `:ok` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - OK
  * `:cancelled` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - CANCELLED
  * `:unknown` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - UNKNOWN
  * `:invalid_argument` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - INVALID_ARGUMENT
  * `:deadline_exceeded` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - DEADLINE_EXCEEDED
  * `:not_found` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - NOT_FOUND
  * `:already_exists` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ALREADY_EXISTS
  * `:permission_denied` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - PERMISSION_DENIED
  * `:resource_exhausted` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - RESOURCE_EXHAUSTED
  * `:failed_precondition` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - FAILED_PRECONDITION
  * `:aborted` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ABORTED
  * `:out_of_range` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - OUT_OF_RANGE
  * `:unimplemented` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - UNIMPLEMENTED
  * `:internal` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - INTERNAL
  * `:unavailable` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - UNAVAILABLE
  * `:data_loss` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - DATA_LOSS
  * `:unauthenticated` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - UNAUTHENTICATED
  """
  @type rpc_grpc_statuscode() :: %{
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


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_grpc_statuscode().ok
      0
      
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_grpc_statuscode(27)
      27
  """
  @spec rpc_grpc_statuscode() :: rpc_grpc_statuscode()
  def rpc_grpc_statuscode() do
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

  @spec rpc_grpc_statuscode(integer()) :: integer()
  def rpc_grpc_statuscode(custom_value) do
    custom_value
  end

  @doc """
  `error.code` property of response if it is an error response.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_jsonrpc_errorcode()
      :"rpc.jsonrpc.error_code"
  """
  @spec rpc_jsonrpc_errorcode :: :"rpc.jsonrpc.error_code"
  def rpc_jsonrpc_errorcode do
    :"rpc.jsonrpc.error_code"
  end

  @doc """
  `error.message` property of response if it is an error response.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_jsonrpc_errormessage()
      :"rpc.jsonrpc.error_message"
  """
  @spec rpc_jsonrpc_errormessage :: :"rpc.jsonrpc.error_message"
  def rpc_jsonrpc_errormessage do
    :"rpc.jsonrpc.error_message"
  end

  @doc """
  `id` property of request or response. Since protocol allows id to be int, string, `null` or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of `null` value. Omit entirely if this is a notification.



  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_jsonrpc_requestid()
      :"rpc.jsonrpc.request_id"
  """
  @spec rpc_jsonrpc_requestid :: :"rpc.jsonrpc.request_id"
  def rpc_jsonrpc_requestid do
    :"rpc.jsonrpc.request_id"
  end

  @doc """
  Protocol version as in `jsonrpc` property of request/response. Since JSON-RPC 1.0 doesn't specify this, the value can be omitted.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_jsonrpc_version()
      :"rpc.jsonrpc.version"
  """
  @spec rpc_jsonrpc_version :: :"rpc.jsonrpc.version"
  def rpc_jsonrpc_version do
    :"rpc.jsonrpc.version"
  end

  @doc """
  Compressed size of the message in bytes.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_message_compressedsize()
      :"rpc.message.compressed_size"
  """
  @spec rpc_message_compressedsize :: :"rpc.message.compressed_size"
  def rpc_message_compressedsize do
    :"rpc.message.compressed_size"
  end

  @doc """
  MUST be calculated as two different counters starting from `1` one for sent messages and one for received message.
  ### Notes

  This way we guarantee that the values will be consistent between different implementations.

  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_message_id()
      :"rpc.message.id"
  """
  @spec rpc_message_id :: :"rpc.message.id"
  def rpc_message_id do
    :"rpc.message.id"
  end

  @typedoc """
  Whether this is a received or sent message.

  ### Enum Values
  * `:sent` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:received` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  """
  @type rpc_message_type() :: %{
          :sent => :SENT,
          :received => :RECEIVED
        }
  @doc """
  Whether this is a received or sent message.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_message_type().sent
      :SENT
      
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_message_type(:custom_value)
      :custom_value
  """
  @spec rpc_message_type() :: rpc_message_type()
  def rpc_message_type() do
    %{
      :sent => :SENT,
      :received => :RECEIVED
    }
  end

  @spec rpc_message_type(atom() | String.t()) :: atom() | String.t()
  def rpc_message_type(custom_value) do
    custom_value
  end

  @doc """
  Uncompressed size of the message in bytes.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_message_uncompressedsize()
      :"rpc.message.uncompressed_size"
  """
  @spec rpc_message_uncompressedsize :: :"rpc.message.uncompressed_size"
  def rpc_message_uncompressedsize do
    :"rpc.message.uncompressed_size"
  end

  @doc """
  The name of the (logical) method being called, must be equal to the $method part in the span name.
  ### Notes

  This is the logical name of the method from the RPC interface perspective, which can be different from the name of any implementing method/function. The `code.function` attribute may be used to store the latter (e.g., method actually executing the call on the server side, RPC client stub method on the client side).


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_method()
      :"rpc.method"
  """
  @spec rpc_method :: :"rpc.method"
  def rpc_method do
    :"rpc.method"
  end

  @doc """
  The full (logical) name of the service being called, including its package name, if applicable.
  ### Notes

  This is the logical name of the service from the RPC interface perspective, which can be different from the name of any implementing class. The `code.namespace` attribute may be used to store the latter (despite the attribute name, it may include a class name; e.g., class with method actually executing the call on the server side, RPC client stub class on the client side).


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_service()
      :"rpc.service"
  """
  @spec rpc_service :: :"rpc.service"
  def rpc_service do
    :"rpc.service"
  end

  @typedoc """
  A string identifying the remoting system. See below for a list of well-known identifiers.

  ### Enum Values
  * `:grpc` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - gRPC
  * `:java_rmi` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Java RMI
  * `:dotnet_wcf` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - .NET WCF
  * `:apache_dubbo` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Apache Dubbo
  * `:connect_rpc` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Connect RPC
  """
  @type rpc_system() :: %{
          :grpc => :grpc,
          :java_rmi => :java_rmi,
          :dotnet_wcf => :dotnet_wcf,
          :apache_dubbo => :apache_dubbo,
          :connect_rpc => :connect_rpc
        }
  @doc """
  A string identifying the remoting system. See below for a list of well-known identifiers.


  ### Example
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_system().grpc
      :grpc
      
      iex> OpenTelemetry.SemanticConventions.RpcAttributes.rpc_system(:custom_value)
      :custom_value
  """
  @spec rpc_system() :: rpc_system()
  def rpc_system() do
    %{
      :grpc => :grpc,
      :java_rmi => :java_rmi,
      :dotnet_wcf => :dotnet_wcf,
      :apache_dubbo => :apache_dubbo,
      :connect_rpc => :connect_rpc
    }
  end

  @spec rpc_system(atom() | String.t()) :: atom() | String.t()
  def rpc_system(custom_value) do
    custom_value
  end
end
