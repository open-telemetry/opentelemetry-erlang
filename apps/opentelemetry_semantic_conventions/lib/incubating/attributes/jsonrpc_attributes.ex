defmodule OpenTelemetry.SemConv.Incubating.JsonrpcAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Jsonrpc attributes.
  """

  @doc """
  Protocol version, as specified in the `jsonrpc` property of the request and its corresponding response.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2.0", "1.0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.JsonrpcAttributes.jsonrpc_protocol_version()
      :"jsonrpc.protocol.version"

  ### Erlang

  ```erlang
  ?JSONRPC_PROTOCOL_VERSION.
  'jsonrpc.protocol.version'
  ```

  <!-- tabs-close -->
  """
  @spec jsonrpc_protocol_version :: :"jsonrpc.protocol.version"
  def jsonrpc_protocol_version do
    :"jsonrpc.protocol.version"
  end

  @doc """
  A string representation of the `id` property of the request and its corresponding response.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Under the [JSON-RPC specification](https://www.jsonrpc.org/specification), the `id` property may be a string, number, null, or omitted entirely. When omitted, the request is treated as a notification. Using `null` is not equivalent to omitting the `id`, but it is discouraged.
  Instrumentations **SHOULD** **NOT** capture this attribute when the `id` is `null` or omitted.

  ### Examples

  ```
  ["10", "request-7"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.JsonrpcAttributes.jsonrpc_request_id()
      :"jsonrpc.request.id"

  ### Erlang

  ```erlang
  ?JSONRPC_REQUEST_ID.
  'jsonrpc.request.id'
  ```

  <!-- tabs-close -->
  """
  @spec jsonrpc_request_id :: :"jsonrpc.request.id"
  def jsonrpc_request_id do
    :"jsonrpc.request.id"
  end
end
