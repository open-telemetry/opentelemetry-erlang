defmodule OpenTelemetry.SemConv.ServerAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Server attributes.
  """

  @doc """
  Server domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  When observed from the client side, and when communicating through an intermediary, `server.address` **SHOULD** represent the server address behind any intermediaries, for example proxies, if it's available.

  ### Examples

  ```
  ["example.com", "10.1.2.80", "/tmp/my.sock"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ServerAttributes.server_address()
      :"server.address"

  ### Erlang

  ```erlang
  ?SERVER_ADDRESS.
  'server.address'
  ```

  <!-- tabs-close -->
  """
  @spec server_address :: :"server.address"
  def server_address do
    :"server.address"
  end

  @doc """
  Server port number.
  ### Value type

  Value must be of type `integer()`.
  ### Notes

  When observed from the client side, and when communicating through an intermediary, `server.port` **SHOULD** represent the server port behind any intermediaries, for example proxies, if it's available.

  ### Examples

  ```
  [80, 8080, 443]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ServerAttributes.server_port()
      :"server.port"

  ### Erlang

  ```erlang
  ?SERVER_PORT.
  'server.port'
  ```

  <!-- tabs-close -->
  """
  @spec server_port :: :"server.port"
  def server_port do
    :"server.port"
  end
end
