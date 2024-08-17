defmodule OpenTelemetry.SemConv.ClientAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Client attributes.
  """

  @doc """
  Client address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  When observed from the server side, and when communicating through an intermediary, `client.address` **SHOULD** represent the client address behind any intermediaries,  for example proxies, if it's available.

  ### Examples

  ```
  ["client.example.com", "10.1.2.80", "/tmp/my.sock"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ClientAttributes.client_address()
      :"client.address"

  ### Erlang

  ```erlang
  ?CLIENT_ADDRESS.
  'client.address'
  ```

  <!-- tabs-close -->
  """
  @spec client_address :: :"client.address"
  def client_address do
    :"client.address"
  end

  @doc """
  Client port number.
  ### Value type

  Value must be of type `integer()`.
  ### Notes

  When observed from the server side, and when communicating through an intermediary, `client.port` **SHOULD** represent the client port behind any intermediaries,  for example proxies, if it's available.

  ### Examples

  ```
  [65123]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.ClientAttributes.client_port()
      :"client.port"

  ### Erlang

  ```erlang
  ?CLIENT_PORT.
  'client.port'
  ```

  <!-- tabs-close -->
  """
  @spec client_port :: :"client.port"
  def client_port do
    :"client.port"
  end
end
