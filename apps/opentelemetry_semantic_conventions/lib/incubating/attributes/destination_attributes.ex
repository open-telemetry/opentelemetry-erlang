defmodule OpenTelemetry.SemConv.Incubating.DestinationAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Destination attributes.
  """

  @doc """
  Destination address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  When observed from the source side, and when communicating through an intermediary, `destination.address` **SHOULD** represent the destination address behind any intermediaries, for example proxies, if it's available.

  ### Examples

  ```
  ["destination.example.com", "10.1.2.80", "/tmp/my.sock"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DestinationAttributes.destination_address()
      :"destination.address"

  ### Erlang

  ```erlang
  ?DESTINATION_ADDRESS.
  'destination.address'
  ```

  <!-- tabs-close -->
  """
  @spec destination_address :: :"destination.address"
  def destination_address do
    :"destination.address"
  end

  @doc """
  Destination port number
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [3389, 2888]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DestinationAttributes.destination_port()
      :"destination.port"

  ### Erlang

  ```erlang
  ?DESTINATION_PORT.
  'destination.port'
  ```

  <!-- tabs-close -->
  """
  @spec destination_port :: :"destination.port"
  def destination_port do
    :"destination.port"
  end
end
