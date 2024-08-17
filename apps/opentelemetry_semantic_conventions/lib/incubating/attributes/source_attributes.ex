defmodule OpenTelemetry.SemConv.Incubating.SourceAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Source attributes.
  """

  @doc """
  Source address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  When observed from the destination side, and when communicating through an intermediary, `source.address` **SHOULD** represent the source address behind any intermediaries, for example proxies, if it's available.

  ### Examples

  ```
  ["source.example.com", "10.1.2.80", "/tmp/my.sock"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SourceAttributes.source_address()
      :"source.address"

  ### Erlang

  ```erlang
  ?SOURCE_ADDRESS.
  'source.address'
  ```

  <!-- tabs-close -->
  """
  @spec source_address :: :"source.address"
  def source_address do
    :"source.address"
  end

  @doc """
  Source port number
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [3389, 2888]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SourceAttributes.source_port()
      :"source.port"

  ### Erlang

  ```erlang
  ?SOURCE_PORT.
  'source.port'
  ```

  <!-- tabs-close -->
  """
  @spec source_port :: :"source.port"
  def source_port do
    :"source.port"
  end
end
