defmodule OpenTelemetry.SemanticConventions.ServerAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Server attributes.
  """

  @doc """
  Server domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
  ### Notes

  When observed from the client side, and when communicating through an intermediary, `server.address` **SHOULD** represent the server address behind any intermediaries, for example proxies, if it's available.


  ### Example
      iex> OpenTelemetry.SemanticConventions.ServerAttributes.server_address()
      :"server.address"
  """
  @spec server_address :: :"server.address"
  def server_address do
    :"server.address"
  end

  @doc """
  Server port number.
  ### Notes

  When observed from the client side, and when communicating through an intermediary, `server.port` **SHOULD** represent the server port behind any intermediaries, for example proxies, if it's available.


  ### Example
      iex> OpenTelemetry.SemanticConventions.ServerAttributes.server_port()
      :"server.port"
  """
  @spec server_port :: :"server.port"
  def server_port do
    :"server.port"
  end
end
