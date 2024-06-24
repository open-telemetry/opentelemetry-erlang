defmodule OpenTelemetry.SemanticConventions.ClientAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Client attributes.
  """

  @doc """
  Client address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
  ### Notes

  When observed from the server side, and when communicating through an intermediary, `client.address` **SHOULD** represent the client address behind any intermediaries,  for example proxies, if it's available.


  ### Example
      iex> OpenTelemetry.SemanticConventions.ClientAttributes.client_address()
      :"client.address"
  """
  @spec client_address :: :"client.address"
  def client_address do
    :"client.address"
  end

  @doc """
  Client port number.
  ### Notes

  When observed from the server side, and when communicating through an intermediary, `client.port` **SHOULD** represent the client port behind any intermediaries,  for example proxies, if it's available.


  ### Example
      iex> OpenTelemetry.SemanticConventions.ClientAttributes.client_port()
      :"client.port"
  """
  @spec client_port :: :"client.port"
  def client_port do
    :"client.port"
  end
end
