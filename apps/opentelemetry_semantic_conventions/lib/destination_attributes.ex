defmodule OpenTelemetry.SemanticConventions.DestinationAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Destination attributes.
  """

  @doc """
  Destination address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
  ### Notes

  When observed from the source side, and when communicating through an intermediary, `destination.address` **SHOULD** represent the destination address behind any intermediaries, for example proxies, if it's available.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DestinationAttributes.destination_address()
      :"destination.address"
  """
  @spec destination_address :: :"destination.address"
  def destination_address do
    :"destination.address"
  end

  @doc """
  Destination port number


  ### Example
      iex> OpenTelemetry.SemanticConventions.DestinationAttributes.destination_port()
      :"destination.port"
  """
  @spec destination_port :: :"destination.port"
  def destination_port do
    :"destination.port"
  end
end
