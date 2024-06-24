defmodule OpenTelemetry.SemanticConventions.SourceAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Source attributes.
  """

  @doc """
  Source address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
  ### Notes

  When observed from the destination side, and when communicating through an intermediary, `source.address` **SHOULD** represent the source address behind any intermediaries, for example proxies, if it's available.


  ### Example
      iex> OpenTelemetry.SemanticConventions.SourceAttributes.source_address()
      :"source.address"
  """
  @spec source_address :: :"source.address"
  def source_address do
    :"source.address"
  end

  @doc """
  Source port number


  ### Example
      iex> OpenTelemetry.SemanticConventions.SourceAttributes.source_port()
      :"source.port"
  """
  @spec source_port :: :"source.port"
  def source_port do
    :"source.port"
  end
end
