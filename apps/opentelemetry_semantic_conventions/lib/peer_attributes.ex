defmodule OpenTelemetry.SemanticConventions.PeerAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Peer attributes.
  """

  @doc """
  The [`service.name`](/docs/resource/README.md#service) of the remote service. **SHOULD** be equal to the actual `service.name` resource attribute of the remote service if any.



  ### Example
      iex> OpenTelemetry.SemanticConventions.PeerAttributes.peer_service()
      :"peer.service"
  """
  @spec peer_service :: :"peer.service"
  def peer_service do
    :"peer.service"
  end
end
