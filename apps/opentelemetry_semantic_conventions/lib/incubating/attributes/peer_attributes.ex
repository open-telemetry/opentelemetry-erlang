defmodule OpenTelemetry.SemConv.Incubating.PeerAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Peer attributes.
  """

  @doc """
  The [`service.name`](/docs/resource/README.md#service) of the remote service. **SHOULD** be equal to the actual `service.name` resource attribute of the remote service if any.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  AuthTokenCache
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.PeerAttributes.peer_service()
      :"peer.service"

  ### Erlang

  ```erlang
  ?PEER_SERVICE.
  'peer.service'
  ```

  <!-- tabs-close -->
  """
  @spec peer_service :: :"peer.service"
  def peer_service do
    :"peer.service"
  end
end
