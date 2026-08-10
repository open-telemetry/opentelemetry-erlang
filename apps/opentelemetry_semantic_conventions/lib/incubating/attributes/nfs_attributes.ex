defmodule OpenTelemetry.SemConv.Incubating.NfsAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Nfs attributes.
  """

  @doc """
  NFSv4+ operation name.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["OPEN", "READ", "GETATTR"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.NfsAttributes.nfs_operation_name()
      :"nfs.operation.name"

  ### Erlang

  ```erlang
  ?NFS_OPERATION_NAME.
  'nfs.operation.name'
  ```

  <!-- tabs-close -->
  """
  @spec nfs_operation_name :: :"nfs.operation.name"
  def nfs_operation_name do
    :"nfs.operation.name"
  end

  @doc """
  Linux: one of "hit" (NFSD_STATS_RC_HITS), "miss" (NFSD_STATS_RC_MISSES), or "nocache" (NFSD_STATS_RC_NOCACHE -- uncacheable)

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  hit
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.NfsAttributes.nfs_server_repcache_status()
      :"nfs.server.repcache.status"

  ### Erlang

  ```erlang
  ?NFS_SERVER_REPCACHE_STATUS.
  'nfs.server.repcache.status'
  ```

  <!-- tabs-close -->
  """
  @spec nfs_server_repcache_status :: :"nfs.server.repcache.status"
  def nfs_server_repcache_status do
    :"nfs.server.repcache.status"
  end
end
