defmodule OpenTelemetry.SemConv.Incubating.OracleCloudAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Oracle_Cloud attributes.
  """

  @doc """
  The OCI realm identifier that indicates the isolated partition in which the tenancy and its resources reside.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  See [OCI documentation on realms](https://docs.oracle.com/iaas/Content/General/Concepts/regions.htm)

  ### Examples

  ```
  ["oc1", "oc2"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OracleCloudAttributes.oracle_cloud_realm()
      :"oracle_cloud.realm"

  ### Erlang

  ```erlang
  ?ORACLE_CLOUD_REALM.
  'oracle_cloud.realm'
  ```

  <!-- tabs-close -->
  """
  @spec oracle_cloud_realm :: :"oracle_cloud.realm"
  def oracle_cloud_realm do
    :"oracle_cloud.realm"
  end
end
