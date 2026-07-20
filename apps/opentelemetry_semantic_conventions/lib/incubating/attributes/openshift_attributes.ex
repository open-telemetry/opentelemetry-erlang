defmodule OpenTelemetry.SemConv.Incubating.OpenshiftAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Openshift attributes.
  """

  @doc """
  The name of the cluster quota.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OpenshiftAttributes.openshift_clusterquota_name()
      :"openshift.clusterquota.name"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_NAME.
  'openshift.clusterquota.name'
  ```

  <!-- tabs-close -->
  """
  @spec openshift_clusterquota_name :: :"openshift.clusterquota.name"
  def openshift_clusterquota_name do
    :"openshift.clusterquota.name"
  end

  @doc """
  The UID of the cluster quota.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OpenshiftAttributes.openshift_clusterquota_uid()
      :"openshift.clusterquota.uid"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_UID.
  'openshift.clusterquota.uid'
  ```

  <!-- tabs-close -->
  """
  @spec openshift_clusterquota_uid :: :"openshift.clusterquota.uid"
  def openshift_clusterquota_uid do
    :"openshift.clusterquota.uid"
  end
end
