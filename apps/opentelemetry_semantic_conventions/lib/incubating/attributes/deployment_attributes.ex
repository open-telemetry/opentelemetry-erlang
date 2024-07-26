defmodule OpenTelemetry.SemConv.Incubating.DeploymentAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Deployment attributes.
  """

  @doc """
  Name of the [deployment environment](https://wikipedia.org/wiki/Deployment_environment) (aka deployment tier).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  `deployment.environment` does not affect the uniqueness constraints defined through
  the `service.namespace`, `service.name` and `service.instance.id` resource attributes.
  This implies that resources carrying the following attribute combinations **MUST** be
  considered to be identifying the same service:

  * `service.name=frontend`, `deployment.environment=production`
  * `service.name=frontend`, `deployment.environment=staging`.

  ### Examples

  ```
  ["staging", "production"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DeploymentAttributes.deployment_environment()
      :"deployment.environment"

  ### Erlang

  ```erlang
  ?'DEPLOYMENT_ENVIRONMENT'.
  'deployment.environment'
  ```

  <!-- tabs-close -->
  """
  @spec deployment_environment :: :"deployment.environment"
  def deployment_environment do
    :"deployment.environment"
  end
end
