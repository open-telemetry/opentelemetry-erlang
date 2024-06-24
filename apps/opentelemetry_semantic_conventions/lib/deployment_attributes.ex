defmodule OpenTelemetry.SemanticConventions.DeploymentAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Deployment attributes.
  """

  @doc """
  Name of the [deployment environment](https://wikipedia.org/wiki/Deployment_environment) (aka deployment tier).

  ### Notes

  `deployment.environment` does not affect the uniqueness constraints defined through
  the `service.namespace`, `service.name` and `service.instance.id` resource attributes.
  This implies that resources carrying the following attribute combinations **MUST** be
  considered to be identifying the same service:

  * `service.name=frontend`, `deployment.environment=production`
  * `service.name=frontend`, `deployment.environment=staging`.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DeploymentAttributes.deployment_environment()
      :"deployment.environment"
  """
  @spec deployment_environment :: :"deployment.environment"
  def deployment_environment do
    :"deployment.environment"
  end
end
