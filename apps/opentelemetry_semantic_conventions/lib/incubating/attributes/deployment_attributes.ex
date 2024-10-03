defmodule OpenTelemetry.SemConv.Incubating.DeploymentAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Deployment attributes.
  """

  @deprecated """
  Deprecated, use `deployment.environment.name` instead.
  """
  @spec deployment_environment :: :"deployment.environment"
  def deployment_environment do
    :"deployment.environment"
  end

  @doc """
  Name of the [deployment environment](https://wikipedia.org/wiki/Deployment_environment) (aka deployment tier).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  `deployment.environment.name` does not affect the uniqueness constraints defined through
  the `service.namespace`, `service.name` and `service.instance.id` resource attributes.
  This implies that resources carrying the following attribute combinations **MUST** be
  considered to be identifying the same service:

  * `service.name=frontend`, `deployment.environment.name=production`
  * `service.name=frontend`, `deployment.environment.name=staging`.

  ### Examples

  ```
  ["staging", "production"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DeploymentAttributes.deployment_environment_name()
      :"deployment.environment.name"

  ### Erlang

  ```erlang
  ?DEPLOYMENT_ENVIRONMENT_NAME.
  'deployment.environment.name'
  ```

  <!-- tabs-close -->
  """
  @spec deployment_environment_name :: :"deployment.environment.name"
  def deployment_environment_name do
    :"deployment.environment.name"
  end

  @doc """
  The id of the deployment.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1208"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DeploymentAttributes.deployment_id()
      :"deployment.id"

  ### Erlang

  ```erlang
  ?DEPLOYMENT_ID.
  'deployment.id'
  ```

  <!-- tabs-close -->
  """
  @spec deployment_id :: :"deployment.id"
  def deployment_id do
    :"deployment.id"
  end

  @doc """
  The name of the deployment.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["deploy my app", "deploy-frontend"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DeploymentAttributes.deployment_name()
      :"deployment.name"

  ### Erlang

  ```erlang
  ?DEPLOYMENT_NAME.
  'deployment.name'
  ```

  <!-- tabs-close -->
  """
  @spec deployment_name :: :"deployment.name"
  def deployment_name do
    :"deployment.name"
  end

  @typedoc """
  The status of the deployment.


  ### Enum Values
  * `:failed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - failed
  * `:succeeded` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - succeeded
  """
  @type deployment_status_values() :: %{
          :failed => :failed,
          :succeeded => :succeeded
        }
  @doc """
  The status of the deployment.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DeploymentAttributes.deployment_status()
      :"deployment.status"

      iex> OpenTelemetry.SemConv.Incubating.DeploymentAttributes.deployment_status_values().failed
      :failed

      iex> %{OpenTelemetry.SemConv.Incubating.DeploymentAttributes.deployment_status() => OpenTelemetry.SemConv.Incubating.DeploymentAttributes.deployment_status_values().failed}
      %{:"deployment.status" => :failed}

  ### Erlang

  ```erlang
  ?DEPLOYMENT_STATUS.
  'deployment.status'

  ?DEPLOYMENT_STATUS_VALUES_FAILED.
  'failed'

  \#{?DEPLOYMENT_STATUS => ?DEPLOYMENT_STATUS_VALUES_FAILED}.
  \#{'deployment.status' => 'failed'}
  ```

  <!-- tabs-close -->
  """
  @spec deployment_status :: :"deployment.status"
  def deployment_status do
    :"deployment.status"
  end

  @spec deployment_status_values() :: deployment_status_values()
  def deployment_status_values() do
    %{
      :failed => :failed,
      :succeeded => :succeeded
    }
  end
end
