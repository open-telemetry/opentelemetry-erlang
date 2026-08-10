defmodule OpenTelemetry.SemConv.Incubating.GCPAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for GCP attributes.
  """

  @doc """
  The container within GCP where the AppHub application is defined.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["projects/my-container-project"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_application_container()
      :"gcp.apphub.application.container"

  ### Erlang

  ```erlang
  ?GCP_APPHUB_APPLICATION_CONTAINER.
  'gcp.apphub.application.container'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_application_container :: :"gcp.apphub.application.container"
  def gcp_apphub_application_container do
    :"gcp.apphub.application.container"
  end

  @doc """
  The name of the application as configured in AppHub.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["my-application"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_application_id()
      :"gcp.apphub.application.id"

  ### Erlang

  ```erlang
  ?GCP_APPHUB_APPLICATION_ID.
  'gcp.apphub.application.id'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_application_id :: :"gcp.apphub.application.id"
  def gcp_apphub_application_id do
    :"gcp.apphub.application.id"
  end

  @doc """
  The GCP zone or region where the application is defined.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["us-central1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_application_location()
      :"gcp.apphub.application.location"

  ### Erlang

  ```erlang
  ?GCP_APPHUB_APPLICATION_LOCATION.
  'gcp.apphub.application.location'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_application_location :: :"gcp.apphub.application.location"
  def gcp_apphub_application_location do
    :"gcp.apphub.application.location"
  end

  @typedoc """
  Criticality of a service indicates its importance to the business.


  ### Enum Values
  * `:mission_critical` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Mission critical service.
  * `:high` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - High impact.
  * `:medium` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Medium impact.
  * `:low` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Low impact.
  """
  @type gcp_apphub_service_criticality_type_values() :: %{
          :mission_critical => :MISSION_CRITICAL,
          :high => :HIGH,
          :medium => :MEDIUM,
          :low => :LOW
        }
  @doc """
  Criticality of a service indicates its importance to the business.


  ### Notes

  [See AppHub type enum](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type)


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_service_criticality_type()
      :"gcp.apphub.service.criticality_type"

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_service_criticality_type_values().mission_critical
      :MISSION_CRITICAL

      iex> %{OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_service_criticality_type() => OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_service_criticality_type_values().mission_critical}
      %{:"gcp.apphub.service.criticality_type" => :MISSION_CRITICAL}

  ### Erlang

  ```erlang
  ?GCP_APPHUB_SERVICE_CRITICALITY_TYPE.
  'gcp.apphub.service.criticality_type'

  ?GCP_APPHUB_SERVICE_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL.
  'MISSION_CRITICAL'

  \#{?GCP_APPHUB_SERVICE_CRITICALITY_TYPE => ?GCP_APPHUB_SERVICE_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL}.
  \#{'gcp.apphub.service.criticality_type' => 'MISSION_CRITICAL'}
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_service_criticality_type :: :"gcp.apphub.service.criticality_type"
  def gcp_apphub_service_criticality_type do
    :"gcp.apphub.service.criticality_type"
  end

  @spec gcp_apphub_service_criticality_type_values() ::
          gcp_apphub_service_criticality_type_values()
  def gcp_apphub_service_criticality_type_values() do
    %{
      :mission_critical => :MISSION_CRITICAL,
      :high => :HIGH,
      :medium => :MEDIUM,
      :low => :LOW
    }
  end

  @typedoc """
  Environment of a service is the stage of a software lifecycle.


  ### Enum Values
  * `:production` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Production environment.
  * `:staging` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Staging environment.
  * `:test` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Test environment.
  * `:development` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Development environment.
  """
  @type gcp_apphub_service_environment_type_values() :: %{
          :production => :PRODUCTION,
          :staging => :STAGING,
          :test => :TEST,
          :development => :DEVELOPMENT
        }
  @doc """
  Environment of a service is the stage of a software lifecycle.


  ### Notes

  [See AppHub environment type](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type_1)


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_service_environment_type()
      :"gcp.apphub.service.environment_type"

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_service_environment_type_values().production
      :PRODUCTION

      iex> %{OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_service_environment_type() => OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_service_environment_type_values().production}
      %{:"gcp.apphub.service.environment_type" => :PRODUCTION}

  ### Erlang

  ```erlang
  ?GCP_APPHUB_SERVICE_ENVIRONMENT_TYPE.
  'gcp.apphub.service.environment_type'

  ?GCP_APPHUB_SERVICE_ENVIRONMENT_TYPE_VALUES_PRODUCTION.
  'PRODUCTION'

  \#{?GCP_APPHUB_SERVICE_ENVIRONMENT_TYPE => ?GCP_APPHUB_SERVICE_ENVIRONMENT_TYPE_VALUES_PRODUCTION}.
  \#{'gcp.apphub.service.environment_type' => 'PRODUCTION'}
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_service_environment_type :: :"gcp.apphub.service.environment_type"
  def gcp_apphub_service_environment_type do
    :"gcp.apphub.service.environment_type"
  end

  @spec gcp_apphub_service_environment_type_values() ::
          gcp_apphub_service_environment_type_values()
  def gcp_apphub_service_environment_type_values() do
    %{
      :production => :PRODUCTION,
      :staging => :STAGING,
      :test => :TEST,
      :development => :DEVELOPMENT
    }
  end

  @doc """
  The name of the service as configured in AppHub.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["my-service"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_service_id()
      :"gcp.apphub.service.id"

  ### Erlang

  ```erlang
  ?GCP_APPHUB_SERVICE_ID.
  'gcp.apphub.service.id'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_service_id :: :"gcp.apphub.service.id"
  def gcp_apphub_service_id do
    :"gcp.apphub.service.id"
  end

  @typedoc """
  Criticality of a workload indicates its importance to the business.


  ### Enum Values
  * `:mission_critical` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Mission critical service.
  * `:high` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - High impact.
  * `:medium` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Medium impact.
  * `:low` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Low impact.
  """
  @type gcp_apphub_workload_criticality_type_values() :: %{
          :mission_critical => :MISSION_CRITICAL,
          :high => :HIGH,
          :medium => :MEDIUM,
          :low => :LOW
        }
  @doc """
  Criticality of a workload indicates its importance to the business.


  ### Notes

  [See AppHub type enum](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type)


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_workload_criticality_type()
      :"gcp.apphub.workload.criticality_type"

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_workload_criticality_type_values().mission_critical
      :MISSION_CRITICAL

      iex> %{OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_workload_criticality_type() => OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_workload_criticality_type_values().mission_critical}
      %{:"gcp.apphub.workload.criticality_type" => :MISSION_CRITICAL}

  ### Erlang

  ```erlang
  ?GCP_APPHUB_WORKLOAD_CRITICALITY_TYPE.
  'gcp.apphub.workload.criticality_type'

  ?GCP_APPHUB_WORKLOAD_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL.
  'MISSION_CRITICAL'

  \#{?GCP_APPHUB_WORKLOAD_CRITICALITY_TYPE => ?GCP_APPHUB_WORKLOAD_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL}.
  \#{'gcp.apphub.workload.criticality_type' => 'MISSION_CRITICAL'}
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_workload_criticality_type :: :"gcp.apphub.workload.criticality_type"
  def gcp_apphub_workload_criticality_type do
    :"gcp.apphub.workload.criticality_type"
  end

  @spec gcp_apphub_workload_criticality_type_values() ::
          gcp_apphub_workload_criticality_type_values()
  def gcp_apphub_workload_criticality_type_values() do
    %{
      :mission_critical => :MISSION_CRITICAL,
      :high => :HIGH,
      :medium => :MEDIUM,
      :low => :LOW
    }
  end

  @typedoc """
  Environment of a workload is the stage of a software lifecycle.


  ### Enum Values
  * `:production` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Production environment.
  * `:staging` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Staging environment.
  * `:test` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Test environment.
  * `:development` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Development environment.
  """
  @type gcp_apphub_workload_environment_type_values() :: %{
          :production => :PRODUCTION,
          :staging => :STAGING,
          :test => :TEST,
          :development => :DEVELOPMENT
        }
  @doc """
  Environment of a workload is the stage of a software lifecycle.


  ### Notes

  [See AppHub environment type](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type_1)


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_workload_environment_type()
      :"gcp.apphub.workload.environment_type"

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_workload_environment_type_values().production
      :PRODUCTION

      iex> %{OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_workload_environment_type() => OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_workload_environment_type_values().production}
      %{:"gcp.apphub.workload.environment_type" => :PRODUCTION}

  ### Erlang

  ```erlang
  ?GCP_APPHUB_WORKLOAD_ENVIRONMENT_TYPE.
  'gcp.apphub.workload.environment_type'

  ?GCP_APPHUB_WORKLOAD_ENVIRONMENT_TYPE_VALUES_PRODUCTION.
  'PRODUCTION'

  \#{?GCP_APPHUB_WORKLOAD_ENVIRONMENT_TYPE => ?GCP_APPHUB_WORKLOAD_ENVIRONMENT_TYPE_VALUES_PRODUCTION}.
  \#{'gcp.apphub.workload.environment_type' => 'PRODUCTION'}
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_workload_environment_type :: :"gcp.apphub.workload.environment_type"
  def gcp_apphub_workload_environment_type do
    :"gcp.apphub.workload.environment_type"
  end

  @spec gcp_apphub_workload_environment_type_values() ::
          gcp_apphub_workload_environment_type_values()
  def gcp_apphub_workload_environment_type_values() do
    %{
      :production => :PRODUCTION,
      :staging => :STAGING,
      :test => :TEST,
      :development => :DEVELOPMENT
    }
  end

  @doc """
  The name of the workload as configured in AppHub.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["my-workload"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_workload_id()
      :"gcp.apphub.workload.id"

  ### Erlang

  ```erlang
  ?GCP_APPHUB_WORKLOAD_ID.
  'gcp.apphub.workload.id'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_workload_id :: :"gcp.apphub.workload.id"
  def gcp_apphub_workload_id do
    :"gcp.apphub.workload.id"
  end

  @doc """
  The container within GCP where the AppHub destination application is defined.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["projects/my-container-project"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_application_container()
      :"gcp.apphub_destination.application.container"

  ### Erlang

  ```erlang
  ?GCP_APPHUB_DESTINATION_APPLICATION_CONTAINER.
  'gcp.apphub_destination.application.container'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_destination_application_container ::
          :"gcp.apphub_destination.application.container"
  def gcp_apphub_destination_application_container do
    :"gcp.apphub_destination.application.container"
  end

  @doc """
  The name of the destination application as configured in AppHub.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["my-application"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_application_id()
      :"gcp.apphub_destination.application.id"

  ### Erlang

  ```erlang
  ?GCP_APPHUB_DESTINATION_APPLICATION_ID.
  'gcp.apphub_destination.application.id'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_destination_application_id :: :"gcp.apphub_destination.application.id"
  def gcp_apphub_destination_application_id do
    :"gcp.apphub_destination.application.id"
  end

  @doc """
  The GCP zone or region where the destination application is defined.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["us-central1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_application_location()
      :"gcp.apphub_destination.application.location"

  ### Erlang

  ```erlang
  ?GCP_APPHUB_DESTINATION_APPLICATION_LOCATION.
  'gcp.apphub_destination.application.location'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_destination_application_location ::
          :"gcp.apphub_destination.application.location"
  def gcp_apphub_destination_application_location do
    :"gcp.apphub_destination.application.location"
  end

  @typedoc """
  Criticality of a destination workload indicates its importance to the business as specified in [AppHub type enum](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type)


  ### Enum Values
  * `:mission_critical` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Mission critical service.
  * `:high` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - High impact.
  * `:medium` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Medium impact.
  * `:low` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Low impact.
  """
  @type gcp_apphub_destination_service_criticality_type_values() :: %{
          :mission_critical => :MISSION_CRITICAL,
          :high => :HIGH,
          :medium => :MEDIUM,
          :low => :LOW
        }
  @doc """
  Criticality of a destination workload indicates its importance to the business as specified in [AppHub type enum](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type)



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_service_criticality_type()
      :"gcp.apphub_destination.service.criticality_type"

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_service_criticality_type_values().mission_critical
      :MISSION_CRITICAL

      iex> %{OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_service_criticality_type() => OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_service_criticality_type_values().mission_critical}
      %{:"gcp.apphub_destination.service.criticality_type" => :MISSION_CRITICAL}

  ### Erlang

  ```erlang
  ?GCP_APPHUB_DESTINATION_SERVICE_CRITICALITY_TYPE.
  'gcp.apphub_destination.service.criticality_type'

  ?GCP_APPHUB_DESTINATION_SERVICE_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL.
  'MISSION_CRITICAL'

  \#{?GCP_APPHUB_DESTINATION_SERVICE_CRITICALITY_TYPE => ?GCP_APPHUB_DESTINATION_SERVICE_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL}.
  \#{'gcp.apphub_destination.service.criticality_type' => 'MISSION_CRITICAL'}
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_destination_service_criticality_type ::
          :"gcp.apphub_destination.service.criticality_type"
  def gcp_apphub_destination_service_criticality_type do
    :"gcp.apphub_destination.service.criticality_type"
  end

  @spec gcp_apphub_destination_service_criticality_type_values() ::
          gcp_apphub_destination_service_criticality_type_values()
  def gcp_apphub_destination_service_criticality_type_values() do
    %{
      :mission_critical => :MISSION_CRITICAL,
      :high => :HIGH,
      :medium => :MEDIUM,
      :low => :LOW
    }
  end

  @typedoc """
  Software lifecycle stage of a destination service as defined [AppHub environment type](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type_1)


  ### Enum Values
  * `:production` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Production environment.
  * `:staging` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Staging environment.
  * `:test` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Test environment.
  * `:development` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Development environment.
  """
  @type gcp_apphub_destination_service_environment_type_values() :: %{
          :production => :PRODUCTION,
          :staging => :STAGING,
          :test => :TEST,
          :development => :DEVELOPMENT
        }
  @doc """
  Software lifecycle stage of a destination service as defined [AppHub environment type](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type_1)



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_service_environment_type()
      :"gcp.apphub_destination.service.environment_type"

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_service_environment_type_values().production
      :PRODUCTION

      iex> %{OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_service_environment_type() => OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_service_environment_type_values().production}
      %{:"gcp.apphub_destination.service.environment_type" => :PRODUCTION}

  ### Erlang

  ```erlang
  ?GCP_APPHUB_DESTINATION_SERVICE_ENVIRONMENT_TYPE.
  'gcp.apphub_destination.service.environment_type'

  ?GCP_APPHUB_DESTINATION_SERVICE_ENVIRONMENT_TYPE_VALUES_PRODUCTION.
  'PRODUCTION'

  \#{?GCP_APPHUB_DESTINATION_SERVICE_ENVIRONMENT_TYPE => ?GCP_APPHUB_DESTINATION_SERVICE_ENVIRONMENT_TYPE_VALUES_PRODUCTION}.
  \#{'gcp.apphub_destination.service.environment_type' => 'PRODUCTION'}
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_destination_service_environment_type ::
          :"gcp.apphub_destination.service.environment_type"
  def gcp_apphub_destination_service_environment_type do
    :"gcp.apphub_destination.service.environment_type"
  end

  @spec gcp_apphub_destination_service_environment_type_values() ::
          gcp_apphub_destination_service_environment_type_values()
  def gcp_apphub_destination_service_environment_type_values() do
    %{
      :production => :PRODUCTION,
      :staging => :STAGING,
      :test => :TEST,
      :development => :DEVELOPMENT
    }
  end

  @doc """
  The name of the destination service as configured in AppHub.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["my-service"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_service_id()
      :"gcp.apphub_destination.service.id"

  ### Erlang

  ```erlang
  ?GCP_APPHUB_DESTINATION_SERVICE_ID.
  'gcp.apphub_destination.service.id'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_destination_service_id :: :"gcp.apphub_destination.service.id"
  def gcp_apphub_destination_service_id do
    :"gcp.apphub_destination.service.id"
  end

  @typedoc """
  Criticality of a destination workload indicates its importance to the business as specified in [AppHub type enum](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type)


  ### Enum Values
  * `:mission_critical` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Mission critical service.
  * `:high` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - High impact.
  * `:medium` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Medium impact.
  * `:low` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Low impact.
  """
  @type gcp_apphub_destination_workload_criticality_type_values() :: %{
          :mission_critical => :MISSION_CRITICAL,
          :high => :HIGH,
          :medium => :MEDIUM,
          :low => :LOW
        }
  @doc """
  Criticality of a destination workload indicates its importance to the business as specified in [AppHub type enum](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type)



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_workload_criticality_type()
      :"gcp.apphub_destination.workload.criticality_type"

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_workload_criticality_type_values().mission_critical
      :MISSION_CRITICAL

      iex> %{OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_workload_criticality_type() => OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_workload_criticality_type_values().mission_critical}
      %{:"gcp.apphub_destination.workload.criticality_type" => :MISSION_CRITICAL}

  ### Erlang

  ```erlang
  ?GCP_APPHUB_DESTINATION_WORKLOAD_CRITICALITY_TYPE.
  'gcp.apphub_destination.workload.criticality_type'

  ?GCP_APPHUB_DESTINATION_WORKLOAD_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL.
  'MISSION_CRITICAL'

  \#{?GCP_APPHUB_DESTINATION_WORKLOAD_CRITICALITY_TYPE => ?GCP_APPHUB_DESTINATION_WORKLOAD_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL}.
  \#{'gcp.apphub_destination.workload.criticality_type' => 'MISSION_CRITICAL'}
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_destination_workload_criticality_type ::
          :"gcp.apphub_destination.workload.criticality_type"
  def gcp_apphub_destination_workload_criticality_type do
    :"gcp.apphub_destination.workload.criticality_type"
  end

  @spec gcp_apphub_destination_workload_criticality_type_values() ::
          gcp_apphub_destination_workload_criticality_type_values()
  def gcp_apphub_destination_workload_criticality_type_values() do
    %{
      :mission_critical => :MISSION_CRITICAL,
      :high => :HIGH,
      :medium => :MEDIUM,
      :low => :LOW
    }
  end

  @typedoc """
  Environment of a destination workload is the stage of a software lifecycle as provided in the [AppHub environment type](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type_1)


  ### Enum Values
  * `:production` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Production environment.
  * `:staging` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Staging environment.
  * `:test` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Test environment.
  * `:development` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Development environment.
  """
  @type gcp_apphub_destination_workload_environment_type_values() :: %{
          :production => :PRODUCTION,
          :staging => :STAGING,
          :test => :TEST,
          :development => :DEVELOPMENT
        }
  @doc """
  Environment of a destination workload is the stage of a software lifecycle as provided in the [AppHub environment type](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type_1)



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_workload_environment_type()
      :"gcp.apphub_destination.workload.environment_type"

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_workload_environment_type_values().production
      :PRODUCTION

      iex> %{OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_workload_environment_type() => OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_workload_environment_type_values().production}
      %{:"gcp.apphub_destination.workload.environment_type" => :PRODUCTION}

  ### Erlang

  ```erlang
  ?GCP_APPHUB_DESTINATION_WORKLOAD_ENVIRONMENT_TYPE.
  'gcp.apphub_destination.workload.environment_type'

  ?GCP_APPHUB_DESTINATION_WORKLOAD_ENVIRONMENT_TYPE_VALUES_PRODUCTION.
  'PRODUCTION'

  \#{?GCP_APPHUB_DESTINATION_WORKLOAD_ENVIRONMENT_TYPE => ?GCP_APPHUB_DESTINATION_WORKLOAD_ENVIRONMENT_TYPE_VALUES_PRODUCTION}.
  \#{'gcp.apphub_destination.workload.environment_type' => 'PRODUCTION'}
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_destination_workload_environment_type ::
          :"gcp.apphub_destination.workload.environment_type"
  def gcp_apphub_destination_workload_environment_type do
    :"gcp.apphub_destination.workload.environment_type"
  end

  @spec gcp_apphub_destination_workload_environment_type_values() ::
          gcp_apphub_destination_workload_environment_type_values()
  def gcp_apphub_destination_workload_environment_type_values() do
    %{
      :production => :PRODUCTION,
      :staging => :STAGING,
      :test => :TEST,
      :development => :DEVELOPMENT
    }
  end

  @doc """
  The name of the destination workload as configured in AppHub.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["my-workload"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_apphub_destination_workload_id()
      :"gcp.apphub_destination.workload.id"

  ### Erlang

  ```erlang
  ?GCP_APPHUB_DESTINATION_WORKLOAD_ID.
  'gcp.apphub_destination.workload.id'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_apphub_destination_workload_id :: :"gcp.apphub_destination.workload.id"
  def gcp_apphub_destination_workload_id do
    :"gcp.apphub_destination.workload.id"
  end

  @doc """
  Identifies the Google Cloud service for which the official client library is intended.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Intended to be a stable identifier for Google Cloud client libraries that is uniform across implementation languages. The value should be derived from the canonical service domain for the service; for example, 'foo.googleapis.com' should result in a value of 'foo'.

  ### Examples

  ```
  ["appengine", "run", "firestore", "alloydb", "spanner"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_client_service()
      :"gcp.client.service"

  ### Erlang

  ```erlang
  ?GCP_CLIENT_SERVICE.
  'gcp.client.service'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_client_service :: :"gcp.client.service"
  def gcp_client_service do
    :"gcp.client.service"
  end

  @doc """
  The name of the Cloud Run [execution](https://cloud.google.com/run/docs/managing/job-executions) being run for the Job, as set by the [`CLOUD_RUN_EXECUTION`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["job-name-xxxx", "sample-job-mdw84"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_cloud_run_job_execution()
      :"gcp.cloud_run.job.execution"

  ### Erlang

  ```erlang
  ?GCP_CLOUD_RUN_JOB_EXECUTION.
  'gcp.cloud_run.job.execution'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_cloud_run_job_execution :: :"gcp.cloud_run.job.execution"
  def gcp_cloud_run_job_execution do
    :"gcp.cloud_run.job.execution"
  end

  @doc """
  The index for a task within an execution as provided by the [`CLOUD_RUN_TASK_INDEX`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [0, 1]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_cloud_run_job_task_index()
      :"gcp.cloud_run.job.task_index"

  ### Erlang

  ```erlang
  ?GCP_CLOUD_RUN_JOB_TASK_INDEX.
  'gcp.cloud_run.job.task_index'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_cloud_run_job_task_index :: :"gcp.cloud_run.job.task_index"
  def gcp_cloud_run_job_task_index do
    :"gcp.cloud_run.job.task_index"
  end

  @doc """
  The hostname of a GCE instance. This is the full value of the default or [custom hostname](https://cloud.google.com/compute/docs/instances/custom-hostname-vm).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["my-host1234.example.com", "sample-vm.us-west1-b.c.my-project.internal"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_gce_instance_hostname()
      :"gcp.gce.instance.hostname"

  ### Erlang

  ```erlang
  ?GCP_GCE_INSTANCE_HOSTNAME.
  'gcp.gce.instance.hostname'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_gce_instance_hostname :: :"gcp.gce.instance.hostname"
  def gcp_gce_instance_hostname do
    :"gcp.gce.instance.hostname"
  end

  @doc """
  The instance name of a GCE instance. This is the value provided by `host.name`, the visible name of the instance in the Cloud Console UI, and the prefix for the default hostname of the instance as defined by the [default internal DNS name](https://cloud.google.com/compute/docs/internal-dns#instance-fully-qualified-domain-names).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["instance-1", "my-vm-name"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_gce_instance_name()
      :"gcp.gce.instance.name"

  ### Erlang

  ```erlang
  ?GCP_GCE_INSTANCE_NAME.
  'gcp.gce.instance.name'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_gce_instance_name :: :"gcp.gce.instance.name"
  def gcp_gce_instance_name do
    :"gcp.gce.instance.name"
  end

  @doc """
  The name of the Instance Group Manager (IGM) that manages this VM, if any.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["web-igm", "my-managed-group"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_gce_instance_group_manager_name()
      :"gcp.gce.instance_group_manager.name"

  ### Erlang

  ```erlang
  ?GCP_GCE_INSTANCE_GROUP_MANAGER_NAME.
  'gcp.gce.instance_group_manager.name'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_gce_instance_group_manager_name :: :"gcp.gce.instance_group_manager.name"
  def gcp_gce_instance_group_manager_name do
    :"gcp.gce.instance_group_manager.name"
  end

  @doc """
  The region of a **regional** Instance Group Manager (e.g., `us-central1`). Set this **only** when the IGM is regional.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["us-central1", "europe-west1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_gce_instance_group_manager_region()
      :"gcp.gce.instance_group_manager.region"

  ### Erlang

  ```erlang
  ?GCP_GCE_INSTANCE_GROUP_MANAGER_REGION.
  'gcp.gce.instance_group_manager.region'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_gce_instance_group_manager_region :: :"gcp.gce.instance_group_manager.region"
  def gcp_gce_instance_group_manager_region do
    :"gcp.gce.instance_group_manager.region"
  end

  @doc """
  The zone of a **zonal** Instance Group Manager (e.g., `us-central1-a`). Set this **only** when the IGM is zonal.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["us-central1-a", "europe-west1-b"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GCPAttributes.gcp_gce_instance_group_manager_zone()
      :"gcp.gce.instance_group_manager.zone"

  ### Erlang

  ```erlang
  ?GCP_GCE_INSTANCE_GROUP_MANAGER_ZONE.
  'gcp.gce.instance_group_manager.zone'
  ```

  <!-- tabs-close -->
  """
  @spec gcp_gce_instance_group_manager_zone :: :"gcp.gce.instance_group_manager.zone"
  def gcp_gce_instance_group_manager_zone do
    :"gcp.gce.instance_group_manager.zone"
  end
end
