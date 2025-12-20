defmodule OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cloudfoundry attributes.
  """

  @doc """
  The guid of the application.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Application instrumentation should use the value from environment
  variable `VCAP_APPLICATION.application_id`. This is the same value as
  reported by `cf app <app-name> --guid`.

  ### Examples

  ```
  ["218fc5a9-a5f1-4b54-aa05-46717d0ab26d"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_app_id()
      :"cloudfoundry.app.id"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_APP_ID.
  'cloudfoundry.app.id'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_app_id :: :"cloudfoundry.app.id"
  def cloudfoundry_app_id do
    :"cloudfoundry.app.id"
  end

  @doc """
  The index of the application instance. 0 when just one instance is active.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  CloudFoundry defines the `instance_id` in the [Loggregator v2 envelope](https://github.com/cloudfoundry/loggregator-api#v2-envelope).
  It is used for logs and metrics emitted by CloudFoundry. It is
  supposed to contain the application instance index for applications
  deployed on the runtime.

  Application instrumentation should use the value from environment
  variable `CF_INSTANCE_INDEX`.

  ### Examples

  ```
  ["0", "1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_app_instance_id()
      :"cloudfoundry.app.instance.id"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_APP_INSTANCE_ID.
  'cloudfoundry.app.instance.id'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_app_instance_id :: :"cloudfoundry.app.instance.id"
  def cloudfoundry_app_instance_id do
    :"cloudfoundry.app.instance.id"
  end

  @doc """
  The name of the application.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Application instrumentation should use the value from environment
  variable `VCAP_APPLICATION.application_name`. This is the same value
  as reported by `cf apps`.

  ### Examples

  ```
  ["my-app-name"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_app_name()
      :"cloudfoundry.app.name"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_APP_NAME.
  'cloudfoundry.app.name'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_app_name :: :"cloudfoundry.app.name"
  def cloudfoundry_app_name do
    :"cloudfoundry.app.name"
  end

  @doc """
  The guid of the CloudFoundry org the application is running in.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Application instrumentation should use the value from environment
  variable `VCAP_APPLICATION.org_id`. This is the same value as
  reported by `cf org <org-name> --guid`.

  ### Examples

  ```
  ["218fc5a9-a5f1-4b54-aa05-46717d0ab26d"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_org_id()
      :"cloudfoundry.org.id"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_ORG_ID.
  'cloudfoundry.org.id'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_org_id :: :"cloudfoundry.org.id"
  def cloudfoundry_org_id do
    :"cloudfoundry.org.id"
  end

  @doc """
  The name of the CloudFoundry organization the app is running in.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Application instrumentation should use the value from environment
  variable `VCAP_APPLICATION.org_name`. This is the same value as
  reported by `cf orgs`.

  ### Examples

  ```
  ["my-org-name"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_org_name()
      :"cloudfoundry.org.name"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_ORG_NAME.
  'cloudfoundry.org.name'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_org_name :: :"cloudfoundry.org.name"
  def cloudfoundry_org_name do
    :"cloudfoundry.org.name"
  end

  @doc """
  The UID identifying the process.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Application instrumentation should use the value from environment
  variable `VCAP_APPLICATION.process_id`. It is supposed to be equal to
  `VCAP_APPLICATION.app_id` for applications deployed to the runtime.
  For system components, this could be the actual PID.

  ### Examples

  ```
  ["218fc5a9-a5f1-4b54-aa05-46717d0ab26d"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_process_id()
      :"cloudfoundry.process.id"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_PROCESS_ID.
  'cloudfoundry.process.id'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_process_id :: :"cloudfoundry.process.id"
  def cloudfoundry_process_id do
    :"cloudfoundry.process.id"
  end

  @doc """
  The type of process.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  CloudFoundry applications can consist of multiple jobs. Usually the
  main process will be of type `web`. There can be additional background
  tasks or side-cars with different process types.

  ### Examples

  ```
  ["web"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_process_type()
      :"cloudfoundry.process.type"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_PROCESS_TYPE.
  'cloudfoundry.process.type'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_process_type :: :"cloudfoundry.process.type"
  def cloudfoundry_process_type do
    :"cloudfoundry.process.type"
  end

  @doc """
  The guid of the CloudFoundry space the application is running in.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Application instrumentation should use the value from environment
  variable `VCAP_APPLICATION.space_id`. This is the same value as
  reported by `cf space <space-name> --guid`.

  ### Examples

  ```
  ["218fc5a9-a5f1-4b54-aa05-46717d0ab26d"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_space_id()
      :"cloudfoundry.space.id"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_SPACE_ID.
  'cloudfoundry.space.id'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_space_id :: :"cloudfoundry.space.id"
  def cloudfoundry_space_id do
    :"cloudfoundry.space.id"
  end

  @doc """
  The name of the CloudFoundry space the application is running in.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Application instrumentation should use the value from environment
  variable `VCAP_APPLICATION.space_name`. This is the same value as
  reported by `cf spaces`.

  ### Examples

  ```
  ["my-space-name"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_space_name()
      :"cloudfoundry.space.name"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_SPACE_NAME.
  'cloudfoundry.space.name'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_space_name :: :"cloudfoundry.space.name"
  def cloudfoundry_space_name do
    :"cloudfoundry.space.name"
  end

  @doc """
  A guid or another name describing the event source.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  CloudFoundry defines the `source_id` in the [Loggregator v2 envelope](https://github.com/cloudfoundry/loggregator-api#v2-envelope).
  It is used for logs and metrics emitted by CloudFoundry. It is
  supposed to contain the component name, e.g. "gorouter", for
  CloudFoundry components.

  When system components are instrumented, values from the
  [Bosh spec](https://bosh.io/docs/jobs/#properties-spec)
  should be used. The `system.id` should be set to
  `spec.deployment/spec.name`.

  ### Examples

  ```
  ["cf/gorouter"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_system_id()
      :"cloudfoundry.system.id"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_SYSTEM_ID.
  'cloudfoundry.system.id'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_system_id :: :"cloudfoundry.system.id"
  def cloudfoundry_system_id do
    :"cloudfoundry.system.id"
  end

  @doc """
  A guid describing the concrete instance of the event source.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  CloudFoundry defines the `instance_id` in the [Loggregator v2 envelope](https://github.com/cloudfoundry/loggregator-api#v2-envelope).
  It is used for logs and metrics emitted by CloudFoundry. It is
  supposed to contain the vm id for CloudFoundry components.

  When system components are instrumented, values from the
  [Bosh spec](https://bosh.io/docs/jobs/#properties-spec)
  should be used. The `system.instance.id` should be set to `spec.id`.

  ### Examples

  ```
  ["218fc5a9-a5f1-4b54-aa05-46717d0ab26d"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudfoundryAttributes.cloudfoundry_system_instance_id()
      :"cloudfoundry.system.instance.id"

  ### Erlang

  ```erlang
  ?CLOUDFOUNDRY_SYSTEM_INSTANCE_ID.
  'cloudfoundry.system.instance.id'
  ```

  <!-- tabs-close -->
  """
  @spec cloudfoundry_system_instance_id :: :"cloudfoundry.system.instance.id"
  def cloudfoundry_system_instance_id do
    :"cloudfoundry.system.instance.id"
  end
end
