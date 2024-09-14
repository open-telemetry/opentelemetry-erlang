defmodule OpenTelemetry.SemConv.Incubating.GCPAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for GCP attributes.
  """

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
end
