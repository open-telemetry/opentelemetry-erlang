defmodule OpenTelemetry.SemanticConventions.GcpAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Gcp attributes.
  """

  @doc """
  The name of the Cloud Run [execution](https://cloud.google.com/run/docs/managing/job-executions) being run for the Job, as set by the [`CLOUD_RUN_EXECUTION`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable.



  ### Example
      iex> OpenTelemetry.SemanticConventions.GcpAttributes.gcp_cloudrun_job_execution()
      :"gcp.cloud_run.job.execution"
  """
  @spec gcp_cloudrun_job_execution :: :"gcp.cloud_run.job.execution"
  def gcp_cloudrun_job_execution do
    :"gcp.cloud_run.job.execution"
  end

  @doc """
  The index for a task within an execution as provided by the [`CLOUD_RUN_TASK_INDEX`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable.



  ### Example
      iex> OpenTelemetry.SemanticConventions.GcpAttributes.gcp_cloudrun_job_taskindex()
      :"gcp.cloud_run.job.task_index"
  """
  @spec gcp_cloudrun_job_taskindex :: :"gcp.cloud_run.job.task_index"
  def gcp_cloudrun_job_taskindex do
    :"gcp.cloud_run.job.task_index"
  end

  @doc """
  The hostname of a GCE instance. This is the full value of the default or [custom hostname](https://cloud.google.com/compute/docs/instances/custom-hostname-vm).



  ### Example
      iex> OpenTelemetry.SemanticConventions.GcpAttributes.gcp_gce_instance_hostname()
      :"gcp.gce.instance.hostname"
  """
  @spec gcp_gce_instance_hostname :: :"gcp.gce.instance.hostname"
  def gcp_gce_instance_hostname do
    :"gcp.gce.instance.hostname"
  end

  @doc """
  The instance name of a GCE instance. This is the value provided by `host.name`, the visible name of the instance in the Cloud Console UI, and the prefix for the default hostname of the instance as defined by the [default internal DNS name](https://cloud.google.com/compute/docs/internal-dns#instance-fully-qualified-domain-names).



  ### Example
      iex> OpenTelemetry.SemanticConventions.GcpAttributes.gcp_gce_instance_name()
      :"gcp.gce.instance.name"
  """
  @spec gcp_gce_instance_name :: :"gcp.gce.instance.name"
  def gcp_gce_instance_name do
    :"gcp.gce.instance.name"
  end
end
