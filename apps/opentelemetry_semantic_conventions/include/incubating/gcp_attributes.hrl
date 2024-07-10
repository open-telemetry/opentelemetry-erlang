

%% The name of the Cloud Run [execution](https://cloud.google.com/run/docs/managing/job-executions) being run for the Job, as set by the [`CLOUD_RUN_EXECUTION`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable.
%%  
-define(GCP_CLOUDRUN_JOB_EXECUTION, 'gcp.cloud_run.job.execution').


%% The index for a task within an execution as provided by the [`CLOUD_RUN_TASK_INDEX`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable.
%%  
-define(GCP_CLOUDRUN_JOB_TASKINDEX, 'gcp.cloud_run.job.task_index').


%% The hostname of a GCE instance. This is the full value of the default or [custom hostname](https://cloud.google.com/compute/docs/instances/custom-hostname-vm).
%%  
-define(GCP_GCE_INSTANCE_HOSTNAME, 'gcp.gce.instance.hostname').


%% The instance name of a GCE instance. This is the value provided by `host.name`, the visible name of the instance in the Cloud Console UI, and the prefix for the default hostname of the instance as defined by the [default internal DNS name](https://cloud.google.com/compute/docs/internal-dns#instance-fully-qualified-domain-names).
%%  
-define(GCP_GCE_INSTANCE_NAME, 'gcp.gce.instance.name').
