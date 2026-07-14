defmodule OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for K8S metrics.
  """
  @doc """
  The number of actively running jobs for a cronjob

  Instrument: `updowncounter`
  Unit: `{job}`
  ### Notes

  This metric aligns with the `active` field of the
  [K8s CronJobStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#cronjobstatus-v1-batch).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.cronjob`](../resource/k8s.md#cronjob) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_cronjob_active_jobs()
      :"k8s.cronjob.active_jobs"

  ### Erlang

  ```erlang
  ?K8S_CRONJOB_ACTIVE_JOBS.
  'k8s.cronjob.active_jobs'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_cronjob_active_jobs :: :"k8s.cronjob.active_jobs"
  def k8s_cronjob_active_jobs do
    :"k8s.cronjob.active_jobs"
  end

  @doc """
  Number of nodes that are running at least 1 daemon pod and are supposed to run the daemon pod

  Instrument: `updowncounter`
  Unit: `{node}`
  ### Notes

  This metric aligns with the `currentNumberScheduled` field of the
  [K8s DaemonSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#daemonsetstatus-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.daemonset`](../resource/k8s.md#daemonset) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_daemonset_current_scheduled_nodes()
      :"k8s.daemonset.current_scheduled_nodes"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_CURRENT_SCHEDULED_NODES.
  'k8s.daemonset.current_scheduled_nodes'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_daemonset_current_scheduled_nodes :: :"k8s.daemonset.current_scheduled_nodes"
  def k8s_daemonset_current_scheduled_nodes do
    :"k8s.daemonset.current_scheduled_nodes"
  end

  @doc """
  Number of nodes that should be running the daemon pod (including nodes currently running the daemon pod)

  Instrument: `updowncounter`
  Unit: `{node}`
  ### Notes

  This metric aligns with the `desiredNumberScheduled` field of the
  [K8s DaemonSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#daemonsetstatus-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.daemonset`](../resource/k8s.md#daemonset) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_daemonset_desired_scheduled_nodes()
      :"k8s.daemonset.desired_scheduled_nodes"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_DESIRED_SCHEDULED_NODES.
  'k8s.daemonset.desired_scheduled_nodes'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_daemonset_desired_scheduled_nodes :: :"k8s.daemonset.desired_scheduled_nodes"
  def k8s_daemonset_desired_scheduled_nodes do
    :"k8s.daemonset.desired_scheduled_nodes"
  end

  @doc """
  Number of nodes that are running the daemon pod, but are not supposed to run the daemon pod

  Instrument: `updowncounter`
  Unit: `{node}`
  ### Notes

  This metric aligns with the `numberMisscheduled` field of the
  [K8s DaemonSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#daemonsetstatus-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.daemonset`](../resource/k8s.md#daemonset) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_daemonset_misscheduled_nodes()
      :"k8s.daemonset.misscheduled_nodes"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_MISSCHEDULED_NODES.
  'k8s.daemonset.misscheduled_nodes'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_daemonset_misscheduled_nodes :: :"k8s.daemonset.misscheduled_nodes"
  def k8s_daemonset_misscheduled_nodes do
    :"k8s.daemonset.misscheduled_nodes"
  end

  @doc """
  Number of nodes that should be running the daemon pod and have one or more of the daemon pod running and ready

  Instrument: `updowncounter`
  Unit: `{node}`
  ### Notes

  This metric aligns with the `numberReady` field of the
  [K8s DaemonSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#daemonsetstatus-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.daemonset`](../resource/k8s.md#daemonset) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_daemonset_ready_nodes()
      :"k8s.daemonset.ready_nodes"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_READY_NODES.
  'k8s.daemonset.ready_nodes'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_daemonset_ready_nodes :: :"k8s.daemonset.ready_nodes"
  def k8s_daemonset_ready_nodes do
    :"k8s.daemonset.ready_nodes"
  end

  @doc """
  Total number of available replica pods (ready for at least minReadySeconds) targeted by this deployment

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `availableReplicas` field of the
  [K8s DeploymentStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#deploymentstatus-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.deployment`](../resource/k8s.md#deployment) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_deployment_available_pods()
      :"k8s.deployment.available_pods"

  ### Erlang

  ```erlang
  ?K8S_DEPLOYMENT_AVAILABLE_PODS.
  'k8s.deployment.available_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_deployment_available_pods :: :"k8s.deployment.available_pods"
  def k8s_deployment_available_pods do
    :"k8s.deployment.available_pods"
  end

  @doc """
  Number of desired replica pods in this deployment

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `replicas` field of the
  [K8s DeploymentSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#deploymentspec-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.deployment`](../resource/k8s.md#deployment) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_deployment_desired_pods()
      :"k8s.deployment.desired_pods"

  ### Erlang

  ```erlang
  ?K8S_DEPLOYMENT_DESIRED_PODS.
  'k8s.deployment.desired_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_deployment_desired_pods :: :"k8s.deployment.desired_pods"
  def k8s_deployment_desired_pods do
    :"k8s.deployment.desired_pods"
  end

  @doc """
  Current number of replica pods managed by this horizontal pod autoscaler, as last seen by the autoscaler

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `currentReplicas` field of the
  [K8s HorizontalPodAutoscalerStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#horizontalpodautoscalerstatus-v2-autoscaling)

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.hpa`](../resource/k8s.md#horizontalpodautoscaler) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_current_pods()
      :"k8s.hpa.current_pods"

  ### Erlang

  ```erlang
  ?K8S_HPA_CURRENT_PODS.
  'k8s.hpa.current_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_current_pods :: :"k8s.hpa.current_pods"
  def k8s_hpa_current_pods do
    :"k8s.hpa.current_pods"
  end

  @doc """
  Desired number of replica pods managed by this horizontal pod autoscaler, as last calculated by the autoscaler

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `desiredReplicas` field of the
  [K8s HorizontalPodAutoscalerStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#horizontalpodautoscalerstatus-v2-autoscaling)

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.hpa`](../resource/k8s.md#horizontalpodautoscaler) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_desired_pods()
      :"k8s.hpa.desired_pods"

  ### Erlang

  ```erlang
  ?K8S_HPA_DESIRED_PODS.
  'k8s.hpa.desired_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_desired_pods :: :"k8s.hpa.desired_pods"
  def k8s_hpa_desired_pods do
    :"k8s.hpa.desired_pods"
  end

  @doc """
  The upper limit for the number of replica pods to which the autoscaler can scale up

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `maxReplicas` field of the
  [K8s HorizontalPodAutoscalerSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#horizontalpodautoscalerspec-v2-autoscaling)

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.hpa`](../resource/k8s.md#horizontalpodautoscaler) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_max_pods()
      :"k8s.hpa.max_pods"

  ### Erlang

  ```erlang
  ?K8S_HPA_MAX_PODS.
  'k8s.hpa.max_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_max_pods :: :"k8s.hpa.max_pods"
  def k8s_hpa_max_pods do
    :"k8s.hpa.max_pods"
  end

  @doc """
  The lower limit for the number of replica pods to which the autoscaler can scale down

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `minReplicas` field of the
  [K8s HorizontalPodAutoscalerSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#horizontalpodautoscalerspec-v2-autoscaling)

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.hpa`](../resource/k8s.md#horizontalpodautoscaler) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_min_pods()
      :"k8s.hpa.min_pods"

  ### Erlang

  ```erlang
  ?K8S_HPA_MIN_PODS.
  'k8s.hpa.min_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_min_pods :: :"k8s.hpa.min_pods"
  def k8s_hpa_min_pods do
    :"k8s.hpa.min_pods"
  end

  @doc """
  The number of pending and actively running pods for a job

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `active` field of the
  [K8s JobStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#jobstatus-v1-batch).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.job`](../resource/k8s.md#job) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_job_active_pods()
      :"k8s.job.active_pods"

  ### Erlang

  ```erlang
  ?K8S_JOB_ACTIVE_PODS.
  'k8s.job.active_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_job_active_pods :: :"k8s.job.active_pods"
  def k8s_job_active_pods do
    :"k8s.job.active_pods"
  end

  @doc """
  The desired number of successfully finished pods the job should be run with

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `completions` field of the
  [K8s JobSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#jobspec-v1-batch).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.job`](../resource/k8s.md#job) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_job_desired_successful_pods()
      :"k8s.job.desired_successful_pods"

  ### Erlang

  ```erlang
  ?K8S_JOB_DESIRED_SUCCESSFUL_PODS.
  'k8s.job.desired_successful_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_job_desired_successful_pods :: :"k8s.job.desired_successful_pods"
  def k8s_job_desired_successful_pods do
    :"k8s.job.desired_successful_pods"
  end

  @doc """
  The number of pods which reached phase Failed for a job

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `failed` field of the
  [K8s JobStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#jobstatus-v1-batch).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.job`](../resource/k8s.md#job) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_job_failed_pods()
      :"k8s.job.failed_pods"

  ### Erlang

  ```erlang
  ?K8S_JOB_FAILED_PODS.
  'k8s.job.failed_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_job_failed_pods :: :"k8s.job.failed_pods"
  def k8s_job_failed_pods do
    :"k8s.job.failed_pods"
  end

  @doc """
  The max desired number of pods the job should run at any given time

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `parallelism` field of the
  [K8s JobSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#jobspec-v1-batch).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.job`](../resource/k8s.md#job) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_job_max_parallel_pods()
      :"k8s.job.max_parallel_pods"

  ### Erlang

  ```erlang
  ?K8S_JOB_MAX_PARALLEL_PODS.
  'k8s.job.max_parallel_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_job_max_parallel_pods :: :"k8s.job.max_parallel_pods"
  def k8s_job_max_parallel_pods do
    :"k8s.job.max_parallel_pods"
  end

  @doc """
  The number of pods which reached phase Succeeded for a job

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `succeeded` field of the
  [K8s JobStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#jobstatus-v1-batch).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.job`](../resource/k8s.md#job) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_job_successful_pods()
      :"k8s.job.successful_pods"

  ### Erlang

  ```erlang
  ?K8S_JOB_SUCCESSFUL_PODS.
  'k8s.job.successful_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_job_successful_pods :: :"k8s.job.successful_pods"
  def k8s_job_successful_pods do
    :"k8s.job.successful_pods"
  end

  @doc """
  Describes number of K8s namespaces that are currently in a given phase.

  Instrument: `updowncounter`
  Unit: `{namespace}`
  ### Notes

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.namespace`](../resource/k8s.md#namespace) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_namespace_phase()
      :"k8s.namespace.phase"

  ### Erlang

  ```erlang
  ?K8S_NAMESPACE_PHASE.
  'k8s.namespace.phase'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_namespace_phase :: :"k8s.namespace.phase"
  def k8s_namespace_phase do
    :"k8s.namespace.phase"
  end

  @doc """
  Total CPU time consumed

  Instrument: `counter`
  Unit: `s`
  ### Notes

  Total CPU time consumed by the specific Node on all available CPU cores


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_cpu_time()
      :"k8s.node.cpu.time"

  ### Erlang

  ```erlang
  ?K8S_NODE_CPU_TIME.
  'k8s.node.cpu.time'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_cpu_time :: :"k8s.node.cpu.time"
  def k8s_node_cpu_time do
    :"k8s.node.cpu.time"
  end

  @doc """
  Node's CPU usage, measured in cpus. Range from 0 to the number of allocatable CPUs

  Instrument: `gauge`
  Unit: `{cpu}`
  ### Notes

  CPU usage of the specific Node on all available CPU cores, averaged over the sample window


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_cpu_usage()
      :"k8s.node.cpu.usage"

  ### Erlang

  ```erlang
  ?K8S_NODE_CPU_USAGE.
  'k8s.node.cpu.usage'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_cpu_usage :: :"k8s.node.cpu.usage"
  def k8s_node_cpu_usage do
    :"k8s.node.cpu.usage"
  end

  @doc """
  Memory usage of the Node

  Instrument: `gauge`
  Unit: `By`
  ### Notes

  Total memory usage of the Node


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_memory_usage()
      :"k8s.node.memory.usage"

  ### Erlang

  ```erlang
  ?K8S_NODE_MEMORY_USAGE.
  'k8s.node.memory.usage'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_memory_usage :: :"k8s.node.memory.usage"
  def k8s_node_memory_usage do
    :"k8s.node.memory.usage"
  end

  @doc """
  Node network errors

  Instrument: `counter`
  Unit: `{error}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_network_errors()
      :"k8s.node.network.errors"

  ### Erlang

  ```erlang
  ?K8S_NODE_NETWORK_ERRORS.
  'k8s.node.network.errors'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_network_errors :: :"k8s.node.network.errors"
  def k8s_node_network_errors do
    :"k8s.node.network.errors"
  end

  @doc """
  Network bytes for the Node

  Instrument: `counter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_network_io()
      :"k8s.node.network.io"

  ### Erlang

  ```erlang
  ?K8S_NODE_NETWORK_IO.
  'k8s.node.network.io'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_network_io :: :"k8s.node.network.io"
  def k8s_node_network_io do
    :"k8s.node.network.io"
  end

  @doc """
  The time the Node has been running

  Instrument: `gauge`
  Unit: `s`
  ### Notes

  Instrumentations **SHOULD** use a gauge with type `double` and measure uptime in seconds as a floating point number with the highest precision available.
  The actual accuracy would depend on the instrumentation and operating system.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_uptime()
      :"k8s.node.uptime"

  ### Erlang

  ```erlang
  ?K8S_NODE_UPTIME.
  'k8s.node.uptime'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_uptime :: :"k8s.node.uptime"
  def k8s_node_uptime do
    :"k8s.node.uptime"
  end

  @doc """
  Total CPU time consumed

  Instrument: `counter`
  Unit: `s`
  ### Notes

  Total CPU time consumed by the specific Pod on all available CPU cores


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_cpu_time()
      :"k8s.pod.cpu.time"

  ### Erlang

  ```erlang
  ?K8S_POD_CPU_TIME.
  'k8s.pod.cpu.time'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_cpu_time :: :"k8s.pod.cpu.time"
  def k8s_pod_cpu_time do
    :"k8s.pod.cpu.time"
  end

  @doc """
  Pod's CPU usage, measured in cpus. Range from 0 to the number of allocatable CPUs

  Instrument: `gauge`
  Unit: `{cpu}`
  ### Notes

  CPU usage of the specific Pod on all available CPU cores, averaged over the sample window


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_cpu_usage()
      :"k8s.pod.cpu.usage"

  ### Erlang

  ```erlang
  ?K8S_POD_CPU_USAGE.
  'k8s.pod.cpu.usage'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_cpu_usage :: :"k8s.pod.cpu.usage"
  def k8s_pod_cpu_usage do
    :"k8s.pod.cpu.usage"
  end

  @doc """
  Memory usage of the Pod

  Instrument: `gauge`
  Unit: `By`
  ### Notes

  Total memory usage of the Pod


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_memory_usage()
      :"k8s.pod.memory.usage"

  ### Erlang

  ```erlang
  ?K8S_POD_MEMORY_USAGE.
  'k8s.pod.memory.usage'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_memory_usage :: :"k8s.pod.memory.usage"
  def k8s_pod_memory_usage do
    :"k8s.pod.memory.usage"
  end

  @doc """
  Pod network errors

  Instrument: `counter`
  Unit: `{error}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_network_errors()
      :"k8s.pod.network.errors"

  ### Erlang

  ```erlang
  ?K8S_POD_NETWORK_ERRORS.
  'k8s.pod.network.errors'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_network_errors :: :"k8s.pod.network.errors"
  def k8s_pod_network_errors do
    :"k8s.pod.network.errors"
  end

  @doc """
  Network bytes for the Pod

  Instrument: `counter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_network_io()
      :"k8s.pod.network.io"

  ### Erlang

  ```erlang
  ?K8S_POD_NETWORK_IO.
  'k8s.pod.network.io'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_network_io :: :"k8s.pod.network.io"
  def k8s_pod_network_io do
    :"k8s.pod.network.io"
  end

  @doc """
  The time the Pod has been running

  Instrument: `gauge`
  Unit: `s`
  ### Notes

  Instrumentations **SHOULD** use a gauge with type `double` and measure uptime in seconds as a floating point number with the highest precision available.
  The actual accuracy would depend on the instrumentation and operating system.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_uptime()
      :"k8s.pod.uptime"

  ### Erlang

  ```erlang
  ?K8S_POD_UPTIME.
  'k8s.pod.uptime'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_uptime :: :"k8s.pod.uptime"
  def k8s_pod_uptime do
    :"k8s.pod.uptime"
  end

  @doc """
  Total number of available replica pods (ready for at least minReadySeconds) targeted by this replicaset

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `availableReplicas` field of the
  [K8s ReplicaSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#replicasetstatus-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.replicaset`](../resource/k8s.md#replicaset) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_replicaset_available_pods()
      :"k8s.replicaset.available_pods"

  ### Erlang

  ```erlang
  ?K8S_REPLICASET_AVAILABLE_PODS.
  'k8s.replicaset.available_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_replicaset_available_pods :: :"k8s.replicaset.available_pods"
  def k8s_replicaset_available_pods do
    :"k8s.replicaset.available_pods"
  end

  @doc """
  Number of desired replica pods in this replicaset

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `replicas` field of the
  [K8s ReplicaSetSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#replicasetspec-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.replicaset`](../resource/k8s.md#replicaset) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_replicaset_desired_pods()
      :"k8s.replicaset.desired_pods"

  ### Erlang

  ```erlang
  ?K8S_REPLICASET_DESIRED_PODS.
  'k8s.replicaset.desired_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_replicaset_desired_pods :: :"k8s.replicaset.desired_pods"
  def k8s_replicaset_desired_pods do
    :"k8s.replicaset.desired_pods"
  end

  @deprecated """
  Replaced by `k8s.replicationcontroller.available_pods`.
  """

  @spec k8s_replication_controller_available_pods :: :"k8s.replication_controller.available_pods"
  def k8s_replication_controller_available_pods do
    :"k8s.replication_controller.available_pods"
  end

  @deprecated """
  Replaced by `k8s.replicationcontroller.desired_pods`.
  """

  @spec k8s_replication_controller_desired_pods :: :"k8s.replication_controller.desired_pods"
  def k8s_replication_controller_desired_pods do
    :"k8s.replication_controller.desired_pods"
  end

  @doc """
  Total number of available replica pods (ready for at least minReadySeconds) targeted by this replication controller

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `availableReplicas` field of the
  [K8s ReplicationControllerStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#replicationcontrollerstatus-v1-core)

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.replicationcontroller`](../resource/k8s.md#replicationcontroller) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_replicationcontroller_available_pods()
      :"k8s.replicationcontroller.available_pods"

  ### Erlang

  ```erlang
  ?K8S_REPLICATIONCONTROLLER_AVAILABLE_PODS.
  'k8s.replicationcontroller.available_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_replicationcontroller_available_pods :: :"k8s.replicationcontroller.available_pods"
  def k8s_replicationcontroller_available_pods do
    :"k8s.replicationcontroller.available_pods"
  end

  @doc """
  Number of desired replica pods in this replication controller

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `replicas` field of the
  [K8s ReplicationControllerSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#replicationcontrollerspec-v1-core)

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.replicationcontroller`](../resource/k8s.md#replicationcontroller) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_replicationcontroller_desired_pods()
      :"k8s.replicationcontroller.desired_pods"

  ### Erlang

  ```erlang
  ?K8S_REPLICATIONCONTROLLER_DESIRED_PODS.
  'k8s.replicationcontroller.desired_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_replicationcontroller_desired_pods :: :"k8s.replicationcontroller.desired_pods"
  def k8s_replicationcontroller_desired_pods do
    :"k8s.replicationcontroller.desired_pods"
  end

  @doc """
  The number of replica pods created by the statefulset controller from the statefulset version indicated by currentRevision

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `currentReplicas` field of the
  [K8s StatefulSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#statefulsetstatus-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.statefulset`](../resource/k8s.md#statefulset) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_statefulset_current_pods()
      :"k8s.statefulset.current_pods"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_CURRENT_PODS.
  'k8s.statefulset.current_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_statefulset_current_pods :: :"k8s.statefulset.current_pods"
  def k8s_statefulset_current_pods do
    :"k8s.statefulset.current_pods"
  end

  @doc """
  Number of desired replica pods in this statefulset

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `replicas` field of the
  [K8s StatefulSetSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#statefulsetspec-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.statefulset`](../resource/k8s.md#statefulset) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_statefulset_desired_pods()
      :"k8s.statefulset.desired_pods"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_DESIRED_PODS.
  'k8s.statefulset.desired_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_statefulset_desired_pods :: :"k8s.statefulset.desired_pods"
  def k8s_statefulset_desired_pods do
    :"k8s.statefulset.desired_pods"
  end

  @doc """
  The number of replica pods created for this statefulset with a Ready Condition

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `readyReplicas` field of the
  [K8s StatefulSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#statefulsetstatus-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.statefulset`](../resource/k8s.md#statefulset) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_statefulset_ready_pods()
      :"k8s.statefulset.ready_pods"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_READY_PODS.
  'k8s.statefulset.ready_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_statefulset_ready_pods :: :"k8s.statefulset.ready_pods"
  def k8s_statefulset_ready_pods do
    :"k8s.statefulset.ready_pods"
  end

  @doc """
  Number of replica pods created by the statefulset controller from the statefulset version indicated by updateRevision

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `updatedReplicas` field of the
  [K8s StatefulSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#statefulsetstatus-v1-apps).

  This metric SHOULD, at a minimum, be reported against a
  [`k8s.statefulset`](../resource/k8s.md#statefulset) resource.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_statefulset_updated_pods()
      :"k8s.statefulset.updated_pods"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_UPDATED_PODS.
  'k8s.statefulset.updated_pods'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_statefulset_updated_pods :: :"k8s.statefulset.updated_pods"
  def k8s_statefulset_updated_pods do
    :"k8s.statefulset.updated_pods"
  end
end
