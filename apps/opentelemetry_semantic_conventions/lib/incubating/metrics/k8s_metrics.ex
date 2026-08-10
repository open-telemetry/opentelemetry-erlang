defmodule OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for K8S metrics.
  """
  @doc """
  Maximum CPU resource limit set for the container.

  Instrument: `updowncounter`
  Unit: `{cpu}`
  ### Notes

  See https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#resourcerequirements-v1-core for details.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_cpu_limit()
      :"k8s.container.cpu.limit"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_CPU_LIMIT.
  'k8s.container.cpu.limit'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_cpu_limit :: :"k8s.container.cpu.limit"
  def k8s_container_cpu_limit do
    :"k8s.container.cpu.limit"
  end

  @doc """
  The ratio of container CPU usage to its CPU limit.

  Instrument: `gauge`
  Unit: `1`
  ### Notes

  The value range is [0.0,1.0]. A value of 1.0 means the container is using 100% of its CPU limit. If the CPU limit is not set, this metric **SHOULD** **NOT** be emitted for that container.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_cpu_limit_utilization()
      :"k8s.container.cpu.limit_utilization"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_CPU_LIMIT_UTILIZATION.
  'k8s.container.cpu.limit_utilization'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_cpu_limit_utilization :: :"k8s.container.cpu.limit_utilization"
  def k8s_container_cpu_limit_utilization do
    :"k8s.container.cpu.limit_utilization"
  end

  @doc """
  CPU resource requested for the container.

  Instrument: `updowncounter`
  Unit: `{cpu}`
  ### Notes

  See https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#resourcerequirements-v1-core for details.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_cpu_request()
      :"k8s.container.cpu.request"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_CPU_REQUEST.
  'k8s.container.cpu.request'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_cpu_request :: :"k8s.container.cpu.request"
  def k8s_container_cpu_request do
    :"k8s.container.cpu.request"
  end

  @doc """
  The ratio of container CPU usage to its CPU request.

  Instrument: `gauge`
  Unit: `1`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_cpu_request_utilization()
      :"k8s.container.cpu.request_utilization"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_CPU_REQUEST_UTILIZATION.
  'k8s.container.cpu.request_utilization'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_cpu_request_utilization :: :"k8s.container.cpu.request_utilization"
  def k8s_container_cpu_request_utilization do
    :"k8s.container.cpu.request_utilization"
  end

  @doc """
  Maximum ephemeral storage resource limit set for the container.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  See https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#resourcerequirements-v1-core for details.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_ephemeral_storage_limit()
      :"k8s.container.ephemeral_storage.limit"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_EPHEMERAL_STORAGE_LIMIT.
  'k8s.container.ephemeral_storage.limit'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_ephemeral_storage_limit :: :"k8s.container.ephemeral_storage.limit"
  def k8s_container_ephemeral_storage_limit do
    :"k8s.container.ephemeral_storage.limit"
  end

  @doc """
  Ephemeral storage resource requested for the container.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  See https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#resourcerequirements-v1-core for details.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_ephemeral_storage_request()
      :"k8s.container.ephemeral_storage.request"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_EPHEMERAL_STORAGE_REQUEST.
  'k8s.container.ephemeral_storage.request'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_ephemeral_storage_request :: :"k8s.container.ephemeral_storage.request"
  def k8s_container_ephemeral_storage_request do
    :"k8s.container.ephemeral_storage.request"
  end

  @doc """
  Maximum memory resource limit set for the container.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  See https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#resourcerequirements-v1-core for details.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_memory_limit()
      :"k8s.container.memory.limit"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_MEMORY_LIMIT.
  'k8s.container.memory.limit'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_memory_limit :: :"k8s.container.memory.limit"
  def k8s_container_memory_limit do
    :"k8s.container.memory.limit"
  end

  @doc """
  Memory resource requested for the container.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  See https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#resourcerequirements-v1-core for details.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_memory_request()
      :"k8s.container.memory.request"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_MEMORY_REQUEST.
  'k8s.container.memory.request'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_memory_request :: :"k8s.container.memory.request"
  def k8s_container_memory_request do
    :"k8s.container.memory.request"
  end

  @doc """
  Indicates whether the container is currently marked as ready to accept traffic, based on its readiness probe (1 = ready, 0 = not ready).


  Instrument: `updowncounter`
  Unit: `{container}`
  ### Notes

  This metric **SHOULD** reflect the value of the `ready` field in the
  [K8s ContainerStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#containerstatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_ready()
      :"k8s.container.ready"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_READY.
  'k8s.container.ready'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_ready :: :"k8s.container.ready"
  def k8s_container_ready do
    :"k8s.container.ready"
  end

  @doc """
  Describes how many times the container has restarted (since the last counter reset).

  Instrument: `updowncounter`
  Unit: `{restart}`
  ### Notes

  This value is pulled directly from the K8s API and the value can go indefinitely high and be reset to 0
  at any time depending on how your kubelet is configured to prune dead containers.
  It is best to not depend too much on the exact value but rather look at it as
  either == 0, in which case you can conclude there were no restarts in the recent past, or > 0, in which case
  you can conclude there were restarts in the recent past, and not try and analyze the value beyond that.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_restart_count()
      :"k8s.container.restart.count"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_RESTART_COUNT.
  'k8s.container.restart.count'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_restart_count :: :"k8s.container.restart.count"
  def k8s_container_restart_count do
    :"k8s.container.restart.count"
  end

  @doc """
  Describes the number of K8s containers that are currently in a state for a given reason.

  Instrument: `updowncounter`
  Unit: `{container}`
  ### Notes

  All possible container state reasons will be reported at each time interval to avoid missing metrics.
  Only the value corresponding to the current state reason will be non-zero.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_status_reason()
      :"k8s.container.status.reason"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_STATUS_REASON.
  'k8s.container.status.reason'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_status_reason :: :"k8s.container.status.reason"
  def k8s_container_status_reason do
    :"k8s.container.status.reason"
  end

  @doc """
  Describes the number of K8s containers that are currently in a given state.

  Instrument: `updowncounter`
  Unit: `{container}`
  ### Notes

  All possible container states will be reported at each time interval to avoid missing metrics.
  Only the value corresponding to the current state will be non-zero.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_status_state()
      :"k8s.container.status.state"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_STATUS_STATE.
  'k8s.container.status.state'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_status_state :: :"k8s.container.status.state"
  def k8s_container_status_state do
    :"k8s.container.status.state"
  end

  @doc """
  Maximum storage resource limit set for the container.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  See https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#resourcerequirements-v1-core for details.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_storage_limit()
      :"k8s.container.storage.limit"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_STORAGE_LIMIT.
  'k8s.container.storage.limit'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_storage_limit :: :"k8s.container.storage.limit"
  def k8s_container_storage_limit do
    :"k8s.container.storage.limit"
  end

  @doc """
  Storage resource requested for the container.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  See https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#resourcerequirements-v1-core for details.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_container_storage_request()
      :"k8s.container.storage.request"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_STORAGE_REQUEST.
  'k8s.container.storage.request'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_container_storage_request :: :"k8s.container.storage.request"
  def k8s_container_storage_request do
    :"k8s.container.storage.request"
  end

  @deprecated """
  Replaced by `k8s.cronjob.job.active`.
  """

  @spec k8s_cronjob_active_jobs :: :"k8s.cronjob.active_jobs"
  def k8s_cronjob_active_jobs do
    :"k8s.cronjob.active_jobs"
  end

  @doc """
  The number of actively running jobs for a cronjob.

  Instrument: `updowncounter`
  Unit: `{job}`
  ### Notes

  This metric aligns with the `active` field of the
  [K8s CronJobStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#cronjobstatus-v1-batch).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_cronjob_job_active()
      :"k8s.cronjob.job.active"

  ### Erlang

  ```erlang
  ?K8S_CRONJOB_JOB_ACTIVE.
  'k8s.cronjob.job.active'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_cronjob_job_active :: :"k8s.cronjob.job.active"
  def k8s_cronjob_job_active do
    :"k8s.cronjob.job.active"
  end

  @deprecated """
  Replaced by `k8s.daemonset.node.current_scheduled`.
  """

  @spec k8s_daemonset_current_scheduled_nodes :: :"k8s.daemonset.current_scheduled_nodes"
  def k8s_daemonset_current_scheduled_nodes do
    :"k8s.daemonset.current_scheduled_nodes"
  end

  @deprecated """
  Replaced by `k8s.daemonset.node.desired_scheduled`.
  """

  @spec k8s_daemonset_desired_scheduled_nodes :: :"k8s.daemonset.desired_scheduled_nodes"
  def k8s_daemonset_desired_scheduled_nodes do
    :"k8s.daemonset.desired_scheduled_nodes"
  end

  @deprecated """
  Replaced by `k8s.daemonset.node.misscheduled`.
  """

  @spec k8s_daemonset_misscheduled_nodes :: :"k8s.daemonset.misscheduled_nodes"
  def k8s_daemonset_misscheduled_nodes do
    :"k8s.daemonset.misscheduled_nodes"
  end

  @doc """
  Number of nodes that are running at least 1 daemon pod and are supposed to run the daemon pod.

  Instrument: `updowncounter`
  Unit: `{node}`
  ### Notes

  This metric aligns with the `currentNumberScheduled` field of the
  [K8s DaemonSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#daemonsetstatus-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_daemonset_node_current_scheduled()
      :"k8s.daemonset.node.current_scheduled"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_NODE_CURRENT_SCHEDULED.
  'k8s.daemonset.node.current_scheduled'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_daemonset_node_current_scheduled :: :"k8s.daemonset.node.current_scheduled"
  def k8s_daemonset_node_current_scheduled do
    :"k8s.daemonset.node.current_scheduled"
  end

  @doc """
  Number of nodes that should be running the daemon pod (including nodes currently running the daemon pod).

  Instrument: `updowncounter`
  Unit: `{node}`
  ### Notes

  This metric aligns with the `desiredNumberScheduled` field of the
  [K8s DaemonSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#daemonsetstatus-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_daemonset_node_desired_scheduled()
      :"k8s.daemonset.node.desired_scheduled"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_NODE_DESIRED_SCHEDULED.
  'k8s.daemonset.node.desired_scheduled'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_daemonset_node_desired_scheduled :: :"k8s.daemonset.node.desired_scheduled"
  def k8s_daemonset_node_desired_scheduled do
    :"k8s.daemonset.node.desired_scheduled"
  end

  @doc """
  Number of nodes that are running the daemon pod, but are not supposed to run the daemon pod.

  Instrument: `updowncounter`
  Unit: `{node}`
  ### Notes

  This metric aligns with the `numberMisscheduled` field of the
  [K8s DaemonSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#daemonsetstatus-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_daemonset_node_misscheduled()
      :"k8s.daemonset.node.misscheduled"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_NODE_MISSCHEDULED.
  'k8s.daemonset.node.misscheduled'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_daemonset_node_misscheduled :: :"k8s.daemonset.node.misscheduled"
  def k8s_daemonset_node_misscheduled do
    :"k8s.daemonset.node.misscheduled"
  end

  @doc """
  Number of nodes that should be running the daemon pod and have one or more of the daemon pod running and ready.

  Instrument: `updowncounter`
  Unit: `{node}`
  ### Notes

  This metric aligns with the `numberReady` field of the
  [K8s DaemonSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#daemonsetstatus-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_daemonset_node_ready()
      :"k8s.daemonset.node.ready"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_NODE_READY.
  'k8s.daemonset.node.ready'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_daemonset_node_ready :: :"k8s.daemonset.node.ready"
  def k8s_daemonset_node_ready do
    :"k8s.daemonset.node.ready"
  end

  @deprecated """
  Replaced by `k8s.daemonset.node.ready`.
  """

  @spec k8s_daemonset_ready_nodes :: :"k8s.daemonset.ready_nodes"
  def k8s_daemonset_ready_nodes do
    :"k8s.daemonset.ready_nodes"
  end

  @deprecated """
  Replaced by `k8s.deployment.pod.available`.
  """

  @spec k8s_deployment_available_pods :: :"k8s.deployment.available_pods"
  def k8s_deployment_available_pods do
    :"k8s.deployment.available_pods"
  end

  @deprecated """
  Replaced by `k8s.deployment.pod.desired`.
  """

  @spec k8s_deployment_desired_pods :: :"k8s.deployment.desired_pods"
  def k8s_deployment_desired_pods do
    :"k8s.deployment.desired_pods"
  end

  @doc """
  Total number of available replica pods (ready for at least minReadySeconds) targeted by this deployment.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `availableReplicas` field of the
  [K8s DeploymentStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#deploymentstatus-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_deployment_pod_available()
      :"k8s.deployment.pod.available"

  ### Erlang

  ```erlang
  ?K8S_DEPLOYMENT_POD_AVAILABLE.
  'k8s.deployment.pod.available'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_deployment_pod_available :: :"k8s.deployment.pod.available"
  def k8s_deployment_pod_available do
    :"k8s.deployment.pod.available"
  end

  @doc """
  Number of desired replica pods in this deployment.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `replicas` field of the
  [K8s DeploymentSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#deploymentspec-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_deployment_pod_desired()
      :"k8s.deployment.pod.desired"

  ### Erlang

  ```erlang
  ?K8S_DEPLOYMENT_POD_DESIRED.
  'k8s.deployment.pod.desired'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_deployment_pod_desired :: :"k8s.deployment.pod.desired"
  def k8s_deployment_pod_desired do
    :"k8s.deployment.pod.desired"
  end

  @deprecated """
  Replaced by `k8s.hpa.pod.current`.
  """

  @spec k8s_hpa_current_pods :: :"k8s.hpa.current_pods"
  def k8s_hpa_current_pods do
    :"k8s.hpa.current_pods"
  end

  @deprecated """
  Replaced by `k8s.hpa.pod.desired`.
  """

  @spec k8s_hpa_desired_pods :: :"k8s.hpa.desired_pods"
  def k8s_hpa_desired_pods do
    :"k8s.hpa.desired_pods"
  end

  @deprecated """
  Replaced by `k8s.hpa.pod.max`.
  """

  @spec k8s_hpa_max_pods :: :"k8s.hpa.max_pods"
  def k8s_hpa_max_pods do
    :"k8s.hpa.max_pods"
  end

  @doc """
  Target average utilization, in percentage, for CPU resource in HPA config.

  Instrument: `gauge`
  Unit: `1`
  ### Notes

  This metric aligns with the `averageUtilization` field of the
  [K8s HPA MetricTarget](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#metrictarget-v2-autoscaling).
  If the type of the metric is [`ContainerResource`](https://kubernetes.io/docs/tasks/run-application/horizontal-pod-autoscale/#support-for-metrics-apis),
  the `k8s.container.name` attribute **MUST** be set to identify the specific container within the pod to which the metric applies.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_metric_target_cpu_average_utilization()
      :"k8s.hpa.metric.target.cpu.average_utilization"

  ### Erlang

  ```erlang
  ?K8S_HPA_METRIC_TARGET_CPU_AVERAGE_UTILIZATION.
  'k8s.hpa.metric.target.cpu.average_utilization'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_metric_target_cpu_average_utilization ::
          :"k8s.hpa.metric.target.cpu.average_utilization"
  def k8s_hpa_metric_target_cpu_average_utilization do
    :"k8s.hpa.metric.target.cpu.average_utilization"
  end

  @doc """
  Target average value for CPU resource in HPA config.

  Instrument: `gauge`
  Unit: `{cpu}`
  ### Notes

  This metric aligns with the `averageValue` field of the
  [K8s HPA MetricTarget](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#metrictarget-v2-autoscaling).
  If the type of the metric is [`ContainerResource`](https://kubernetes.io/docs/tasks/run-application/horizontal-pod-autoscale/#support-for-metrics-apis),
  the `k8s.container.name` attribute **MUST** be set to identify the specific container within the pod to which the metric applies.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_metric_target_cpu_average_value()
      :"k8s.hpa.metric.target.cpu.average_value"

  ### Erlang

  ```erlang
  ?K8S_HPA_METRIC_TARGET_CPU_AVERAGE_VALUE.
  'k8s.hpa.metric.target.cpu.average_value'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_metric_target_cpu_average_value :: :"k8s.hpa.metric.target.cpu.average_value"
  def k8s_hpa_metric_target_cpu_average_value do
    :"k8s.hpa.metric.target.cpu.average_value"
  end

  @doc """
  Target value for CPU resource in HPA config.

  Instrument: `gauge`
  Unit: `{cpu}`
  ### Notes

  This metric aligns with the `value` field of the
  [K8s HPA MetricTarget](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#metrictarget-v2-autoscaling).
  If the type of the metric is [`ContainerResource`](https://kubernetes.io/docs/tasks/run-application/horizontal-pod-autoscale/#support-for-metrics-apis),
  the `k8s.container.name` attribute **MUST** be set to identify the specific container within the pod to which the metric applies.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_metric_target_cpu_value()
      :"k8s.hpa.metric.target.cpu.value"

  ### Erlang

  ```erlang
  ?K8S_HPA_METRIC_TARGET_CPU_VALUE.
  'k8s.hpa.metric.target.cpu.value'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_metric_target_cpu_value :: :"k8s.hpa.metric.target.cpu.value"
  def k8s_hpa_metric_target_cpu_value do
    :"k8s.hpa.metric.target.cpu.value"
  end

  @deprecated """
  Replaced by `k8s.hpa.pod.min`.
  """

  @spec k8s_hpa_min_pods :: :"k8s.hpa.min_pods"
  def k8s_hpa_min_pods do
    :"k8s.hpa.min_pods"
  end

  @doc """
  Current number of replica pods managed by this horizontal pod autoscaler, as last seen by the autoscaler.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `currentReplicas` field of the
  [K8s HorizontalPodAutoscalerStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#horizontalpodautoscalerstatus-v2-autoscaling)


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_pod_current()
      :"k8s.hpa.pod.current"

  ### Erlang

  ```erlang
  ?K8S_HPA_POD_CURRENT.
  'k8s.hpa.pod.current'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_pod_current :: :"k8s.hpa.pod.current"
  def k8s_hpa_pod_current do
    :"k8s.hpa.pod.current"
  end

  @doc """
  Desired number of replica pods managed by this horizontal pod autoscaler, as last calculated by the autoscaler.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `desiredReplicas` field of the
  [K8s HorizontalPodAutoscalerStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#horizontalpodautoscalerstatus-v2-autoscaling)


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_pod_desired()
      :"k8s.hpa.pod.desired"

  ### Erlang

  ```erlang
  ?K8S_HPA_POD_DESIRED.
  'k8s.hpa.pod.desired'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_pod_desired :: :"k8s.hpa.pod.desired"
  def k8s_hpa_pod_desired do
    :"k8s.hpa.pod.desired"
  end

  @doc """
  The upper limit for the number of replica pods to which the autoscaler can scale up.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `maxReplicas` field of the
  [K8s HorizontalPodAutoscalerSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#horizontalpodautoscalerspec-v2-autoscaling)


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_pod_max()
      :"k8s.hpa.pod.max"

  ### Erlang

  ```erlang
  ?K8S_HPA_POD_MAX.
  'k8s.hpa.pod.max'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_pod_max :: :"k8s.hpa.pod.max"
  def k8s_hpa_pod_max do
    :"k8s.hpa.pod.max"
  end

  @doc """
  The lower limit for the number of replica pods to which the autoscaler can scale down.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `minReplicas` field of the
  [K8s HorizontalPodAutoscalerSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#horizontalpodautoscalerspec-v2-autoscaling)


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_hpa_pod_min()
      :"k8s.hpa.pod.min"

  ### Erlang

  ```erlang
  ?K8S_HPA_POD_MIN.
  'k8s.hpa.pod.min'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_hpa_pod_min :: :"k8s.hpa.pod.min"
  def k8s_hpa_pod_min do
    :"k8s.hpa.pod.min"
  end

  @deprecated """
  Replaced by `k8s.job.pod.active`.
  """

  @spec k8s_job_active_pods :: :"k8s.job.active_pods"
  def k8s_job_active_pods do
    :"k8s.job.active_pods"
  end

  @deprecated """
  Replaced by `k8s.job.pod.desired_successful`.
  """

  @spec k8s_job_desired_successful_pods :: :"k8s.job.desired_successful_pods"
  def k8s_job_desired_successful_pods do
    :"k8s.job.desired_successful_pods"
  end

  @deprecated """
  Replaced by `k8s.job.pod.failed`.
  """

  @spec k8s_job_failed_pods :: :"k8s.job.failed_pods"
  def k8s_job_failed_pods do
    :"k8s.job.failed_pods"
  end

  @deprecated """
  Replaced by `k8s.job.pod.max_parallel`.
  """

  @spec k8s_job_max_parallel_pods :: :"k8s.job.max_parallel_pods"
  def k8s_job_max_parallel_pods do
    :"k8s.job.max_parallel_pods"
  end

  @doc """
  The number of pending and actively running pods for a job.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `active` field of the
  [K8s JobStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#jobstatus-v1-batch).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_job_pod_active()
      :"k8s.job.pod.active"

  ### Erlang

  ```erlang
  ?K8S_JOB_POD_ACTIVE.
  'k8s.job.pod.active'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_job_pod_active :: :"k8s.job.pod.active"
  def k8s_job_pod_active do
    :"k8s.job.pod.active"
  end

  @doc """
  The desired number of successfully finished pods the job should be run with.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `completions` field of the
  [K8s JobSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#jobspec-v1-batch)..


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_job_pod_desired_successful()
      :"k8s.job.pod.desired_successful"

  ### Erlang

  ```erlang
  ?K8S_JOB_POD_DESIRED_SUCCESSFUL.
  'k8s.job.pod.desired_successful'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_job_pod_desired_successful :: :"k8s.job.pod.desired_successful"
  def k8s_job_pod_desired_successful do
    :"k8s.job.pod.desired_successful"
  end

  @doc """
  The number of pods which reached phase Failed for a job.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `failed` field of the
  [K8s JobStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#jobstatus-v1-batch).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_job_pod_failed()
      :"k8s.job.pod.failed"

  ### Erlang

  ```erlang
  ?K8S_JOB_POD_FAILED.
  'k8s.job.pod.failed'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_job_pod_failed :: :"k8s.job.pod.failed"
  def k8s_job_pod_failed do
    :"k8s.job.pod.failed"
  end

  @doc """
  The max desired number of pods the job should run at any given time.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `parallelism` field of the
  [K8s JobSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#jobspec-v1-batch).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_job_pod_max_parallel()
      :"k8s.job.pod.max_parallel"

  ### Erlang

  ```erlang
  ?K8S_JOB_POD_MAX_PARALLEL.
  'k8s.job.pod.max_parallel'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_job_pod_max_parallel :: :"k8s.job.pod.max_parallel"
  def k8s_job_pod_max_parallel do
    :"k8s.job.pod.max_parallel"
  end

  @doc """
  The number of pods which reached phase Succeeded for a job.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `succeeded` field of the
  [K8s JobStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#jobstatus-v1-batch).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_job_pod_successful()
      :"k8s.job.pod.successful"

  ### Erlang

  ```erlang
  ?K8S_JOB_POD_SUCCESSFUL.
  'k8s.job.pod.successful'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_job_pod_successful :: :"k8s.job.pod.successful"
  def k8s_job_pod_successful do
    :"k8s.job.pod.successful"
  end

  @deprecated """
  Replaced by `k8s.job.pod.successful`.
  """

  @spec k8s_job_successful_pods :: :"k8s.job.successful_pods"
  def k8s_job_successful_pods do
    :"k8s.job.successful_pods"
  end

  @doc """
  Describes number of K8s namespaces that are currently in a given phase.

  Instrument: `updowncounter`
  Unit: `{namespace}`

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

  @deprecated """
  Replaced by `k8s.node.cpu.allocatable`.
  """

  @spec k8s_node_allocatable_cpu :: :"k8s.node.allocatable.cpu"
  def k8s_node_allocatable_cpu do
    :"k8s.node.allocatable.cpu"
  end

  @deprecated """
  Replaced by `k8s.node.ephemeral_storage.allocatable`.
  """

  @spec k8s_node_allocatable_ephemeral_storage :: :"k8s.node.allocatable.ephemeral_storage"
  def k8s_node_allocatable_ephemeral_storage do
    :"k8s.node.allocatable.ephemeral_storage"
  end

  @deprecated """
  Replaced by `k8s.node.memory.allocatable`.
  """

  @spec k8s_node_allocatable_memory :: :"k8s.node.allocatable.memory"
  def k8s_node_allocatable_memory do
    :"k8s.node.allocatable.memory"
  end

  @deprecated """
  Replaced by `k8s.node.pod.allocatable`.
  """

  @spec k8s_node_allocatable_pods :: :"k8s.node.allocatable.pods"
  def k8s_node_allocatable_pods do
    :"k8s.node.allocatable.pods"
  end

  @doc """
  Describes the condition of a particular Node.

  Instrument: `updowncounter`
  Unit: `{node}`
  ### Notes

  All possible node condition pairs (type and status) will be reported at each time interval to avoid missing metrics. Condition pairs corresponding to the current conditions' statuses will be non-zero.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_condition_status()
      :"k8s.node.condition.status"

  ### Erlang

  ```erlang
  ?K8S_NODE_CONDITION_STATUS.
  'k8s.node.condition.status'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_condition_status :: :"k8s.node.condition.status"
  def k8s_node_condition_status do
    :"k8s.node.condition.status"
  end

  @doc """
  Amount of cpu allocatable on the node.

  Instrument: `updowncounter`
  Unit: `{cpu}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_cpu_allocatable()
      :"k8s.node.cpu.allocatable"

  ### Erlang

  ```erlang
  ?K8S_NODE_CPU_ALLOCATABLE.
  'k8s.node.cpu.allocatable'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_cpu_allocatable :: :"k8s.node.cpu.allocatable"
  def k8s_node_cpu_allocatable do
    :"k8s.node.cpu.allocatable"
  end

  @doc """
  Total CPU time consumed.

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
  Node's CPU usage, measured in cpus. Range from 0 to the number of allocatable CPUs.

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
  Amount of ephemeral-storage allocatable on the node.

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_ephemeral_storage_allocatable()
      :"k8s.node.ephemeral_storage.allocatable"

  ### Erlang

  ```erlang
  ?K8S_NODE_EPHEMERAL_STORAGE_ALLOCATABLE.
  'k8s.node.ephemeral_storage.allocatable'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_ephemeral_storage_allocatable :: :"k8s.node.ephemeral_storage.allocatable"
  def k8s_node_ephemeral_storage_allocatable do
    :"k8s.node.ephemeral_storage.allocatable"
  end

  @doc """
  Node filesystem available bytes.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is derived from the
  [FsStats.AvailableBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#FsStats) field
  of the [NodeStats.Fs](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#NodeStats)
  of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_filesystem_available()
      :"k8s.node.filesystem.available"

  ### Erlang

  ```erlang
  ?K8S_NODE_FILESYSTEM_AVAILABLE.
  'k8s.node.filesystem.available'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_filesystem_available :: :"k8s.node.filesystem.available"
  def k8s_node_filesystem_available do
    :"k8s.node.filesystem.available"
  end

  @doc """
  Node filesystem capacity.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is derived from the
  [FsStats.CapacityBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#FsStats) field
  of the [NodeStats.Fs](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#NodeStats)
  of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_filesystem_capacity()
      :"k8s.node.filesystem.capacity"

  ### Erlang

  ```erlang
  ?K8S_NODE_FILESYSTEM_CAPACITY.
  'k8s.node.filesystem.capacity'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_filesystem_capacity :: :"k8s.node.filesystem.capacity"
  def k8s_node_filesystem_capacity do
    :"k8s.node.filesystem.capacity"
  end

  @doc """
  Node filesystem usage.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This may not equal capacity - available.

  This metric is derived from the
  [FsStats.UsedBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#FsStats) field
  of the [NodeStats.Fs](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#NodeStats)
  of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_filesystem_usage()
      :"k8s.node.filesystem.usage"

  ### Erlang

  ```erlang
  ?K8S_NODE_FILESYSTEM_USAGE.
  'k8s.node.filesystem.usage'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_filesystem_usage :: :"k8s.node.filesystem.usage"
  def k8s_node_filesystem_usage do
    :"k8s.node.filesystem.usage"
  end

  @doc """
  Amount of memory allocatable on the node.

  Instrument: `updowncounter`
  Unit: `By`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_memory_allocatable()
      :"k8s.node.memory.allocatable"

  ### Erlang

  ```erlang
  ?K8S_NODE_MEMORY_ALLOCATABLE.
  'k8s.node.memory.allocatable'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_memory_allocatable :: :"k8s.node.memory.allocatable"
  def k8s_node_memory_allocatable do
    :"k8s.node.memory.allocatable"
  end

  @doc """
  Node memory available.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  Available memory for use.  This is defined as the memory limit - workingSetBytes. If memory limit is undefined, the available bytes is omitted.
  This metric is derived from the [MemoryStats.AvailableBytes](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [NodeStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#NodeStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_memory_available()
      :"k8s.node.memory.available"

  ### Erlang

  ```erlang
  ?K8S_NODE_MEMORY_AVAILABLE.
  'k8s.node.memory.available'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_memory_available :: :"k8s.node.memory.available"
  def k8s_node_memory_available do
    :"k8s.node.memory.available"
  end

  @doc """
  Node memory paging faults.

  Instrument: `counter`
  Unit: `{fault}`
  ### Notes

  Cumulative number of major/minor page faults.
  This metric is derived from the [MemoryStats.PageFaults](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) and [MemoryStats.MajorPageFaults](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) fields of the [NodeStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#NodeStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_memory_paging_faults()
      :"k8s.node.memory.paging.faults"

  ### Erlang

  ```erlang
  ?K8S_NODE_MEMORY_PAGING_FAULTS.
  'k8s.node.memory.paging.faults'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_memory_paging_faults :: :"k8s.node.memory.paging.faults"
  def k8s_node_memory_paging_faults do
    :"k8s.node.memory.paging.faults"
  end

  @doc """
  Node memory RSS.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  The amount of anonymous and swap cache memory (includes transparent hugepages).
  This metric is derived from the [MemoryStats.RSSBytes](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [NodeStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#NodeStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_memory_rss()
      :"k8s.node.memory.rss"

  ### Erlang

  ```erlang
  ?K8S_NODE_MEMORY_RSS.
  'k8s.node.memory.rss'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_memory_rss :: :"k8s.node.memory.rss"
  def k8s_node_memory_rss do
    :"k8s.node.memory.rss"
  end

  @doc """
  Memory usage of the Node.

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
  Node memory working set.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  The amount of working set memory. This includes recently accessed memory, dirty memory, and kernel memory. WorkingSetBytes is <= UsageBytes.
  This metric is derived from the [MemoryStats.WorkingSetBytes](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [NodeStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#NodeStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_memory_working_set()
      :"k8s.node.memory.working_set"

  ### Erlang

  ```erlang
  ?K8S_NODE_MEMORY_WORKING_SET.
  'k8s.node.memory.working_set'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_memory_working_set :: :"k8s.node.memory.working_set"
  def k8s_node_memory_working_set do
    :"k8s.node.memory.working_set"
  end

  @doc """
  Node network errors.

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
  Network bytes for the Node.

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
  Amount of pods allocatable on the node.

  Instrument: `updowncounter`
  Unit: `{pod}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_node_pod_allocatable()
      :"k8s.node.pod.allocatable"

  ### Erlang

  ```erlang
  ?K8S_NODE_POD_ALLOCATABLE.
  'k8s.node.pod.allocatable'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_node_pod_allocatable :: :"k8s.node.pod.allocatable"
  def k8s_node_pod_allocatable do
    :"k8s.node.pod.allocatable"
  end

  @doc """
  The time the Node has been running.

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
  Total CPU time consumed.

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
  Pod's CPU usage, measured in cpus. Range from 0 to the number of allocatable CPUs.

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
  Pod filesystem available bytes.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is derived from the
  [FsStats.AvailableBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#FsStats) field
  of the [PodStats.EphemeralStorage](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#PodStats)
  of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_filesystem_available()
      :"k8s.pod.filesystem.available"

  ### Erlang

  ```erlang
  ?K8S_POD_FILESYSTEM_AVAILABLE.
  'k8s.pod.filesystem.available'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_filesystem_available :: :"k8s.pod.filesystem.available"
  def k8s_pod_filesystem_available do
    :"k8s.pod.filesystem.available"
  end

  @doc """
  Pod filesystem capacity.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is derived from the
  [FsStats.CapacityBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#FsStats) field
  of the [PodStats.EphemeralStorage](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#PodStats)
  of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_filesystem_capacity()
      :"k8s.pod.filesystem.capacity"

  ### Erlang

  ```erlang
  ?K8S_POD_FILESYSTEM_CAPACITY.
  'k8s.pod.filesystem.capacity'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_filesystem_capacity :: :"k8s.pod.filesystem.capacity"
  def k8s_pod_filesystem_capacity do
    :"k8s.pod.filesystem.capacity"
  end

  @doc """
  Pod filesystem usage.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This may not equal capacity - available.

  This metric is derived from the
  [FsStats.UsedBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#FsStats) field
  of the [PodStats.EphemeralStorage](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#PodStats)
  of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_filesystem_usage()
      :"k8s.pod.filesystem.usage"

  ### Erlang

  ```erlang
  ?K8S_POD_FILESYSTEM_USAGE.
  'k8s.pod.filesystem.usage'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_filesystem_usage :: :"k8s.pod.filesystem.usage"
  def k8s_pod_filesystem_usage do
    :"k8s.pod.filesystem.usage"
  end

  @doc """
  Pod memory available.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  Available memory for use.  This is defined as the memory limit - workingSetBytes. If memory limit is undefined, the available bytes is omitted.
  This metric is derived from the [MemoryStats.AvailableBytes](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [PodStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#PodStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_memory_available()
      :"k8s.pod.memory.available"

  ### Erlang

  ```erlang
  ?K8S_POD_MEMORY_AVAILABLE.
  'k8s.pod.memory.available'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_memory_available :: :"k8s.pod.memory.available"
  def k8s_pod_memory_available do
    :"k8s.pod.memory.available"
  end

  @doc """
  Pod memory paging faults.

  Instrument: `counter`
  Unit: `{fault}`
  ### Notes

  Cumulative number of major/minor page faults.
  This metric is derived from the [MemoryStats.PageFaults](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) and [MemoryStats.MajorPageFaults](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [PodStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#PodStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_memory_paging_faults()
      :"k8s.pod.memory.paging.faults"

  ### Erlang

  ```erlang
  ?K8S_POD_MEMORY_PAGING_FAULTS.
  'k8s.pod.memory.paging.faults'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_memory_paging_faults :: :"k8s.pod.memory.paging.faults"
  def k8s_pod_memory_paging_faults do
    :"k8s.pod.memory.paging.faults"
  end

  @doc """
  Pod memory RSS.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  The amount of anonymous and swap cache memory (includes transparent hugepages).
  This metric is derived from the [MemoryStats.RSSBytes](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [PodStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#PodStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_memory_rss()
      :"k8s.pod.memory.rss"

  ### Erlang

  ```erlang
  ?K8S_POD_MEMORY_RSS.
  'k8s.pod.memory.rss'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_memory_rss :: :"k8s.pod.memory.rss"
  def k8s_pod_memory_rss do
    :"k8s.pod.memory.rss"
  end

  @doc """
  Memory usage of the Pod.

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
  Pod memory working set.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  The amount of working set memory. This includes recently accessed memory, dirty memory, and kernel memory. WorkingSetBytes is <= UsageBytes.
  This metric is derived from the [MemoryStats.WorkingSetBytes](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#MemoryStats) field of the [PodStats.Memory](https://pkg.go.dev/k8s.io/kubelet@v0.34.0/pkg/apis/stats/v1alpha1#PodStats) of the Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_memory_working_set()
      :"k8s.pod.memory.working_set"

  ### Erlang

  ```erlang
  ?K8S_POD_MEMORY_WORKING_SET.
  'k8s.pod.memory.working_set'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_memory_working_set :: :"k8s.pod.memory.working_set"
  def k8s_pod_memory_working_set do
    :"k8s.pod.memory.working_set"
  end

  @doc """
  Pod network errors.

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
  Network bytes for the Pod.

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
  Describes number of K8s Pods that are currently in a given phase.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  All possible pod phases will be reported at each time interval to avoid missing metrics.
  Only the value corresponding to the current phase will be non-zero.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_status_phase()
      :"k8s.pod.status.phase"

  ### Erlang

  ```erlang
  ?K8S_POD_STATUS_PHASE.
  'k8s.pod.status.phase'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_status_phase :: :"k8s.pod.status.phase"
  def k8s_pod_status_phase do
    :"k8s.pod.status.phase"
  end

  @doc """
  Describes the number of K8s Pods that are currently in a state for a given reason.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  All possible pod status reasons will be reported at each time interval to avoid missing metrics.
  Only the value corresponding to the current reason will be non-zero.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_status_reason()
      :"k8s.pod.status.reason"

  ### Erlang

  ```erlang
  ?K8S_POD_STATUS_REASON.
  'k8s.pod.status.reason'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_status_reason :: :"k8s.pod.status.reason"
  def k8s_pod_status_reason do
    :"k8s.pod.status.reason"
  end

  @doc """
  The time the Pod has been running.

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
  Pod volume storage space available.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is derived from the
  [VolumeStats.AvailableBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#VolumeStats) field
  of the [PodStats](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#PodStats) of the
  Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_volume_available()
      :"k8s.pod.volume.available"

  ### Erlang

  ```erlang
  ?K8S_POD_VOLUME_AVAILABLE.
  'k8s.pod.volume.available'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_volume_available :: :"k8s.pod.volume.available"
  def k8s_pod_volume_available do
    :"k8s.pod.volume.available"
  end

  @doc """
  Pod volume total capacity.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is derived from the
  [VolumeStats.CapacityBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#VolumeStats) field
  of the [PodStats](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#PodStats) of the
  Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_volume_capacity()
      :"k8s.pod.volume.capacity"

  ### Erlang

  ```erlang
  ?K8S_POD_VOLUME_CAPACITY.
  'k8s.pod.volume.capacity'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_volume_capacity :: :"k8s.pod.volume.capacity"
  def k8s_pod_volume_capacity do
    :"k8s.pod.volume.capacity"
  end

  @doc """
  The total inodes in the filesystem of the Pod's volume.

  Instrument: `updowncounter`
  Unit: `{inode}`
  ### Notes

  This metric is derived from the
  [VolumeStats.Inodes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#VolumeStats) field
  of the [PodStats](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#PodStats) of the
  Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_volume_inode_count()
      :"k8s.pod.volume.inode.count"

  ### Erlang

  ```erlang
  ?K8S_POD_VOLUME_INODE_COUNT.
  'k8s.pod.volume.inode.count'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_volume_inode_count :: :"k8s.pod.volume.inode.count"
  def k8s_pod_volume_inode_count do
    :"k8s.pod.volume.inode.count"
  end

  @doc """
  The free inodes in the filesystem of the Pod's volume.

  Instrument: `updowncounter`
  Unit: `{inode}`
  ### Notes

  This metric is derived from the
  [VolumeStats.InodesFree](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#VolumeStats) field
  of the [PodStats](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#PodStats) of the
  Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_volume_inode_free()
      :"k8s.pod.volume.inode.free"

  ### Erlang

  ```erlang
  ?K8S_POD_VOLUME_INODE_FREE.
  'k8s.pod.volume.inode.free'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_volume_inode_free :: :"k8s.pod.volume.inode.free"
  def k8s_pod_volume_inode_free do
    :"k8s.pod.volume.inode.free"
  end

  @doc """
  The inodes used by the filesystem of the Pod's volume.

  Instrument: `updowncounter`
  Unit: `{inode}`
  ### Notes

  This metric is derived from the
  [VolumeStats.InodesUsed](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#VolumeStats) field
  of the [PodStats](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#PodStats) of the
  Kubelet's stats API.

  This may not be equal to `inodes - free` because filesystem may share inodes with other filesystems.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_volume_inode_used()
      :"k8s.pod.volume.inode.used"

  ### Erlang

  ```erlang
  ?K8S_POD_VOLUME_INODE_USED.
  'k8s.pod.volume.inode.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_volume_inode_used :: :"k8s.pod.volume.inode.used"
  def k8s_pod_volume_inode_used do
    :"k8s.pod.volume.inode.used"
  end

  @doc """
  Pod volume usage.

  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This may not equal capacity - available.

  This metric is derived from the
  [VolumeStats.UsedBytes](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#VolumeStats) field
  of the [PodStats](https://pkg.go.dev/k8s.io/kubelet@v0.33.0/pkg/apis/stats/v1alpha1#PodStats) of the
  Kubelet's stats API.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_pod_volume_usage()
      :"k8s.pod.volume.usage"

  ### Erlang

  ```erlang
  ?K8S_POD_VOLUME_USAGE.
  'k8s.pod.volume.usage'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_pod_volume_usage :: :"k8s.pod.volume.usage"
  def k8s_pod_volume_usage do
    :"k8s.pod.volume.usage"
  end

  @deprecated """
  Replaced by `k8s.replicaset.pod.available`.
  """

  @spec k8s_replicaset_available_pods :: :"k8s.replicaset.available_pods"
  def k8s_replicaset_available_pods do
    :"k8s.replicaset.available_pods"
  end

  @deprecated """
  Replaced by `k8s.replicaset.pod.desired`.
  """

  @spec k8s_replicaset_desired_pods :: :"k8s.replicaset.desired_pods"
  def k8s_replicaset_desired_pods do
    :"k8s.replicaset.desired_pods"
  end

  @doc """
  Total number of available replica pods (ready for at least minReadySeconds) targeted by this replicaset.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `availableReplicas` field of the
  [K8s ReplicaSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#replicasetstatus-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_replicaset_pod_available()
      :"k8s.replicaset.pod.available"

  ### Erlang

  ```erlang
  ?K8S_REPLICASET_POD_AVAILABLE.
  'k8s.replicaset.pod.available'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_replicaset_pod_available :: :"k8s.replicaset.pod.available"
  def k8s_replicaset_pod_available do
    :"k8s.replicaset.pod.available"
  end

  @doc """
  Number of desired replica pods in this replicaset.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `replicas` field of the
  [K8s ReplicaSetSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#replicasetspec-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_replicaset_pod_desired()
      :"k8s.replicaset.pod.desired"

  ### Erlang

  ```erlang
  ?K8S_REPLICASET_POD_DESIRED.
  'k8s.replicaset.pod.desired'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_replicaset_pod_desired :: :"k8s.replicaset.pod.desired"
  def k8s_replicaset_pod_desired do
    :"k8s.replicaset.pod.desired"
  end

  @deprecated """
  Replaced by `k8s.replicationcontroller.pod.available`.
  """

  @spec k8s_replication_controller_available_pods :: :"k8s.replication_controller.available_pods"
  def k8s_replication_controller_available_pods do
    :"k8s.replication_controller.available_pods"
  end

  @deprecated """
  Replaced by `k8s.replicationcontroller.pod.desired`.
  """

  @spec k8s_replication_controller_desired_pods :: :"k8s.replication_controller.desired_pods"
  def k8s_replication_controller_desired_pods do
    :"k8s.replication_controller.desired_pods"
  end

  @deprecated """
  Replaced by `k8s.replicationcontroller.pod.available`.
  """

  @spec k8s_replicationcontroller_available_pods :: :"k8s.replicationcontroller.available_pods"
  def k8s_replicationcontroller_available_pods do
    :"k8s.replicationcontroller.available_pods"
  end

  @deprecated """
  Replaced by `k8s.replicationcontroller.pod.desired`.
  """

  @spec k8s_replicationcontroller_desired_pods :: :"k8s.replicationcontroller.desired_pods"
  def k8s_replicationcontroller_desired_pods do
    :"k8s.replicationcontroller.desired_pods"
  end

  @doc """
  Total number of available replica pods (ready for at least minReadySeconds) targeted by this replication controller.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `availableReplicas` field of the
  [K8s ReplicationControllerStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#replicationcontrollerstatus-v1-core)


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_replicationcontroller_pod_available()
      :"k8s.replicationcontroller.pod.available"

  ### Erlang

  ```erlang
  ?K8S_REPLICATIONCONTROLLER_POD_AVAILABLE.
  'k8s.replicationcontroller.pod.available'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_replicationcontroller_pod_available :: :"k8s.replicationcontroller.pod.available"
  def k8s_replicationcontroller_pod_available do
    :"k8s.replicationcontroller.pod.available"
  end

  @doc """
  Number of desired replica pods in this replication controller.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `replicas` field of the
  [K8s ReplicationControllerSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#replicationcontrollerspec-v1-core)


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_replicationcontroller_pod_desired()
      :"k8s.replicationcontroller.pod.desired"

  ### Erlang

  ```erlang
  ?K8S_REPLICATIONCONTROLLER_POD_DESIRED.
  'k8s.replicationcontroller.pod.desired'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_replicationcontroller_pod_desired :: :"k8s.replicationcontroller.pod.desired"
  def k8s_replicationcontroller_pod_desired do
    :"k8s.replicationcontroller.pod.desired"
  end

  @doc """
  The CPU limits in a specific namespace.
  The value represents the configured quota limit of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `{cpu}`
  ### Notes

  This metric is retrieved from the `hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_cpu_limit_hard()
      :"k8s.resourcequota.cpu.limit.hard"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_CPU_LIMIT_HARD.
  'k8s.resourcequota.cpu.limit.hard'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_cpu_limit_hard :: :"k8s.resourcequota.cpu.limit.hard"
  def k8s_resourcequota_cpu_limit_hard do
    :"k8s.resourcequota.cpu.limit.hard"
  end

  @doc """
  The CPU limits in a specific namespace.
  The value represents the current observed total usage of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `{cpu}`
  ### Notes

  This metric is retrieved from the `used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_cpu_limit_used()
      :"k8s.resourcequota.cpu.limit.used"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_CPU_LIMIT_USED.
  'k8s.resourcequota.cpu.limit.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_cpu_limit_used :: :"k8s.resourcequota.cpu.limit.used"
  def k8s_resourcequota_cpu_limit_used do
    :"k8s.resourcequota.cpu.limit.used"
  end

  @doc """
  The CPU requests in a specific namespace.
  The value represents the configured quota limit of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `{cpu}`
  ### Notes

  This metric is retrieved from the `hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_cpu_request_hard()
      :"k8s.resourcequota.cpu.request.hard"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_CPU_REQUEST_HARD.
  'k8s.resourcequota.cpu.request.hard'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_cpu_request_hard :: :"k8s.resourcequota.cpu.request.hard"
  def k8s_resourcequota_cpu_request_hard do
    :"k8s.resourcequota.cpu.request.hard"
  end

  @doc """
  The CPU requests in a specific namespace.
  The value represents the current observed total usage of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `{cpu}`
  ### Notes

  This metric is retrieved from the `used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_cpu_request_used()
      :"k8s.resourcequota.cpu.request.used"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_CPU_REQUEST_USED.
  'k8s.resourcequota.cpu.request.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_cpu_request_used :: :"k8s.resourcequota.cpu.request.used"
  def k8s_resourcequota_cpu_request_used do
    :"k8s.resourcequota.cpu.request.used"
  end

  @doc """
  The sum of local ephemeral storage limits in the namespace.
  The value represents the configured quota limit of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_ephemeral_storage_limit_hard()
      :"k8s.resourcequota.ephemeral_storage.limit.hard"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_EPHEMERAL_STORAGE_LIMIT_HARD.
  'k8s.resourcequota.ephemeral_storage.limit.hard'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_ephemeral_storage_limit_hard ::
          :"k8s.resourcequota.ephemeral_storage.limit.hard"
  def k8s_resourcequota_ephemeral_storage_limit_hard do
    :"k8s.resourcequota.ephemeral_storage.limit.hard"
  end

  @doc """
  The sum of local ephemeral storage limits in the namespace.
  The value represents the current observed total usage of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_ephemeral_storage_limit_used()
      :"k8s.resourcequota.ephemeral_storage.limit.used"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_EPHEMERAL_STORAGE_LIMIT_USED.
  'k8s.resourcequota.ephemeral_storage.limit.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_ephemeral_storage_limit_used ::
          :"k8s.resourcequota.ephemeral_storage.limit.used"
  def k8s_resourcequota_ephemeral_storage_limit_used do
    :"k8s.resourcequota.ephemeral_storage.limit.used"
  end

  @doc """
  The sum of local ephemeral storage requests in the namespace.
  The value represents the configured quota limit of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_ephemeral_storage_request_hard()
      :"k8s.resourcequota.ephemeral_storage.request.hard"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_EPHEMERAL_STORAGE_REQUEST_HARD.
  'k8s.resourcequota.ephemeral_storage.request.hard'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_ephemeral_storage_request_hard ::
          :"k8s.resourcequota.ephemeral_storage.request.hard"
  def k8s_resourcequota_ephemeral_storage_request_hard do
    :"k8s.resourcequota.ephemeral_storage.request.hard"
  end

  @doc """
  The sum of local ephemeral storage requests in the namespace.
  The value represents the current observed total usage of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_ephemeral_storage_request_used()
      :"k8s.resourcequota.ephemeral_storage.request.used"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_EPHEMERAL_STORAGE_REQUEST_USED.
  'k8s.resourcequota.ephemeral_storage.request.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_ephemeral_storage_request_used ::
          :"k8s.resourcequota.ephemeral_storage.request.used"
  def k8s_resourcequota_ephemeral_storage_request_used do
    :"k8s.resourcequota.ephemeral_storage.request.used"
  end

  @doc """
  The huge page requests in a specific namespace.
  The value represents the configured quota limit of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `{hugepage}`
  ### Notes

  This metric is retrieved from the `hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_hugepage_count_request_hard()
      :"k8s.resourcequota.hugepage_count.request.hard"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_HUGEPAGE_COUNT_REQUEST_HARD.
  'k8s.resourcequota.hugepage_count.request.hard'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_hugepage_count_request_hard ::
          :"k8s.resourcequota.hugepage_count.request.hard"
  def k8s_resourcequota_hugepage_count_request_hard do
    :"k8s.resourcequota.hugepage_count.request.hard"
  end

  @doc """
  The huge page requests in a specific namespace.
  The value represents the current observed total usage of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `{hugepage}`
  ### Notes

  This metric is retrieved from the `used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_hugepage_count_request_used()
      :"k8s.resourcequota.hugepage_count.request.used"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_HUGEPAGE_COUNT_REQUEST_USED.
  'k8s.resourcequota.hugepage_count.request.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_hugepage_count_request_used ::
          :"k8s.resourcequota.hugepage_count.request.used"
  def k8s_resourcequota_hugepage_count_request_used do
    :"k8s.resourcequota.hugepage_count.request.used"
  end

  @doc """
  The memory limits in a specific namespace.
  The value represents the configured quota limit of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_memory_limit_hard()
      :"k8s.resourcequota.memory.limit.hard"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_MEMORY_LIMIT_HARD.
  'k8s.resourcequota.memory.limit.hard'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_memory_limit_hard :: :"k8s.resourcequota.memory.limit.hard"
  def k8s_resourcequota_memory_limit_hard do
    :"k8s.resourcequota.memory.limit.hard"
  end

  @doc """
  The memory limits in a specific namespace.
  The value represents the current observed total usage of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_memory_limit_used()
      :"k8s.resourcequota.memory.limit.used"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_MEMORY_LIMIT_USED.
  'k8s.resourcequota.memory.limit.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_memory_limit_used :: :"k8s.resourcequota.memory.limit.used"
  def k8s_resourcequota_memory_limit_used do
    :"k8s.resourcequota.memory.limit.used"
  end

  @doc """
  The memory requests in a specific namespace.
  The value represents the configured quota limit of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_memory_request_hard()
      :"k8s.resourcequota.memory.request.hard"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_MEMORY_REQUEST_HARD.
  'k8s.resourcequota.memory.request.hard'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_memory_request_hard :: :"k8s.resourcequota.memory.request.hard"
  def k8s_resourcequota_memory_request_hard do
    :"k8s.resourcequota.memory.request.hard"
  end

  @doc """
  The memory requests in a specific namespace.
  The value represents the current observed total usage of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_memory_request_used()
      :"k8s.resourcequota.memory.request.used"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_MEMORY_REQUEST_USED.
  'k8s.resourcequota.memory.request.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_memory_request_used :: :"k8s.resourcequota.memory.request.used"
  def k8s_resourcequota_memory_request_used do
    :"k8s.resourcequota.memory.request.used"
  end

  @doc """
  The object count limits in a specific namespace.
  The value represents the configured quota limit of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `{object}`
  ### Notes

  This metric is retrieved from the `hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_object_count_hard()
      :"k8s.resourcequota.object_count.hard"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_OBJECT_COUNT_HARD.
  'k8s.resourcequota.object_count.hard'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_object_count_hard :: :"k8s.resourcequota.object_count.hard"
  def k8s_resourcequota_object_count_hard do
    :"k8s.resourcequota.object_count.hard"
  end

  @doc """
  The object count limits in a specific namespace.
  The value represents the current observed total usage of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `{object}`
  ### Notes

  This metric is retrieved from the `used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_object_count_used()
      :"k8s.resourcequota.object_count.used"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_OBJECT_COUNT_USED.
  'k8s.resourcequota.object_count.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_object_count_used :: :"k8s.resourcequota.object_count.used"
  def k8s_resourcequota_object_count_used do
    :"k8s.resourcequota.object_count.used"
  end

  @doc """
  The total number of PersistentVolumeClaims that can exist in the namespace.
  The value represents the configured quota limit of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `{persistentvolumeclaim}`
  ### Notes

  This metric is retrieved from the `hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).

  The `k8s.storageclass.name` should be required when a resource quota is defined for a specific
  storage class.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_persistentvolumeclaim_count_hard()
      :"k8s.resourcequota.persistentvolumeclaim_count.hard"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_PERSISTENTVOLUMECLAIM_COUNT_HARD.
  'k8s.resourcequota.persistentvolumeclaim_count.hard'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_persistentvolumeclaim_count_hard ::
          :"k8s.resourcequota.persistentvolumeclaim_count.hard"
  def k8s_resourcequota_persistentvolumeclaim_count_hard do
    :"k8s.resourcequota.persistentvolumeclaim_count.hard"
  end

  @doc """
  The total number of PersistentVolumeClaims that can exist in the namespace.
  The value represents the current observed total usage of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `{persistentvolumeclaim}`
  ### Notes

  This metric is retrieved from the `used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).

  The `k8s.storageclass.name` should be required when a resource quota is defined for a specific
  storage class.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_persistentvolumeclaim_count_used()
      :"k8s.resourcequota.persistentvolumeclaim_count.used"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_PERSISTENTVOLUMECLAIM_COUNT_USED.
  'k8s.resourcequota.persistentvolumeclaim_count.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_persistentvolumeclaim_count_used ::
          :"k8s.resourcequota.persistentvolumeclaim_count.used"
  def k8s_resourcequota_persistentvolumeclaim_count_used do
    :"k8s.resourcequota.persistentvolumeclaim_count.used"
  end

  @doc """
  The storage requests in a specific namespace.
  The value represents the configured quota limit of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).

  The `k8s.storageclass.name` should be required when a resource quota is defined for a specific
  storage class.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_storage_request_hard()
      :"k8s.resourcequota.storage.request.hard"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_STORAGE_REQUEST_HARD.
  'k8s.resourcequota.storage.request.hard'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_storage_request_hard :: :"k8s.resourcequota.storage.request.hard"
  def k8s_resourcequota_storage_request_hard do
    :"k8s.resourcequota.storage.request.hard"
  end

  @doc """
  The storage requests in a specific namespace.
  The value represents the current observed total usage of the resource in the namespace.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core).

  The `k8s.storageclass.name` should be required when a resource quota is defined for a specific
  storage class.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_resourcequota_storage_request_used()
      :"k8s.resourcequota.storage.request.used"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_STORAGE_REQUEST_USED.
  'k8s.resourcequota.storage.request.used'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_resourcequota_storage_request_used :: :"k8s.resourcequota.storage.request.used"
  def k8s_resourcequota_storage_request_used do
    :"k8s.resourcequota.storage.request.used"
  end

  @doc """
  Number of endpoints for a service by condition and address type.

  Instrument: `gauge`
  Unit: `{endpoint}`
  ### Notes

  This metric is derived from the Kubernetes [EndpointSlice API](https://kubernetes.io/docs/reference/kubernetes-api/service-resources/endpoint-slice-v1/).
  It reports the number of network endpoints backing a Service, broken down by their condition and address type.

  In dual-stack or multi-protocol clusters, separate counts are reported for each address family (`IPv4`, `IPv6`, `FQDN`).

  When the optional `zone` attribute is enabled, counts are further broken down by availability zone for zone-aware monitoring.

  An endpoint may be reported under multiple conditions simultaneously (e.g., both `serving` and `terminating` during a graceful shutdown).
  See [K8s EndpointConditions](https://kubernetes.io/docs/reference/kubernetes-api/service-resources/endpoint-slice-v1/) for more details.

  The conditions represent:
  - `ready`: Endpoints capable of receiving new connections.
  - `serving`: Endpoints currently handling traffic.
  - `terminating`: Endpoints that are being phased out but may still be handling existing connections.

  For Services with `publishNotReadyAddresses` enabled (common for headless StatefulSets),
  this metric will include endpoints that are published despite not being ready.
  The `k8s.service.publish_not_ready_addresses` resource attribute indicates this setting.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_service_endpoint_count()
      :"k8s.service.endpoint.count"

  ### Erlang

  ```erlang
  ?K8S_SERVICE_ENDPOINT_COUNT.
  'k8s.service.endpoint.count'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_service_endpoint_count :: :"k8s.service.endpoint.count"
  def k8s_service_endpoint_count do
    :"k8s.service.endpoint.count"
  end

  @doc """
  Number of load balancer ingress points (external IPs/hostnames) assigned to the service.

  Instrument: `gauge`
  Unit: `{ingress}`
  ### Notes

  This metric reports the number of external ingress points (IP addresses or hostnames)
  assigned to a LoadBalancer Service.

  It is only emitted for Services of type `LoadBalancer` and reflects the assignments
  made by the underlying infrastructure's load balancer controller in the
  [.status.loadBalancer.ingress](https://kubernetes.io/docs/reference/kubernetes-api/service-resources/service-v1/#ServiceStatus) field.

  A value of `0` indicates that no ingress points have been assigned yet (e.g., during provisioning).
  A value greater than `1` may occur when multiple IPs or hostnames are assigned (e.g., dual-stack configurations).

  This metric signals that external endpoints have been assigned by the load balancer controller, but it does not
  guarantee that the load balancer is healthy.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_service_load_balancer_ingress_count()
      :"k8s.service.load_balancer.ingress.count"

  ### Erlang

  ```erlang
  ?K8S_SERVICE_LOAD_BALANCER_INGRESS_COUNT.
  'k8s.service.load_balancer.ingress.count'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_service_load_balancer_ingress_count :: :"k8s.service.load_balancer.ingress.count"
  def k8s_service_load_balancer_ingress_count do
    :"k8s.service.load_balancer.ingress.count"
  end

  @deprecated """
  Replaced by `k8s.statefulset.pod.current`.
  """

  @spec k8s_statefulset_current_pods :: :"k8s.statefulset.current_pods"
  def k8s_statefulset_current_pods do
    :"k8s.statefulset.current_pods"
  end

  @deprecated """
  Replaced by `k8s.statefulset.pod.desired`.
  """

  @spec k8s_statefulset_desired_pods :: :"k8s.statefulset.desired_pods"
  def k8s_statefulset_desired_pods do
    :"k8s.statefulset.desired_pods"
  end

  @doc """
  The number of replica pods created by the statefulset controller from the statefulset version indicated by currentRevision.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `currentReplicas` field of the
  [K8s StatefulSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#statefulsetstatus-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_statefulset_pod_current()
      :"k8s.statefulset.pod.current"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_POD_CURRENT.
  'k8s.statefulset.pod.current'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_statefulset_pod_current :: :"k8s.statefulset.pod.current"
  def k8s_statefulset_pod_current do
    :"k8s.statefulset.pod.current"
  end

  @doc """
  Number of desired replica pods in this statefulset.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `replicas` field of the
  [K8s StatefulSetSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#statefulsetspec-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_statefulset_pod_desired()
      :"k8s.statefulset.pod.desired"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_POD_DESIRED.
  'k8s.statefulset.pod.desired'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_statefulset_pod_desired :: :"k8s.statefulset.pod.desired"
  def k8s_statefulset_pod_desired do
    :"k8s.statefulset.pod.desired"
  end

  @doc """
  The number of replica pods created for this statefulset with a Ready Condition.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `readyReplicas` field of the
  [K8s StatefulSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#statefulsetstatus-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_statefulset_pod_ready()
      :"k8s.statefulset.pod.ready"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_POD_READY.
  'k8s.statefulset.pod.ready'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_statefulset_pod_ready :: :"k8s.statefulset.pod.ready"
  def k8s_statefulset_pod_ready do
    :"k8s.statefulset.pod.ready"
  end

  @doc """
  Number of replica pods created by the statefulset controller from the statefulset version indicated by updateRevision.

  Instrument: `updowncounter`
  Unit: `{pod}`
  ### Notes

  This metric aligns with the `updatedReplicas` field of the
  [K8s StatefulSetStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#statefulsetstatus-v1-apps).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.K8SMetrics.k8s_statefulset_pod_updated()
      :"k8s.statefulset.pod.updated"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_POD_UPDATED.
  'k8s.statefulset.pod.updated'
  ```

  <!-- tabs-close -->
  """

  @spec k8s_statefulset_pod_updated :: :"k8s.statefulset.pod.updated"
  def k8s_statefulset_pod_updated do
    :"k8s.statefulset.pod.updated"
  end

  @deprecated """
  Replaced by `k8s.statefulset.pod.ready`.
  """

  @spec k8s_statefulset_ready_pods :: :"k8s.statefulset.ready_pods"
  def k8s_statefulset_ready_pods do
    :"k8s.statefulset.ready_pods"
  end

  @deprecated """
  Replaced by `k8s.statefulset.pod.updated`.
  """

  @spec k8s_statefulset_updated_pods :: :"k8s.statefulset.updated_pods"
  def k8s_statefulset_updated_pods do
    :"k8s.statefulset.updated_pods"
  end
end
