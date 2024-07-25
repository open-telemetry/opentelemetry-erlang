defmodule OpenTelemetry.SemConv.Incubating.K8SAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for K8S attributes.
  """

  @doc """
  The name of the cluster.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry-cluster"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_cluster_name()
      :"k8s.cluster.name"

  ### Erlang

  ```erlang
  ?K8S_CLUSTER_NAME.
  'k8s.cluster.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_cluster_name :: :"k8s.cluster.name"
  def k8s_cluster_name do
    :"k8s.cluster.name"
  end

  @doc """
  A pseudo-ID for the cluster, set to the UID of the `kube-system` namespace.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  K8s doesn't have support for obtaining a cluster ID. If this is ever
  added, we will recommend collecting the `k8s.cluster.uid` through the
  official APIs. In the meantime, we are able to use the `uid` of the
  `kube-system` namespace as a proxy for cluster ID. Read on for the
  rationale.

  Every object created in a K8s cluster is assigned a distinct UID. The
  `kube-system` namespace is used by Kubernetes itself and will exist
  for the lifetime of the cluster. Using the `uid` of the `kube-system`
  namespace is a reasonable proxy for the K8s ClusterID as it will only
  change if the cluster is rebuilt. Furthermore, Kubernetes UIDs are
  UUIDs as standardized by
  [ISO/IEC 9834-8 and ITU-T X.667](https://www.itu.int/ITU-T/studygroups/com17/oid.html).
  Which states:

  > If generated according to one of the mechanisms defined in Rec.
    ITU-T X.667 | ISO/IEC 9834-8, a UUID is either guaranteed to be
    different from all other UUIDs generated before 3603 A.D., or is
    extremely likely to be different (depending on the mechanism chosen).

  Therefore, UIDs between clusters should be extremely unlikely to
  conflict.

  ### Examples

  ```
  ["218fc5a9-a5f1-4b54-aa05-46717d0ab26d"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_cluster_uid()
      :"k8s.cluster.uid"

  ### Erlang

  ```erlang
  ?K8S_CLUSTER_UID.
  'k8s.cluster.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_cluster_uid :: :"k8s.cluster.uid"
  def k8s_cluster_uid do
    :"k8s.cluster.uid"
  end

  @doc """
  The name of the Container from Pod specification, must be unique within a Pod. Container runtime usually uses different globally unique name (`container.name`).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["redis"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_name()
      :"k8s.container.name"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_NAME.
  'k8s.container.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_container_name :: :"k8s.container.name"
  def k8s_container_name do
    :"k8s.container.name"
  end

  @doc """
  Number of times the container was restarted. This attribute can be used to identify a particular container (running or stopped) within a container spec.

  ### Value type

  Value must be of type `integer()`.

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_restart_count()
      :"k8s.container.restart_count"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_RESTART_COUNT.
  'k8s.container.restart_count'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_container_restart_count :: :"k8s.container.restart_count"
  def k8s_container_restart_count do
    :"k8s.container.restart_count"
  end

  @doc """
  Last terminated reason of the Container.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Evicted", "Error"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_status_last_terminated_reason()
      :"k8s.container.status.last_terminated_reason"

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_STATUS_LAST_TERMINATED_REASON.
  'k8s.container.status.last_terminated_reason'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_container_status_last_terminated_reason ::
          :"k8s.container.status.last_terminated_reason"
  def k8s_container_status_last_terminated_reason do
    :"k8s.container.status.last_terminated_reason"
  end

  @doc """
  The name of the CronJob.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_cronjob_name()
      :"k8s.cronjob.name"

  ### Erlang

  ```erlang
  ?K8S_CRONJOB_NAME.
  'k8s.cronjob.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_cronjob_name :: :"k8s.cronjob.name"
  def k8s_cronjob_name do
    :"k8s.cronjob.name"
  end

  @doc """
  The UID of the CronJob.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_cronjob_uid()
      :"k8s.cronjob.uid"

  ### Erlang

  ```erlang
  ?K8S_CRONJOB_UID.
  'k8s.cronjob.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_cronjob_uid :: :"k8s.cronjob.uid"
  def k8s_cronjob_uid do
    :"k8s.cronjob.uid"
  end

  @doc """
  The name of the DaemonSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_daemonset_name()
      :"k8s.daemonset.name"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_NAME.
  'k8s.daemonset.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_daemonset_name :: :"k8s.daemonset.name"
  def k8s_daemonset_name do
    :"k8s.daemonset.name"
  end

  @doc """
  The UID of the DaemonSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_daemonset_uid()
      :"k8s.daemonset.uid"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_UID.
  'k8s.daemonset.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_daemonset_uid :: :"k8s.daemonset.uid"
  def k8s_daemonset_uid do
    :"k8s.daemonset.uid"
  end

  @doc """
  The name of the Deployment.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_deployment_name()
      :"k8s.deployment.name"

  ### Erlang

  ```erlang
  ?K8S_DEPLOYMENT_NAME.
  'k8s.deployment.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_deployment_name :: :"k8s.deployment.name"
  def k8s_deployment_name do
    :"k8s.deployment.name"
  end

  @doc """
  The UID of the Deployment.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_deployment_uid()
      :"k8s.deployment.uid"

  ### Erlang

  ```erlang
  ?K8S_DEPLOYMENT_UID.
  'k8s.deployment.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_deployment_uid :: :"k8s.deployment.uid"
  def k8s_deployment_uid do
    :"k8s.deployment.uid"
  end

  @doc """
  The name of the Job.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_job_name()
      :"k8s.job.name"

  ### Erlang

  ```erlang
  ?K8S_JOB_NAME.
  'k8s.job.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_job_name :: :"k8s.job.name"
  def k8s_job_name do
    :"k8s.job.name"
  end

  @doc """
  The UID of the Job.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_job_uid()
      :"k8s.job.uid"

  ### Erlang

  ```erlang
  ?K8S_JOB_UID.
  'k8s.job.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_job_uid :: :"k8s.job.uid"
  def k8s_job_uid do
    :"k8s.job.uid"
  end

  @doc """
  The name of the namespace that the pod is running in.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["default"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_namespace_name()
      :"k8s.namespace.name"

  ### Erlang

  ```erlang
  ?K8S_NAMESPACE_NAME.
  'k8s.namespace.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_namespace_name :: :"k8s.namespace.name"
  def k8s_namespace_name do
    :"k8s.namespace.name"
  end

  @doc """
  The name of the Node.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["node-1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_name()
      :"k8s.node.name"

  ### Erlang

  ```erlang
  ?K8S_NODE_NAME.
  'k8s.node.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_node_name :: :"k8s.node.name"
  def k8s_node_name do
    :"k8s.node.name"
  end

  @doc """
  The UID of the Node.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1eb3a0c6-0477-4080-a9cb-0cb7db65c6a2"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_uid()
      :"k8s.node.uid"

  ### Erlang

  ```erlang
  ?K8S_NODE_UID.
  'k8s.node.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_node_uid :: :"k8s.node.uid"
  def k8s_node_uid do
    :"k8s.node.uid"
  end

  @doc """
  The annotation key-value pairs placed on the Pod, the `<key>` being the annotation name, the value being the annotation value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["k8s.pod.annotation.kubernetes.io/enforce-mountable-secrets=true", "k8s.pod.annotation.mycompany.io/arch=x64", "k8s.pod.annotation.data="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_annotation()
      :"k8s.pod.annotation"

  ### Erlang

  ```erlang
  ?K8S_POD_ANNOTATION.
  'k8s.pod.annotation'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_pod_annotation :: :"k8s.pod.annotation"
  def k8s_pod_annotation do
    :"k8s.pod.annotation"
  end

  @doc """
  The label key-value pairs placed on the Pod, the `<key>` being the label name, the value being the label value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["k8s.pod.label.app=my-app", "k8s.pod.label.mycompany.io/arch=x64", "k8s.pod.label.data="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_label()
      :"k8s.pod.label"

  ### Erlang

  ```erlang
  ?K8S_POD_LABEL.
  'k8s.pod.label'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_pod_label :: :"k8s.pod.label"
  def k8s_pod_label do
    :"k8s.pod.label"
  end

  @deprecated """
  Replaced by `k8s.pod.label`.
  """
  @spec k8s_pod_labels :: :"k8s.pod.labels"
  def k8s_pod_labels do
    :"k8s.pod.labels"
  end

  @doc """
  The name of the Pod.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry-pod-autoconf"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_name()
      :"k8s.pod.name"

  ### Erlang

  ```erlang
  ?K8S_POD_NAME.
  'k8s.pod.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_pod_name :: :"k8s.pod.name"
  def k8s_pod_name do
    :"k8s.pod.name"
  end

  @doc """
  The UID of the Pod.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_uid()
      :"k8s.pod.uid"

  ### Erlang

  ```erlang
  ?K8S_POD_UID.
  'k8s.pod.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_pod_uid :: :"k8s.pod.uid"
  def k8s_pod_uid do
    :"k8s.pod.uid"
  end

  @doc """
  The name of the ReplicaSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_replicaset_name()
      :"k8s.replicaset.name"

  ### Erlang

  ```erlang
  ?K8S_REPLICASET_NAME.
  'k8s.replicaset.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_replicaset_name :: :"k8s.replicaset.name"
  def k8s_replicaset_name do
    :"k8s.replicaset.name"
  end

  @doc """
  The UID of the ReplicaSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_replicaset_uid()
      :"k8s.replicaset.uid"

  ### Erlang

  ```erlang
  ?K8S_REPLICASET_UID.
  'k8s.replicaset.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_replicaset_uid :: :"k8s.replicaset.uid"
  def k8s_replicaset_uid do
    :"k8s.replicaset.uid"
  end

  @doc """
  The name of the StatefulSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_statefulset_name()
      :"k8s.statefulset.name"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_NAME.
  'k8s.statefulset.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_statefulset_name :: :"k8s.statefulset.name"
  def k8s_statefulset_name do
    :"k8s.statefulset.name"
  end

  @doc """
  The UID of the StatefulSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_statefulset_uid()
      :"k8s.statefulset.uid"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_UID.
  'k8s.statefulset.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_statefulset_uid :: :"k8s.statefulset.uid"
  def k8s_statefulset_uid do
    :"k8s.statefulset.uid"
  end
end
