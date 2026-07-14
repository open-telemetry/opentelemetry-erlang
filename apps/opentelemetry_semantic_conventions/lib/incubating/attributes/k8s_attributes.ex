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
  > ITU-T X.667 | ISO/IEC 9834-8, a UUID is either guaranteed to be
  > different from all other UUIDs generated before 3603 A.D., or is
  > extremely likely to be different (depending on the mechanism chosen).

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
  The cronjob annotation placed on the CronJob, the `<key>` being the annotation name, the value being the annotation value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Examples:

  - An annotation `retries` with value `4` **SHOULD** be recorded as the
    `k8s.cronjob.annotation.retries` attribute with value `"4"`.
  - An annotation `data` with empty string value **SHOULD** be recorded as
    the `k8s.cronjob.annotation.data` attribute with value `""`.

  ### Examples

  ```
  ["4", ""]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_cronjob_annotation()
      :"k8s.cronjob.annotation"

  ### Erlang

  ```erlang
  ?K8S_CRONJOB_ANNOTATION.
  'k8s.cronjob.annotation'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_cronjob_annotation :: :"k8s.cronjob.annotation"
  def k8s_cronjob_annotation do
    :"k8s.cronjob.annotation"
  end

  @doc """
  The label placed on the CronJob, the `<key>` being the label name, the value being the label value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Examples:

  - A label `type` with value `weekly` **SHOULD** be recorded as the
    `k8s.cronjob.label.type` attribute with value `"weekly"`.
  - A label `automated` with empty string value **SHOULD** be recorded as
    the `k8s.cronjob.label.automated` attribute with value `""`.

  ### Examples

  ```
  ["weekly", ""]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_cronjob_label()
      :"k8s.cronjob.label"

  ### Erlang

  ```erlang
  ?K8S_CRONJOB_LABEL.
  'k8s.cronjob.label'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_cronjob_label :: :"k8s.cronjob.label"
  def k8s_cronjob_label do
    :"k8s.cronjob.label"
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
  The annotation key-value pairs placed on the DaemonSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Examples

  ```
  ["k8s.daemonset.annotation.replicas=1", "k8s.daemonset.annotation.data="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_daemonset_annotation()
      :"k8s.daemonset.annotation"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_ANNOTATION.
  'k8s.daemonset.annotation'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_daemonset_annotation :: :"k8s.daemonset.annotation"
  def k8s_daemonset_annotation do
    :"k8s.daemonset.annotation"
  end

  @doc """
  The label key-value pairs placed on the DaemonSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Examples

  ```
  ["k8s.daemonset.label.app=guestbook", "k8s.daemonset.label.injected="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_daemonset_label()
      :"k8s.daemonset.label"

  ### Erlang

  ```erlang
  ?K8S_DAEMONSET_LABEL.
  'k8s.daemonset.label'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_daemonset_label :: :"k8s.daemonset.label"
  def k8s_daemonset_label do
    :"k8s.daemonset.label"
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
  The annotation key-value pairs placed on the Deployment.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Examples

  ```
  ["k8s.deployment.annotation.replicas=1", "k8s.deployment.annotation.data="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_deployment_annotation()
      :"k8s.deployment.annotation"

  ### Erlang

  ```erlang
  ?K8S_DEPLOYMENT_ANNOTATION.
  'k8s.deployment.annotation'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_deployment_annotation :: :"k8s.deployment.annotation"
  def k8s_deployment_annotation do
    :"k8s.deployment.annotation"
  end

  @doc """
  The label key-value pairs placed on the Deployment.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Examples

  ```
  ["k8s.deployment.label.app=guestbook", "k8s.deployment.label.injected="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_deployment_label()
      :"k8s.deployment.label"

  ### Erlang

  ```erlang
  ?K8S_DEPLOYMENT_LABEL.
  'k8s.deployment.label'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_deployment_label :: :"k8s.deployment.label"
  def k8s_deployment_label do
    :"k8s.deployment.label"
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
  The name of the horizontal pod autoscaler.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_hpa_name()
      :"k8s.hpa.name"

  ### Erlang

  ```erlang
  ?K8S_HPA_NAME.
  'k8s.hpa.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_hpa_name :: :"k8s.hpa.name"
  def k8s_hpa_name do
    :"k8s.hpa.name"
  end

  @doc """
  The UID of the horizontal pod autoscaler.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_hpa_uid()
      :"k8s.hpa.uid"

  ### Erlang

  ```erlang
  ?K8S_HPA_UID.
  'k8s.hpa.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_hpa_uid :: :"k8s.hpa.uid"
  def k8s_hpa_uid do
    :"k8s.hpa.uid"
  end

  @doc """
  The annotation key-value pairs placed on the Job.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Examples

  ```
  ["k8s.job.annotation.number=1", "k8s.job.annotation.data="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_job_annotation()
      :"k8s.job.annotation"

  ### Erlang

  ```erlang
  ?K8S_JOB_ANNOTATION.
  'k8s.job.annotation'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_job_annotation :: :"k8s.job.annotation"
  def k8s_job_annotation do
    :"k8s.job.annotation"
  end

  @doc """
  The label key-value pairs placed on the Job.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Examples

  ```
  ["k8s.job.label.jobtype=ci", "k8s.job.label.automated="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_job_label()
      :"k8s.job.label"

  ### Erlang

  ```erlang
  ?K8S_JOB_LABEL.
  'k8s.job.label'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_job_label :: :"k8s.job.label"
  def k8s_job_label do
    :"k8s.job.label"
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
  The annotation key-value pairs placed on the Namespace.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Examples

  ```
  ["k8s.namespace.annotation.ttl=0", "k8s.namespace.annotation.data="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_namespace_annotation()
      :"k8s.namespace.annotation"

  ### Erlang

  ```erlang
  ?K8S_NAMESPACE_ANNOTATION.
  'k8s.namespace.annotation'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_namespace_annotation :: :"k8s.namespace.annotation"
  def k8s_namespace_annotation do
    :"k8s.namespace.annotation"
  end

  @doc """
  The label key-value pairs placed on the Namespace.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Examples

  ```
  ["k8s.namespace.label.kubernetes.io/metadata.name=default", "k8s.namespace.label.data="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_namespace_label()
      :"k8s.namespace.label"

  ### Erlang

  ```erlang
  ?K8S_NAMESPACE_LABEL.
  'k8s.namespace.label'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_namespace_label :: :"k8s.namespace.label"
  def k8s_namespace_label do
    :"k8s.namespace.label"
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

  @typedoc """
  The phase of the K8s namespace.


  ### Enum Values
  * `:active` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Active namespace phase as described by [K8s API](https://pkg.go.dev/k8s.io/api@v0.31.3/core/v1#NamespacePhase)
  * `:terminating` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Terminating namespace phase as described by [K8s API](https://pkg.go.dev/k8s.io/api@v0.31.3/core/v1#NamespacePhase)
  """
  @type k8s_namespace_phase_values() :: %{
          :active => :active,
          :terminating => :terminating
        }
  @doc """
  The phase of the K8s namespace.


  ### Notes

  This attribute aligns with the `phase` field of the
  [K8s NamespaceStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#namespacestatus-v1-core)

  ### Examples

  ```
  ["active", "terminating"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_namespace_phase()
      :"k8s.namespace.phase"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_namespace_phase_values().active
      :active

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_namespace_phase() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_namespace_phase_values().active}
      %{:"k8s.namespace.phase" => :active}

  ### Erlang

  ```erlang
  ?K8S_NAMESPACE_PHASE.
  'k8s.namespace.phase'

  ?K8S_NAMESPACE_PHASE_VALUES_ACTIVE.
  'active'

  \#{?K8S_NAMESPACE_PHASE => ?K8S_NAMESPACE_PHASE_VALUES_ACTIVE}.
  \#{'k8s.namespace.phase' => 'active'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_namespace_phase :: :"k8s.namespace.phase"
  def k8s_namespace_phase do
    :"k8s.namespace.phase"
  end

  @spec k8s_namespace_phase_values() :: k8s_namespace_phase_values()
  def k8s_namespace_phase_values() do
    %{
      :active => :active,
      :terminating => :terminating
    }
  end

  @doc """
  The annotation placed on the Node, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Examples:

  - An annotation `node.alpha.kubernetes.io/ttl` with value `0` **SHOULD** be recorded as
    the `k8s.node.annotation.node.alpha.kubernetes.io/ttl` attribute with value `"0"`.
  - An annotation `data` with empty string value **SHOULD** be recorded as
    the `k8s.node.annotation.data` attribute with value `""`.

  ### Examples

  ```
  ["0", ""]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_annotation()
      :"k8s.node.annotation"

  ### Erlang

  ```erlang
  ?K8S_NODE_ANNOTATION.
  'k8s.node.annotation'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_node_annotation :: :"k8s.node.annotation"
  def k8s_node_annotation do
    :"k8s.node.annotation"
  end

  @doc """
  The label placed on the Node, the `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Examples:

  - A label `kubernetes.io/arch` with value `arm64` **SHOULD** be recorded
    as the `k8s.node.label.kubernetes.io/arch` attribute with value `"arm64"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.node.label.data` attribute with value `""`.

  ### Examples

  ```
  ["arm64", ""]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_label()
      :"k8s.node.label"

  ### Erlang

  ```erlang
  ?K8S_NODE_LABEL.
  'k8s.node.label'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_node_label :: :"k8s.node.label"
  def k8s_node_label do
    :"k8s.node.label"
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
  The annotation placed on the Pod, the `<key>` being the annotation name, the value being the annotation value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Examples:

  - An annotation `kubernetes.io/enforce-mountable-secrets` with value `true` **SHOULD** be recorded as
    the `k8s.pod.annotation.kubernetes.io/enforce-mountable-secrets` attribute with value `"true"`.
  - An annotation `mycompany.io/arch` with value `x64` **SHOULD** be recorded as
    the `k8s.pod.annotation.mycompany.io/arch` attribute with value `"x64"`.
  - An annotation `data` with empty string value **SHOULD** be recorded as
    the `k8s.pod.annotation.data` attribute with value `""`.

  ### Examples

  ```
  ["true", "x64", ""]
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
  The label placed on the Pod, the `<key>` being the label name, the value being the label value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Examples:

  - A label `app` with value `my-app` **SHOULD** be recorded as
    the `k8s.pod.label.app` attribute with value `"my-app"`.
  - A label `mycompany.io/arch` with value `x64` **SHOULD** be recorded as
    the `k8s.pod.label.mycompany.io/arch` attribute with value `"x64"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.pod.label.data` attribute with value `""`.

  ### Examples

  ```
  ["my-app", "x64", ""]
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
  The annotation key-value pairs placed on the ReplicaSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Examples

  ```
  ["k8s.replicaset.annotation.replicas=0", "k8s.replicaset.annotation.data="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_replicaset_annotation()
      :"k8s.replicaset.annotation"

  ### Erlang

  ```erlang
  ?K8S_REPLICASET_ANNOTATION.
  'k8s.replicaset.annotation'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_replicaset_annotation :: :"k8s.replicaset.annotation"
  def k8s_replicaset_annotation do
    :"k8s.replicaset.annotation"
  end

  @doc """
  The label key-value pairs placed on the ReplicaSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Examples

  ```
  ["k8s.replicaset.label.app=guestbook", "k8s.replicaset.label.injected="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_replicaset_label()
      :"k8s.replicaset.label"

  ### Erlang

  ```erlang
  ?K8S_REPLICASET_LABEL.
  'k8s.replicaset.label'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_replicaset_label :: :"k8s.replicaset.label"
  def k8s_replicaset_label do
    :"k8s.replicaset.label"
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
  The name of the replication controller.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_replicationcontroller_name()
      :"k8s.replicationcontroller.name"

  ### Erlang

  ```erlang
  ?K8S_REPLICATIONCONTROLLER_NAME.
  'k8s.replicationcontroller.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_replicationcontroller_name :: :"k8s.replicationcontroller.name"
  def k8s_replicationcontroller_name do
    :"k8s.replicationcontroller.name"
  end

  @doc """
  The UID of the replication controller.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_replicationcontroller_uid()
      :"k8s.replicationcontroller.uid"

  ### Erlang

  ```erlang
  ?K8S_REPLICATIONCONTROLLER_UID.
  'k8s.replicationcontroller.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_replicationcontroller_uid :: :"k8s.replicationcontroller.uid"
  def k8s_replicationcontroller_uid do
    :"k8s.replicationcontroller.uid"
  end

  @doc """
  The name of the resource quota.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_resourcequota_name()
      :"k8s.resourcequota.name"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_NAME.
  'k8s.resourcequota.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_resourcequota_name :: :"k8s.resourcequota.name"
  def k8s_resourcequota_name do
    :"k8s.resourcequota.name"
  end

  @doc """
  The UID of the resource quota.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_resourcequota_uid()
      :"k8s.resourcequota.uid"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_UID.
  'k8s.resourcequota.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_resourcequota_uid :: :"k8s.resourcequota.uid"
  def k8s_resourcequota_uid do
    :"k8s.resourcequota.uid"
  end

  @doc """
  The annotation key-value pairs placed on the StatefulSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Examples

  ```
  ["k8s.statefulset.annotation.replicas=1", "k8s.statefulset.annotation.data="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_statefulset_annotation()
      :"k8s.statefulset.annotation"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_ANNOTATION.
  'k8s.statefulset.annotation'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_statefulset_annotation :: :"k8s.statefulset.annotation"
  def k8s_statefulset_annotation do
    :"k8s.statefulset.annotation"
  end

  @doc """
  The label key-value pairs placed on the StatefulSet.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Examples

  ```
  ["k8s.statefulset.label.app=guestbook", "k8s.statefulset.label.injected="]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_statefulset_label()
      :"k8s.statefulset.label"

  ### Erlang

  ```erlang
  ?K8S_STATEFULSET_LABEL.
  'k8s.statefulset.label'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_statefulset_label :: :"k8s.statefulset.label"
  def k8s_statefulset_label do
    :"k8s.statefulset.label"
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

  @doc """
  The name of the K8s volume.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["volume0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_volume_name()
      :"k8s.volume.name"

  ### Erlang

  ```erlang
  ?K8S_VOLUME_NAME.
  'k8s.volume.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_volume_name :: :"k8s.volume.name"
  def k8s_volume_name do
    :"k8s.volume.name"
  end

  @typedoc """
  The type of the K8s volume.


  ### Enum Values
  * `:persistent_volume_claim` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A [persistentVolumeClaim](https://v1-30.docs.kubernetes.io/docs/concepts/storage/volumes/#persistentvolumeclaim) volume
  * `:config_map` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A [configMap](https://v1-30.docs.kubernetes.io/docs/concepts/storage/volumes/#configmap) volume
  * `:downward_api` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A [downwardAPI](https://v1-30.docs.kubernetes.io/docs/concepts/storage/volumes/#downwardapi) volume
  * `:empty_dir` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - An [emptyDir](https://v1-30.docs.kubernetes.io/docs/concepts/storage/volumes/#emptydir) volume
  * `:secret` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A [secret](https://v1-30.docs.kubernetes.io/docs/concepts/storage/volumes/#secret) volume
  * `:local` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - A [local](https://v1-30.docs.kubernetes.io/docs/concepts/storage/volumes/#local) volume
  """
  @type k8s_volume_type_values() :: %{
          :persistent_volume_claim => :persistentVolumeClaim,
          :config_map => :configMap,
          :downward_api => :downwardAPI,
          :empty_dir => :emptyDir,
          :secret => :secret,
          :local => :local
        }
  @doc """
  The type of the K8s volume.


  ### Examples

  ```
  ["emptyDir", "persistentVolumeClaim"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_volume_type()
      :"k8s.volume.type"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_volume_type_values().persistent_volume_claim
      :persistentVolumeClaim

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_volume_type() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_volume_type_values().persistent_volume_claim}
      %{:"k8s.volume.type" => :persistentVolumeClaim}

  ### Erlang

  ```erlang
  ?K8S_VOLUME_TYPE.
  'k8s.volume.type'

  ?K8S_VOLUME_TYPE_VALUES_PERSISTENT_VOLUME_CLAIM.
  'persistentVolumeClaim'

  \#{?K8S_VOLUME_TYPE => ?K8S_VOLUME_TYPE_VALUES_PERSISTENT_VOLUME_CLAIM}.
  \#{'k8s.volume.type' => 'persistentVolumeClaim'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_volume_type :: :"k8s.volume.type"
  def k8s_volume_type do
    :"k8s.volume.type"
  end

  @spec k8s_volume_type_values() :: k8s_volume_type_values()
  def k8s_volume_type_values() do
    %{
      :persistent_volume_claim => :persistentVolumeClaim,
      :config_map => :configMap,
      :downward_api => :downwardAPI,
      :empty_dir => :emptyDir,
      :secret => :secret,
      :local => :local
    }
  end
end
