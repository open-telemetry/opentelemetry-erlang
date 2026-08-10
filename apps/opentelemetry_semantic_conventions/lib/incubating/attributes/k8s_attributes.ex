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

  @typedoc """
  The reason for the container state. Corresponds to the `reason` field of the: [K8s ContainerStateWaiting](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#containerstatewaiting-v1-core) or [K8s ContainerStateTerminated](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#containerstateterminated-v1-core)


  ### Enum Values
  * `:container_creating` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The container is being created.
  * `:crash_loop_back_off` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The container is in a crash loop back off state.
  * `:create_container_config_error` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - There was an error creating the container configuration.
  * `:err_image_pull` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - There was an error pulling the container image.
  * `:image_pull_back_off` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The container image pull is in back off state.
  * `:oom_killed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The container was killed due to out of memory.
  * `:completed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The container has completed execution.
  * `:error` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - There was an error with the container.
  * `:container_cannot_run` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The container cannot run.
  """
  @type k8s_container_status_reason_values() :: %{
          :container_creating => :ContainerCreating,
          :crash_loop_back_off => :CrashLoopBackOff,
          :create_container_config_error => :CreateContainerConfigError,
          :err_image_pull => :ErrImagePull,
          :image_pull_back_off => :ImagePullBackOff,
          :oom_killed => :OOMKilled,
          :completed => :Completed,
          :error => :Error,
          :container_cannot_run => :ContainerCannotRun
        }
  @doc """
  The reason for the container state. Corresponds to the `reason` field of the: [K8s ContainerStateWaiting](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#containerstatewaiting-v1-core) or [K8s ContainerStateTerminated](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#containerstateterminated-v1-core)


  ### Examples

  ```
  ["ContainerCreating", "CrashLoopBackOff", "CreateContainerConfigError", "ErrImagePull", "ImagePullBackOff", "OOMKilled", "Completed", "Error", "ContainerCannotRun"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_status_reason()
      :"k8s.container.status.reason"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_status_reason_values().container_creating
      :ContainerCreating

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_status_reason() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_status_reason_values().container_creating}
      %{:"k8s.container.status.reason" => :ContainerCreating}

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_STATUS_REASON.
  'k8s.container.status.reason'

  ?K8S_CONTAINER_STATUS_REASON_VALUES_CONTAINER_CREATING.
  'ContainerCreating'

  \#{?K8S_CONTAINER_STATUS_REASON => ?K8S_CONTAINER_STATUS_REASON_VALUES_CONTAINER_CREATING}.
  \#{'k8s.container.status.reason' => 'ContainerCreating'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_container_status_reason :: :"k8s.container.status.reason"
  def k8s_container_status_reason do
    :"k8s.container.status.reason"
  end

  @spec k8s_container_status_reason_values() :: k8s_container_status_reason_values()
  def k8s_container_status_reason_values() do
    %{
      :container_creating => :ContainerCreating,
      :crash_loop_back_off => :CrashLoopBackOff,
      :create_container_config_error => :CreateContainerConfigError,
      :err_image_pull => :ErrImagePull,
      :image_pull_back_off => :ImagePullBackOff,
      :oom_killed => :OOMKilled,
      :completed => :Completed,
      :error => :Error,
      :container_cannot_run => :ContainerCannotRun
    }
  end

  @typedoc """
  The state of the container. [K8s ContainerState](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#containerstate-v1-core)


  ### Enum Values
  * `:terminated` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The container has terminated.
  * `:running` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The container is running.
  * `:waiting` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The container is waiting.
  """
  @type k8s_container_status_state_values() :: %{
          :terminated => :terminated,
          :running => :running,
          :waiting => :waiting
        }
  @doc """
  The state of the container. [K8s ContainerState](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#containerstate-v1-core)


  ### Examples

  ```
  ["terminated", "running", "waiting"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_status_state()
      :"k8s.container.status.state"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_status_state_values().terminated
      :terminated

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_status_state() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_container_status_state_values().terminated}
      %{:"k8s.container.status.state" => :terminated}

  ### Erlang

  ```erlang
  ?K8S_CONTAINER_STATUS_STATE.
  'k8s.container.status.state'

  ?K8S_CONTAINER_STATUS_STATE_VALUES_TERMINATED.
  'terminated'

  \#{?K8S_CONTAINER_STATUS_STATE => ?K8S_CONTAINER_STATUS_STATE_VALUES_TERMINATED}.
  \#{'k8s.container.status.state' => 'terminated'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_container_status_state :: :"k8s.container.status.state"
  def k8s_container_status_state do
    :"k8s.container.status.state"
  end

  @spec k8s_container_status_state_values() :: k8s_container_status_state_values()
  def k8s_container_status_state_values() do
    %{
      :terminated => :terminated,
      :running => :running,
      :waiting => :waiting
    }
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
  The annotation placed on the DaemonSet, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `replicas` with value `1` **SHOULD** be recorded
    as the `k8s.daemonset.annotation.replicas` attribute with value `"1"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.daemonset.annotation.data` attribute with value `""`.

  ### Examples

  ```
  ["1", ""]
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
  The label placed on the DaemonSet, the `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `app` with value `guestbook` **SHOULD** be recorded
    as the `k8s.daemonset.label.app` attribute with value `"guestbook"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.daemonset.label.injected` attribute with value `""`.

  ### Examples

  ```
  ["guestbook", ""]
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
  The annotation placed on the Deployment, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `replicas` with value `1` **SHOULD** be recorded
    as the `k8s.deployment.annotation.replicas` attribute with value `"1"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.deployment.annotation.data` attribute with value `""`.

  ### Examples

  ```
  ["1", ""]
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
  The label placed on the Deployment, the `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `replicas` with value `0` **SHOULD** be recorded
    as the `k8s.deployment.label.app` attribute with value `"guestbook"`.
  - A label `injected` with empty string value **SHOULD** be recorded as
    the `k8s.deployment.label.injected` attribute with value `""`.

  ### Examples

  ```
  ["guestbook", ""]
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
  The type of metric source for the horizontal pod autoscaler.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This attribute reflects the `type` field of spec.metrics[] in the HPA.

  ### Examples

  ```
  ["Resource", "ContainerResource"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_hpa_metric_type()
      :"k8s.hpa.metric.type"

  ### Erlang

  ```erlang
  ?K8S_HPA_METRIC_TYPE.
  'k8s.hpa.metric.type'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_hpa_metric_type :: :"k8s.hpa.metric.type"
  def k8s_hpa_metric_type do
    :"k8s.hpa.metric.type"
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
  The API version of the target resource to scale for the HorizontalPodAutoscaler.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This maps to the `apiVersion` field in the `scaleTargetRef` of the HPA spec.

  ### Examples

  ```
  ["apps/v1", "autoscaling/v2"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_hpa_scaletargetref_api_version()
      :"k8s.hpa.scaletargetref.api_version"

  ### Erlang

  ```erlang
  ?K8S_HPA_SCALETARGETREF_API_VERSION.
  'k8s.hpa.scaletargetref.api_version'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_hpa_scaletargetref_api_version :: :"k8s.hpa.scaletargetref.api_version"
  def k8s_hpa_scaletargetref_api_version do
    :"k8s.hpa.scaletargetref.api_version"
  end

  @doc """
  The kind of the target resource to scale for the HorizontalPodAutoscaler.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This maps to the `kind` field in the `scaleTargetRef` of the HPA spec.

  ### Examples

  ```
  ["Deployment", "StatefulSet"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_hpa_scaletargetref_kind()
      :"k8s.hpa.scaletargetref.kind"

  ### Erlang

  ```erlang
  ?K8S_HPA_SCALETARGETREF_KIND.
  'k8s.hpa.scaletargetref.kind'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_hpa_scaletargetref_kind :: :"k8s.hpa.scaletargetref.kind"
  def k8s_hpa_scaletargetref_kind do
    :"k8s.hpa.scaletargetref.kind"
  end

  @doc """
  The name of the target resource to scale for the HorizontalPodAutoscaler.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This maps to the `name` field in the `scaleTargetRef` of the HPA spec.

  ### Examples

  ```
  ["my-deployment", "my-statefulset"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_hpa_scaletargetref_name()
      :"k8s.hpa.scaletargetref.name"

  ### Erlang

  ```erlang
  ?K8S_HPA_SCALETARGETREF_NAME.
  'k8s.hpa.scaletargetref.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_hpa_scaletargetref_name :: :"k8s.hpa.scaletargetref.name"
  def k8s_hpa_scaletargetref_name do
    :"k8s.hpa.scaletargetref.name"
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
  The size (identifier) of the K8s huge page.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2Mi"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_hugepage_size()
      :"k8s.hugepage.size"

  ### Erlang

  ```erlang
  ?K8S_HUGEPAGE_SIZE.
  'k8s.hugepage.size'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_hugepage_size :: :"k8s.hugepage.size"
  def k8s_hugepage_size do
    :"k8s.hugepage.size"
  end

  @doc """
  The annotation placed on the Job, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `number` with value `1` **SHOULD** be recorded
    as the `k8s.job.annotation.number` attribute with value `"1"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.job.annotation.data` attribute with value `""`.

  ### Examples

  ```
  ["1", ""]
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
  The label placed on the Job, the `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `jobtype` with value `ci` **SHOULD** be recorded
    as the `k8s.job.label.jobtype` attribute with value `"ci"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.job.label.automated` attribute with value `""`.

  ### Examples

  ```
  ["ci", ""]
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
  The annotation placed on the Namespace, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `ttl` with value `0` **SHOULD** be recorded
    as the `k8s.namespace.annotation.ttl` attribute with value `"0"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.namespace.annotation.data` attribute with value `""`.

  ### Examples

  ```
  ["0", ""]
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
  The label placed on the Namespace, the `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `kubernetes.io/metadata.name` with value `default` **SHOULD** be recorded
    as the `k8s.namespace.label.kubernetes.io/metadata.name` attribute with value `"default"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.namespace.label.data` attribute with value `""`.

  ### Examples

  ```
  ["default", ""]
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

  @typedoc """
  The status of the condition, one of True, False, Unknown.


  ### Enum Values
  * `:condition_true` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:condition_false` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  * `:condition_unknown` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - 
  """
  @type k8s_node_condition_status_values() :: %{
          :condition_true => true,
          :condition_false => false,
          :condition_unknown => :unknown
        }
  @doc """
  The status of the condition, one of True, False, Unknown.


  ### Notes

  This attribute aligns with the `status` field of the
  [NodeCondition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#nodecondition-v1-core)

  ### Examples

  ```
  ["true", "false", "unknown"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_condition_status()
      :"k8s.node.condition.status"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_condition_status_values().condition_true
      :true

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_condition_status() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_condition_status_values().condition_true}
      %{:"k8s.node.condition.status" => :true}

  ### Erlang

  ```erlang
  ?K8S_NODE_CONDITION_STATUS.
  'k8s.node.condition.status'

  ?K8S_NODE_CONDITION_STATUS_VALUES_CONDITION_TRUE.
  'true'

  \#{?K8S_NODE_CONDITION_STATUS => ?K8S_NODE_CONDITION_STATUS_VALUES_CONDITION_TRUE}.
  \#{'k8s.node.condition.status' => 'true'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_node_condition_status :: :"k8s.node.condition.status"
  def k8s_node_condition_status do
    :"k8s.node.condition.status"
  end

  @spec k8s_node_condition_status_values() :: k8s_node_condition_status_values()
  def k8s_node_condition_status_values() do
    %{
      :condition_true => true,
      :condition_false => false,
      :condition_unknown => :unknown
    }
  end

  @typedoc """
  The condition type of a K8s Node.


  ### Enum Values
  * `:ready` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The node is healthy and ready to accept pods
  * `:disk_pressure` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Pressure exists on the disk size—that is, if the disk capacity is low
  * `:memory_pressure` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Pressure exists on the node memory—that is, if the node memory is low
  * `:pid_pressure` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Pressure exists on the processes—that is, if there are too many processes on the node
  * `:network_unavailable` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The network for the node is not correctly configured
  """
  @type k8s_node_condition_type_values() :: %{
          :ready => :Ready,
          :disk_pressure => :DiskPressure,
          :memory_pressure => :MemoryPressure,
          :pid_pressure => :PIDPressure,
          :network_unavailable => :NetworkUnavailable
        }
  @doc """
  The condition type of a K8s Node.


  ### Notes

  K8s Node conditions as described
  by [K8s documentation](https://v1-32.docs.kubernetes.io/docs/reference/node/node-status/#condition).

  This attribute aligns with the `type` field of the
  [NodeCondition](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#nodecondition-v1-core)

  The set of possible values is not limited to those listed here. Managed Kubernetes environments,
  or custom controllers **MAY** introduce additional node condition types.
  When this occurs, the exact value as reported by the Kubernetes API **SHOULD** be used.

  ### Examples

  ```
  ["Ready", "DiskPressure"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_condition_type()
      :"k8s.node.condition.type"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_condition_type_values().ready
      :Ready

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_condition_type() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_node_condition_type_values().ready}
      %{:"k8s.node.condition.type" => :Ready}

  ### Erlang

  ```erlang
  ?K8S_NODE_CONDITION_TYPE.
  'k8s.node.condition.type'

  ?K8S_NODE_CONDITION_TYPE_VALUES_READY.
  'Ready'

  \#{?K8S_NODE_CONDITION_TYPE => ?K8S_NODE_CONDITION_TYPE_VALUES_READY}.
  \#{'k8s.node.condition.type' => 'Ready'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_node_condition_type :: :"k8s.node.condition.type"
  def k8s_node_condition_type do
    :"k8s.node.condition.type"
  end

  @spec k8s_node_condition_type_values() :: k8s_node_condition_type_values()
  def k8s_node_condition_type_values() do
    %{
      :ready => :Ready,
      :disk_pressure => :DiskPressure,
      :memory_pressure => :MemoryPressure,
      :pid_pressure => :PIDPressure,
      :network_unavailable => :NetworkUnavailable
    }
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
  Specifies the hostname of the Pod.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The K8s Pod spec has an optional hostname field, which can be used to specify a hostname.
  Refer to [K8s docs](https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-hostname-and-subdomain-field)
  for more information about this field.

  This attribute aligns with the `hostname` field of the
  [K8s PodSpec](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.34/#podspec-v1-core).

  ### Examples

  ```
  ["collector-gateway"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_hostname()
      :"k8s.pod.hostname"

  ### Erlang

  ```erlang
  ?K8S_POD_HOSTNAME.
  'k8s.pod.hostname'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_pod_hostname :: :"k8s.pod.hostname"
  def k8s_pod_hostname do
    :"k8s.pod.hostname"
  end

  @doc """
  IP address allocated to the Pod.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This attribute aligns with the `podIP` field of the
  [K8s PodStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.34/#podstatus-v1-core).

  ### Examples

  ```
  ["172.18.0.2"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_ip()
      :"k8s.pod.ip"

  ### Erlang

  ```erlang
  ?K8S_POD_IP.
  'k8s.pod.ip'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_pod_ip :: :"k8s.pod.ip"
  def k8s_pod_ip do
    :"k8s.pod.ip"
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
  The start timestamp of the Pod.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Date and time at which the object was acknowledged by the Kubelet.
  This is before the Kubelet pulled the container image(s) for the pod.

  This attribute aligns with the `startTime` field of the
  [K8s PodStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.34/#podstatus-v1-core),
  in ISO 8601 (RFC 3339 compatible) format.

  ### Examples

  ```
  ["2025-12-04T08:41:03Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_start_time()
      :"k8s.pod.start_time"

  ### Erlang

  ```erlang
  ?K8S_POD_START_TIME.
  'k8s.pod.start_time'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_pod_start_time :: :"k8s.pod.start_time"
  def k8s_pod_start_time do
    :"k8s.pod.start_time"
  end

  @typedoc """
  The phase for the pod. Corresponds to the `phase` field of the: [K8s PodStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.33/#podstatus-v1-core)


  ### Enum Values
  * `:pending` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pod has been accepted by the system, but one or more of the containers has not been started. This includes time before being bound to a node, as well as time spent pulling images onto the host.

  * `:running` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pod has been bound to a node and all of the containers have been started. At least one container is still running or is in the process of being restarted.

  * `:succeeded` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - All containers in the pod have voluntarily terminated with a container exit code of 0, and the system is not going to restart any of these containers.

  * `:failed` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - All containers in the pod have terminated, and at least one container has terminated in a failure (exited with a non-zero exit code or was stopped by the system).

  * `:unknown` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - For some reason the state of the pod could not be obtained, typically due to an error in communicating with the host of the pod.

  """
  @type k8s_pod_status_phase_values() :: %{
          :pending => :Pending,
          :running => :Running,
          :succeeded => :Succeeded,
          :failed => :Failed,
          :unknown => :Unknown
        }
  @doc """
  The phase for the pod. Corresponds to the `phase` field of the: [K8s PodStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.33/#podstatus-v1-core)


  ### Examples

  ```
  ["Pending", "Running"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_status_phase()
      :"k8s.pod.status.phase"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_status_phase_values().pending
      :Pending

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_status_phase() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_status_phase_values().pending}
      %{:"k8s.pod.status.phase" => :Pending}

  ### Erlang

  ```erlang
  ?K8S_POD_STATUS_PHASE.
  'k8s.pod.status.phase'

  ?K8S_POD_STATUS_PHASE_VALUES_PENDING.
  'Pending'

  \#{?K8S_POD_STATUS_PHASE => ?K8S_POD_STATUS_PHASE_VALUES_PENDING}.
  \#{'k8s.pod.status.phase' => 'Pending'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_pod_status_phase :: :"k8s.pod.status.phase"
  def k8s_pod_status_phase do
    :"k8s.pod.status.phase"
  end

  @spec k8s_pod_status_phase_values() :: k8s_pod_status_phase_values()
  def k8s_pod_status_phase_values() do
    %{
      :pending => :Pending,
      :running => :Running,
      :succeeded => :Succeeded,
      :failed => :Failed,
      :unknown => :Unknown
    }
  end

  @typedoc """
  The reason for the pod state. Corresponds to the `reason` field of the: [K8s PodStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.33/#podstatus-v1-core)


  ### Enum Values
  * `:evicted` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pod is evicted.
  * `:node_affinity` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pod is in a status because of its node affinity
  * `:node_lost` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The reason on a pod when its state cannot be confirmed as kubelet is unresponsive on the node it is (was) running.

  * `:shutdown` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The node is shutdown
  * `:unexpected_admission_error` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The pod was rejected admission to the node because of an error during admission that could not be categorized.

  """
  @type k8s_pod_status_reason_values() :: %{
          :evicted => :Evicted,
          :node_affinity => :NodeAffinity,
          :node_lost => :NodeLost,
          :shutdown => :Shutdown,
          :unexpected_admission_error => :UnexpectedAdmissionError
        }
  @doc """
  The reason for the pod state. Corresponds to the `reason` field of the: [K8s PodStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.33/#podstatus-v1-core)


  ### Examples

  ```
  ["Evicted", "NodeAffinity"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_status_reason()
      :"k8s.pod.status.reason"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_status_reason_values().evicted
      :Evicted

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_status_reason() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_pod_status_reason_values().evicted}
      %{:"k8s.pod.status.reason" => :Evicted}

  ### Erlang

  ```erlang
  ?K8S_POD_STATUS_REASON.
  'k8s.pod.status.reason'

  ?K8S_POD_STATUS_REASON_VALUES_EVICTED.
  'Evicted'

  \#{?K8S_POD_STATUS_REASON => ?K8S_POD_STATUS_REASON_VALUES_EVICTED}.
  \#{'k8s.pod.status.reason' => 'Evicted'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_pod_status_reason :: :"k8s.pod.status.reason"
  def k8s_pod_status_reason do
    :"k8s.pod.status.reason"
  end

  @spec k8s_pod_status_reason_values() :: k8s_pod_status_reason_values()
  def k8s_pod_status_reason_values() do
    %{
      :evicted => :Evicted,
      :node_affinity => :NodeAffinity,
      :node_lost => :NodeLost,
      :shutdown => :Shutdown,
      :unexpected_admission_error => :UnexpectedAdmissionError
    }
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
  The annotation placed on the ReplicaSet, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `replicas` with value `0` **SHOULD** be recorded
    as the `k8s.replicaset.annotation.replicas` attribute with value `"0"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.replicaset.annotation.data` attribute with value `""`.

  ### Examples

  ```
  ["0", ""]
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
  The label placed on the ReplicaSet, the `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `app` with value `guestbook` **SHOULD** be recorded
    as the `k8s.replicaset.label.app` attribute with value `"guestbook"`.
  - A label `injected` with empty string value **SHOULD** be recorded as
    the `k8s.replicaset.label.injected` attribute with value `""`.

  ### Examples

  ```
  ["guestbook", ""]
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
  The name of the K8s resource a resource quota defines.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The value for this attribute can be either the full `count/<resource>[.<group>]` string (e.g., count/deployments.apps, count/pods), or, for certain core Kubernetes resources, just the resource name (e.g., pods, services, configmaps). Both forms are supported by Kubernetes for object count quotas. See [Kubernetes Resource Quotas documentation](https://kubernetes.io/docs/concepts/policy/resource-quotas/#quota-on-object-count) for more details.

  ### Examples

  ```
  ["count/replicationcontrollers"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_resourcequota_resource_name()
      :"k8s.resourcequota.resource_name"

  ### Erlang

  ```erlang
  ?K8S_RESOURCEQUOTA_RESOURCE_NAME.
  'k8s.resourcequota.resource_name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_resourcequota_resource_name :: :"k8s.resourcequota.resource_name"
  def k8s_resourcequota_resource_name do
    :"k8s.resourcequota.resource_name"
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
  The annotation placed on the Service, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Examples:

  - An annotation `prometheus.io/scrape` with value `true` **SHOULD** be recorded as
    the `k8s.service.annotation.prometheus.io/scrape` attribute with value `"true"`.
  - An annotation `data` with empty string value **SHOULD** be recorded as
    the `k8s.service.annotation.data` attribute with value `""`.

  ### Examples

  ```
  ["true", ""]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_annotation()
      :"k8s.service.annotation"

  ### Erlang

  ```erlang
  ?K8S_SERVICE_ANNOTATION.
  'k8s.service.annotation'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_annotation :: :"k8s.service.annotation"
  def k8s_service_annotation do
    :"k8s.service.annotation"
  end

  @typedoc """
  The address type of the service endpoint.


  ### Enum Values
  * `:ipv4` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IPv4 address type
  * `:ipv6` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IPv6 address type
  * `:fqdn` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - FQDN address type
  """
  @type k8s_service_endpoint_address_type_values() :: %{
          :ipv4 => :IPv4,
          :ipv6 => :IPv6,
          :fqdn => :FQDN
        }
  @doc """
  The address type of the service endpoint.


  ### Notes

  The network address family or type of the endpoint.
  This attribute aligns with the `addressType` field of the
  [K8s EndpointSlice](https://kubernetes.io/docs/reference/kubernetes-api/service-resources/endpoint-slice-v1/).
  It is used to differentiate metrics when a Service is backed by multiple address types
  (e.g., in dual-stack clusters).

  ### Examples

  ```
  ["IPv4", "IPv6"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_endpoint_address_type()
      :"k8s.service.endpoint.address_type"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_endpoint_address_type_values().ipv4
      :IPv4

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_endpoint_address_type() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_endpoint_address_type_values().ipv4}
      %{:"k8s.service.endpoint.address_type" => :IPv4}

  ### Erlang

  ```erlang
  ?K8S_SERVICE_ENDPOINT_ADDRESS_TYPE.
  'k8s.service.endpoint.address_type'

  ?K8S_SERVICE_ENDPOINT_ADDRESS_TYPE_VALUES_IPV4.
  'IPv4'

  \#{?K8S_SERVICE_ENDPOINT_ADDRESS_TYPE => ?K8S_SERVICE_ENDPOINT_ADDRESS_TYPE_VALUES_IPV4}.
  \#{'k8s.service.endpoint.address_type' => 'IPv4'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_endpoint_address_type :: :"k8s.service.endpoint.address_type"
  def k8s_service_endpoint_address_type do
    :"k8s.service.endpoint.address_type"
  end

  @spec k8s_service_endpoint_address_type_values() :: k8s_service_endpoint_address_type_values()
  def k8s_service_endpoint_address_type_values() do
    %{
      :ipv4 => :IPv4,
      :ipv6 => :IPv6,
      :fqdn => :FQDN
    }
  end

  @typedoc """
  The condition of the service endpoint.


  ### Enum Values
  * `:ready` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The endpoint is ready to receive new connections.
  * `:serving` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The endpoint is currently handling traffic.
  * `:terminating` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The endpoint is in the process of shutting down.
  """
  @type k8s_service_endpoint_condition_values() :: %{
          :ready => :ready,
          :serving => :serving,
          :terminating => :terminating
        }
  @doc """
  The condition of the service endpoint.


  ### Notes

  The current operational condition of the service endpoint.
  An endpoint can have multiple conditions set at once (e.g., both `serving` and `terminating` during rollout).
  This attribute aligns with the condition fields in the [K8s EndpointSlice](https://kubernetes.io/docs/reference/kubernetes-api/service-resources/endpoint-slice-v1/).

  ### Examples

  ```
  ["ready", "serving", "terminating"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_endpoint_condition()
      :"k8s.service.endpoint.condition"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_endpoint_condition_values().ready
      :ready

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_endpoint_condition() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_endpoint_condition_values().ready}
      %{:"k8s.service.endpoint.condition" => :ready}

  ### Erlang

  ```erlang
  ?K8S_SERVICE_ENDPOINT_CONDITION.
  'k8s.service.endpoint.condition'

  ?K8S_SERVICE_ENDPOINT_CONDITION_VALUES_READY.
  'ready'

  \#{?K8S_SERVICE_ENDPOINT_CONDITION => ?K8S_SERVICE_ENDPOINT_CONDITION_VALUES_READY}.
  \#{'k8s.service.endpoint.condition' => 'ready'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_endpoint_condition :: :"k8s.service.endpoint.condition"
  def k8s_service_endpoint_condition do
    :"k8s.service.endpoint.condition"
  end

  @spec k8s_service_endpoint_condition_values() :: k8s_service_endpoint_condition_values()
  def k8s_service_endpoint_condition_values() do
    %{
      :ready => :ready,
      :serving => :serving,
      :terminating => :terminating
    }
  end

  @doc """
  The zone of the service endpoint.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The zone where the endpoint is located, typically corresponding to a failure domain.
  This attribute aligns with the `zone` field of endpoints in the
  [K8s EndpointSlice](https://kubernetes.io/docs/reference/kubernetes-api/service-resources/endpoint-slice-v1/).
  It enables zone-aware monitoring of service endpoint distribution and supports
  features like [Topology Aware Routing](https://kubernetes.io/docs/concepts/services-networking/topology-aware-routing/).

  If the zone is not populated (e.g., nodes without the `topology.kubernetes.io/zone` label),
  the attribute value will be an empty string.

  ### Examples

  ```
  ["us-east-1a", "us-west-2b", "zone-a", ""]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_endpoint_zone()
      :"k8s.service.endpoint.zone"

  ### Erlang

  ```erlang
  ?K8S_SERVICE_ENDPOINT_ZONE.
  'k8s.service.endpoint.zone'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_endpoint_zone :: :"k8s.service.endpoint.zone"
  def k8s_service_endpoint_zone do
    :"k8s.service.endpoint.zone"
  end

  @doc """
  The label placed on the Service, the `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Examples:

  - A label `app` with value `my-service` **SHOULD** be recorded as
    the `k8s.service.label.app` attribute with value `"my-service"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.service.label.data` attribute with value `""`.

  ### Examples

  ```
  ["my-service", ""]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_label()
      :"k8s.service.label"

  ### Erlang

  ```erlang
  ?K8S_SERVICE_LABEL.
  'k8s.service.label'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_label :: :"k8s.service.label"
  def k8s_service_label do
    :"k8s.service.label"
  end

  @doc """
  The name of the Service.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["my-service"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_name()
      :"k8s.service.name"

  ### Erlang

  ```erlang
  ?K8S_SERVICE_NAME.
  'k8s.service.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_name :: :"k8s.service.name"
  def k8s_service_name do
    :"k8s.service.name"
  end

  @doc """
  Whether the Service publishes not-ready endpoints.

  ### Value type

  Value must be of type `boolean()`.
  ### Notes

  Whether the Service is configured to publish endpoints before the pods are ready.
  This attribute is typically used to indicate that a Service (such as a headless
  Service for a StatefulSet) allows peer discovery before pods pass their readiness probes.
  It aligns with the `publishNotReadyAddresses` field of the
  [K8s ServiceSpec](https://kubernetes.io/docs/reference/kubernetes-api/service-resources/service-v1/#ServiceSpec).

  ### Examples

  ```
  [true, false]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_publish_not_ready_addresses()
      :"k8s.service.publish_not_ready_addresses"

  ### Erlang

  ```erlang
  ?K8S_SERVICE_PUBLISH_NOT_READY_ADDRESSES.
  'k8s.service.publish_not_ready_addresses'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_publish_not_ready_addresses :: :"k8s.service.publish_not_ready_addresses"
  def k8s_service_publish_not_ready_addresses do
    :"k8s.service.publish_not_ready_addresses"
  end

  @doc """
  The selector key-value pair placed on the Service, the `<key>` being the selector key, the value being the selector value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  These selectors are used to correlate with pod labels. Each selector key-value pair becomes a separate attribute.

  Examples:

  - A selector `app=my-app` **SHOULD** be recorded as
    the `k8s.service.selector.app` attribute with value `"my-app"`.
  - A selector `version=v1` **SHOULD** be recorded as
    the `k8s.service.selector.version` attribute with value `"v1"`.

  ### Examples

  ```
  ["my-app", "v1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_selector()
      :"k8s.service.selector"

  ### Erlang

  ```erlang
  ?K8S_SERVICE_SELECTOR.
  'k8s.service.selector'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_selector :: :"k8s.service.selector"
  def k8s_service_selector do
    :"k8s.service.selector"
  end

  @doc """
  The traffic distribution policy for the Service.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Specifies how traffic is distributed to endpoints for this Service.
  This attribute aligns with the `trafficDistribution` field of the
  [K8s ServiceSpec](https://kubernetes.io/docs/reference/networking/virtual-ips/#traffic-distribution).
  Known values include `PreferSameZone` (prefer endpoints in the same zone as the client) and
  `PreferSameNode` (prefer endpoints on the same node, fallback to same zone, then cluster-wide).
  If this field is not set on the Service, the attribute **SHOULD** **NOT** be emitted.
  When not set, Kubernetes distributes traffic evenly across all endpoints cluster-wide.

  ### Examples

  ```
  ["PreferSameZone", "PreferSameNode"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_traffic_distribution()
      :"k8s.service.traffic_distribution"

  ### Erlang

  ```erlang
  ?K8S_SERVICE_TRAFFIC_DISTRIBUTION.
  'k8s.service.traffic_distribution'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_traffic_distribution :: :"k8s.service.traffic_distribution"
  def k8s_service_traffic_distribution do
    :"k8s.service.traffic_distribution"
  end

  @typedoc """
  The type of the Kubernetes Service.


  ### Enum Values
  * `:cluster_ip` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - ClusterIP service type
  * `:node_port` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - NodePort service type
  * `:load_balancer` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - LoadBalancer service type
  * `:external_name` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - ExternalName service type
  """
  @type k8s_service_type_values() :: %{
          :cluster_ip => :ClusterIP,
          :node_port => :NodePort,
          :load_balancer => :LoadBalancer,
          :external_name => :ExternalName
        }
  @doc """
  The type of the Kubernetes Service.


  ### Notes

  This attribute aligns with the `type` field of the
  [K8s ServiceSpec](https://kubernetes.io/docs/reference/kubernetes-api/service-resources/service-v1/#ServiceSpec).

  ### Examples

  ```
  ["ClusterIP", "NodePort", "LoadBalancer"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_type()
      :"k8s.service.type"

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_type_values().cluster_ip
      :ClusterIP

      iex> %{OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_type() => OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_type_values().cluster_ip}
      %{:"k8s.service.type" => :ClusterIP}

  ### Erlang

  ```erlang
  ?K8S_SERVICE_TYPE.
  'k8s.service.type'

  ?K8S_SERVICE_TYPE_VALUES_CLUSTER_IP.
  'ClusterIP'

  \#{?K8S_SERVICE_TYPE => ?K8S_SERVICE_TYPE_VALUES_CLUSTER_IP}.
  \#{'k8s.service.type' => 'ClusterIP'}
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_type :: :"k8s.service.type"
  def k8s_service_type do
    :"k8s.service.type"
  end

  @spec k8s_service_type_values() :: k8s_service_type_values()
  def k8s_service_type_values() do
    %{
      :cluster_ip => :ClusterIP,
      :node_port => :NodePort,
      :load_balancer => :LoadBalancer,
      :external_name => :ExternalName
    }
  end

  @doc """
  The UID of the Service.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["275ecb36-5aa8-4c2a-9c47-d8bb681b9aff"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_service_uid()
      :"k8s.service.uid"

  ### Erlang

  ```erlang
  ?K8S_SERVICE_UID.
  'k8s.service.uid'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_service_uid :: :"k8s.service.uid"
  def k8s_service_uid do
    :"k8s.service.uid"
  end

  @doc """
  The annotation placed on the StatefulSet, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `replicas` with value `1` **SHOULD** be recorded
    as the `k8s.statefulset.annotation.replicas` attribute with value `"1"`.
  - A label `data` with empty string value **SHOULD** be recorded as
    the `k8s.statefulset.annotation.data` attribute with value `""`.

  ### Examples

  ```
  ["1", ""]
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
  The label placed on the StatefulSet, the `<key>` being the label name, the value being the label value, even if the value is empty.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples:

  - A label `replicas` with value `0` **SHOULD** be recorded
    as the `k8s.statefulset.label.app` attribute with value `"guestbook"`.
  - A label `injected` with empty string value **SHOULD** be recorded as
    the `k8s.statefulset.label.injected` attribute with value `""`.

  ### Examples

  ```
  ["guestbook", ""]
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
  The name of K8s [StorageClass](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#storageclass-v1-storage-k8s-io) object.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["gold.storageclass.storage.k8s.io"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.K8SAttributes.k8s_storageclass_name()
      :"k8s.storageclass.name"

  ### Erlang

  ```erlang
  ?K8S_STORAGECLASS_NAME.
  'k8s.storageclass.name'
  ```

  <!-- tabs-close -->
  """
  @spec k8s_storageclass_name :: :"k8s.storageclass.name"
  def k8s_storageclass_name do
    :"k8s.storageclass.name"
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
