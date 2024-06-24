defmodule OpenTelemetry.SemanticConventions.K8SAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for K8s attributes.
  """

  @doc """
  The name of the cluster.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_cluster_name()
      :"k8s.cluster.name"
  """
  @spec k_8_s_cluster_name :: :"k8s.cluster.name"
  def k_8_s_cluster_name do
    :"k8s.cluster.name"
  end

  @doc """
  A pseudo-ID for the cluster, set to the UID of the `kube-system` namespace.

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


  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_cluster_uid()
      :"k8s.cluster.uid"
  """
  @spec k_8_s_cluster_uid :: :"k8s.cluster.uid"
  def k_8_s_cluster_uid do
    :"k8s.cluster.uid"
  end

  @doc """
  The name of the Container from Pod specification, must be unique within a Pod. Container runtime usually uses different globally unique name (`container.name`).



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_container_name()
      :"k8s.container.name"
  """
  @spec k_8_s_container_name :: :"k8s.container.name"
  def k_8_s_container_name do
    :"k8s.container.name"
  end

  @doc """
  Number of times the container was restarted. This attribute can be used to identify a particular container (running or stopped) within a container spec.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_container_restartcount()
      :"k8s.container.restart_count"
  """
  @spec k_8_s_container_restartcount :: :"k8s.container.restart_count"
  def k_8_s_container_restartcount do
    :"k8s.container.restart_count"
  end

  @doc """
  Last terminated reason of the Container.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_container_status_lastterminatedreason()
      :"k8s.container.status.last_terminated_reason"
  """
  @spec k_8_s_container_status_lastterminatedreason ::
          :"k8s.container.status.last_terminated_reason"
  def k_8_s_container_status_lastterminatedreason do
    :"k8s.container.status.last_terminated_reason"
  end

  @doc """
  The name of the CronJob.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_cronjob_name()
      :"k8s.cronjob.name"
  """
  @spec k_8_s_cronjob_name :: :"k8s.cronjob.name"
  def k_8_s_cronjob_name do
    :"k8s.cronjob.name"
  end

  @doc """
  The UID of the CronJob.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_cronjob_uid()
      :"k8s.cronjob.uid"
  """
  @spec k_8_s_cronjob_uid :: :"k8s.cronjob.uid"
  def k_8_s_cronjob_uid do
    :"k8s.cronjob.uid"
  end

  @doc """
  The name of the DaemonSet.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_daemonset_name()
      :"k8s.daemonset.name"
  """
  @spec k_8_s_daemonset_name :: :"k8s.daemonset.name"
  def k_8_s_daemonset_name do
    :"k8s.daemonset.name"
  end

  @doc """
  The UID of the DaemonSet.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_daemonset_uid()
      :"k8s.daemonset.uid"
  """
  @spec k_8_s_daemonset_uid :: :"k8s.daemonset.uid"
  def k_8_s_daemonset_uid do
    :"k8s.daemonset.uid"
  end

  @doc """
  The name of the Deployment.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_deployment_name()
      :"k8s.deployment.name"
  """
  @spec k_8_s_deployment_name :: :"k8s.deployment.name"
  def k_8_s_deployment_name do
    :"k8s.deployment.name"
  end

  @doc """
  The UID of the Deployment.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_deployment_uid()
      :"k8s.deployment.uid"
  """
  @spec k_8_s_deployment_uid :: :"k8s.deployment.uid"
  def k_8_s_deployment_uid do
    :"k8s.deployment.uid"
  end

  @doc """
  The name of the Job.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_job_name()
      :"k8s.job.name"
  """
  @spec k_8_s_job_name :: :"k8s.job.name"
  def k_8_s_job_name do
    :"k8s.job.name"
  end

  @doc """
  The UID of the Job.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_job_uid()
      :"k8s.job.uid"
  """
  @spec k_8_s_job_uid :: :"k8s.job.uid"
  def k_8_s_job_uid do
    :"k8s.job.uid"
  end

  @doc """
  The name of the namespace that the pod is running in.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_namespace_name()
      :"k8s.namespace.name"
  """
  @spec k_8_s_namespace_name :: :"k8s.namespace.name"
  def k_8_s_namespace_name do
    :"k8s.namespace.name"
  end

  @doc """
  The name of the Node.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_node_name()
      :"k8s.node.name"
  """
  @spec k_8_s_node_name :: :"k8s.node.name"
  def k_8_s_node_name do
    :"k8s.node.name"
  end

  @doc """
  The UID of the Node.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_node_uid()
      :"k8s.node.uid"
  """
  @spec k_8_s_node_uid :: :"k8s.node.uid"
  def k_8_s_node_uid do
    :"k8s.node.uid"
  end

  @doc """
  The annotation key-value pairs placed on the Pod, the `<key>` being the annotation name, the value being the annotation value.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_pod_annotation()
      :"k8s.pod.annotation"
  """
  @spec k_8_s_pod_annotation :: :"k8s.pod.annotation"
  def k_8_s_pod_annotation do
    :"k8s.pod.annotation"
  end

  @doc """
  The label key-value pairs placed on the Pod, the `<key>` being the label name, the value being the label value.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_pod_label()
      :"k8s.pod.label"
  """
  @spec k_8_s_pod_label :: :"k8s.pod.label"
  def k_8_s_pod_label do
    :"k8s.pod.label"
  end

  @deprecated """
  Replaced by `k8s.pod.label`.
  """

  @spec k_8_s_pod_labels :: :"k8s.pod.labels"
  def k_8_s_pod_labels do
    :"k8s.pod.labels"
  end

  @doc """
  The name of the Pod.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_pod_name()
      :"k8s.pod.name"
  """
  @spec k_8_s_pod_name :: :"k8s.pod.name"
  def k_8_s_pod_name do
    :"k8s.pod.name"
  end

  @doc """
  The UID of the Pod.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_pod_uid()
      :"k8s.pod.uid"
  """
  @spec k_8_s_pod_uid :: :"k8s.pod.uid"
  def k_8_s_pod_uid do
    :"k8s.pod.uid"
  end

  @doc """
  The name of the ReplicaSet.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_replicaset_name()
      :"k8s.replicaset.name"
  """
  @spec k_8_s_replicaset_name :: :"k8s.replicaset.name"
  def k_8_s_replicaset_name do
    :"k8s.replicaset.name"
  end

  @doc """
  The UID of the ReplicaSet.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_replicaset_uid()
      :"k8s.replicaset.uid"
  """
  @spec k_8_s_replicaset_uid :: :"k8s.replicaset.uid"
  def k_8_s_replicaset_uid do
    :"k8s.replicaset.uid"
  end

  @doc """
  The name of the StatefulSet.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_statefulset_name()
      :"k8s.statefulset.name"
  """
  @spec k_8_s_statefulset_name :: :"k8s.statefulset.name"
  def k_8_s_statefulset_name do
    :"k8s.statefulset.name"
  end

  @doc """
  The UID of the StatefulSet.



  ### Example
      iex> OpenTelemetry.SemanticConventions.K8SAttributes.k_8_s_statefulset_uid()
      :"k8s.statefulset.uid"
  """
  @spec k_8_s_statefulset_uid :: :"k8s.statefulset.uid"
  def k_8_s_statefulset_uid do
    :"k8s.statefulset.uid"
  end
end
