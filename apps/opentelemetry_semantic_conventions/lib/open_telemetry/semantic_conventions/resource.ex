defmodule OpenTelemetry.SemanticConventions.Resource do
  @moduledoc """
  WARNING: This module is deprecated and will be removed in a future release.
  Migrate to >= v1.27.0 semantic conventions.
  """

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec resource_schema_url :: String.t()
  def resource_schema_url do
    "https://opentelemetry.io/schemas/1.13.0"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec browser_brands :: :"browser.brands"
  def browser_brands do
    :"browser.brands"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec browser_platform :: :"browser.platform"
  def browser_platform do
    :"browser.platform"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec browser_user_agent :: :"browser.user_agent"
  def browser_user_agent do
    :"browser.user_agent"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec cloud_provider :: :"cloud.provider"
  def cloud_provider do
    :"cloud.provider"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec cloud_account_id :: :"cloud.account.id"
  def cloud_account_id do
    :"cloud.account.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec cloud_region :: :"cloud.region"
  def cloud_region do
    :"cloud.region"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec cloud_availability_zone :: :"cloud.availability_zone"
  def cloud_availability_zone do
    :"cloud.availability_zone"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec cloud_platform :: :"cloud.platform"
  def cloud_platform do
    :"cloud.platform"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_ecs_container_arn :: :"aws.ecs.container.arn"
  def aws_ecs_container_arn do
    :"aws.ecs.container.arn"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_ecs_cluster_arn :: :"aws.ecs.cluster.arn"
  def aws_ecs_cluster_arn do
    :"aws.ecs.cluster.arn"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_ecs_launchtype :: :"aws.ecs.launchtype"
  def aws_ecs_launchtype do
    :"aws.ecs.launchtype"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_ecs_task_arn :: :"aws.ecs.task.arn"
  def aws_ecs_task_arn do
    :"aws.ecs.task.arn"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_ecs_task_family :: :"aws.ecs.task.family"
  def aws_ecs_task_family do
    :"aws.ecs.task.family"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_ecs_task_revision :: :"aws.ecs.task.revision"
  def aws_ecs_task_revision do
    :"aws.ecs.task.revision"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_eks_cluster_arn :: :"aws.eks.cluster.arn"
  def aws_eks_cluster_arn do
    :"aws.eks.cluster.arn"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_log_group_names :: :"aws.log.group.names"
  def aws_log_group_names do
    :"aws.log.group.names"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_log_group_arns :: :"aws.log.group.arns"
  def aws_log_group_arns do
    :"aws.log.group.arns"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_log_stream_names :: :"aws.log.stream.names"
  def aws_log_stream_names do
    :"aws.log.stream.names"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec aws_log_stream_arns :: :"aws.log.stream.arns"
  def aws_log_stream_arns do
    :"aws.log.stream.arns"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec container_name :: :"container.name"
  def container_name do
    :"container.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec container_id :: :"container.id"
  def container_id do
    :"container.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec container_runtime :: :"container.runtime"
  def container_runtime do
    :"container.runtime"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec container_image_name :: :"container.image.name"
  def container_image_name do
    :"container.image.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec container_image_tag :: :"container.image.tag"
  def container_image_tag do
    :"container.image.tag"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec deployment_environment :: :"deployment.environment"
  def deployment_environment do
    :"deployment.environment"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec device_id :: :"device.id"
  def device_id do
    :"device.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec device_model_identifier :: :"device.model.identifier"
  def device_model_identifier do
    :"device.model.identifier"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec device_model_name :: :"device.model.name"
  def device_model_name do
    :"device.model.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec device_manufacturer :: :"device.manufacturer"
  def device_manufacturer do
    :"device.manufacturer"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_name :: :"faas.name"
  def faas_name do
    :"faas.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_id :: :"faas.id"
  def faas_id do
    :"faas.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_version :: :"faas.version"
  def faas_version do
    :"faas.version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_instance :: :"faas.instance"
  def faas_instance do
    :"faas.instance"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec faas_max_memory :: :"faas.max_memory"
  def faas_max_memory do
    :"faas.max_memory"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec host_id :: :"host.id"
  def host_id do
    :"host.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec host_name :: :"host.name"
  def host_name do
    :"host.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec host_type :: :"host.type"
  def host_type do
    :"host.type"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec host_arch :: :"host.arch"
  def host_arch do
    :"host.arch"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec host_image_name :: :"host.image.name"
  def host_image_name do
    :"host.image.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec host_image_id :: :"host.image.id"
  def host_image_id do
    :"host.image.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec host_image_version :: :"host.image.version"
  def host_image_version do
    :"host.image.version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_cluster_name :: :"k8s.cluster.name"
  def k8s_cluster_name do
    :"k8s.cluster.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_node_name :: :"k8s.node.name"
  def k8s_node_name do
    :"k8s.node.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_node_uid :: :"k8s.node.uid"
  def k8s_node_uid do
    :"k8s.node.uid"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_namespace_name :: :"k8s.namespace.name"
  def k8s_namespace_name do
    :"k8s.namespace.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_pod_uid :: :"k8s.pod.uid"
  def k8s_pod_uid do
    :"k8s.pod.uid"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_pod_name :: :"k8s.pod.name"
  def k8s_pod_name do
    :"k8s.pod.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_container_name :: :"k8s.container.name"
  def k8s_container_name do
    :"k8s.container.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_container_restart_count :: :"k8s.container.restart_count"
  def k8s_container_restart_count do
    :"k8s.container.restart_count"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_replicaset_uid :: :"k8s.replicaset.uid"
  def k8s_replicaset_uid do
    :"k8s.replicaset.uid"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_replicaset_name :: :"k8s.replicaset.name"
  def k8s_replicaset_name do
    :"k8s.replicaset.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_deployment_uid :: :"k8s.deployment.uid"
  def k8s_deployment_uid do
    :"k8s.deployment.uid"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_deployment_name :: :"k8s.deployment.name"
  def k8s_deployment_name do
    :"k8s.deployment.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_statefulset_uid :: :"k8s.statefulset.uid"
  def k8s_statefulset_uid do
    :"k8s.statefulset.uid"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_statefulset_name :: :"k8s.statefulset.name"
  def k8s_statefulset_name do
    :"k8s.statefulset.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_daemonset_uid :: :"k8s.daemonset.uid"
  def k8s_daemonset_uid do
    :"k8s.daemonset.uid"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_daemonset_name :: :"k8s.daemonset.name"
  def k8s_daemonset_name do
    :"k8s.daemonset.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_job_uid :: :"k8s.job.uid"
  def k8s_job_uid do
    :"k8s.job.uid"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_job_name :: :"k8s.job.name"
  def k8s_job_name do
    :"k8s.job.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_cronjob_uid :: :"k8s.cronjob.uid"
  def k8s_cronjob_uid do
    :"k8s.cronjob.uid"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec k8s_cronjob_name :: :"k8s.cronjob.name"
  def k8s_cronjob_name do
    :"k8s.cronjob.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec os_type :: :"os.type"
  def os_type do
    :"os.type"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec os_description :: :"os.description"
  def os_description do
    :"os.description"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec os_name :: :"os.name"
  def os_name do
    :"os.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec os_version :: :"os.version"
  def os_version do
    :"os.version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_pid :: :"process.pid"
  def process_pid do
    :"process.pid"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_parent_pid :: :"process.parent_pid"
  def process_parent_pid do
    :"process.parent_pid"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_executable_name :: :"process.executable.name"
  def process_executable_name do
    :"process.executable.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_executable_path :: :"process.executable.path"
  def process_executable_path do
    :"process.executable.path"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_command :: :"process.command"
  def process_command do
    :"process.command"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_command_line :: :"process.command_line"
  def process_command_line do
    :"process.command_line"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_command_args :: :"process.command_args"
  def process_command_args do
    :"process.command_args"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_owner :: :"process.owner"
  def process_owner do
    :"process.owner"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_runtime_name :: :"process.runtime.name"
  def process_runtime_name do
    :"process.runtime.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_runtime_version :: :"process.runtime.version"
  def process_runtime_version do
    :"process.runtime.version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec process_runtime_description :: :"process.runtime.description"
  def process_runtime_description do
    :"process.runtime.description"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec service_name :: :"service.name"
  def service_name do
    :"service.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec service_namespace :: :"service.namespace"
  def service_namespace do
    :"service.namespace"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec service_instance_id :: :"service.instance.id"
  def service_instance_id do
    :"service.instance.id"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec service_version :: :"service.version"
  def service_version do
    :"service.version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec telemetry_sdk_name :: :"telemetry.sdk.name"
  def telemetry_sdk_name do
    :"telemetry.sdk.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec telemetry_sdk_language :: :"telemetry.sdk.language"
  def telemetry_sdk_language do
    :"telemetry.sdk.language"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec telemetry_sdk_version :: :"telemetry.sdk.version"
  def telemetry_sdk_version do
    :"telemetry.sdk.version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec telemetry_auto_version :: :"telemetry.auto.version"
  def telemetry_auto_version do
    :"telemetry.auto.version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec webengine_name :: :"webengine.name"
  def webengine_name do
    :"webengine.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec webengine_version :: :"webengine.version"
  def webengine_version do
    :"webengine.version"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec webengine_description :: :"webengine.description"
  def webengine_description do
    :"webengine.description"
  end
end
