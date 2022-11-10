defmodule OpenTelemetry.SemanticConventions.Resource do
  @doc """
  The schema url for telemetry resources.

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.resource_schema_url()
      "https://opentelemetry.io/schemas/1.13.0"
  """
  @spec resource_schema_url :: String.t()
  defmacro resource_schema_url do
    "https://opentelemetry.io/schemas/1.13.0"
  end

  @doc """
  Array of brand name and version separated by a space

  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (navigator.userAgentData.brands)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.browser_brands()
      :"browser.brands"
  """
  @spec browser_brands :: :"browser.brands"
  defmacro browser_brands do
    :"browser.brands"
  end

  @doc """
  The platform on which the browser is running

  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (navigator.userAgentData.platform). If unavailable, the legacy `navigator.platform` API SHOULD NOT be used instead and this attribute SHOULD be left unset in order for the values to be consistent.
  The list of possible values is defined in the [W3C User-Agent Client Hints specification](https://wicg.github.io/ua-client-hints/#sec-ch-ua-platform). Note that some (but not all) of these values can overlap with values in the [os.type and os.name attributes](./os.md). However, for consistency, the values in the `browser.platform` attribute should capture the exact value that the user agent provides

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.browser_platform()
      :"browser.platform"
  """
  @spec browser_platform :: :"browser.platform"
  defmacro browser_platform do
    :"browser.platform"
  end

  @doc """
  Full user-agent string provided by the browser

  ### Notes

  The user-agent value SHOULD be provided only from browsers that do not have a mechanism to retrieve brands and platform individually from the User-Agent Client Hints API. To retrieve the value, the legacy `navigator.userAgent` API can be used

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.browser_user_agent()
      :"browser.user_agent"
  """
  @spec browser_user_agent :: :"browser.user_agent"
  defmacro browser_user_agent do
    :"browser.user_agent"
  end

  @doc """
  Name of the cloud provider

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.cloud_provider()
      :"cloud.provider"
  """
  @spec cloud_provider :: :"cloud.provider"
  defmacro cloud_provider do
    :"cloud.provider"
  end

  @doc """
  The cloud account ID the resource is assigned to

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.cloud_account_id()
      :"cloud.account.id"
  """
  @spec cloud_account_id :: :"cloud.account.id"
  defmacro cloud_account_id do
    :"cloud.account.id"
  end

  @doc """
  The geographical region the resource is running

  ### Notes

  Refer to your provider's docs to see the available regions, for example [Alibaba Cloud regions](https://www.alibabacloud.com/help/doc-detail/40654.htm), [AWS regions](https://aws.amazon.com/about-aws/global-infrastructure/regions_az/), [Azure regions](https://azure.microsoft.com/en-us/global-infrastructure/geographies/), [Google Cloud regions](https://cloud.google.com/about/locations), or [Tencent Cloud regions](https://intl.cloud.tencent.com/document/product/213/6091)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.cloud_region()
      :"cloud.region"
  """
  @spec cloud_region :: :"cloud.region"
  defmacro cloud_region do
    :"cloud.region"
  end

  @doc """
  Cloud regions often have multiple, isolated locations known as zones to increase availability. Availability zone represents the zone where the resource is running

  ### Notes

  Availability zones are called "zones" on Alibaba Cloud and Google Cloud

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.cloud_availability_zone()
      :"cloud.availability_zone"
  """
  @spec cloud_availability_zone :: :"cloud.availability_zone"
  defmacro cloud_availability_zone do
    :"cloud.availability_zone"
  end

  @doc """
  The cloud platform in use

  ### Notes

  The prefix of the service SHOULD match the one specified in `cloud.provider`

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.cloud_platform()
      :"cloud.platform"
  """
  @spec cloud_platform :: :"cloud.platform"
  defmacro cloud_platform do
    :"cloud.platform"
  end

  @doc """
  The Amazon Resource Name (ARN) of an [ECS container instance](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_instances.html)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_ecs_container_arn()
      :"aws.ecs.container.arn"
  """
  @spec aws_ecs_container_arn :: :"aws.ecs.container.arn"
  defmacro aws_ecs_container_arn do
    :"aws.ecs.container.arn"
  end

  @doc """
  The ARN of an [ECS cluster](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/clusters.html)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_ecs_cluster_arn()
      :"aws.ecs.cluster.arn"
  """
  @spec aws_ecs_cluster_arn :: :"aws.ecs.cluster.arn"
  defmacro aws_ecs_cluster_arn do
    :"aws.ecs.cluster.arn"
  end

  @doc """
  The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_ecs_launchtype()
      :"aws.ecs.launchtype"
  """
  @spec aws_ecs_launchtype :: :"aws.ecs.launchtype"
  defmacro aws_ecs_launchtype do
    :"aws.ecs.launchtype"
  end

  @doc """
  The ARN of an [ECS task definition](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_ecs_task_arn()
      :"aws.ecs.task.arn"
  """
  @spec aws_ecs_task_arn :: :"aws.ecs.task.arn"
  defmacro aws_ecs_task_arn do
    :"aws.ecs.task.arn"
  end

  @doc """
  The task definition family this task definition is a member of

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_ecs_task_family()
      :"aws.ecs.task.family"
  """
  @spec aws_ecs_task_family :: :"aws.ecs.task.family"
  defmacro aws_ecs_task_family do
    :"aws.ecs.task.family"
  end

  @doc """
  The revision for this task definition

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_ecs_task_revision()
      :"aws.ecs.task.revision"
  """
  @spec aws_ecs_task_revision :: :"aws.ecs.task.revision"
  defmacro aws_ecs_task_revision do
    :"aws.ecs.task.revision"
  end

  @doc """
  The ARN of an EKS cluster

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_eks_cluster_arn()
      :"aws.eks.cluster.arn"
  """
  @spec aws_eks_cluster_arn :: :"aws.eks.cluster.arn"
  defmacro aws_eks_cluster_arn do
    :"aws.eks.cluster.arn"
  end

  @doc """
  The name(s) of the AWS log group(s) an application is writing to

  ### Notes

  Multiple log groups must be supported for cases like multi-container applications, where a single application has sidecar containers, and each write to their own log group

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_log_group_names()
      :"aws.log.group.names"
  """
  @spec aws_log_group_names :: :"aws.log.group.names"
  defmacro aws_log_group_names do
    :"aws.log.group.names"
  end

  @doc """
  The Amazon Resource Name(s) (ARN) of the AWS log group(s)

  ### Notes

  See the [log group ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_log_group_arns()
      :"aws.log.group.arns"
  """
  @spec aws_log_group_arns :: :"aws.log.group.arns"
  defmacro aws_log_group_arns do
    :"aws.log.group.arns"
  end

  @doc """
  The name(s) of the AWS log stream(s) an application is writing to

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_log_stream_names()
      :"aws.log.stream.names"
  """
  @spec aws_log_stream_names :: :"aws.log.stream.names"
  defmacro aws_log_stream_names do
    :"aws.log.stream.names"
  end

  @doc """
  The ARN(s) of the AWS log stream(s)

  ### Notes

  See the [log stream ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format). One log group can contain several log streams, so these ARNs necessarily identify both a log group and a log stream

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.aws_log_stream_arns()
      :"aws.log.stream.arns"
  """
  @spec aws_log_stream_arns :: :"aws.log.stream.arns"
  defmacro aws_log_stream_arns do
    :"aws.log.stream.arns"
  end

  @doc """
  Container name used by container runtime

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.container_name()
      :"container.name"
  """
  @spec container_name :: :"container.name"
  defmacro container_name do
    :"container.name"
  end

  @doc """
  Container ID. Usually a UUID, as for example used to [identify Docker containers](https://docs.docker.com/engine/reference/run/#container-identification). The UUID might be abbreviated

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.container_id()
      :"container.id"
  """
  @spec container_id :: :"container.id"
  defmacro container_id do
    :"container.id"
  end

  @doc """
  The container runtime managing this container

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.container_runtime()
      :"container.runtime"
  """
  @spec container_runtime :: :"container.runtime"
  defmacro container_runtime do
    :"container.runtime"
  end

  @doc """
  Name of the image the container was built on

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.container_image_name()
      :"container.image.name"
  """
  @spec container_image_name :: :"container.image.name"
  defmacro container_image_name do
    :"container.image.name"
  end

  @doc """
  Container image tag

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.container_image_tag()
      :"container.image.tag"
  """
  @spec container_image_tag :: :"container.image.tag"
  defmacro container_image_tag do
    :"container.image.tag"
  end

  @doc """
  Name of the [deployment environment](https://en.wikipedia.org/wiki/Deployment_environment) (aka deployment tier)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.deployment_environment()
      :"deployment.environment"
  """
  @spec deployment_environment :: :"deployment.environment"
  defmacro deployment_environment do
    :"deployment.environment"
  end

  @doc """
  A unique identifier representing the device

  ### Notes

  The device identifier MUST only be defined using the values outlined below. This value is not an advertising identifier and MUST NOT be used as such. On iOS (Swift or Objective-C), this value MUST be equal to the [vendor identifier](https://developer.apple.com/documentation/uikit/uidevice/1620059-identifierforvendor). On Android (Java or Kotlin), this value MUST be equal to the Firebase Installation ID or a globally unique UUID which is persisted across sessions in your application. More information can be found [here](https://developer.android.com/training/articles/user-data-ids) on best practices and exact implementation details. Caution should be taken when storing personal data or anything which can identify a user. GDPR and data protection laws may apply, ensure you do your own due diligence

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.device_id()
      :"device.id"
  """
  @spec device_id :: :"device.id"
  defmacro device_id do
    :"device.id"
  end

  @doc """
  The model identifier for the device

  ### Notes

  It's recommended this value represents a machine readable version of the model identifier rather than the market or consumer-friendly name of the device

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.device_model_identifier()
      :"device.model.identifier"
  """
  @spec device_model_identifier :: :"device.model.identifier"
  defmacro device_model_identifier do
    :"device.model.identifier"
  end

  @doc """
  The marketing name for the device model

  ### Notes

  It's recommended this value represents a human readable version of the device model rather than a machine readable alternative

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.device_model_name()
      :"device.model.name"
  """
  @spec device_model_name :: :"device.model.name"
  defmacro device_model_name do
    :"device.model.name"
  end

  @doc """
  The name of the device manufacturer

  ### Notes

  The Android OS provides this field via [Build](https://developer.android.com/reference/android/os/Build#MANUFACTURER). iOS apps SHOULD hardcode the value `Apple`

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.device_manufacturer()
      :"device.manufacturer"
  """
  @spec device_manufacturer :: :"device.manufacturer"
  defmacro device_manufacturer do
    :"device.manufacturer"
  end

  @doc """
  The name of the single function that this runtime instance executes

  ### Notes

  This is the name of the function as configured/deployed on the FaaS
  platform and is usually different from the name of the callback
  function (which may be stored in the
  [`code.namespace`/`code.function`](../../trace/semantic_conventions/span-general.md#source-code-attributes)
  span attributes).

  For some cloud providers, the above definition is ambiguous. The following
  definition of function name MUST be used for this attribute
  (and consequently the span name) for the listed cloud providers/products:

  * **Azure:**  The full name `<FUNCAPP>/<FUNC>`, i.e., function app name
    followed by a forward slash followed by the function name (this form
    can also be seen in the resource JSON for the function).
    This means that a span attribute MUST be used, as an Azure function
    app can host multiple functions that would usually share
    a TracerProvider (see also the `faas.id` attribute)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.faas_name()
      :"faas.name"
  """
  @spec faas_name :: :"faas.name"
  defmacro faas_name do
    :"faas.name"
  end

  @doc """
  The unique ID of the single function that this runtime instance executes

  ### Notes

  On some cloud providers, it may not be possible to determine the full ID at startup,
  so consider setting `faas.id` as a span attribute instead.

  The exact value to use for `faas.id` depends on the cloud provider:

  * **AWS Lambda:** The function [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html).
    Take care not to use the "invoked ARN" directly but replace any
    [alias suffix](https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html)
    with the resolved function version, as the same runtime instance may be invokable with
    multiple different aliases.
  * **GCP:** The [URI of the resource](https://cloud.google.com/iam/docs/full-resource-names)
  * **Azure:** The [Fully Qualified Resource ID](https://docs.microsoft.com/en-us/rest/api/resources/resources/get-by-id) of the invoked function,
    *not* the function app, having the form
    `/subscriptions/<SUBSCIPTION_GUID>/resourceGroups/<RG>/providers/Microsoft.Web/sites/<FUNCAPP>/functions/<FUNC>`.
    This means that a span attribute MUST be used, as an Azure function app can host multiple functions that would usually share
    a TracerProvider

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.faas_id()
      :"faas.id"
  """
  @spec faas_id :: :"faas.id"
  defmacro faas_id do
    :"faas.id"
  end

  @doc """
  The immutable version of the function being executed

  ### Notes

  Depending on the cloud provider and platform, use:

  * **AWS Lambda:** The [function version](https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html)
    (an integer represented as a decimal string).
  * **Google Cloud Run:** The [revision](https://cloud.google.com/run/docs/managing/revisions)
    (i.e., the function name plus the revision suffix).
  * **Google Cloud Functions:** The value of the
    [`K_REVISION` environment variable](https://cloud.google.com/functions/docs/env-var#runtime_environment_variables_set_automatically).
  * **Azure Functions:** Not applicable. Do not set this attribute

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.faas_version()
      :"faas.version"
  """
  @spec faas_version :: :"faas.version"
  defmacro faas_version do
    :"faas.version"
  end

  @doc """
  The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version

  ### Notes

  * **AWS Lambda:** Use the (full) log stream name

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.faas_instance()
      :"faas.instance"
  """
  @spec faas_instance :: :"faas.instance"
  defmacro faas_instance do
    :"faas.instance"
  end

  @doc """
  The amount of memory available to the serverless function in MiB

  ### Notes

  It's recommended to set this attribute since e.g. too little memory can easily stop a Java AWS Lambda function from working correctly. On AWS Lambda, the environment variable `AWS_LAMBDA_FUNCTION_MEMORY_SIZE` provides this information

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.faas_max_memory()
      :"faas.max_memory"
  """
  @spec faas_max_memory :: :"faas.max_memory"
  defmacro faas_max_memory do
    :"faas.max_memory"
  end

  @doc """
  Unique host ID. For Cloud, this must be the instance_id assigned by the cloud provider

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.host_id()
      :"host.id"
  """
  @spec host_id :: :"host.id"
  defmacro host_id do
    :"host.id"
  end

  @doc """
  Name of the host. On Unix systems, it may contain what the hostname command returns, or the fully qualified hostname, or another name specified by the user

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.host_name()
      :"host.name"
  """
  @spec host_name :: :"host.name"
  defmacro host_name do
    :"host.name"
  end

  @doc """
  Type of host. For Cloud, this must be the machine type

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.host_type()
      :"host.type"
  """
  @spec host_type :: :"host.type"
  defmacro host_type do
    :"host.type"
  end

  @doc """
  The CPU architecture the host system is running on

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.host_arch()
      :"host.arch"
  """
  @spec host_arch :: :"host.arch"
  defmacro host_arch do
    :"host.arch"
  end

  @doc """
  Name of the VM image or OS install the host was instantiated from

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.host_image_name()
      :"host.image.name"
  """
  @spec host_image_name :: :"host.image.name"
  defmacro host_image_name do
    :"host.image.name"
  end

  @doc """
  VM image ID. For Cloud, this value is from the provider

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.host_image_id()
      :"host.image.id"
  """
  @spec host_image_id :: :"host.image.id"
  defmacro host_image_id do
    :"host.image.id"
  end

  @doc """
  The version string of the VM image as defined in [Version Attributes](README.md#version-attributes)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.host_image_version()
      :"host.image.version"
  """
  @spec host_image_version :: :"host.image.version"
  defmacro host_image_version do
    :"host.image.version"
  end

  @doc """
  The name of the cluster

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_cluster_name()
      :"k8s.cluster.name"
  """
  @spec k8s_cluster_name :: :"k8s.cluster.name"
  defmacro k8s_cluster_name do
    :"k8s.cluster.name"
  end

  @doc """
  The name of the Node

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_node_name()
      :"k8s.node.name"
  """
  @spec k8s_node_name :: :"k8s.node.name"
  defmacro k8s_node_name do
    :"k8s.node.name"
  end

  @doc """
  The UID of the Node

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_node_uid()
      :"k8s.node.uid"
  """
  @spec k8s_node_uid :: :"k8s.node.uid"
  defmacro k8s_node_uid do
    :"k8s.node.uid"
  end

  @doc """
  The name of the namespace that the pod is running in

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_namespace_name()
      :"k8s.namespace.name"
  """
  @spec k8s_namespace_name :: :"k8s.namespace.name"
  defmacro k8s_namespace_name do
    :"k8s.namespace.name"
  end

  @doc """
  The UID of the Pod

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_pod_uid()
      :"k8s.pod.uid"
  """
  @spec k8s_pod_uid :: :"k8s.pod.uid"
  defmacro k8s_pod_uid do
    :"k8s.pod.uid"
  end

  @doc """
  The name of the Pod

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_pod_name()
      :"k8s.pod.name"
  """
  @spec k8s_pod_name :: :"k8s.pod.name"
  defmacro k8s_pod_name do
    :"k8s.pod.name"
  end

  @doc """
  The name of the Container from Pod specification, must be unique within a Pod. Container runtime usually uses different globally unique name (`container.name`)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_container_name()
      :"k8s.container.name"
  """
  @spec k8s_container_name :: :"k8s.container.name"
  defmacro k8s_container_name do
    :"k8s.container.name"
  end

  @doc """
  Number of times the container was restarted. This attribute can be used to identify a particular container (running or stopped) within a container spec

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_container_restart_count()
      :"k8s.container.restart_count"
  """
  @spec k8s_container_restart_count :: :"k8s.container.restart_count"
  defmacro k8s_container_restart_count do
    :"k8s.container.restart_count"
  end

  @doc """
  The UID of the ReplicaSet

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_replicaset_uid()
      :"k8s.replicaset.uid"
  """
  @spec k8s_replicaset_uid :: :"k8s.replicaset.uid"
  defmacro k8s_replicaset_uid do
    :"k8s.replicaset.uid"
  end

  @doc """
  The name of the ReplicaSet

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_replicaset_name()
      :"k8s.replicaset.name"
  """
  @spec k8s_replicaset_name :: :"k8s.replicaset.name"
  defmacro k8s_replicaset_name do
    :"k8s.replicaset.name"
  end

  @doc """
  The UID of the Deployment

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_deployment_uid()
      :"k8s.deployment.uid"
  """
  @spec k8s_deployment_uid :: :"k8s.deployment.uid"
  defmacro k8s_deployment_uid do
    :"k8s.deployment.uid"
  end

  @doc """
  The name of the Deployment

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_deployment_name()
      :"k8s.deployment.name"
  """
  @spec k8s_deployment_name :: :"k8s.deployment.name"
  defmacro k8s_deployment_name do
    :"k8s.deployment.name"
  end

  @doc """
  The UID of the StatefulSet

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_statefulset_uid()
      :"k8s.statefulset.uid"
  """
  @spec k8s_statefulset_uid :: :"k8s.statefulset.uid"
  defmacro k8s_statefulset_uid do
    :"k8s.statefulset.uid"
  end

  @doc """
  The name of the StatefulSet

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_statefulset_name()
      :"k8s.statefulset.name"
  """
  @spec k8s_statefulset_name :: :"k8s.statefulset.name"
  defmacro k8s_statefulset_name do
    :"k8s.statefulset.name"
  end

  @doc """
  The UID of the DaemonSet

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_daemonset_uid()
      :"k8s.daemonset.uid"
  """
  @spec k8s_daemonset_uid :: :"k8s.daemonset.uid"
  defmacro k8s_daemonset_uid do
    :"k8s.daemonset.uid"
  end

  @doc """
  The name of the DaemonSet

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_daemonset_name()
      :"k8s.daemonset.name"
  """
  @spec k8s_daemonset_name :: :"k8s.daemonset.name"
  defmacro k8s_daemonset_name do
    :"k8s.daemonset.name"
  end

  @doc """
  The UID of the Job

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_job_uid()
      :"k8s.job.uid"
  """
  @spec k8s_job_uid :: :"k8s.job.uid"
  defmacro k8s_job_uid do
    :"k8s.job.uid"
  end

  @doc """
  The name of the Job

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_job_name()
      :"k8s.job.name"
  """
  @spec k8s_job_name :: :"k8s.job.name"
  defmacro k8s_job_name do
    :"k8s.job.name"
  end

  @doc """
  The UID of the CronJob

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_cronjob_uid()
      :"k8s.cronjob.uid"
  """
  @spec k8s_cronjob_uid :: :"k8s.cronjob.uid"
  defmacro k8s_cronjob_uid do
    :"k8s.cronjob.uid"
  end

  @doc """
  The name of the CronJob

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.k8s_cronjob_name()
      :"k8s.cronjob.name"
  """
  @spec k8s_cronjob_name :: :"k8s.cronjob.name"
  defmacro k8s_cronjob_name do
    :"k8s.cronjob.name"
  end

  @doc """
  The operating system type

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.os_type()
      :"os.type"
  """
  @spec os_type :: :"os.type"
  defmacro os_type do
    :"os.type"
  end

  @doc """
  Human readable (not intended to be parsed) OS version information, like e.g. reported by `ver` or `lsb_release -a` commands

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.os_description()
      :"os.description"
  """
  @spec os_description :: :"os.description"
  defmacro os_description do
    :"os.description"
  end

  @doc """
  Human readable operating system name

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.os_name()
      :"os.name"
  """
  @spec os_name :: :"os.name"
  defmacro os_name do
    :"os.name"
  end

  @doc """
  The version string of the operating system as defined in [Version Attributes](../../resource/semantic_conventions/README.md#version-attributes)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.os_version()
      :"os.version"
  """
  @spec os_version :: :"os.version"
  defmacro os_version do
    :"os.version"
  end

  @doc """
  Process identifier (PID)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_pid()
      :"process.pid"
  """
  @spec process_pid :: :"process.pid"
  defmacro process_pid do
    :"process.pid"
  end

  @doc """
  Parent Process identifier (PID)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_parent_pid()
      :"process.parent_pid"
  """
  @spec process_parent_pid :: :"process.parent_pid"
  defmacro process_parent_pid do
    :"process.parent_pid"
  end

  @doc """
  The name of the process executable. On Linux based systems, can be set to the `Name` in `proc/[pid]/status`. On Windows, can be set to the base name of `GetProcessImageFileNameW`

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_executable_name()
      :"process.executable.name"
  """
  @spec process_executable_name :: :"process.executable.name"
  defmacro process_executable_name do
    :"process.executable.name"
  end

  @doc """
  The full path to the process executable. On Linux based systems, can be set to the target of `proc/[pid]/exe`. On Windows, can be set to the result of `GetProcessImageFileNameW`

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_executable_path()
      :"process.executable.path"
  """
  @spec process_executable_path :: :"process.executable.path"
  defmacro process_executable_path do
    :"process.executable.path"
  end

  @doc """
  The command used to launch the process (i.e. the command name). On Linux based systems, can be set to the zeroth string in `proc/[pid]/cmdline`. On Windows, can be set to the first parameter extracted from `GetCommandLineW`

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_command()
      :"process.command"
  """
  @spec process_command :: :"process.command"
  defmacro process_command do
    :"process.command"
  end

  @doc """
  The full command used to launch the process as a single string representing the full command. On Windows, can be set to the result of `GetCommandLineW`. Do not set this if you have to assemble it just for monitoring; use `process.command_args` instead

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_command_line()
      :"process.command_line"
  """
  @spec process_command_line :: :"process.command_line"
  defmacro process_command_line do
    :"process.command_line"
  end

  @doc """
  All the command arguments (including the command/executable itself) as received by the process. On Linux-based systems (and some other Unixoid systems supporting procfs), can be set according to the list of null-delimited strings extracted from `proc/[pid]/cmdline`. For libc-based executables, this would be the full argv vector passed to `main`

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_command_args()
      :"process.command_args"
  """
  @spec process_command_args :: :"process.command_args"
  defmacro process_command_args do
    :"process.command_args"
  end

  @doc """
  The username of the user that owns the process

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_owner()
      :"process.owner"
  """
  @spec process_owner :: :"process.owner"
  defmacro process_owner do
    :"process.owner"
  end

  @doc """
  The name of the runtime of this process. For compiled native binaries, this SHOULD be the name of the compiler

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_runtime_name()
      :"process.runtime.name"
  """
  @spec process_runtime_name :: :"process.runtime.name"
  defmacro process_runtime_name do
    :"process.runtime.name"
  end

  @doc """
  The version of the runtime of this process, as returned by the runtime without modification

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_runtime_version()
      :"process.runtime.version"
  """
  @spec process_runtime_version :: :"process.runtime.version"
  defmacro process_runtime_version do
    :"process.runtime.version"
  end

  @doc """
  An additional description about the runtime of the process, for example a specific vendor customization of the runtime environment

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.process_runtime_description()
      :"process.runtime.description"
  """
  @spec process_runtime_description :: :"process.runtime.description"
  defmacro process_runtime_description do
    :"process.runtime.description"
  end

  @doc """
  Logical name of the service

  ### Notes

  MUST be the same for all instances of horizontally scaled services. If the value was not specified, SDKs MUST fallback to `unknown_service:` concatenated with [`process.executable.name`](process.md#process), e.g. `unknown_service:bash`. If `process.executable.name` is not available, the value MUST be set to `unknown_service`

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.service_name()
      :"service.name"
  """
  @spec service_name :: :"service.name"
  defmacro service_name do
    :"service.name"
  end

  @doc """
  A namespace for `service.name`

  ### Notes

  A string value having a meaning that helps to distinguish a group of services, for example the team name that owns a group of services. `service.name` is expected to be unique within the same namespace. If `service.namespace` is not specified in the Resource then `service.name` is expected to be unique for all services that have no explicit namespace defined (so the empty/unspecified namespace is simply one more valid namespace). Zero-length namespace string is assumed equal to unspecified namespace

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.service_namespace()
      :"service.namespace"
  """
  @spec service_namespace :: :"service.namespace"
  defmacro service_namespace do
    :"service.namespace"
  end

  @doc """
  The string ID of the service instance

  ### Notes

  MUST be unique for each instance of the same `service.namespace,service.name` pair (in other words `service.namespace,service.name,service.instance.id` triplet MUST be globally unique). The ID helps to distinguish instances of the same service that exist at the same time (e.g. instances of a horizontally scaled service). It is preferable for the ID to be persistent and stay the same for the lifetime of the service instance, however it is acceptable that the ID is ephemeral and changes during important lifetime events for the service (e.g. service restarts). If the service has no inherent unique ID that can be used as the value of this attribute it is recommended to generate a random Version 1 or Version 4 RFC 4122 UUID (services aiming for reproducible UUIDs may also use Version 5, see RFC 4122 for more recommendations)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.service_instance_id()
      :"service.instance.id"
  """
  @spec service_instance_id :: :"service.instance.id"
  defmacro service_instance_id do
    :"service.instance.id"
  end

  @doc """
  The version string of the service API or implementation

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.service_version()
      :"service.version"
  """
  @spec service_version :: :"service.version"
  defmacro service_version do
    :"service.version"
  end

  @doc """
  The name of the telemetry SDK as defined above

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.telemetry_sdk_name()
      :"telemetry.sdk.name"
  """
  @spec telemetry_sdk_name :: :"telemetry.sdk.name"
  defmacro telemetry_sdk_name do
    :"telemetry.sdk.name"
  end

  @doc """
  The language of the telemetry SDK

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.telemetry_sdk_language()
      :"telemetry.sdk.language"
  """
  @spec telemetry_sdk_language :: :"telemetry.sdk.language"
  defmacro telemetry_sdk_language do
    :"telemetry.sdk.language"
  end

  @doc """
  The version string of the telemetry SDK

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.telemetry_sdk_version()
      :"telemetry.sdk.version"
  """
  @spec telemetry_sdk_version :: :"telemetry.sdk.version"
  defmacro telemetry_sdk_version do
    :"telemetry.sdk.version"
  end

  @doc """
  The version string of the auto instrumentation agent, if used

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.telemetry_auto_version()
      :"telemetry.auto.version"
  """
  @spec telemetry_auto_version :: :"telemetry.auto.version"
  defmacro telemetry_auto_version do
    :"telemetry.auto.version"
  end

  @doc """
  The name of the web engine

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.webengine_name()
      :"webengine.name"
  """
  @spec webengine_name :: :"webengine.name"
  defmacro webengine_name do
    :"webengine.name"
  end

  @doc """
  The version of the web engine

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.webengine_version()
      :"webengine.version"
  """
  @spec webengine_version :: :"webengine.version"
  defmacro webengine_version do
    :"webengine.version"
  end

  @doc """
  Additional description of the web engine (e.g. detailed version and edition information)

      iex> require OpenTelemetry.SemanticConventions.Resource
      ...> OpenTelemetry.SemanticConventions.Resource.webengine_description()
      :"webengine.description"
  """
  @spec webengine_description :: :"webengine.description"
  defmacro webengine_description do
    :"webengine.description"
  end
end
