defmodule OpenTelemetry.SemanticConventions.Resource do
  @moduledoc """
  OpenTelemetry Semantic Conventions for Attributes.
  """

  @doc namespace: :aws
  @typedoc """
  The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task

  ### Options

  * `:ec2`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ec2

  * `:fargate`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - fargate

  """
  @type aws_ecs_launchtype() :: :ec2 | :fargate

  @doc namespace: :host
  @typedoc """
  The CPU architecture the host system is running on

  ### Options

  * `:amd64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - AMD64

  * `:arm32`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ARM32

  * `:arm64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - ARM64

  * `:ia64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - Itanium

  * `:ppc32`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 32-bit PowerPC

  * `:ppc64`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 64-bit PowerPC

  * `:s390x`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - IBM z/Architecture

  * `:x86`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - 32-bit x86

  """
  @type host_arch() :: :amd64 | :arm32 | :arm64 | :ia64 | :ppc32 | :ppc64 | :s390x | :x86 | atom()

  @doc namespace: :telemetry
  @typedoc """
  The language of the telemetry SDK

  ### Options

  * `:cpp` - cpp

  * `:dotnet` - dotnet

  * `:erlang` - erlang

  * `:go` - go

  * `:java` - java

  * `:nodejs` - nodejs

  * `:php` - php

  * `:python` - python

  * `:ruby` - ruby

  * `:rust` - rust

  * `:swift` - swift

  * `:webjs` - webjs

  """
  @type telemetry_sdk_language() ::
          :cpp
          | :dotnet
          | :erlang
          | :go
          | :java
          | :nodejs
          | :php
          | :python
          | :ruby
          | :rust
          | :swift
          | :webjs
          | atom()

  @doc """
  The URL of the OpenTelemetry schema for these keys and values.

      iex> OpenTelemetry.SemanticConventions.Resource.schema_url()
      "https://opentelemetry.io/schemas/1.25.0"
  """
  @spec schema_url :: String.t()
  def schema_url do
    "https://opentelemetry.io/schemas/1.25.0"
  end

  @doc namespace: :browser
  @doc """
  Array of brand name and version separated by a space

  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.brands`)

      iex> OpenTelemetry.SemanticConventions.Resource.browser_brands()
      :"browser.brands"
  """
  @spec browser_brands :: :"browser.brands"
  def browser_brands do
    :"browser.brands"
  end

  @doc namespace: :browser
  @doc """
  Preferred language of the user using the browser

  ### Notes

  This value is intended to be taken from the Navigator API `navigator.language`

      iex> OpenTelemetry.SemanticConventions.Resource.browser_language()
      :"browser.language"
  """
  @spec browser_language :: :"browser.language"
  def browser_language do
    :"browser.language"
  end

  @doc namespace: :browser
  @doc """
  A boolean that is true if the browser is running on a mobile device

  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.mobile`). If unavailable, this attribute **SHOULD** be left unset

      iex> OpenTelemetry.SemanticConventions.Resource.browser_mobile()
      :"browser.mobile"
  """
  @spec browser_mobile :: :"browser.mobile"
  def browser_mobile do
    :"browser.mobile"
  end

  @doc namespace: :browser
  @doc """
  The platform on which the browser is running

  ### Notes

  This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.platform`). If unavailable, the legacy `navigator.platform` API **SHOULD NOT** be used instead and this attribute **SHOULD** be left unset in order for the values to be consistent.
  The list of possible values is defined in the [W3C User-Agent Client Hints specification](https://wicg.github.io/ua-client-hints/#sec-ch-ua-platform). Note that some (but not all) of these values can overlap with values in the [`os.type` and `os.name` attributes](./os.md). However, for consistency, the values in the `browser.platform` attribute should capture the exact value that the user agent provides

      iex> OpenTelemetry.SemanticConventions.Resource.browser_platform()
      :"browser.platform"
  """
  @spec browser_platform :: :"browser.platform"
  def browser_platform do
    :"browser.platform"
  end

  @doc namespace: :user_agent
  @doc """
  Full user-agent string provided by the browser

  ### Notes

  The user-agent value **SHOULD** be provided only from browsers that do not have a mechanism to retrieve brands and platform individually from the User-Agent Client Hints API. To retrieve the value, the legacy `navigator.userAgent` API can be used

      iex> OpenTelemetry.SemanticConventions.Resource.user_agent_original()
      :"user_agent.original"
  """
  @spec user_agent_original :: :"user_agent.original"
  def user_agent_original do
    :"user_agent.original"
  end

  @doc namespace: :aws
  @doc """
  The ID of a running ECS task. The ID **MUST** be extracted from `task.arn`

      iex> OpenTelemetry.SemanticConventions.Resource.aws_ecs_task_id()
      :"aws.ecs.task.id"
  """
  @spec aws_ecs_task_id :: :"aws.ecs.task.id"
  def aws_ecs_task_id do
    :"aws.ecs.task.id"
  end

  @doc namespace: :aws
  @doc """
  The ARN of an [ECS cluster](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/clusters.html)

      iex> OpenTelemetry.SemanticConventions.Resource.aws_ecs_cluster_arn()
      :"aws.ecs.cluster.arn"
  """
  @spec aws_ecs_cluster_arn :: :"aws.ecs.cluster.arn"
  def aws_ecs_cluster_arn do
    :"aws.ecs.cluster.arn"
  end

  @doc namespace: :aws
  @doc """
  The Amazon Resource Name (ARN) of an [ECS container instance](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_instances.html)

      iex> OpenTelemetry.SemanticConventions.Resource.aws_ecs_container_arn()
      :"aws.ecs.container.arn"
  """
  @spec aws_ecs_container_arn :: :"aws.ecs.container.arn"
  def aws_ecs_container_arn do
    :"aws.ecs.container.arn"
  end

  @doc namespace: :aws
  @doc """
  The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task

      iex> OpenTelemetry.SemanticConventions.Resource.aws_ecs_launchtype()
      :"aws.ecs.launchtype"
  """
  @spec aws_ecs_launchtype :: :"aws.ecs.launchtype"
  def aws_ecs_launchtype do
    :"aws.ecs.launchtype"
  end

  @doc namespace: :aws
  @doc """
  The ARN of a running [ECS task](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids)

      iex> OpenTelemetry.SemanticConventions.Resource.aws_ecs_task_arn()
      :"aws.ecs.task.arn"
  """
  @spec aws_ecs_task_arn :: :"aws.ecs.task.arn"
  def aws_ecs_task_arn do
    :"aws.ecs.task.arn"
  end

  @doc namespace: :aws
  @doc """
  The family name of the [ECS task definition](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html) used to create the ECS task

      iex> OpenTelemetry.SemanticConventions.Resource.aws_ecs_task_family()
      :"aws.ecs.task.family"
  """
  @spec aws_ecs_task_family :: :"aws.ecs.task.family"
  def aws_ecs_task_family do
    :"aws.ecs.task.family"
  end

  @doc namespace: :aws
  @doc """
  The revision for the task definition used to create the ECS task

      iex> OpenTelemetry.SemanticConventions.Resource.aws_ecs_task_revision()
      :"aws.ecs.task.revision"
  """
  @spec aws_ecs_task_revision :: :"aws.ecs.task.revision"
  def aws_ecs_task_revision do
    :"aws.ecs.task.revision"
  end

  @doc namespace: :aws
  @doc """
  The ARN of an EKS cluster

      iex> OpenTelemetry.SemanticConventions.Resource.aws_eks_cluster_arn()
      :"aws.eks.cluster.arn"
  """
  @spec aws_eks_cluster_arn :: :"aws.eks.cluster.arn"
  def aws_eks_cluster_arn do
    :"aws.eks.cluster.arn"
  end

  @doc namespace: :aws
  @doc """
  The Amazon Resource Name(s) (ARN) of the AWS log group(s)

  ### Notes

  See the [log group ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format)

      iex> OpenTelemetry.SemanticConventions.Resource.aws_log_group_arns()
      :"aws.log.group.arns"
  """
  @spec aws_log_group_arns :: :"aws.log.group.arns"
  def aws_log_group_arns do
    :"aws.log.group.arns"
  end

  @doc namespace: :aws
  @doc """
  The name(s) of the AWS log group(s) an application is writing to

  ### Notes

  Multiple log groups must be supported for cases like multi-container applications, where a single application has sidecar containers, and each write to their own log group

      iex> OpenTelemetry.SemanticConventions.Resource.aws_log_group_names()
      :"aws.log.group.names"
  """
  @spec aws_log_group_names :: :"aws.log.group.names"
  def aws_log_group_names do
    :"aws.log.group.names"
  end

  @doc namespace: :aws
  @doc """
  The ARN(s) of the AWS log stream(s)

  ### Notes

  See the [log stream ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format). One log group can contain several log streams, so these ARNs necessarily identify both a log group and a log stream

      iex> OpenTelemetry.SemanticConventions.Resource.aws_log_stream_arns()
      :"aws.log.stream.arns"
  """
  @spec aws_log_stream_arns :: :"aws.log.stream.arns"
  def aws_log_stream_arns do
    :"aws.log.stream.arns"
  end

  @doc namespace: :aws
  @doc """
  The name(s) of the AWS log stream(s) an application is writing to

      iex> OpenTelemetry.SemanticConventions.Resource.aws_log_stream_names()
      :"aws.log.stream.names"
  """
  @spec aws_log_stream_names :: :"aws.log.stream.names"
  def aws_log_stream_names do
    :"aws.log.stream.names"
  end

  @doc namespace: :gcp
  @doc """
  The name of the Cloud Run [execution](https://cloud.google.com/run/docs/managing/job-executions) being run for the Job, as set by the [`CLOUD_RUN_EXECUTION`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable

      iex> OpenTelemetry.SemanticConventions.Resource.gcp_cloud_run_job_execution()
      :"gcp.cloud_run.job.execution"
  """
  @spec gcp_cloud_run_job_execution :: :"gcp.cloud_run.job.execution"
  def gcp_cloud_run_job_execution do
    :"gcp.cloud_run.job.execution"
  end

  @doc namespace: :gcp
  @doc """
  The index for a task within an execution as provided by the [`CLOUD_RUN_TASK_INDEX`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable

      iex> OpenTelemetry.SemanticConventions.Resource.gcp_cloud_run_job_task_index()
      :"gcp.cloud_run.job.task_index"
  """
  @spec gcp_cloud_run_job_task_index :: :"gcp.cloud_run.job.task_index"
  def gcp_cloud_run_job_task_index do
    :"gcp.cloud_run.job.task_index"
  end

  @doc namespace: :gcp
  @doc """
  The hostname of a GCE instance. This is the full value of the default or [custom hostname](https://cloud.google.com/compute/docs/instances/custom-hostname-vm)

      iex> OpenTelemetry.SemanticConventions.Resource.gcp_gce_instance_hostname()
      :"gcp.gce.instance.hostname"
  """
  @spec gcp_gce_instance_hostname :: :"gcp.gce.instance.hostname"
  def gcp_gce_instance_hostname do
    :"gcp.gce.instance.hostname"
  end

  @doc namespace: :gcp
  @doc """
  The instance name of a GCE instance. This is the value provided by `host.name`, the visible name of the instance in the Cloud Console UI, and the prefix for the default hostname of the instance as defined by the [default internal DNS name](https://cloud.google.com/compute/docs/internal-dns#instance-fully-qualified-domain-names)

      iex> OpenTelemetry.SemanticConventions.Resource.gcp_gce_instance_name()
      :"gcp.gce.instance.name"
  """
  @spec gcp_gce_instance_name :: :"gcp.gce.instance.name"
  def gcp_gce_instance_name do
    :"gcp.gce.instance.name"
  end

  @doc namespace: :heroku
  @doc """
  Unique identifier for the application

      iex> OpenTelemetry.SemanticConventions.Resource.heroku_app_id()
      :"heroku.app.id"
  """
  @spec heroku_app_id :: :"heroku.app.id"
  def heroku_app_id do
    :"heroku.app.id"
  end

  @doc namespace: :heroku
  @doc """
  Commit hash for the current release

      iex> OpenTelemetry.SemanticConventions.Resource.heroku_release_commit()
      :"heroku.release.commit"
  """
  @spec heroku_release_commit :: :"heroku.release.commit"
  def heroku_release_commit do
    :"heroku.release.commit"
  end

  @doc namespace: :heroku
  @doc """
  Time and date the release was created

      iex> OpenTelemetry.SemanticConventions.Resource.heroku_release_creation_timestamp()
      :"heroku.release.creation_timestamp"
  """
  @spec heroku_release_creation_timestamp :: :"heroku.release.creation_timestamp"
  def heroku_release_creation_timestamp do
    :"heroku.release.creation_timestamp"
  end

  @doc namespace: :container
  @doc """
  Container ID. Usually a UUID, as for example used to [identify Docker containers](https://docs.docker.com/engine/reference/run/#container-identification). The UUID might be abbreviated

      iex> OpenTelemetry.SemanticConventions.Resource.container_id()
      :"container.id"
  """
  @spec container_id :: :"container.id"
  def container_id do
    :"container.id"
  end

  @doc namespace: :container
  @doc """
  Runtime specific image identifier. Usually a hash algorithm followed by a UUID

  ### Notes

  Docker defines a sha256 of the image id; `container.image.id` corresponds to the `Image` field from the Docker container inspect [API](https://docs.docker.com/engine/api/v1.43/#tag/Container/operation/ContainerInspect) endpoint.
  K8s defines a link to the container registry repository with digest `"imageID": "registry.azurecr.io /namespace/service/dockerfile@sha256:bdeabd40c3a8a492eaf9e8e44d0ebbb84bac7ee25ac0cf8a7159d25f62555625"`.
  The ID is assinged by the container runtime and can vary in different environments. Consider using `oci.manifest.digest` if it is important to identify the same image in different environments/runtimes

      iex> OpenTelemetry.SemanticConventions.Resource.container_image_id()
      :"container.image.id"
  """
  @spec container_image_id :: :"container.image.id"
  def container_image_id do
    :"container.image.id"
  end

  @doc namespace: :container
  @doc """
  Name of the image the container was built on

      iex> OpenTelemetry.SemanticConventions.Resource.container_image_name()
      :"container.image.name"
  """
  @spec container_image_name :: :"container.image.name"
  def container_image_name do
    :"container.image.name"
  end

  @doc namespace: :container
  @doc """
  Repo digests of the container image as provided by the container runtime

  ### Notes

  [Docker](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect) and [CRI](https://github.com/kubernetes/cri-api/blob/c75ef5b473bbe2d0a4fc92f82235efd665ea8e9f/pkg/apis/runtime/v1/api.proto#L1237-L1238) report those under the `RepoDigests` field

      iex> OpenTelemetry.SemanticConventions.Resource.container_image_repo_digests()
      :"container.image.repo_digests"
  """
  @spec container_image_repo_digests :: :"container.image.repo_digests"
  def container_image_repo_digests do
    :"container.image.repo_digests"
  end

  @doc namespace: :container
  @doc """
  Container image tags. An example can be found in [Docker Image Inspect](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect). Should be only the `<tag>` section of the full name for example from `registry.example.com/my-org/my-image:<tag>`

      iex> OpenTelemetry.SemanticConventions.Resource.container_image_tags()
      :"container.image.tags"
  """
  @spec container_image_tags :: :"container.image.tags"
  def container_image_tags do
    :"container.image.tags"
  end

  @doc namespace: :container
  @doc """
  Container name used by container runtime

      iex> OpenTelemetry.SemanticConventions.Resource.container_name()
      :"container.name"
  """
  @spec container_name :: :"container.name"
  def container_name do
    :"container.name"
  end

  @doc namespace: :container
  @doc """
  The container runtime managing this container

      iex> OpenTelemetry.SemanticConventions.Resource.container_runtime()
      :"container.runtime"
  """
  @spec container_runtime :: :"container.runtime"
  def container_runtime do
    :"container.runtime"
  end

  @doc namespace: :oci
  @doc """
  The digest of the OCI image manifest. For container images specifically is the digest by which the container image is known

  ### Notes

  Follows [OCI Image Manifest Specification](https://github.com/opencontainers/image-spec/blob/main/manifest.md), and specifically the [Digest property](https://github.com/opencontainers/image-spec/blob/main/descriptor.md#digests).
  An example can be found in [Example Image Manifest](https://docs.docker.com/registry/spec/manifest-v2-2/#example-image-manifest)

      iex> OpenTelemetry.SemanticConventions.Resource.oci_manifest_digest()
      :"oci.manifest.digest"
  """
  @spec oci_manifest_digest :: :"oci.manifest.digest"
  def oci_manifest_digest do
    :"oci.manifest.digest"
  end

  @doc namespace: :container
  @doc """
  The command used to run the container (i.e. the command name)

  ### Notes

  If using embedded credentials or sensitive data, it is recommended to remove them to prevent potential leakage

      iex> OpenTelemetry.SemanticConventions.Resource.container_command()
      :"container.command"
  """
  @spec container_command :: :"container.command"
  def container_command do
    :"container.command"
  end

  @doc namespace: :container
  @doc """
  All the command arguments (including the command/executable itself) run by the container. [2]

      iex> OpenTelemetry.SemanticConventions.Resource.container_command_args()
      :"container.command_args"
  """
  @spec container_command_args :: :"container.command_args"
  def container_command_args do
    :"container.command_args"
  end

  @doc namespace: :container
  @doc """
  The full command run by the container as a single string representing the full command. [2]

      iex> OpenTelemetry.SemanticConventions.Resource.container_command_line()
      :"container.command_line"
  """
  @spec container_command_line :: :"container.command_line"
  def container_command_line do
    :"container.command_line"
  end

  @doc namespace: :deployment
  @doc """
  Name of the [deployment environment](https://wikipedia.org/wiki/Deployment_environment) (aka deployment tier)

  ### Notes

  `deployment.environment` does not affect the uniqueness constraints defined through
  the `service.namespace`, `service.name` and `service.instance.id` resource attributes.
  This implies that resources carrying the following attribute combinations **MUST** be
  considered to be identifying the same service:

  * `service.name=frontend`, `deployment.environment=production`
  * `service.name=frontend`, `deployment.environment=staging`

      iex> OpenTelemetry.SemanticConventions.Resource.deployment_environment()
      :"deployment.environment"
  """
  @spec deployment_environment :: :"deployment.environment"
  def deployment_environment do
    :"deployment.environment"
  end

  @doc namespace: :device
  @doc """
  A unique identifier representing the device

  ### Notes

  The device identifier **MUST** only be defined using the values outlined below. This value is not an advertising identifier and **MUST NOT** be used as such. On iOS (Swift or Objective-C), this value **MUST** be equal to the [vendor identifier](https://developer.apple.com/documentation/uikit/uidevice/1620059-identifierforvendor). On Android (Java or Kotlin), this value **MUST** be equal to the Firebase Installation ID or a globally unique UUID which is persisted across sessions in your application. More information can be found [here](https://developer.android.com/training/articles/user-data-ids) on best practices and exact implementation details. Caution should be taken when storing personal data or anything which can identify a user. GDPR and data protection laws may apply, ensure you do your own due diligence

      iex> OpenTelemetry.SemanticConventions.Resource.device_id()
      :"device.id"
  """
  @spec device_id :: :"device.id"
  def device_id do
    :"device.id"
  end

  @doc namespace: :device
  @doc """
  The name of the device manufacturer

  ### Notes

  The Android OS provides this field via [Build](https://developer.android.com/reference/android/os/Build#MANUFACTURER). iOS apps **SHOULD** hardcode the value `Apple`

      iex> OpenTelemetry.SemanticConventions.Resource.device_manufacturer()
      :"device.manufacturer"
  """
  @spec device_manufacturer :: :"device.manufacturer"
  def device_manufacturer do
    :"device.manufacturer"
  end

  @doc namespace: :device
  @doc """
  The model identifier for the device

  ### Notes

  It's recommended this value represents a machine-readable version of the model identifier rather than the market or consumer-friendly name of the device

      iex> OpenTelemetry.SemanticConventions.Resource.device_model_identifier()
      :"device.model.identifier"
  """
  @spec device_model_identifier :: :"device.model.identifier"
  def device_model_identifier do
    :"device.model.identifier"
  end

  @doc namespace: :device
  @doc """
  The marketing name for the device model

  ### Notes

  It's recommended this value represents a human-readable version of the device model rather than a machine-readable alternative

      iex> OpenTelemetry.SemanticConventions.Resource.device_model_name()
      :"device.model.name"
  """
  @spec device_model_name :: :"device.model.name"
  def device_model_name do
    :"device.model.name"
  end

  @doc namespace: :faas
  @doc """
  The name of the single function that this runtime instance executes

  ### Notes

  This is the name of the function as configured/deployed on the FaaS
  platform and is usually different from the name of the callback
  function (which may be stored in the
  [`code.namespace`/`code.function`](/docs/general/attributes.md#source-code-attributes)
  span attributes).

  For some cloud providers, the above definition is ambiguous. The following
  definition of function name **MUST** be used for this attribute
  (and consequently the span name) for the listed cloud providers/products:

  * **Azure:**  The full name `<FUNCAPP>/<FUNC>`, i.e., function app name
    followed by a forward slash followed by the function name (this form
    can also be seen in the resource JSON for the function).
    This means that a span attribute **MUST** be used, as an Azure function
    app can host multiple functions that would usually share
    a TracerProvider (see also the `cloud.resource_id` attribute)

      iex> OpenTelemetry.SemanticConventions.Resource.faas_name()
      :"faas.name"
  """
  @spec faas_name :: :"faas.name"
  def faas_name do
    :"faas.name"
  end

  @doc namespace: :faas
  @doc """
  The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version

  ### Notes

  * **AWS Lambda:** Use the (full) log stream name

      iex> OpenTelemetry.SemanticConventions.Resource.faas_instance()
      :"faas.instance"
  """
  @spec faas_instance :: :"faas.instance"
  def faas_instance do
    :"faas.instance"
  end

  @doc namespace: :faas
  @doc """
  The amount of memory available to the serverless function converted to Bytes

  ### Notes

  It's recommended to set this attribute since e.g. too little memory can easily stop a Java AWS Lambda function from working correctly. On AWS Lambda, the environment variable `AWS_LAMBDA_FUNCTION_MEMORY_SIZE` provides this information (which must be multiplied by 1,048,576)

      iex> OpenTelemetry.SemanticConventions.Resource.faas_max_memory()
      :"faas.max_memory"
  """
  @spec faas_max_memory :: :"faas.max_memory"
  def faas_max_memory do
    :"faas.max_memory"
  end

  @doc namespace: :faas
  @doc """
  The immutable version of the function being executed

  ### Notes

  Depending on the cloud provider and platform, use:

  * **AWS Lambda:** The [function version](https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html)
    (an integer represented as a decimal string).
  * **Google Cloud Run (Services):** The [revision](https://cloud.google.com/run/docs/managing/revisions)
    (i.e., the function name plus the revision suffix).
  * **Google Cloud Functions:** The value of the
    [`K_REVISION` environment variable](https://cloud.google.com/functions/docs/env-var#runtime_environment_variables_set_automatically).
  * **Azure Functions:** Not applicable. Do not set this attribute

      iex> OpenTelemetry.SemanticConventions.Resource.faas_version()
      :"faas.version"
  """
  @spec faas_version :: :"faas.version"
  def faas_version do
    :"faas.version"
  end

  @doc namespace: :host
  @doc """
  The CPU architecture the host system is running on

      iex> OpenTelemetry.SemanticConventions.Resource.host_arch()
      :"host.arch"
  """
  @spec host_arch :: :"host.arch"
  def host_arch do
    :"host.arch"
  end

  @doc namespace: :host
  @doc """
  Unique host ID. For Cloud, this must be the instance_id assigned by the cloud provider. For non-containerized systems, this should be the `machine-id`. See the table below for the sources to use to determine the `machine-id` based on operating system

      iex> OpenTelemetry.SemanticConventions.Resource.host_id()
      :"host.id"
  """
  @spec host_id :: :"host.id"
  def host_id do
    :"host.id"
  end

  @doc namespace: :host
  @doc """
  VM image ID or host OS image ID. For Cloud, this value is from the provider

      iex> OpenTelemetry.SemanticConventions.Resource.host_image_id()
      :"host.image.id"
  """
  @spec host_image_id :: :"host.image.id"
  def host_image_id do
    :"host.image.id"
  end

  @doc namespace: :host
  @doc """
  Name of the VM image or OS install the host was instantiated from

      iex> OpenTelemetry.SemanticConventions.Resource.host_image_name()
      :"host.image.name"
  """
  @spec host_image_name :: :"host.image.name"
  def host_image_name do
    :"host.image.name"
  end

  @doc namespace: :host
  @doc """
  The version string of the VM image or host OS as defined in [Version Attributes](/docs/resource/README.md#version-attributes)

      iex> OpenTelemetry.SemanticConventions.Resource.host_image_version()
      :"host.image.version"
  """
  @spec host_image_version :: :"host.image.version"
  def host_image_version do
    :"host.image.version"
  end

  @doc namespace: :host
  @doc """
  Name of the host. On Unix systems, it may contain what the hostname command returns, or the fully qualified hostname, or another name specified by the user

      iex> OpenTelemetry.SemanticConventions.Resource.host_name()
      :"host.name"
  """
  @spec host_name :: :"host.name"
  def host_name do
    :"host.name"
  end

  @doc namespace: :host
  @doc """
  Type of host. For Cloud, this must be the machine type

      iex> OpenTelemetry.SemanticConventions.Resource.host_type()
      :"host.type"
  """
  @spec host_type :: :"host.type"
  def host_type do
    :"host.type"
  end

  @doc namespace: :host
  @doc """
  Available IP addresses of the host, excluding loopback interfaces

  ### Notes

  IPv4 Addresses **MUST** be specified in dotted-quad notation. IPv6 addresses **MUST** be specified in the [RFC 5952](https://www.rfc-editor.org/rfc/rfc5952.html) format

      iex> OpenTelemetry.SemanticConventions.Resource.host_ip()
      :"host.ip"
  """
  @spec host_ip :: :"host.ip"
  def host_ip do
    :"host.ip"
  end

  @doc namespace: :host
  @doc """
  Available MAC addresses of the host, excluding loopback interfaces

  ### Notes

  MAC Addresses **MUST** be represented in [IEEE RA hexadecimal form](https://standards.ieee.org/wp-content/uploads/import/documents/tutorials/eui.pdf): as hyphen-separated octets in uppercase hexadecimal form from most to least significant

      iex> OpenTelemetry.SemanticConventions.Resource.host_mac()
      :"host.mac"
  """
  @spec host_mac :: :"host.mac"
  def host_mac do
    :"host.mac"
  end

  @doc namespace: :host
  @doc """
  The amount of level 2 memory cache available to the processor (in Bytes)

      iex> OpenTelemetry.SemanticConventions.Resource.host_cpu_cache_l2_size()
      :"host.cpu.cache.l2.size"
  """
  @spec host_cpu_cache_l2_size :: :"host.cpu.cache.l2.size"
  def host_cpu_cache_l2_size do
    :"host.cpu.cache.l2.size"
  end

  @doc namespace: :host
  @doc """
  Family or generation of the CPU

      iex> OpenTelemetry.SemanticConventions.Resource.host_cpu_family()
      :"host.cpu.family"
  """
  @spec host_cpu_family :: :"host.cpu.family"
  def host_cpu_family do
    :"host.cpu.family"
  end

  @doc namespace: :host
  @doc """
  Model identifier. It provides more granular information about the CPU, distinguishing it from other CPUs within the same family

      iex> OpenTelemetry.SemanticConventions.Resource.host_cpu_model_id()
      :"host.cpu.model.id"
  """
  @spec host_cpu_model_id :: :"host.cpu.model.id"
  def host_cpu_model_id do
    :"host.cpu.model.id"
  end

  @doc namespace: :host
  @doc """
  Model designation of the processor

      iex> OpenTelemetry.SemanticConventions.Resource.host_cpu_model_name()
      :"host.cpu.model.name"
  """
  @spec host_cpu_model_name :: :"host.cpu.model.name"
  def host_cpu_model_name do
    :"host.cpu.model.name"
  end

  @doc namespace: :host
  @doc """
  Stepping or core revisions

      iex> OpenTelemetry.SemanticConventions.Resource.host_cpu_stepping()
      :"host.cpu.stepping"
  """
  @spec host_cpu_stepping :: :"host.cpu.stepping"
  def host_cpu_stepping do
    :"host.cpu.stepping"
  end

  @doc namespace: :host
  @doc """
  Processor manufacturer identifier. A maximum 12-character string

  ### Notes

  [CPUID](https://wiki.osdev.org/CPUID) command returns the vendor ID string in EBX, EDX and ECX registers. Writing these to memory in this order results in a 12-character string

      iex> OpenTelemetry.SemanticConventions.Resource.host_cpu_vendor_id()
      :"host.cpu.vendor.id"
  """
  @spec host_cpu_vendor_id :: :"host.cpu.vendor.id"
  def host_cpu_vendor_id do
    :"host.cpu.vendor.id"
  end

  @doc namespace: :k8s
  @doc """
  The name of the cluster

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_cluster_name()
      :"k8s.cluster.name"
  """
  @spec k8s_cluster_name :: :"k8s.cluster.name"
  def k8s_cluster_name do
    :"k8s.cluster.name"
  end

  @doc namespace: :k8s
  @doc """
  A pseudo-ID for the cluster, set to the UID of the `kube-system` namespace

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
  conflict

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_cluster_uid()
      :"k8s.cluster.uid"
  """
  @spec k8s_cluster_uid :: :"k8s.cluster.uid"
  def k8s_cluster_uid do
    :"k8s.cluster.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the Node

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_node_name()
      :"k8s.node.name"
  """
  @spec k8s_node_name :: :"k8s.node.name"
  def k8s_node_name do
    :"k8s.node.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the Node

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_node_uid()
      :"k8s.node.uid"
  """
  @spec k8s_node_uid :: :"k8s.node.uid"
  def k8s_node_uid do
    :"k8s.node.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the namespace that the pod is running in

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_namespace_name()
      :"k8s.namespace.name"
  """
  @spec k8s_namespace_name :: :"k8s.namespace.name"
  def k8s_namespace_name do
    :"k8s.namespace.name"
  end

  @doc namespace: :k8s
  @doc """
  The name of the Pod

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_pod_name()
      :"k8s.pod.name"
  """
  @spec k8s_pod_name :: :"k8s.pod.name"
  def k8s_pod_name do
    :"k8s.pod.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the Pod

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_pod_uid()
      :"k8s.pod.uid"
  """
  @spec k8s_pod_uid :: :"k8s.pod.uid"
  def k8s_pod_uid do
    :"k8s.pod.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the Container from Pod specification, must be unique within a Pod. Container runtime usually uses different globally unique name (`container.name`)

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_container_name()
      :"k8s.container.name"
  """
  @spec k8s_container_name :: :"k8s.container.name"
  def k8s_container_name do
    :"k8s.container.name"
  end

  @doc namespace: :k8s
  @doc """
  Number of times the container was restarted. This attribute can be used to identify a particular container (running or stopped) within a container spec

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_container_restart_count()
      :"k8s.container.restart_count"
  """
  @spec k8s_container_restart_count :: :"k8s.container.restart_count"
  def k8s_container_restart_count do
    :"k8s.container.restart_count"
  end

  @doc namespace: :k8s
  @doc """
  The name of the ReplicaSet

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_replicaset_name()
      :"k8s.replicaset.name"
  """
  @spec k8s_replicaset_name :: :"k8s.replicaset.name"
  def k8s_replicaset_name do
    :"k8s.replicaset.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the ReplicaSet

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_replicaset_uid()
      :"k8s.replicaset.uid"
  """
  @spec k8s_replicaset_uid :: :"k8s.replicaset.uid"
  def k8s_replicaset_uid do
    :"k8s.replicaset.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the Deployment

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_deployment_name()
      :"k8s.deployment.name"
  """
  @spec k8s_deployment_name :: :"k8s.deployment.name"
  def k8s_deployment_name do
    :"k8s.deployment.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the Deployment

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_deployment_uid()
      :"k8s.deployment.uid"
  """
  @spec k8s_deployment_uid :: :"k8s.deployment.uid"
  def k8s_deployment_uid do
    :"k8s.deployment.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the StatefulSet

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_statefulset_name()
      :"k8s.statefulset.name"
  """
  @spec k8s_statefulset_name :: :"k8s.statefulset.name"
  def k8s_statefulset_name do
    :"k8s.statefulset.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the StatefulSet

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_statefulset_uid()
      :"k8s.statefulset.uid"
  """
  @spec k8s_statefulset_uid :: :"k8s.statefulset.uid"
  def k8s_statefulset_uid do
    :"k8s.statefulset.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the DaemonSet

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_daemonset_name()
      :"k8s.daemonset.name"
  """
  @spec k8s_daemonset_name :: :"k8s.daemonset.name"
  def k8s_daemonset_name do
    :"k8s.daemonset.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the DaemonSet

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_daemonset_uid()
      :"k8s.daemonset.uid"
  """
  @spec k8s_daemonset_uid :: :"k8s.daemonset.uid"
  def k8s_daemonset_uid do
    :"k8s.daemonset.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the Job

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_job_name()
      :"k8s.job.name"
  """
  @spec k8s_job_name :: :"k8s.job.name"
  def k8s_job_name do
    :"k8s.job.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the Job

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_job_uid()
      :"k8s.job.uid"
  """
  @spec k8s_job_uid :: :"k8s.job.uid"
  def k8s_job_uid do
    :"k8s.job.uid"
  end

  @doc namespace: :k8s
  @doc """
  The name of the CronJob

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_cronjob_name()
      :"k8s.cronjob.name"
  """
  @spec k8s_cronjob_name :: :"k8s.cronjob.name"
  def k8s_cronjob_name do
    :"k8s.cronjob.name"
  end

  @doc namespace: :k8s
  @doc """
  The UID of the CronJob

      iex> OpenTelemetry.SemanticConventions.Resource.k8s_cronjob_uid()
      :"k8s.cronjob.uid"
  """
  @spec k8s_cronjob_uid :: :"k8s.cronjob.uid"
  def k8s_cronjob_uid do
    :"k8s.cronjob.uid"
  end

  @doc namespace: :process
  @doc """
  The command used to launch the process (i.e. the command name). On Linux based systems, can be set to the zeroth string in `proc/[pid]/cmdline`. On Windows, can be set to the first parameter extracted from `GetCommandLineW`

      iex> OpenTelemetry.SemanticConventions.Resource.process_command()
      :"process.command"
  """
  @spec process_command :: :"process.command"
  def process_command do
    :"process.command"
  end

  @doc namespace: :process
  @doc """
  All the command arguments (including the command/executable itself) as received by the process. On Linux-based systems (and some other Unixoid systems supporting procfs), can be set according to the list of null-delimited strings extracted from `proc/[pid]/cmdline`. For libc-based executables, this would be the full argv vector passed to `main`

      iex> OpenTelemetry.SemanticConventions.Resource.process_command_args()
      :"process.command_args"
  """
  @spec process_command_args :: :"process.command_args"
  def process_command_args do
    :"process.command_args"
  end

  @doc namespace: :process
  @doc """
  The full command used to launch the process as a single string representing the full command. On Windows, can be set to the result of `GetCommandLineW`. Do not set this if you have to assemble it just for monitoring; use `process.command_args` instead

      iex> OpenTelemetry.SemanticConventions.Resource.process_command_line()
      :"process.command_line"
  """
  @spec process_command_line :: :"process.command_line"
  def process_command_line do
    :"process.command_line"
  end

  @doc namespace: :process
  @doc """
  The name of the process executable. On Linux based systems, can be set to the `Name` in `proc/[pid]/status`. On Windows, can be set to the base name of `GetProcessImageFileNameW`

      iex> OpenTelemetry.SemanticConventions.Resource.process_executable_name()
      :"process.executable.name"
  """
  @spec process_executable_name :: :"process.executable.name"
  def process_executable_name do
    :"process.executable.name"
  end

  @doc namespace: :process
  @doc """
  The full path to the process executable. On Linux based systems, can be set to the target of `proc/[pid]/exe`. On Windows, can be set to the result of `GetProcessImageFileNameW`

      iex> OpenTelemetry.SemanticConventions.Resource.process_executable_path()
      :"process.executable.path"
  """
  @spec process_executable_path :: :"process.executable.path"
  def process_executable_path do
    :"process.executable.path"
  end

  @doc namespace: :process
  @doc """
  The username of the user that owns the process

      iex> OpenTelemetry.SemanticConventions.Resource.process_owner()
      :"process.owner"
  """
  @spec process_owner :: :"process.owner"
  def process_owner do
    :"process.owner"
  end

  @doc namespace: :process
  @doc """
  Parent Process identifier (PPID)

      iex> OpenTelemetry.SemanticConventions.Resource.process_parent_pid()
      :"process.parent_pid"
  """
  @spec process_parent_pid :: :"process.parent_pid"
  def process_parent_pid do
    :"process.parent_pid"
  end

  @doc namespace: :process
  @doc """
  Process identifier (PID)

      iex> OpenTelemetry.SemanticConventions.Resource.process_pid()
      :"process.pid"
  """
  @spec process_pid :: :"process.pid"
  def process_pid do
    :"process.pid"
  end

  @doc namespace: :process
  @doc """
  An additional description about the runtime of the process, for example a specific vendor customization of the runtime environment

      iex> OpenTelemetry.SemanticConventions.Resource.process_runtime_description()
      :"process.runtime.description"
  """
  @spec process_runtime_description :: :"process.runtime.description"
  def process_runtime_description do
    :"process.runtime.description"
  end

  @doc namespace: :process
  @doc """
  The name of the runtime of this process. For compiled native binaries, this **SHOULD** be the name of the compiler

      iex> OpenTelemetry.SemanticConventions.Resource.process_runtime_name()
      :"process.runtime.name"
  """
  @spec process_runtime_name :: :"process.runtime.name"
  def process_runtime_name do
    :"process.runtime.name"
  end

  @doc namespace: :process
  @doc """
  The version of the runtime of this process, as returned by the runtime without modification

      iex> OpenTelemetry.SemanticConventions.Resource.process_runtime_version()
      :"process.runtime.version"
  """
  @spec process_runtime_version :: :"process.runtime.version"
  def process_runtime_version do
    :"process.runtime.version"
  end

  @doc namespace: :service
  @doc """
  Logical name of the service

  ### Notes

  **MUST** be the same for all instances of horizontally scaled services. If the value was not specified, SDKs **MUST** fallback to `unknown_service:` concatenated with [`process.executable.name`](process.md#process), e.g. `unknown_service:bash`. If `process.executable.name` is not available, the value **MUST** be set to `unknown_service`

      iex> OpenTelemetry.SemanticConventions.Resource.service_name()
      :"service.name"
  """
  @spec service_name :: :"service.name"
  def service_name do
    :"service.name"
  end

  @doc namespace: :service
  @doc """
  The version string of the service API or implementation. The format is not defined by these conventions

      iex> OpenTelemetry.SemanticConventions.Resource.service_version()
      :"service.version"
  """
  @spec service_version :: :"service.version"
  def service_version do
    :"service.version"
  end

  @doc namespace: :service
  @doc """
  The string ID of the service instance

  ### Notes

  **MUST** be unique for each instance of the same `service.namespace,service.name` pair (in other words
  `service.namespace,service.name,service.instance.id` triplet **MUST** be globally unique). The ID helps to
  distinguish instances of the same service that exist at the same time (e.g. instances of a horizontally scaled
  service).

  Implementations, such as SDKs, are recommended to generate a random Version 1 or Version 4 [RFC
  4122](https://www.ietf.org/rfc/rfc4122.txt) UUID, but are free to use an inherent unique ID as the source of
  this value if stability is desirable. In that case, the ID **SHOULD** be used as source of a UUID Version 5 and
  **SHOULD** use the following UUID as the namespace: `4d63009a-8d0f-11ee-aad7-4c796ed8e320`.

  UUIDs are typically recommended, as only an opaque value for the purposes of identifying a service instance is
  needed. Similar to what can be seen in the man page for the
  [`/etc/machine-id`](https://www.freedesktop.org/software/systemd/man/machine-id.html) file, the underlying
  data, such as pod name and namespace should be treated as confidential, being the user's choice to expose it
  or not via another resource attribute.

  For applications running behind an application server (like unicorn), we do not recommend using one identifier
  for all processes participating in the application. Instead, it's recommended each division (e.g. a worker
  thread in unicorn) to have its own instance.id.

  It's not recommended for a Collector to set `service.instance.id` if it can't unambiguously determine the
  service instance that is generating that telemetry. For instance, creating an UUID based on `pod.name` will
  likely be wrong, as the Collector might not know from which container within that pod the telemetry originated.
  However, Collectors can set the `service.instance.id` if they can unambiguously determine the service instance
  for that telemetry. This is typically the case for scraping receivers, as they know the target address and
  port

      iex> OpenTelemetry.SemanticConventions.Resource.service_instance_id()
      :"service.instance.id"
  """
  @spec service_instance_id :: :"service.instance.id"
  def service_instance_id do
    :"service.instance.id"
  end

  @doc namespace: :service
  @doc """
  A namespace for `service.name`

  ### Notes

  A string value having a meaning that helps to distinguish a group of services, for example the team name that owns a group of services. `service.name` is expected to be unique within the same namespace. If `service.namespace` is not specified in the Resource then `service.name` is expected to be unique for all services that have no explicit namespace defined (so the empty/unspecified namespace is simply one more valid namespace). Zero-length namespace string is assumed equal to unspecified namespace

      iex> OpenTelemetry.SemanticConventions.Resource.service_namespace()
      :"service.namespace"
  """
  @spec service_namespace :: :"service.namespace"
  def service_namespace do
    :"service.namespace"
  end

  @doc namespace: :telemetry
  @doc """
  The language of the telemetry SDK

      iex> OpenTelemetry.SemanticConventions.Resource.telemetry_sdk_language()
      :"telemetry.sdk.language"
  """
  @spec telemetry_sdk_language :: :"telemetry.sdk.language"
  def telemetry_sdk_language do
    :"telemetry.sdk.language"
  end

  @doc namespace: :telemetry
  @doc """
  The name of the telemetry SDK as defined above

  ### Notes

  The OpenTelemetry SDK **MUST** set the `telemetry.sdk.name` attribute to `opentelemetry`.
  If another SDK, like a fork or a vendor-provided implementation, is used, this SDK **MUST** set the
  `telemetry.sdk.name` attribute to the fully-qualified class or module name of this SDK's main entry point
  or another suitable identifier depending on the language.
  The identifier `opentelemetry` is reserved and **MUST NOT** be used in this case.
  All custom identifiers **SHOULD** be stable across different versions of an implementation

      iex> OpenTelemetry.SemanticConventions.Resource.telemetry_sdk_name()
      :"telemetry.sdk.name"
  """
  @spec telemetry_sdk_name :: :"telemetry.sdk.name"
  def telemetry_sdk_name do
    :"telemetry.sdk.name"
  end

  @doc namespace: :telemetry
  @doc """
  The version string of the telemetry SDK

      iex> OpenTelemetry.SemanticConventions.Resource.telemetry_sdk_version()
      :"telemetry.sdk.version"
  """
  @spec telemetry_sdk_version :: :"telemetry.sdk.version"
  def telemetry_sdk_version do
    :"telemetry.sdk.version"
  end

  @doc namespace: :telemetry
  @doc """
  The name of the auto instrumentation agent or distribution, if used

  ### Notes

  Official auto instrumentation agents and distributions **SHOULD** set the `telemetry.distro.name` attribute to
  a string starting with `opentelemetry-`, e.g. `opentelemetry-java-instrumentation`

      iex> OpenTelemetry.SemanticConventions.Resource.telemetry_distro_name()
      :"telemetry.distro.name"
  """
  @spec telemetry_distro_name :: :"telemetry.distro.name"
  def telemetry_distro_name do
    :"telemetry.distro.name"
  end

  @doc namespace: :telemetry
  @doc """
  The version string of the auto instrumentation agent or distribution, if used

      iex> OpenTelemetry.SemanticConventions.Resource.telemetry_distro_version()
      :"telemetry.distro.version"
  """
  @spec telemetry_distro_version :: :"telemetry.distro.version"
  def telemetry_distro_version do
    :"telemetry.distro.version"
  end

  @doc namespace: :webengine
  @doc """
  The name of the web engine

      iex> OpenTelemetry.SemanticConventions.Resource.webengine_name()
      :"webengine.name"
  """
  @spec webengine_name :: :"webengine.name"
  def webengine_name do
    :"webengine.name"
  end

  @doc namespace: :webengine
  @doc """
  Additional description of the web engine (e.g. detailed version and edition information)

      iex> OpenTelemetry.SemanticConventions.Resource.webengine_description()
      :"webengine.description"
  """
  @spec webengine_description :: :"webengine.description"
  def webengine_description do
    :"webengine.description"
  end

  @doc namespace: :webengine
  @doc """
  The version of the web engine

      iex> OpenTelemetry.SemanticConventions.Resource.webengine_version()
      :"webengine.version"
  """
  @spec webengine_version :: :"webengine.version"
  def webengine_version do
    :"webengine.version"
  end

  @doc namespace: :otel
  @doc """
  The name of the instrumentation scope - (`InstrumentationScope.Name` in OTLP)

      iex> OpenTelemetry.SemanticConventions.Resource.otel_scope_name()
      :"otel.scope.name"
  """
  @spec otel_scope_name :: :"otel.scope.name"
  def otel_scope_name do
    :"otel.scope.name"
  end

  @doc namespace: :otel
  @doc """
  The version of the instrumentation scope - (`InstrumentationScope.Version` in OTLP)

      iex> OpenTelemetry.SemanticConventions.Resource.otel_scope_version()
      :"otel.scope.version"
  """
  @spec otel_scope_version :: :"otel.scope.version"
  def otel_scope_version do
    :"otel.scope.version"
  end

  @doc namespace: :otel

  @deprecated """
  use the `otel.scope.name` attribute
  """
  @spec otel_library_name :: :"otel.library.name"
  def otel_library_name do
    :"otel.library.name"
  end

  @doc namespace: :otel

  @deprecated """
  use the `otel.scope.version` attribute
  """
  @spec otel_library_version :: :"otel.library.version"
  def otel_library_version do
    :"otel.library.version"
  end
end
