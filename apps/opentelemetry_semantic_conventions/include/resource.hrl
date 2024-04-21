%% The schema url for telemetry resources
-define(RESOURCE_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.25.0">>).

%% Uniquely identifies the framework API revision offered by a version (`os.version`) of the android operating system. More information can be found [here](https://developer.android.com/guide/topics/manifest/uses-sdk-element#ApiLevels)
-define(ANDROID_OS_API_LEVEL, 'android.os.api_level').

%% Array of brand name and version separated by a space
%% This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.brands`)
-define(BROWSER_BRANDS, 'browser.brands').

%% Preferred language of the user using the browser
%% This value is intended to be taken from the Navigator API `navigator.language`
-define(BROWSER_LANGUAGE, 'browser.language').

%% A boolean that is true if the browser is running on a mobile device
%% This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.mobile`). If unavailable, this attribute SHOULD be left unset
-define(BROWSER_MOBILE, 'browser.mobile').

%% The platform on which the browser is running
%% This value is intended to be taken from the [UA client hints API](https://wicg.github.io/ua-client-hints/#interface) (`navigator.userAgentData.platform`). If unavailable, the legacy `navigator.platform` API SHOULD NOT be used instead and this attribute SHOULD be left unset in order for the values to be consistent.
%% The list of possible values is defined in the [W3C User-Agent Client Hints specification](https://wicg.github.io/ua-client-hints/#sec-ch-ua-platform). Note that some (but not all) of these values can overlap with values in the [`os.type` and `os.name` attributes](./os.md). However, for consistency, the values in the `browser.platform` attribute should capture the exact value that the user agent provides
-define(BROWSER_PLATFORM, 'browser.platform').

%% Full user-agent string provided by the browser
%% The user-agent value SHOULD be provided only from browsers that do not have a mechanism to retrieve brands and platform individually from the User-Agent Client Hints API. To retrieve the value, the legacy `navigator.userAgent` API can be used
-define(USER_AGENT_ORIGINAL, 'user_agent.original').

%% The cloud account ID the resource is assigned to
-define(CLOUD_ACCOUNT_ID, 'cloud.account.id').

%% Cloud regions often have multiple, isolated locations known as zones to increase availability. Availability zone represents the zone where the resource is running
%% Availability zones are called "zones" on Alibaba Cloud and Google Cloud
-define(CLOUD_AVAILABILITY_ZONE, 'cloud.availability_zone').

%% The cloud platform in use
%% The prefix of the service SHOULD match the one specified in `cloud.provider`
-define(CLOUD_PLATFORM, 'cloud.platform').

%% Name of the cloud provider
-define(CLOUD_PROVIDER, 'cloud.provider').

%% The geographical region the resource is running
%% Refer to your provider's docs to see the available regions, for example [Alibaba Cloud regions](https://www.alibabacloud.com/help/doc-detail/40654.htm), [AWS regions](https://aws.amazon.com/about-aws/global-infrastructure/regions_az/), [Azure regions](https://azure.microsoft.com/global-infrastructure/geographies/), [Google Cloud regions](https://cloud.google.com/about/locations), or [Tencent Cloud regions](https://www.tencentcloud.com/document/product/213/6091)
-define(CLOUD_REGION, 'cloud.region').

%% Cloud provider-specific native identifier of the monitored cloud resource (e.g. an [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html) on AWS, a [fully qualified resource ID](https://learn.microsoft.com/rest/api/resources/resources/get-by-id) on Azure, a [full resource name](https://cloud.google.com/apis/design/resource_names#full_resource_name) on GCP)
%% On some cloud providers, it may not be possible to determine the full ID at startup,
%% so it may be necessary to set `cloud.resource_id` as a span attribute instead.
%% 
%% The exact value to use for `cloud.resource_id` depends on the cloud provider.
%% The following well-known definitions MUST be used if you set this attribute and they apply:
%% 
%% * **AWS Lambda:** The function [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html).
%%   Take care not to use the "invoked ARN" directly but replace any
%%   [alias suffix](https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html)
%%   with the resolved function version, as the same runtime instance may be invokable with
%%   multiple different aliases.
%% * **GCP:** The [URI of the resource](https://cloud.google.com/iam/docs/full-resource-names)
%% * **Azure:** The [Fully Qualified Resource ID](https://docs.microsoft.com/rest/api/resources/resources/get-by-id) of the invoked function,
%%   *not* the function app, having the form
%%   `/subscriptions/<SUBSCIPTION_GUID>/resourceGroups/<RG>/providers/Microsoft.Web/sites/<FUNCAPP>/functions/<FUNC>`.
%%   This means that a span attribute MUST be used, as an Azure function app can host multiple functions that would usually share
%%   a TracerProvider
-define(CLOUD_RESOURCE_ID, 'cloud.resource_id').

%% The ID of a running ECS task. The ID MUST be extracted from `task.arn`
-define(AWS_ECS_TASK_ID, 'aws.ecs.task.id').

%% The ARN of an [ECS cluster](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/clusters.html)
-define(AWS_ECS_CLUSTER_ARN, 'aws.ecs.cluster.arn').

%% The Amazon Resource Name (ARN) of an [ECS container instance](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_instances.html)
-define(AWS_ECS_CONTAINER_ARN, 'aws.ecs.container.arn').

%% The [launch type](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html) for an ECS task
-define(AWS_ECS_LAUNCHTYPE, 'aws.ecs.launchtype').

%% The ARN of a running [ECS task](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids)
-define(AWS_ECS_TASK_ARN, 'aws.ecs.task.arn').

%% The family name of the [ECS task definition](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definitions.html) used to create the ECS task
-define(AWS_ECS_TASK_FAMILY, 'aws.ecs.task.family').

%% The revision for the task definition used to create the ECS task
-define(AWS_ECS_TASK_REVISION, 'aws.ecs.task.revision').

%% The ARN of an EKS cluster
-define(AWS_EKS_CLUSTER_ARN, 'aws.eks.cluster.arn').

%% The Amazon Resource Name(s) (ARN) of the AWS log group(s)
%% See the [log group ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format)
-define(AWS_LOG_GROUP_ARNS, 'aws.log.group.arns').

%% The name(s) of the AWS log group(s) an application is writing to
%% Multiple log groups must be supported for cases like multi-container applications, where a single application has sidecar containers, and each write to their own log group
-define(AWS_LOG_GROUP_NAMES, 'aws.log.group.names').

%% The ARN(s) of the AWS log stream(s)
%% See the [log stream ARN format documentation](https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html#CWL_ARN_Format). One log group can contain several log streams, so these ARNs necessarily identify both a log group and a log stream
-define(AWS_LOG_STREAM_ARNS, 'aws.log.stream.arns').

%% The name(s) of the AWS log stream(s) an application is writing to
-define(AWS_LOG_STREAM_NAMES, 'aws.log.stream.names').

%% The name of the Cloud Run [execution](https://cloud.google.com/run/docs/managing/job-executions) being run for the Job, as set by the [`CLOUD_RUN_EXECUTION`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable
-define(GCP_CLOUD_RUN_JOB_EXECUTION, 'gcp.cloud_run.job.execution').

%% The index for a task within an execution as provided by the [`CLOUD_RUN_TASK_INDEX`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable
-define(GCP_CLOUD_RUN_JOB_TASK_INDEX, 'gcp.cloud_run.job.task_index').

%% The hostname of a GCE instance. This is the full value of the default or [custom hostname](https://cloud.google.com/compute/docs/instances/custom-hostname-vm)
-define(GCP_GCE_INSTANCE_HOSTNAME, 'gcp.gce.instance.hostname').

%% The instance name of a GCE instance. This is the value provided by `host.name`, the visible name of the instance in the Cloud Console UI, and the prefix for the default hostname of the instance as defined by the [default internal DNS name](https://cloud.google.com/compute/docs/internal-dns#instance-fully-qualified-domain-names)
-define(GCP_GCE_INSTANCE_NAME, 'gcp.gce.instance.name').

%% Unique identifier for the application
-define(HEROKU_APP_ID, 'heroku.app.id').

%% Commit hash for the current release
-define(HEROKU_RELEASE_COMMIT, 'heroku.release.commit').

%% Time and date the release was created
-define(HEROKU_RELEASE_CREATION_TIMESTAMP, 'heroku.release.creation_timestamp').

%% Container ID. Usually a UUID, as for example used to [identify Docker containers](https://docs.docker.com/engine/reference/run/#container-identification). The UUID might be abbreviated
-define(CONTAINER_ID, 'container.id').

%% Runtime specific image identifier. Usually a hash algorithm followed by a UUID
%% Docker defines a sha256 of the image id; `container.image.id` corresponds to the `Image` field from the Docker container inspect [API](https://docs.docker.com/engine/api/v1.43/#tag/Container/operation/ContainerInspect) endpoint.
%% K8s defines a link to the container registry repository with digest `"imageID": "registry.azurecr.io /namespace/service/dockerfile@sha256:bdeabd40c3a8a492eaf9e8e44d0ebbb84bac7ee25ac0cf8a7159d25f62555625"`.
%% The ID is assinged by the container runtime and can vary in different environments. Consider using `oci.manifest.digest` if it is important to identify the same image in different environments/runtimes
-define(CONTAINER_IMAGE_ID, 'container.image.id').

%% Name of the image the container was built on
-define(CONTAINER_IMAGE_NAME, 'container.image.name').

%% Repo digests of the container image as provided by the container runtime
%% [Docker](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect) and [CRI](https://github.com/kubernetes/cri-api/blob/c75ef5b473bbe2d0a4fc92f82235efd665ea8e9f/pkg/apis/runtime/v1/api.proto#L1237-L1238) report those under the `RepoDigests` field
-define(CONTAINER_IMAGE_REPO_DIGESTS, 'container.image.repo_digests').

%% Container image tags. An example can be found in [Docker Image Inspect](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect). Should be only the `<tag>` section of the full name for example from `registry.example.com/my-org/my-image:<tag>`
-define(CONTAINER_IMAGE_TAGS, 'container.image.tags').

%% Container name used by container runtime
-define(CONTAINER_NAME, 'container.name').

%% The container runtime managing this container
-define(CONTAINER_RUNTIME, 'container.runtime').

%% The digest of the OCI image manifest. For container images specifically is the digest by which the container image is known
%% Follows [OCI Image Manifest Specification](https://github.com/opencontainers/image-spec/blob/main/manifest.md), and specifically the [Digest property](https://github.com/opencontainers/image-spec/blob/main/descriptor.md#digests).
%% An example can be found in [Example Image Manifest](https://docs.docker.com/registry/spec/manifest-v2-2/#example-image-manifest)
-define(OCI_MANIFEST_DIGEST, 'oci.manifest.digest').

%% The command used to run the container (i.e. the command name)
%% If using embedded credentials or sensitive data, it is recommended to remove them to prevent potential leakage
-define(CONTAINER_COMMAND, 'container.command').

%% All the command arguments (including the command/executable itself) run by the container. [2]
-define(CONTAINER_COMMAND_ARGS, 'container.command_args').

%% The full command run by the container as a single string representing the full command. [2]
-define(CONTAINER_COMMAND_LINE, 'container.command_line').

%% Name of the [deployment environment](https://wikipedia.org/wiki/Deployment_environment) (aka deployment tier)
%% `deployment.environment` does not affect the uniqueness constraints defined through
%% the `service.namespace`, `service.name` and `service.instance.id` resource attributes.
%% This implies that resources carrying the following attribute combinations MUST be
%% considered to be identifying the same service:
%% 
%% * `service.name=frontend`, `deployment.environment=production`
%% * `service.name=frontend`, `deployment.environment=staging`
-define(DEPLOYMENT_ENVIRONMENT, 'deployment.environment').

%% A unique identifier representing the device
%% The device identifier MUST only be defined using the values outlined below. This value is not an advertising identifier and MUST NOT be used as such. On iOS (Swift or Objective-C), this value MUST be equal to the [vendor identifier](https://developer.apple.com/documentation/uikit/uidevice/1620059-identifierforvendor). On Android (Java or Kotlin), this value MUST be equal to the Firebase Installation ID or a globally unique UUID which is persisted across sessions in your application. More information can be found [here](https://developer.android.com/training/articles/user-data-ids) on best practices and exact implementation details. Caution should be taken when storing personal data or anything which can identify a user. GDPR and data protection laws may apply, ensure you do your own due diligence
-define(DEVICE_ID, 'device.id').

%% The name of the device manufacturer
%% The Android OS provides this field via [Build](https://developer.android.com/reference/android/os/Build#MANUFACTURER). iOS apps SHOULD hardcode the value `Apple`
-define(DEVICE_MANUFACTURER, 'device.manufacturer').

%% The model identifier for the device
%% It's recommended this value represents a machine-readable version of the model identifier rather than the market or consumer-friendly name of the device
-define(DEVICE_MODEL_IDENTIFIER, 'device.model.identifier').

%% The marketing name for the device model
%% It's recommended this value represents a human-readable version of the device model rather than a machine-readable alternative
-define(DEVICE_MODEL_NAME, 'device.model.name').

%% The name of the single function that this runtime instance executes
%% This is the name of the function as configured/deployed on the FaaS
%% platform and is usually different from the name of the callback
%% function (which may be stored in the
%% [`code.namespace`/`code.function`](/docs/general/attributes.md#source-code-attributes)
%% span attributes).
%% 
%% For some cloud providers, the above definition is ambiguous. The following
%% definition of function name MUST be used for this attribute
%% (and consequently the span name) for the listed cloud providers/products:
%% 
%% * **Azure:**  The full name `<FUNCAPP>/<FUNC>`, i.e., function app name
%%   followed by a forward slash followed by the function name (this form
%%   can also be seen in the resource JSON for the function).
%%   This means that a span attribute MUST be used, as an Azure function
%%   app can host multiple functions that would usually share
%%   a TracerProvider (see also the `cloud.resource_id` attribute)
-define(FAAS_NAME, 'faas.name').

%% The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version
%% * **AWS Lambda:** Use the (full) log stream name
-define(FAAS_INSTANCE, 'faas.instance').

%% The amount of memory available to the serverless function converted to Bytes
%% It's recommended to set this attribute since e.g. too little memory can easily stop a Java AWS Lambda function from working correctly. On AWS Lambda, the environment variable `AWS_LAMBDA_FUNCTION_MEMORY_SIZE` provides this information (which must be multiplied by 1,048,576)
-define(FAAS_MAX_MEMORY, 'faas.max_memory').

%% The immutable version of the function being executed
%% Depending on the cloud provider and platform, use:
%% 
%% * **AWS Lambda:** The [function version](https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html)
%%   (an integer represented as a decimal string).
%% * **Google Cloud Run (Services):** The [revision](https://cloud.google.com/run/docs/managing/revisions)
%%   (i.e., the function name plus the revision suffix).
%% * **Google Cloud Functions:** The value of the
%%   [`K_REVISION` environment variable](https://cloud.google.com/functions/docs/env-var#runtime_environment_variables_set_automatically).
%% * **Azure Functions:** Not applicable. Do not set this attribute
-define(FAAS_VERSION, 'faas.version').

%% The CPU architecture the host system is running on
-define(HOST_ARCH, 'host.arch').

%% Unique host ID. For Cloud, this must be the instance_id assigned by the cloud provider. For non-containerized systems, this should be the `machine-id`. See the table below for the sources to use to determine the `machine-id` based on operating system
-define(HOST_ID, 'host.id').

%% VM image ID or host OS image ID. For Cloud, this value is from the provider
-define(HOST_IMAGE_ID, 'host.image.id').

%% Name of the VM image or OS install the host was instantiated from
-define(HOST_IMAGE_NAME, 'host.image.name').

%% The version string of the VM image or host OS as defined in [Version Attributes](/docs/resource/README.md#version-attributes)
-define(HOST_IMAGE_VERSION, 'host.image.version').

%% Name of the host. On Unix systems, it may contain what the hostname command returns, or the fully qualified hostname, or another name specified by the user
-define(HOST_NAME, 'host.name').

%% Type of host. For Cloud, this must be the machine type
-define(HOST_TYPE, 'host.type').

%% Available IP addresses of the host, excluding loopback interfaces
%% IPv4 Addresses MUST be specified in dotted-quad notation. IPv6 addresses MUST be specified in the [RFC 5952](https://www.rfc-editor.org/rfc/rfc5952.html) format
-define(HOST_IP, 'host.ip').

%% Available MAC addresses of the host, excluding loopback interfaces
%% MAC Addresses MUST be represented in [IEEE RA hexadecimal form](https://standards.ieee.org/wp-content/uploads/import/documents/tutorials/eui.pdf): as hyphen-separated octets in uppercase hexadecimal form from most to least significant
-define(HOST_MAC, 'host.mac').

%% The amount of level 2 memory cache available to the processor (in Bytes)
-define(HOST_CPU_CACHE_L2_SIZE, 'host.cpu.cache.l2.size').

%% Family or generation of the CPU
-define(HOST_CPU_FAMILY, 'host.cpu.family').

%% Model identifier. It provides more granular information about the CPU, distinguishing it from other CPUs within the same family
-define(HOST_CPU_MODEL_ID, 'host.cpu.model.id').

%% Model designation of the processor
-define(HOST_CPU_MODEL_NAME, 'host.cpu.model.name').

%% Stepping or core revisions
-define(HOST_CPU_STEPPING, 'host.cpu.stepping').

%% Processor manufacturer identifier. A maximum 12-character string
%% [CPUID](https://wiki.osdev.org/CPUID) command returns the vendor ID string in EBX, EDX and ECX registers. Writing these to memory in this order results in a 12-character string
-define(HOST_CPU_VENDOR_ID, 'host.cpu.vendor.id').

%% The name of the cluster
-define(K8S_CLUSTER_NAME, 'k8s.cluster.name').

%% A pseudo-ID for the cluster, set to the UID of the `kube-system` namespace
%% K8s doesn't have support for obtaining a cluster ID. If this is ever
%% added, we will recommend collecting the `k8s.cluster.uid` through the
%% official APIs. In the meantime, we are able to use the `uid` of the
%% `kube-system` namespace as a proxy for cluster ID. Read on for the
%% rationale.
%% 
%% Every object created in a K8s cluster is assigned a distinct UID. The
%% `kube-system` namespace is used by Kubernetes itself and will exist
%% for the lifetime of the cluster. Using the `uid` of the `kube-system`
%% namespace is a reasonable proxy for the K8s ClusterID as it will only
%% change if the cluster is rebuilt. Furthermore, Kubernetes UIDs are
%% UUIDs as standardized by
%% [ISO/IEC 9834-8 and ITU-T X.667](https://www.itu.int/ITU-T/studygroups/com17/oid.html).
%% Which states:
%% 
%% > If generated according to one of the mechanisms defined in Rec.
%%   ITU-T X.667 | ISO/IEC 9834-8, a UUID is either guaranteed to be
%%   different from all other UUIDs generated before 3603 A.D., or is
%%   extremely likely to be different (depending on the mechanism chosen).
%% 
%% Therefore, UIDs between clusters should be extremely unlikely to
%% conflict
-define(K8S_CLUSTER_UID, 'k8s.cluster.uid').

%% The name of the Node
-define(K8S_NODE_NAME, 'k8s.node.name').

%% The UID of the Node
-define(K8S_NODE_UID, 'k8s.node.uid').

%% The name of the namespace that the pod is running in
-define(K8S_NAMESPACE_NAME, 'k8s.namespace.name').

%% The name of the Pod
-define(K8S_POD_NAME, 'k8s.pod.name').

%% The UID of the Pod
-define(K8S_POD_UID, 'k8s.pod.uid').

%% The name of the Container from Pod specification, must be unique within a Pod. Container runtime usually uses different globally unique name (`container.name`)
-define(K8S_CONTAINER_NAME, 'k8s.container.name').

%% Number of times the container was restarted. This attribute can be used to identify a particular container (running or stopped) within a container spec
-define(K8S_CONTAINER_RESTART_COUNT, 'k8s.container.restart_count').

%% The name of the ReplicaSet
-define(K8S_REPLICASET_NAME, 'k8s.replicaset.name').

%% The UID of the ReplicaSet
-define(K8S_REPLICASET_UID, 'k8s.replicaset.uid').

%% The name of the Deployment
-define(K8S_DEPLOYMENT_NAME, 'k8s.deployment.name').

%% The UID of the Deployment
-define(K8S_DEPLOYMENT_UID, 'k8s.deployment.uid').

%% The name of the StatefulSet
-define(K8S_STATEFULSET_NAME, 'k8s.statefulset.name').

%% The UID of the StatefulSet
-define(K8S_STATEFULSET_UID, 'k8s.statefulset.uid').

%% The name of the DaemonSet
-define(K8S_DAEMONSET_NAME, 'k8s.daemonset.name').

%% The UID of the DaemonSet
-define(K8S_DAEMONSET_UID, 'k8s.daemonset.uid').

%% The name of the Job
-define(K8S_JOB_NAME, 'k8s.job.name').

%% The UID of the Job
-define(K8S_JOB_UID, 'k8s.job.uid').

%% The name of the CronJob
-define(K8S_CRONJOB_NAME, 'k8s.cronjob.name').

%% The UID of the CronJob
-define(K8S_CRONJOB_UID, 'k8s.cronjob.uid').

%% The operating system type
-define(OS_TYPE, 'os.type').

%% Unique identifier for a particular build or compilation of the operating system
-define(OS_BUILD_ID, 'os.build_id').

%% Human readable (not intended to be parsed) OS version information, like e.g. reported by `ver` or `lsb_release -a` commands
-define(OS_DESCRIPTION, 'os.description').

%% Human readable operating system name
-define(OS_NAME, 'os.name').

%% The version string of the operating system as defined in [Version Attributes](/docs/resource/README.md#version-attributes)
-define(OS_VERSION, 'os.version').

%% The command used to launch the process (i.e. the command name). On Linux based systems, can be set to the zeroth string in `proc/[pid]/cmdline`. On Windows, can be set to the first parameter extracted from `GetCommandLineW`
-define(PROCESS_COMMAND, 'process.command').

%% All the command arguments (including the command/executable itself) as received by the process. On Linux-based systems (and some other Unixoid systems supporting procfs), can be set according to the list of null-delimited strings extracted from `proc/[pid]/cmdline`. For libc-based executables, this would be the full argv vector passed to `main`
-define(PROCESS_COMMAND_ARGS, 'process.command_args').

%% The full command used to launch the process as a single string representing the full command. On Windows, can be set to the result of `GetCommandLineW`. Do not set this if you have to assemble it just for monitoring; use `process.command_args` instead
-define(PROCESS_COMMAND_LINE, 'process.command_line').

%% The name of the process executable. On Linux based systems, can be set to the `Name` in `proc/[pid]/status`. On Windows, can be set to the base name of `GetProcessImageFileNameW`
-define(PROCESS_EXECUTABLE_NAME, 'process.executable.name').

%% The full path to the process executable. On Linux based systems, can be set to the target of `proc/[pid]/exe`. On Windows, can be set to the result of `GetProcessImageFileNameW`
-define(PROCESS_EXECUTABLE_PATH, 'process.executable.path').

%% The username of the user that owns the process
-define(PROCESS_OWNER, 'process.owner').

%% Parent Process identifier (PPID)
-define(PROCESS_PARENT_PID, 'process.parent_pid').

%% Process identifier (PID)
-define(PROCESS_PID, 'process.pid').

%% An additional description about the runtime of the process, for example a specific vendor customization of the runtime environment
-define(PROCESS_RUNTIME_DESCRIPTION, 'process.runtime.description').

%% The name of the runtime of this process. For compiled native binaries, this SHOULD be the name of the compiler
-define(PROCESS_RUNTIME_NAME, 'process.runtime.name').

%% The version of the runtime of this process, as returned by the runtime without modification
-define(PROCESS_RUNTIME_VERSION, 'process.runtime.version').

%% Logical name of the service
%% MUST be the same for all instances of horizontally scaled services. If the value was not specified, SDKs MUST fallback to `unknown_service:` concatenated with [`process.executable.name`](process.md#process), e.g. `unknown_service:bash`. If `process.executable.name` is not available, the value MUST be set to `unknown_service`
-define(SERVICE_NAME, 'service.name').

%% The version string of the service API or implementation. The format is not defined by these conventions
-define(SERVICE_VERSION, 'service.version').

%% The string ID of the service instance
%% MUST be unique for each instance of the same `service.namespace,service.name` pair (in other words
%% `service.namespace,service.name,service.instance.id` triplet MUST be globally unique). The ID helps to
%% distinguish instances of the same service that exist at the same time (e.g. instances of a horizontally scaled
%% service).
%% 
%% Implementations, such as SDKs, are recommended to generate a random Version 1 or Version 4 [RFC
%% 4122](https://www.ietf.org/rfc/rfc4122.txt) UUID, but are free to use an inherent unique ID as the source of
%% this value if stability is desirable. In that case, the ID SHOULD be used as source of a UUID Version 5 and
%% SHOULD use the following UUID as the namespace: `4d63009a-8d0f-11ee-aad7-4c796ed8e320`.
%% 
%% UUIDs are typically recommended, as only an opaque value for the purposes of identifying a service instance is
%% needed. Similar to what can be seen in the man page for the
%% [`/etc/machine-id`](https://www.freedesktop.org/software/systemd/man/machine-id.html) file, the underlying
%% data, such as pod name and namespace should be treated as confidential, being the user's choice to expose it
%% or not via another resource attribute.
%% 
%% For applications running behind an application server (like unicorn), we do not recommend using one identifier
%% for all processes participating in the application. Instead, it's recommended each division (e.g. a worker
%% thread in unicorn) to have its own instance.id.
%% 
%% It's not recommended for a Collector to set `service.instance.id` if it can't unambiguously determine the
%% service instance that is generating that telemetry. For instance, creating an UUID based on `pod.name` will
%% likely be wrong, as the Collector might not know from which container within that pod the telemetry originated.
%% However, Collectors can set the `service.instance.id` if they can unambiguously determine the service instance
%% for that telemetry. This is typically the case for scraping receivers, as they know the target address and
%% port
-define(SERVICE_INSTANCE_ID, 'service.instance.id').

%% A namespace for `service.name`
%% A string value having a meaning that helps to distinguish a group of services, for example the team name that owns a group of services. `service.name` is expected to be unique within the same namespace. If `service.namespace` is not specified in the Resource then `service.name` is expected to be unique for all services that have no explicit namespace defined (so the empty/unspecified namespace is simply one more valid namespace). Zero-length namespace string is assumed equal to unspecified namespace
-define(SERVICE_NAMESPACE, 'service.namespace').

%% The language of the telemetry SDK
-define(TELEMETRY_SDK_LANGUAGE, 'telemetry.sdk.language').

%% The name of the telemetry SDK as defined above
%% The OpenTelemetry SDK MUST set the `telemetry.sdk.name` attribute to `opentelemetry`.
%% If another SDK, like a fork or a vendor-provided implementation, is used, this SDK MUST set the
%% `telemetry.sdk.name` attribute to the fully-qualified class or module name of this SDK's main entry point
%% or another suitable identifier depending on the language.
%% The identifier `opentelemetry` is reserved and MUST NOT be used in this case.
%% All custom identifiers SHOULD be stable across different versions of an implementation
-define(TELEMETRY_SDK_NAME, 'telemetry.sdk.name').

%% The version string of the telemetry SDK
-define(TELEMETRY_SDK_VERSION, 'telemetry.sdk.version').

%% The name of the auto instrumentation agent or distribution, if used
%% Official auto instrumentation agents and distributions SHOULD set the `telemetry.distro.name` attribute to
%% a string starting with `opentelemetry-`, e.g. `opentelemetry-java-instrumentation`
-define(TELEMETRY_DISTRO_NAME, 'telemetry.distro.name').

%% The version string of the auto instrumentation agent or distribution, if used
-define(TELEMETRY_DISTRO_VERSION, 'telemetry.distro.version').

%% The name of the web engine
-define(WEBENGINE_NAME, 'webengine.name').

%% Additional description of the web engine (e.g. detailed version and edition information)
-define(WEBENGINE_DESCRIPTION, 'webengine.description').

%% The version of the web engine
-define(WEBENGINE_VERSION, 'webengine.version').

%% The name of the instrumentation scope - (`InstrumentationScope.Name` in OTLP)
-define(OTEL_SCOPE_NAME, 'otel.scope.name').

%% The version of the instrumentation scope - (`InstrumentationScope.Version` in OTLP)
-define(OTEL_SCOPE_VERSION, 'otel.scope.version').

%% None
%% @deprecated use the `otel.scope.name` attribute
-define(OTEL_LIBRARY_NAME, 'otel.library.name').

%% None
%% @deprecated use the `otel.scope.version` attribute
-define(OTEL_LIBRARY_VERSION, 'otel.library.version').
