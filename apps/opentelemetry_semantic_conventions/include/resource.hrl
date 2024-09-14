%% WARNING: These macros are deprecated and will be removed in a future release.
%% Migrate to >= v1.27.0 semantic conventions

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(RESOURCE_SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.13.0">>).

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(BROWSER_BRANDS, 'browser.brands').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(BROWSER_PLATFORM, 'browser.platform').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(BROWSER_USER_AGENT, 'browser.user_agent').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CLOUD_PROVIDER, 'cloud.provider').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CLOUD_ACCOUNT_ID, 'cloud.account.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CLOUD_REGION, 'cloud.region').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CLOUD_AVAILABILITY_ZONE, 'cloud.availability_zone').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CLOUD_PLATFORM, 'cloud.platform').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_ECS_CONTAINER_ARN, 'aws.ecs.container.arn').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_ECS_CLUSTER_ARN, 'aws.ecs.cluster.arn').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_ECS_LAUNCHTYPE, 'aws.ecs.launchtype').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_ECS_TASK_ARN, 'aws.ecs.task.arn').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_ECS_TASK_FAMILY, 'aws.ecs.task.family').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_ECS_TASK_REVISION, 'aws.ecs.task.revision').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_EKS_CLUSTER_ARN, 'aws.eks.cluster.arn').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_LOG_GROUP_NAMES, 'aws.log.group.names').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_LOG_GROUP_ARNS, 'aws.log.group.arns').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_LOG_STREAM_NAMES, 'aws.log.stream.names').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(AWS_LOG_STREAM_ARNS, 'aws.log.stream.arns').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CONTAINER_NAME, 'container.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CONTAINER_ID, 'container.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CONTAINER_RUNTIME, 'container.runtime').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CONTAINER_IMAGE_NAME, 'container.image.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(CONTAINER_IMAGE_TAG, 'container.image.tag').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DEPLOYMENT_ENVIRONMENT, 'deployment.environment').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DEVICE_ID, 'device.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DEVICE_MODEL_IDENTIFIER, 'device.model.identifier').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DEVICE_MODEL_NAME, 'device.model.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(DEVICE_MANUFACTURER, 'device.manufacturer').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_NAME, 'faas.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_ID, 'faas.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_VERSION, 'faas.version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_INSTANCE, 'faas.instance').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(FAAS_MAX_MEMORY, 'faas.max_memory').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HOST_ID, 'host.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HOST_NAME, 'host.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HOST_TYPE, 'host.type').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HOST_ARCH, 'host.arch').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HOST_IMAGE_NAME, 'host.image.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HOST_IMAGE_ID, 'host.image.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(HOST_IMAGE_VERSION, 'host.image.version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_CLUSTER_NAME, 'k8s.cluster.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_NODE_NAME, 'k8s.node.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_NODE_UID, 'k8s.node.uid').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_NAMESPACE_NAME, 'k8s.namespace.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_POD_UID, 'k8s.pod.uid').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_POD_NAME, 'k8s.pod.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_CONTAINER_NAME, 'k8s.container.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_CONTAINER_RESTART_COUNT, 'k8s.container.restart_count').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_REPLICASET_UID, 'k8s.replicaset.uid').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_REPLICASET_NAME, 'k8s.replicaset.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_DEPLOYMENT_UID, 'k8s.deployment.uid').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_DEPLOYMENT_NAME, 'k8s.deployment.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_STATEFULSET_UID, 'k8s.statefulset.uid').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_STATEFULSET_NAME, 'k8s.statefulset.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_DAEMONSET_UID, 'k8s.daemonset.uid').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_DAEMONSET_NAME, 'k8s.daemonset.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_JOB_UID, 'k8s.job.uid').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_JOB_NAME, 'k8s.job.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_CRONJOB_UID, 'k8s.cronjob.uid').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(K8S_CRONJOB_NAME, 'k8s.cronjob.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(OS_TYPE, 'os.type').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(OS_DESCRIPTION, 'os.description').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(OS_NAME, 'os.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(OS_VERSION, 'os.version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_PID, 'process.pid').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_PARENT_PID, 'process.parent_pid').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_EXECUTABLE_NAME, 'process.executable.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_EXECUTABLE_PATH, 'process.executable.path').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_COMMAND, 'process.command').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_COMMAND_LINE, 'process.command_line').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_COMMAND_ARGS, 'process.command_args').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_OWNER, 'process.owner').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_RUNTIME_NAME, 'process.runtime.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_RUNTIME_VERSION, 'process.runtime.version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(PROCESS_RUNTIME_DESCRIPTION, 'process.runtime.description').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(SERVICE_NAME, 'service.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(SERVICE_NAMESPACE, 'service.namespace').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(SERVICE_INSTANCE_ID, 'service.instance.id').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(SERVICE_VERSION, 'service.version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(TELEMETRY_SDK_NAME, 'telemetry.sdk.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(TELEMETRY_SDK_LANGUAGE, 'telemetry.sdk.language').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(TELEMETRY_SDK_VERSION, 'telemetry.sdk.version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(TELEMETRY_AUTO_VERSION, 'telemetry.auto.version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(WEBENGINE_NAME, 'webengine.name').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(WEBENGINE_VERSION, 'webengine.version').

%% @deprecated Migrate to >= v1.27.0 semantic conventions
%% Deprecated Migrate to >= v1.27.0 semantic conventions
-define(WEBENGINE_DESCRIPTION, 'webengine.description').
