
%%%------------------------------------------------------------------------
%% Copyright The OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%-------------------------------------------------------------------------

%% The name of the cluster.
%%  
-define(K8S_CLUSTER_NAME, 'k8s.cluster.name').


%% A pseudo-ID for the cluster, set to the UID of the `kube-system` namespace.
%%  
-define(K8S_CLUSTER_UID, 'k8s.cluster.uid').


%% The name of the Container from Pod specification, must be unique within a Pod. Container runtime usually uses different globally unique name (`container.name`).
%%  
-define(K8S_CONTAINER_NAME, 'k8s.container.name').


%% Number of times the container was restarted. This attribute can be used to identify a particular container (running or stopped) within a container spec.
%%  
-define(K8S_CONTAINER_RESTART_COUNT, 'k8s.container.restart_count').


%% Last terminated reason of the Container.
%%  
-define(K8S_CONTAINER_STATUS_LAST_TERMINATED_REASON, 'k8s.container.status.last_terminated_reason').


%% The reason for the container state. Corresponds to the `reason` field of the: [K8s ContainerStateWaiting](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#containerstatewaiting-v1-core) or [K8s ContainerStateTerminated](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#containerstateterminated-v1-core)
%%  
-define(K8S_CONTAINER_STATUS_REASON, 'k8s.container.status.reason').

-define(K8S_CONTAINER_STATUS_REASON_VALUES_CONTAINER_CREATING, 'ContainerCreating').

-define(K8S_CONTAINER_STATUS_REASON_VALUES_CRASH_LOOP_BACK_OFF, 'CrashLoopBackOff').

-define(K8S_CONTAINER_STATUS_REASON_VALUES_CREATE_CONTAINER_CONFIG_ERROR, 'CreateContainerConfigError').

-define(K8S_CONTAINER_STATUS_REASON_VALUES_ERR_IMAGE_PULL, 'ErrImagePull').

-define(K8S_CONTAINER_STATUS_REASON_VALUES_IMAGE_PULL_BACK_OFF, 'ImagePullBackOff').

-define(K8S_CONTAINER_STATUS_REASON_VALUES_OOM_KILLED, 'OOMKilled').

-define(K8S_CONTAINER_STATUS_REASON_VALUES_COMPLETED, 'Completed').

-define(K8S_CONTAINER_STATUS_REASON_VALUES_ERROR, 'Error').

-define(K8S_CONTAINER_STATUS_REASON_VALUES_CONTAINER_CANNOT_RUN, 'ContainerCannotRun').



%% The state of the container. [K8s ContainerState](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#containerstate-v1-core)
%%  
-define(K8S_CONTAINER_STATUS_STATE, 'k8s.container.status.state').

-define(K8S_CONTAINER_STATUS_STATE_VALUES_TERMINATED, 'terminated').

-define(K8S_CONTAINER_STATUS_STATE_VALUES_RUNNING, 'running').

-define(K8S_CONTAINER_STATUS_STATE_VALUES_WAITING, 'waiting').



%% The cronjob annotation placed on the CronJob, the `<key>` being the annotation name, the value being the annotation value.
%%  
-define(K8S_CRONJOB_ANNOTATION, 'k8s.cronjob.annotation').


%% The label placed on the CronJob, the `<key>` being the label name, the value being the label value.
%%  
-define(K8S_CRONJOB_LABEL, 'k8s.cronjob.label').


%% The name of the CronJob.
%%  
-define(K8S_CRONJOB_NAME, 'k8s.cronjob.name').


%% The UID of the CronJob.
%%  
-define(K8S_CRONJOB_UID, 'k8s.cronjob.uid').


%% The annotation placed on the DaemonSet, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.
%%  
-define(K8S_DAEMONSET_ANNOTATION, 'k8s.daemonset.annotation').


%% The label placed on the DaemonSet, the `<key>` being the label name, the value being the label value, even if the value is empty.
%%  
-define(K8S_DAEMONSET_LABEL, 'k8s.daemonset.label').


%% The name of the DaemonSet.
%%  
-define(K8S_DAEMONSET_NAME, 'k8s.daemonset.name').


%% The UID of the DaemonSet.
%%  
-define(K8S_DAEMONSET_UID, 'k8s.daemonset.uid').


%% The annotation placed on the Deployment, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.
%%  
-define(K8S_DEPLOYMENT_ANNOTATION, 'k8s.deployment.annotation').


%% The label placed on the Deployment, the `<key>` being the label name, the value being the label value, even if the value is empty.
%%  
-define(K8S_DEPLOYMENT_LABEL, 'k8s.deployment.label').


%% The name of the Deployment.
%%  
-define(K8S_DEPLOYMENT_NAME, 'k8s.deployment.name').


%% The UID of the Deployment.
%%  
-define(K8S_DEPLOYMENT_UID, 'k8s.deployment.uid').


%% The type of metric source for the horizontal pod autoscaler.
%%  
-define(K8S_HPA_METRIC_TYPE, 'k8s.hpa.metric.type').


%% The name of the horizontal pod autoscaler.
%%  
-define(K8S_HPA_NAME, 'k8s.hpa.name').


%% The API version of the target resource to scale for the HorizontalPodAutoscaler.
%%  
-define(K8S_HPA_SCALETARGETREF_API_VERSION, 'k8s.hpa.scaletargetref.api_version').


%% The kind of the target resource to scale for the HorizontalPodAutoscaler.
%%  
-define(K8S_HPA_SCALETARGETREF_KIND, 'k8s.hpa.scaletargetref.kind').


%% The name of the target resource to scale for the HorizontalPodAutoscaler.
%%  
-define(K8S_HPA_SCALETARGETREF_NAME, 'k8s.hpa.scaletargetref.name').


%% The UID of the horizontal pod autoscaler.
%%  
-define(K8S_HPA_UID, 'k8s.hpa.uid').


%% The size (identifier) of the K8s huge page.
%%  
-define(K8S_HUGEPAGE_SIZE, 'k8s.hugepage.size').


%% The annotation placed on the Job, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.
%%  
-define(K8S_JOB_ANNOTATION, 'k8s.job.annotation').


%% The label placed on the Job, the `<key>` being the label name, the value being the label value, even if the value is empty.
%%  
-define(K8S_JOB_LABEL, 'k8s.job.label').


%% The name of the Job.
%%  
-define(K8S_JOB_NAME, 'k8s.job.name').


%% The UID of the Job.
%%  
-define(K8S_JOB_UID, 'k8s.job.uid').


%% The annotation placed on the Namespace, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.
%%  
-define(K8S_NAMESPACE_ANNOTATION, 'k8s.namespace.annotation').


%% The label placed on the Namespace, the `<key>` being the label name, the value being the label value, even if the value is empty.
%%  
-define(K8S_NAMESPACE_LABEL, 'k8s.namespace.label').


%% The name of the namespace that the pod is running in.
%%  
-define(K8S_NAMESPACE_NAME, 'k8s.namespace.name').


%% The phase of the K8s namespace.
%%  
-define(K8S_NAMESPACE_PHASE, 'k8s.namespace.phase').

-define(K8S_NAMESPACE_PHASE_VALUES_ACTIVE, 'active').

-define(K8S_NAMESPACE_PHASE_VALUES_TERMINATING, 'terminating').



%% The annotation placed on the Node, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.
%%  
-define(K8S_NODE_ANNOTATION, 'k8s.node.annotation').


%% The status of the condition, one of True, False, Unknown.
%%  
-define(K8S_NODE_CONDITION_STATUS, 'k8s.node.condition.status').

-define(K8S_NODE_CONDITION_STATUS_VALUES_CONDITION_TRUE, 'true').

-define(K8S_NODE_CONDITION_STATUS_VALUES_CONDITION_FALSE, 'false').

-define(K8S_NODE_CONDITION_STATUS_VALUES_CONDITION_UNKNOWN, 'unknown').



%% The condition type of a K8s Node.
%%  
-define(K8S_NODE_CONDITION_TYPE, 'k8s.node.condition.type').

-define(K8S_NODE_CONDITION_TYPE_VALUES_READY, 'Ready').

-define(K8S_NODE_CONDITION_TYPE_VALUES_DISK_PRESSURE, 'DiskPressure').

-define(K8S_NODE_CONDITION_TYPE_VALUES_MEMORY_PRESSURE, 'MemoryPressure').

-define(K8S_NODE_CONDITION_TYPE_VALUES_PID_PRESSURE, 'PIDPressure').

-define(K8S_NODE_CONDITION_TYPE_VALUES_NETWORK_UNAVAILABLE, 'NetworkUnavailable').



%% The label placed on the Node, the `<key>` being the label name, the value being the label value, even if the value is empty.
%%  
-define(K8S_NODE_LABEL, 'k8s.node.label').


%% The name of the Node.
%%  
-define(K8S_NODE_NAME, 'k8s.node.name').


%% The UID of the Node.
%%  
-define(K8S_NODE_UID, 'k8s.node.uid').


%% The annotation placed on the Pod, the `<key>` being the annotation name, the value being the annotation value.
%%  
-define(K8S_POD_ANNOTATION, 'k8s.pod.annotation').


%% Specifies the hostname of the Pod.
%%  
-define(K8S_POD_HOSTNAME, 'k8s.pod.hostname').


%% IP address allocated to the Pod.
%%  
-define(K8S_POD_IP, 'k8s.pod.ip').


%% The label placed on the Pod, the `<key>` being the label name, the value being the label value.
%%  
-define(K8S_POD_LABEL, 'k8s.pod.label').

%% @deprecated Replaced by `k8s.pod.label`.
%% Deprecated, use `k8s.pod.label` instead.
-define(K8S_POD_LABELS, 'k8s.pod.labels').


%% The name of the Pod.
%%  
-define(K8S_POD_NAME, 'k8s.pod.name').


%% The start timestamp of the Pod.
%%  
-define(K8S_POD_START_TIME, 'k8s.pod.start_time').


%% The phase for the pod. Corresponds to the `phase` field of the: [K8s PodStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.33/#podstatus-v1-core)
%%  
-define(K8S_POD_STATUS_PHASE, 'k8s.pod.status.phase').

-define(K8S_POD_STATUS_PHASE_VALUES_PENDING, 'Pending').

-define(K8S_POD_STATUS_PHASE_VALUES_RUNNING, 'Running').

-define(K8S_POD_STATUS_PHASE_VALUES_SUCCEEDED, 'Succeeded').

-define(K8S_POD_STATUS_PHASE_VALUES_FAILED, 'Failed').

-define(K8S_POD_STATUS_PHASE_VALUES_UNKNOWN, 'Unknown').



%% The reason for the pod state. Corresponds to the `reason` field of the: [K8s PodStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.33/#podstatus-v1-core)
%%  
-define(K8S_POD_STATUS_REASON, 'k8s.pod.status.reason').

-define(K8S_POD_STATUS_REASON_VALUES_EVICTED, 'Evicted').

-define(K8S_POD_STATUS_REASON_VALUES_NODE_AFFINITY, 'NodeAffinity').

-define(K8S_POD_STATUS_REASON_VALUES_NODE_LOST, 'NodeLost').

-define(K8S_POD_STATUS_REASON_VALUES_SHUTDOWN, 'Shutdown').

-define(K8S_POD_STATUS_REASON_VALUES_UNEXPECTED_ADMISSION_ERROR, 'UnexpectedAdmissionError').



%% The UID of the Pod.
%%  
-define(K8S_POD_UID, 'k8s.pod.uid').


%% The annotation placed on the ReplicaSet, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.
%%  
-define(K8S_REPLICASET_ANNOTATION, 'k8s.replicaset.annotation').


%% The label placed on the ReplicaSet, the `<key>` being the label name, the value being the label value, even if the value is empty.
%%  
-define(K8S_REPLICASET_LABEL, 'k8s.replicaset.label').


%% The name of the ReplicaSet.
%%  
-define(K8S_REPLICASET_NAME, 'k8s.replicaset.name').


%% The UID of the ReplicaSet.
%%  
-define(K8S_REPLICASET_UID, 'k8s.replicaset.uid').


%% The name of the replication controller.
%%  
-define(K8S_REPLICATIONCONTROLLER_NAME, 'k8s.replicationcontroller.name').


%% The UID of the replication controller.
%%  
-define(K8S_REPLICATIONCONTROLLER_UID, 'k8s.replicationcontroller.uid').


%% The name of the resource quota.
%%  
-define(K8S_RESOURCEQUOTA_NAME, 'k8s.resourcequota.name').


%% The name of the K8s resource a resource quota defines.
%%  
-define(K8S_RESOURCEQUOTA_RESOURCE_NAME, 'k8s.resourcequota.resource_name').


%% The UID of the resource quota.
%%  
-define(K8S_RESOURCEQUOTA_UID, 'k8s.resourcequota.uid').


%% The annotation placed on the Service, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.
%%  
-define(K8S_SERVICE_ANNOTATION, 'k8s.service.annotation').


%% The address type of the service endpoint.
%%  
-define(K8S_SERVICE_ENDPOINT_ADDRESS_TYPE, 'k8s.service.endpoint.address_type').

-define(K8S_SERVICE_ENDPOINT_ADDRESS_TYPE_VALUES_IPV4, 'IPv4').

-define(K8S_SERVICE_ENDPOINT_ADDRESS_TYPE_VALUES_IPV6, 'IPv6').

-define(K8S_SERVICE_ENDPOINT_ADDRESS_TYPE_VALUES_FQDN, 'FQDN').



%% The condition of the service endpoint.
%%  
-define(K8S_SERVICE_ENDPOINT_CONDITION, 'k8s.service.endpoint.condition').

-define(K8S_SERVICE_ENDPOINT_CONDITION_VALUES_READY, 'ready').

-define(K8S_SERVICE_ENDPOINT_CONDITION_VALUES_SERVING, 'serving').

-define(K8S_SERVICE_ENDPOINT_CONDITION_VALUES_TERMINATING, 'terminating').



%% The zone of the service endpoint.
%%  
-define(K8S_SERVICE_ENDPOINT_ZONE, 'k8s.service.endpoint.zone').


%% The label placed on the Service, the `<key>` being the label name, the value being the label value, even if the value is empty.
%%  
-define(K8S_SERVICE_LABEL, 'k8s.service.label').


%% The name of the Service.
%%  
-define(K8S_SERVICE_NAME, 'k8s.service.name').


%% Whether the Service publishes not-ready endpoints.
%%  
-define(K8S_SERVICE_PUBLISH_NOT_READY_ADDRESSES, 'k8s.service.publish_not_ready_addresses').


%% The selector key-value pair placed on the Service, the `<key>` being the selector key, the value being the selector value.
%%  
-define(K8S_SERVICE_SELECTOR, 'k8s.service.selector').


%% The traffic distribution policy for the Service.
%%  
-define(K8S_SERVICE_TRAFFIC_DISTRIBUTION, 'k8s.service.traffic_distribution').


%% The type of the Kubernetes Service.
%%  
-define(K8S_SERVICE_TYPE, 'k8s.service.type').

-define(K8S_SERVICE_TYPE_VALUES_CLUSTER_IP, 'ClusterIP').

-define(K8S_SERVICE_TYPE_VALUES_NODE_PORT, 'NodePort').

-define(K8S_SERVICE_TYPE_VALUES_LOAD_BALANCER, 'LoadBalancer').

-define(K8S_SERVICE_TYPE_VALUES_EXTERNAL_NAME, 'ExternalName').



%% The UID of the Service.
%%  
-define(K8S_SERVICE_UID, 'k8s.service.uid').


%% The annotation placed on the StatefulSet, the `<key>` being the annotation name, the value being the annotation value, even if the value is empty.
%%  
-define(K8S_STATEFULSET_ANNOTATION, 'k8s.statefulset.annotation').


%% The label placed on the StatefulSet, the `<key>` being the label name, the value being the label value, even if the value is empty.
%%  
-define(K8S_STATEFULSET_LABEL, 'k8s.statefulset.label').


%% The name of the StatefulSet.
%%  
-define(K8S_STATEFULSET_NAME, 'k8s.statefulset.name').


%% The UID of the StatefulSet.
%%  
-define(K8S_STATEFULSET_UID, 'k8s.statefulset.uid').


%% The name of K8s [StorageClass](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.30/#storageclass-v1-storage-k8s-io) object.
%%  
-define(K8S_STORAGECLASS_NAME, 'k8s.storageclass.name').


%% The name of the K8s volume.
%%  
-define(K8S_VOLUME_NAME, 'k8s.volume.name').


%% The type of the K8s volume.
%%  
-define(K8S_VOLUME_TYPE, 'k8s.volume.type').

-define(K8S_VOLUME_TYPE_VALUES_PERSISTENT_VOLUME_CLAIM, 'persistentVolumeClaim').

-define(K8S_VOLUME_TYPE_VALUES_CONFIG_MAP, 'configMap').

-define(K8S_VOLUME_TYPE_VALUES_DOWNWARD_API, 'downwardAPI').

-define(K8S_VOLUME_TYPE_VALUES_EMPTY_DIR, 'emptyDir').

-define(K8S_VOLUME_TYPE_VALUES_SECRET, 'secret').

-define(K8S_VOLUME_TYPE_VALUES_LOCAL, 'local').

