
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/k8s_attributes.hrl").


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


%% The annotation key-value pairs placed on the DaemonSet.
%%  
-define(K8S_DAEMONSET_ANNOTATION, 'k8s.daemonset.annotation').


%% The label key-value pairs placed on the DaemonSet.
%%  
-define(K8S_DAEMONSET_LABEL, 'k8s.daemonset.label').


%% The name of the DaemonSet.
%%  
-define(K8S_DAEMONSET_NAME, 'k8s.daemonset.name').


%% The UID of the DaemonSet.
%%  
-define(K8S_DAEMONSET_UID, 'k8s.daemonset.uid').


%% The annotation key-value pairs placed on the Deployment.
%%  
-define(K8S_DEPLOYMENT_ANNOTATION, 'k8s.deployment.annotation').


%% The label key-value pairs placed on the Deployment.
%%  
-define(K8S_DEPLOYMENT_LABEL, 'k8s.deployment.label').


%% The name of the Deployment.
%%  
-define(K8S_DEPLOYMENT_NAME, 'k8s.deployment.name').


%% The UID of the Deployment.
%%  
-define(K8S_DEPLOYMENT_UID, 'k8s.deployment.uid').


%% The name of the horizontal pod autoscaler.
%%  
-define(K8S_HPA_NAME, 'k8s.hpa.name').


%% The UID of the horizontal pod autoscaler.
%%  
-define(K8S_HPA_UID, 'k8s.hpa.uid').


%% The annotation key-value pairs placed on the Job.
%%  
-define(K8S_JOB_ANNOTATION, 'k8s.job.annotation').


%% The label key-value pairs placed on the Job.
%%  
-define(K8S_JOB_LABEL, 'k8s.job.label').


%% The name of the Job.
%%  
-define(K8S_JOB_NAME, 'k8s.job.name').


%% The UID of the Job.
%%  
-define(K8S_JOB_UID, 'k8s.job.uid').


%% The annotation key-value pairs placed on the Namespace.
%%  
-define(K8S_NAMESPACE_ANNOTATION, 'k8s.namespace.annotation').


%% The label key-value pairs placed on the Namespace.
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


%% The label placed on the Pod, the `<key>` being the label name, the value being the label value.
%%  
-define(K8S_POD_LABEL, 'k8s.pod.label').

%% @deprecated Replaced by `k8s.pod.label`.
%% Deprecated, use `k8s.pod.label` instead.
-define(K8S_POD_LABELS, 'k8s.pod.labels').


%% The name of the Pod.
%%  
-define(K8S_POD_NAME, 'k8s.pod.name').


%% The UID of the Pod.
%%  
-define(K8S_POD_UID, 'k8s.pod.uid').


%% The annotation key-value pairs placed on the ReplicaSet.
%%  
-define(K8S_REPLICASET_ANNOTATION, 'k8s.replicaset.annotation').


%% The label key-value pairs placed on the ReplicaSet.
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


%% The UID of the resource quota.
%%  
-define(K8S_RESOURCEQUOTA_UID, 'k8s.resourcequota.uid').


%% The annotation key-value pairs placed on the StatefulSet.
%%  
-define(K8S_STATEFULSET_ANNOTATION, 'k8s.statefulset.annotation').


%% The label key-value pairs placed on the StatefulSet.
%%  
-define(K8S_STATEFULSET_LABEL, 'k8s.statefulset.label').


%% The name of the StatefulSet.
%%  
-define(K8S_STATEFULSET_NAME, 'k8s.statefulset.name').


%% The UID of the StatefulSet.
%%  
-define(K8S_STATEFULSET_UID, 'k8s.statefulset.uid').


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

