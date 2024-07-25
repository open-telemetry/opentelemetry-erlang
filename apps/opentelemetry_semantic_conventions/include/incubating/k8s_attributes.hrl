
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
-define(K_8_S_CLUSTER_NAME, 'k8s.cluster.name').


%% A pseudo-ID for the cluster, set to the UID of the `kube-system` namespace.
%%  
-define(K_8_S_CLUSTER_UID, 'k8s.cluster.uid').


%% The name of the Container from Pod specification, must be unique within a Pod. Container runtime usually uses different globally unique name (`container.name`).
%%  
-define(K_8_S_CONTAINER_NAME, 'k8s.container.name').


%% Number of times the container was restarted. This attribute can be used to identify a particular container (running or stopped) within a container spec.
%%  
-define(K_8_S_CONTAINER_RESTARTCOUNT, 'k8s.container.restart_count').


%% Last terminated reason of the Container.
%%  
-define(K_8_S_CONTAINER_STATUS_LASTTERMINATEDREASON, 'k8s.container.status.last_terminated_reason').


%% The name of the CronJob.
%%  
-define(K_8_S_CRONJOB_NAME, 'k8s.cronjob.name').


%% The UID of the CronJob.
%%  
-define(K_8_S_CRONJOB_UID, 'k8s.cronjob.uid').


%% The name of the DaemonSet.
%%  
-define(K_8_S_DAEMONSET_NAME, 'k8s.daemonset.name').


%% The UID of the DaemonSet.
%%  
-define(K_8_S_DAEMONSET_UID, 'k8s.daemonset.uid').


%% The name of the Deployment.
%%  
-define(K_8_S_DEPLOYMENT_NAME, 'k8s.deployment.name').


%% The UID of the Deployment.
%%  
-define(K_8_S_DEPLOYMENT_UID, 'k8s.deployment.uid').


%% The name of the Job.
%%  
-define(K_8_S_JOB_NAME, 'k8s.job.name').


%% The UID of the Job.
%%  
-define(K_8_S_JOB_UID, 'k8s.job.uid').


%% The name of the namespace that the pod is running in.
%%  
-define(K_8_S_NAMESPACE_NAME, 'k8s.namespace.name').


%% The name of the Node.
%%  
-define(K_8_S_NODE_NAME, 'k8s.node.name').


%% The UID of the Node.
%%  
-define(K_8_S_NODE_UID, 'k8s.node.uid').


%% The annotation key-value pairs placed on the Pod, the `<key>` being the annotation name, the value being the annotation value.
%%  
-define(K_8_S_POD_ANNOTATION, 'k8s.pod.annotation').


%% The label key-value pairs placed on the Pod, the `<key>` being the label name, the value being the label value.
%%  
-define(K_8_S_POD_LABEL, 'k8s.pod.label').

%% @deprecated Replaced by `k8s.pod.label`.
%% Deprecated, use `k8s.pod.label` instead.
-define(K_8_S_POD_LABELS, 'k8s.pod.labels').


%% The name of the Pod.
%%  
-define(K_8_S_POD_NAME, 'k8s.pod.name').


%% The UID of the Pod.
%%  
-define(K_8_S_POD_UID, 'k8s.pod.uid').


%% The name of the ReplicaSet.
%%  
-define(K_8_S_REPLICASET_NAME, 'k8s.replicaset.name').


%% The UID of the ReplicaSet.
%%  
-define(K_8_S_REPLICASET_UID, 'k8s.replicaset.uid').


%% The name of the StatefulSet.
%%  
-define(K_8_S_STATEFULSET_NAME, 'k8s.statefulset.name').


%% The UID of the StatefulSet.
%%  
-define(K_8_S_STATEFULSET_UID, 'k8s.statefulset.uid').
