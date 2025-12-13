
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

%% The number of actively running jobs for a cronjob
-define(K8S_CRONJOB_ACTIVE_JOBS, 'k8s.cronjob.active_jobs').


%% Number of nodes that are running at least 1 daemon pod and are supposed to run the daemon pod
-define(K8S_DAEMONSET_CURRENT_SCHEDULED_NODES, 'k8s.daemonset.current_scheduled_nodes').


%% Number of nodes that should be running the daemon pod (including nodes currently running the daemon pod)
-define(K8S_DAEMONSET_DESIRED_SCHEDULED_NODES, 'k8s.daemonset.desired_scheduled_nodes').


%% Number of nodes that are running the daemon pod, but are not supposed to run the daemon pod
-define(K8S_DAEMONSET_MISSCHEDULED_NODES, 'k8s.daemonset.misscheduled_nodes').


%% Number of nodes that should be running the daemon pod and have one or more of the daemon pod running and ready
-define(K8S_DAEMONSET_READY_NODES, 'k8s.daemonset.ready_nodes').


%% Total number of available replica pods (ready for at least minReadySeconds) targeted by this deployment
-define(K8S_DEPLOYMENT_AVAILABLE_PODS, 'k8s.deployment.available_pods').


%% Number of desired replica pods in this deployment
-define(K8S_DEPLOYMENT_DESIRED_PODS, 'k8s.deployment.desired_pods').


%% Current number of replica pods managed by this horizontal pod autoscaler, as last seen by the autoscaler
-define(K8S_HPA_CURRENT_PODS, 'k8s.hpa.current_pods').


%% Desired number of replica pods managed by this horizontal pod autoscaler, as last calculated by the autoscaler
-define(K8S_HPA_DESIRED_PODS, 'k8s.hpa.desired_pods').


%% The upper limit for the number of replica pods to which the autoscaler can scale up
-define(K8S_HPA_MAX_PODS, 'k8s.hpa.max_pods').


%% The lower limit for the number of replica pods to which the autoscaler can scale down
-define(K8S_HPA_MIN_PODS, 'k8s.hpa.min_pods').


%% The number of pending and actively running pods for a job
-define(K8S_JOB_ACTIVE_PODS, 'k8s.job.active_pods').


%% The desired number of successfully finished pods the job should be run with
-define(K8S_JOB_DESIRED_SUCCESSFUL_PODS, 'k8s.job.desired_successful_pods').


%% The number of pods which reached phase Failed for a job
-define(K8S_JOB_FAILED_PODS, 'k8s.job.failed_pods').


%% The max desired number of pods the job should run at any given time
-define(K8S_JOB_MAX_PARALLEL_PODS, 'k8s.job.max_parallel_pods').


%% The number of pods which reached phase Succeeded for a job
-define(K8S_JOB_SUCCESSFUL_PODS, 'k8s.job.successful_pods').


%% Describes number of K8s namespaces that are currently in a given phase.
-define(K8S_NAMESPACE_PHASE, 'k8s.namespace.phase').


%% Total CPU time consumed
-define(K8S_NODE_CPU_TIME, 'k8s.node.cpu.time').


%% Node's CPU usage, measured in cpus. Range from 0 to the number of allocatable CPUs
-define(K8S_NODE_CPU_USAGE, 'k8s.node.cpu.usage').


%% Memory usage of the Node
-define(K8S_NODE_MEMORY_USAGE, 'k8s.node.memory.usage').


%% Node network errors
-define(K8S_NODE_NETWORK_ERRORS, 'k8s.node.network.errors').


%% Network bytes for the Node
-define(K8S_NODE_NETWORK_IO, 'k8s.node.network.io').


%% The time the Node has been running
-define(K8S_NODE_UPTIME, 'k8s.node.uptime').


%% Total CPU time consumed
-define(K8S_POD_CPU_TIME, 'k8s.pod.cpu.time').


%% Pod's CPU usage, measured in cpus. Range from 0 to the number of allocatable CPUs
-define(K8S_POD_CPU_USAGE, 'k8s.pod.cpu.usage').


%% Memory usage of the Pod
-define(K8S_POD_MEMORY_USAGE, 'k8s.pod.memory.usage').


%% Pod network errors
-define(K8S_POD_NETWORK_ERRORS, 'k8s.pod.network.errors').


%% Network bytes for the Pod
-define(K8S_POD_NETWORK_IO, 'k8s.pod.network.io').


%% The time the Pod has been running
-define(K8S_POD_UPTIME, 'k8s.pod.uptime').


%% Total number of available replica pods (ready for at least minReadySeconds) targeted by this replicaset
-define(K8S_REPLICASET_AVAILABLE_PODS, 'k8s.replicaset.available_pods').


%% Number of desired replica pods in this replicaset
-define(K8S_REPLICASET_DESIRED_PODS, 'k8s.replicaset.desired_pods').

%% @deprecated Replaced by `k8s.replicationcontroller.available_pods`.
%% Deprecated, use `k8s.replicationcontroller.available_pods` instead.
-define(K8S_REPLICATION_CONTROLLER_AVAILABLE_PODS, 'k8s.replication_controller.available_pods').

%% @deprecated Replaced by `k8s.replicationcontroller.desired_pods`.
%% Deprecated, use `k8s.replicationcontroller.desired_pods` instead.
-define(K8S_REPLICATION_CONTROLLER_DESIRED_PODS, 'k8s.replication_controller.desired_pods').


%% Total number of available replica pods (ready for at least minReadySeconds) targeted by this replication controller
-define(K8S_REPLICATIONCONTROLLER_AVAILABLE_PODS, 'k8s.replicationcontroller.available_pods').


%% Number of desired replica pods in this replication controller
-define(K8S_REPLICATIONCONTROLLER_DESIRED_PODS, 'k8s.replicationcontroller.desired_pods').


%% The number of replica pods created by the statefulset controller from the statefulset version indicated by currentRevision
-define(K8S_STATEFULSET_CURRENT_PODS, 'k8s.statefulset.current_pods').


%% Number of desired replica pods in this statefulset
-define(K8S_STATEFULSET_DESIRED_PODS, 'k8s.statefulset.desired_pods').


%% The number of replica pods created for this statefulset with a Ready Condition
-define(K8S_STATEFULSET_READY_PODS, 'k8s.statefulset.ready_pods').


%% Number of replica pods created by the statefulset controller from the statefulset version indicated by updateRevision
-define(K8S_STATEFULSET_UPDATED_PODS, 'k8s.statefulset.updated_pods').
