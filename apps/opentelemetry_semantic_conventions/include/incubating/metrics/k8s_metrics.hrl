
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

%% Maximum CPU resource limit set for the container.
-define(K8S_CONTAINER_CPU_LIMIT, 'k8s.container.cpu.limit').


%% The ratio of container CPU usage to its CPU limit.
-define(K8S_CONTAINER_CPU_LIMIT_UTILIZATION, 'k8s.container.cpu.limit_utilization').


%% CPU resource requested for the container.
-define(K8S_CONTAINER_CPU_REQUEST, 'k8s.container.cpu.request').


%% The ratio of container CPU usage to its CPU request.
-define(K8S_CONTAINER_CPU_REQUEST_UTILIZATION, 'k8s.container.cpu.request_utilization').


%% Maximum ephemeral storage resource limit set for the container.
-define(K8S_CONTAINER_EPHEMERAL_STORAGE_LIMIT, 'k8s.container.ephemeral_storage.limit').


%% Ephemeral storage resource requested for the container.
-define(K8S_CONTAINER_EPHEMERAL_STORAGE_REQUEST, 'k8s.container.ephemeral_storage.request').


%% Maximum memory resource limit set for the container.
-define(K8S_CONTAINER_MEMORY_LIMIT, 'k8s.container.memory.limit').


%% Memory resource requested for the container.
-define(K8S_CONTAINER_MEMORY_REQUEST, 'k8s.container.memory.request').


%% Indicates whether the container is currently marked as ready to accept traffic, based on its readiness probe (1 = ready, 0 = not ready).
%%  
-define(K8S_CONTAINER_READY, 'k8s.container.ready').


%% Describes how many times the container has restarted (since the last counter reset).
-define(K8S_CONTAINER_RESTART_COUNT, 'k8s.container.restart.count').


%% Describes the number of K8s containers that are currently in a state for a given reason.
-define(K8S_CONTAINER_STATUS_REASON, 'k8s.container.status.reason').


%% Describes the number of K8s containers that are currently in a given state.
-define(K8S_CONTAINER_STATUS_STATE, 'k8s.container.status.state').


%% Maximum storage resource limit set for the container.
-define(K8S_CONTAINER_STORAGE_LIMIT, 'k8s.container.storage.limit').


%% Storage resource requested for the container.
-define(K8S_CONTAINER_STORAGE_REQUEST, 'k8s.container.storage.request').

%% @deprecated Replaced by `k8s.cronjob.job.active`.
%% Deprecated, use `k8s.cronjob.job.active` instead.
-define(K8S_CRONJOB_ACTIVE_JOBS, 'k8s.cronjob.active_jobs').


%% The number of actively running jobs for a cronjob.
-define(K8S_CRONJOB_JOB_ACTIVE, 'k8s.cronjob.job.active').

%% @deprecated Replaced by `k8s.daemonset.node.current_scheduled`.
%% Deprecated, use `k8s.daemonset.node.current_scheduled` instead.
-define(K8S_DAEMONSET_CURRENT_SCHEDULED_NODES, 'k8s.daemonset.current_scheduled_nodes').

%% @deprecated Replaced by `k8s.daemonset.node.desired_scheduled`.
%% Deprecated, use `k8s.daemonset.node.desired_scheduled` instead.
-define(K8S_DAEMONSET_DESIRED_SCHEDULED_NODES, 'k8s.daemonset.desired_scheduled_nodes').

%% @deprecated Replaced by `k8s.daemonset.node.misscheduled`.
%% Deprecated, use `k8s.daemonset.node.misscheduled` instead.
-define(K8S_DAEMONSET_MISSCHEDULED_NODES, 'k8s.daemonset.misscheduled_nodes').


%% Number of nodes that are running at least 1 daemon pod and are supposed to run the daemon pod.
-define(K8S_DAEMONSET_NODE_CURRENT_SCHEDULED, 'k8s.daemonset.node.current_scheduled').


%% Number of nodes that should be running the daemon pod (including nodes currently running the daemon pod).
-define(K8S_DAEMONSET_NODE_DESIRED_SCHEDULED, 'k8s.daemonset.node.desired_scheduled').


%% Number of nodes that are running the daemon pod, but are not supposed to run the daemon pod.
-define(K8S_DAEMONSET_NODE_MISSCHEDULED, 'k8s.daemonset.node.misscheduled').


%% Number of nodes that should be running the daemon pod and have one or more of the daemon pod running and ready.
-define(K8S_DAEMONSET_NODE_READY, 'k8s.daemonset.node.ready').

%% @deprecated Replaced by `k8s.daemonset.node.ready`.
%% Deprecated, use `k8s.daemonset.node.ready` instead.
-define(K8S_DAEMONSET_READY_NODES, 'k8s.daemonset.ready_nodes').

%% @deprecated Replaced by `k8s.deployment.pod.available`.
%% Deprecated, use `k8s.deployment.pod.available` instead.
-define(K8S_DEPLOYMENT_AVAILABLE_PODS, 'k8s.deployment.available_pods').

%% @deprecated Replaced by `k8s.deployment.pod.desired`.
%% Deprecated, use `k8s.deployment.pod.desired` instead.
-define(K8S_DEPLOYMENT_DESIRED_PODS, 'k8s.deployment.desired_pods').


%% Total number of available replica pods (ready for at least minReadySeconds) targeted by this deployment.
-define(K8S_DEPLOYMENT_POD_AVAILABLE, 'k8s.deployment.pod.available').


%% Number of desired replica pods in this deployment.
-define(K8S_DEPLOYMENT_POD_DESIRED, 'k8s.deployment.pod.desired').

%% @deprecated Replaced by `k8s.hpa.pod.current`.
%% Deprecated, use `k8s.hpa.pod.current` instead.
-define(K8S_HPA_CURRENT_PODS, 'k8s.hpa.current_pods').

%% @deprecated Replaced by `k8s.hpa.pod.desired`.
%% Deprecated, use `k8s.hpa.pod.desired` instead.
-define(K8S_HPA_DESIRED_PODS, 'k8s.hpa.desired_pods').

%% @deprecated Replaced by `k8s.hpa.pod.max`.
%% Deprecated, use `k8s.hpa.pod.max` instead.
-define(K8S_HPA_MAX_PODS, 'k8s.hpa.max_pods').


%% Target average utilization, in percentage, for CPU resource in HPA config.
-define(K8S_HPA_METRIC_TARGET_CPU_AVERAGE_UTILIZATION, 'k8s.hpa.metric.target.cpu.average_utilization').


%% Target average value for CPU resource in HPA config.
-define(K8S_HPA_METRIC_TARGET_CPU_AVERAGE_VALUE, 'k8s.hpa.metric.target.cpu.average_value').


%% Target value for CPU resource in HPA config.
-define(K8S_HPA_METRIC_TARGET_CPU_VALUE, 'k8s.hpa.metric.target.cpu.value').

%% @deprecated Replaced by `k8s.hpa.pod.min`.
%% Deprecated, use `k8s.hpa.pod.min` instead.
-define(K8S_HPA_MIN_PODS, 'k8s.hpa.min_pods').


%% Current number of replica pods managed by this horizontal pod autoscaler, as last seen by the autoscaler.
-define(K8S_HPA_POD_CURRENT, 'k8s.hpa.pod.current').


%% Desired number of replica pods managed by this horizontal pod autoscaler, as last calculated by the autoscaler.
-define(K8S_HPA_POD_DESIRED, 'k8s.hpa.pod.desired').


%% The upper limit for the number of replica pods to which the autoscaler can scale up.
-define(K8S_HPA_POD_MAX, 'k8s.hpa.pod.max').


%% The lower limit for the number of replica pods to which the autoscaler can scale down.
-define(K8S_HPA_POD_MIN, 'k8s.hpa.pod.min').

%% @deprecated Replaced by `k8s.job.pod.active`.
%% Deprecated, use `k8s.job.pod.active` instead.
-define(K8S_JOB_ACTIVE_PODS, 'k8s.job.active_pods').

%% @deprecated Replaced by `k8s.job.pod.desired_successful`.
%% Deprecated, use `k8s.job.pod.desired_successful` instead.
-define(K8S_JOB_DESIRED_SUCCESSFUL_PODS, 'k8s.job.desired_successful_pods').

%% @deprecated Replaced by `k8s.job.pod.failed`.
%% Deprecated, use `k8s.job.pod.failed` instead.
-define(K8S_JOB_FAILED_PODS, 'k8s.job.failed_pods').

%% @deprecated Replaced by `k8s.job.pod.max_parallel`.
%% Deprecated, use `k8s.job.pod.max_parallel` instead.
-define(K8S_JOB_MAX_PARALLEL_PODS, 'k8s.job.max_parallel_pods').


%% The number of pending and actively running pods for a job.
-define(K8S_JOB_POD_ACTIVE, 'k8s.job.pod.active').


%% The desired number of successfully finished pods the job should be run with.
-define(K8S_JOB_POD_DESIRED_SUCCESSFUL, 'k8s.job.pod.desired_successful').


%% The number of pods which reached phase Failed for a job.
-define(K8S_JOB_POD_FAILED, 'k8s.job.pod.failed').


%% The max desired number of pods the job should run at any given time.
-define(K8S_JOB_POD_MAX_PARALLEL, 'k8s.job.pod.max_parallel').


%% The number of pods which reached phase Succeeded for a job.
-define(K8S_JOB_POD_SUCCESSFUL, 'k8s.job.pod.successful').

%% @deprecated Replaced by `k8s.job.pod.successful`.
%% Deprecated, use `k8s.job.pod.successful` instead.
-define(K8S_JOB_SUCCESSFUL_PODS, 'k8s.job.successful_pods').


%% Describes number of K8s namespaces that are currently in a given phase.
-define(K8S_NAMESPACE_PHASE, 'k8s.namespace.phase').

%% @deprecated Replaced by `k8s.node.cpu.allocatable`.
%% Deprecated, use `k8s.node.cpu.allocatable` instead.
-define(K8S_NODE_ALLOCATABLE_CPU, 'k8s.node.allocatable.cpu').

%% @deprecated Replaced by `k8s.node.ephemeral_storage.allocatable`.
%% Deprecated, use `k8s.node.ephemeral_storage.allocatable` instead.
-define(K8S_NODE_ALLOCATABLE_EPHEMERAL_STORAGE, 'k8s.node.allocatable.ephemeral_storage').

%% @deprecated Replaced by `k8s.node.memory.allocatable`.
%% Deprecated, use `k8s.node.memory.allocatable` instead.
-define(K8S_NODE_ALLOCATABLE_MEMORY, 'k8s.node.allocatable.memory').

%% @deprecated Replaced by `k8s.node.pod.allocatable`.
%% Deprecated, use `k8s.node.pod.allocatable` instead.
-define(K8S_NODE_ALLOCATABLE_PODS, 'k8s.node.allocatable.pods').


%% Describes the condition of a particular Node.
-define(K8S_NODE_CONDITION_STATUS, 'k8s.node.condition.status').


%% Amount of cpu allocatable on the node.
-define(K8S_NODE_CPU_ALLOCATABLE, 'k8s.node.cpu.allocatable').


%% Total CPU time consumed.
-define(K8S_NODE_CPU_TIME, 'k8s.node.cpu.time').


%% Node's CPU usage, measured in cpus. Range from 0 to the number of allocatable CPUs.
-define(K8S_NODE_CPU_USAGE, 'k8s.node.cpu.usage').


%% Amount of ephemeral-storage allocatable on the node.
-define(K8S_NODE_EPHEMERAL_STORAGE_ALLOCATABLE, 'k8s.node.ephemeral_storage.allocatable').


%% Node filesystem available bytes.
-define(K8S_NODE_FILESYSTEM_AVAILABLE, 'k8s.node.filesystem.available').


%% Node filesystem capacity.
-define(K8S_NODE_FILESYSTEM_CAPACITY, 'k8s.node.filesystem.capacity').


%% Node filesystem usage.
-define(K8S_NODE_FILESYSTEM_USAGE, 'k8s.node.filesystem.usage').


%% Amount of memory allocatable on the node.
-define(K8S_NODE_MEMORY_ALLOCATABLE, 'k8s.node.memory.allocatable').


%% Node memory available.
-define(K8S_NODE_MEMORY_AVAILABLE, 'k8s.node.memory.available').


%% Node memory paging faults.
-define(K8S_NODE_MEMORY_PAGING_FAULTS, 'k8s.node.memory.paging.faults').


%% Node memory RSS.
-define(K8S_NODE_MEMORY_RSS, 'k8s.node.memory.rss').


%% Memory usage of the Node.
-define(K8S_NODE_MEMORY_USAGE, 'k8s.node.memory.usage').


%% Node memory working set.
-define(K8S_NODE_MEMORY_WORKING_SET, 'k8s.node.memory.working_set').


%% Node network errors.
-define(K8S_NODE_NETWORK_ERRORS, 'k8s.node.network.errors').


%% Network bytes for the Node.
-define(K8S_NODE_NETWORK_IO, 'k8s.node.network.io').


%% Amount of pods allocatable on the node.
-define(K8S_NODE_POD_ALLOCATABLE, 'k8s.node.pod.allocatable').


%% The time the Node has been running.
-define(K8S_NODE_UPTIME, 'k8s.node.uptime').


%% Total CPU time consumed.
-define(K8S_POD_CPU_TIME, 'k8s.pod.cpu.time').


%% Pod's CPU usage, measured in cpus. Range from 0 to the number of allocatable CPUs.
-define(K8S_POD_CPU_USAGE, 'k8s.pod.cpu.usage').


%% Pod filesystem available bytes.
-define(K8S_POD_FILESYSTEM_AVAILABLE, 'k8s.pod.filesystem.available').


%% Pod filesystem capacity.
-define(K8S_POD_FILESYSTEM_CAPACITY, 'k8s.pod.filesystem.capacity').


%% Pod filesystem usage.
-define(K8S_POD_FILESYSTEM_USAGE, 'k8s.pod.filesystem.usage').


%% Pod memory available.
-define(K8S_POD_MEMORY_AVAILABLE, 'k8s.pod.memory.available').


%% Pod memory paging faults.
-define(K8S_POD_MEMORY_PAGING_FAULTS, 'k8s.pod.memory.paging.faults').


%% Pod memory RSS.
-define(K8S_POD_MEMORY_RSS, 'k8s.pod.memory.rss').


%% Memory usage of the Pod.
-define(K8S_POD_MEMORY_USAGE, 'k8s.pod.memory.usage').


%% Pod memory working set.
-define(K8S_POD_MEMORY_WORKING_SET, 'k8s.pod.memory.working_set').


%% Pod network errors.
-define(K8S_POD_NETWORK_ERRORS, 'k8s.pod.network.errors').


%% Network bytes for the Pod.
-define(K8S_POD_NETWORK_IO, 'k8s.pod.network.io').


%% Describes number of K8s Pods that are currently in a given phase.
-define(K8S_POD_STATUS_PHASE, 'k8s.pod.status.phase').


%% Describes the number of K8s Pods that are currently in a state for a given reason.
-define(K8S_POD_STATUS_REASON, 'k8s.pod.status.reason').


%% The time the Pod has been running.
-define(K8S_POD_UPTIME, 'k8s.pod.uptime').


%% Pod volume storage space available.
-define(K8S_POD_VOLUME_AVAILABLE, 'k8s.pod.volume.available').


%% Pod volume total capacity.
-define(K8S_POD_VOLUME_CAPACITY, 'k8s.pod.volume.capacity').


%% The total inodes in the filesystem of the Pod's volume.
-define(K8S_POD_VOLUME_INODE_COUNT, 'k8s.pod.volume.inode.count').


%% The free inodes in the filesystem of the Pod's volume.
-define(K8S_POD_VOLUME_INODE_FREE, 'k8s.pod.volume.inode.free').


%% The inodes used by the filesystem of the Pod's volume.
-define(K8S_POD_VOLUME_INODE_USED, 'k8s.pod.volume.inode.used').


%% Pod volume usage.
-define(K8S_POD_VOLUME_USAGE, 'k8s.pod.volume.usage').

%% @deprecated Replaced by `k8s.replicaset.pod.available`.
%% Deprecated, use `k8s.replicaset.pod.available` instead.
-define(K8S_REPLICASET_AVAILABLE_PODS, 'k8s.replicaset.available_pods').

%% @deprecated Replaced by `k8s.replicaset.pod.desired`.
%% Deprecated, use `k8s.replicaset.pod.desired` instead.
-define(K8S_REPLICASET_DESIRED_PODS, 'k8s.replicaset.desired_pods').


%% Total number of available replica pods (ready for at least minReadySeconds) targeted by this replicaset.
-define(K8S_REPLICASET_POD_AVAILABLE, 'k8s.replicaset.pod.available').


%% Number of desired replica pods in this replicaset.
-define(K8S_REPLICASET_POD_DESIRED, 'k8s.replicaset.pod.desired').

%% @deprecated Replaced by `k8s.replicationcontroller.pod.available`.
%% Deprecated, use `k8s.replicationcontroller.pod.available` instead.
-define(K8S_REPLICATION_CONTROLLER_AVAILABLE_PODS, 'k8s.replication_controller.available_pods').

%% @deprecated Replaced by `k8s.replicationcontroller.pod.desired`.
%% Deprecated, use `k8s.replicationcontroller.pod.desired` instead.
-define(K8S_REPLICATION_CONTROLLER_DESIRED_PODS, 'k8s.replication_controller.desired_pods').

%% @deprecated Replaced by `k8s.replicationcontroller.pod.available`.
%% Deprecated, use `k8s.replicationcontroller.pod.available` instead.
-define(K8S_REPLICATIONCONTROLLER_AVAILABLE_PODS, 'k8s.replicationcontroller.available_pods').

%% @deprecated Replaced by `k8s.replicationcontroller.pod.desired`.
%% Deprecated, use `k8s.replicationcontroller.pod.desired` instead.
-define(K8S_REPLICATIONCONTROLLER_DESIRED_PODS, 'k8s.replicationcontroller.desired_pods').


%% Total number of available replica pods (ready for at least minReadySeconds) targeted by this replication controller.
-define(K8S_REPLICATIONCONTROLLER_POD_AVAILABLE, 'k8s.replicationcontroller.pod.available').


%% Number of desired replica pods in this replication controller.
-define(K8S_REPLICATIONCONTROLLER_POD_DESIRED, 'k8s.replicationcontroller.pod.desired').


%% The CPU limits in a specific namespace.
%%  The value represents the configured quota limit of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_CPU_LIMIT_HARD, 'k8s.resourcequota.cpu.limit.hard').


%% The CPU limits in a specific namespace.
%%  The value represents the current observed total usage of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_CPU_LIMIT_USED, 'k8s.resourcequota.cpu.limit.used').


%% The CPU requests in a specific namespace.
%%  The value represents the configured quota limit of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_CPU_REQUEST_HARD, 'k8s.resourcequota.cpu.request.hard').


%% The CPU requests in a specific namespace.
%%  The value represents the current observed total usage of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_CPU_REQUEST_USED, 'k8s.resourcequota.cpu.request.used').


%% The sum of local ephemeral storage limits in the namespace.
%%  The value represents the configured quota limit of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_EPHEMERAL_STORAGE_LIMIT_HARD, 'k8s.resourcequota.ephemeral_storage.limit.hard').


%% The sum of local ephemeral storage limits in the namespace.
%%  The value represents the current observed total usage of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_EPHEMERAL_STORAGE_LIMIT_USED, 'k8s.resourcequota.ephemeral_storage.limit.used').


%% The sum of local ephemeral storage requests in the namespace.
%%  The value represents the configured quota limit of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_EPHEMERAL_STORAGE_REQUEST_HARD, 'k8s.resourcequota.ephemeral_storage.request.hard').


%% The sum of local ephemeral storage requests in the namespace.
%%  The value represents the current observed total usage of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_EPHEMERAL_STORAGE_REQUEST_USED, 'k8s.resourcequota.ephemeral_storage.request.used').


%% The huge page requests in a specific namespace.
%%  The value represents the configured quota limit of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_HUGEPAGE_COUNT_REQUEST_HARD, 'k8s.resourcequota.hugepage_count.request.hard').


%% The huge page requests in a specific namespace.
%%  The value represents the current observed total usage of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_HUGEPAGE_COUNT_REQUEST_USED, 'k8s.resourcequota.hugepage_count.request.used').


%% The memory limits in a specific namespace.
%%  The value represents the configured quota limit of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_MEMORY_LIMIT_HARD, 'k8s.resourcequota.memory.limit.hard').


%% The memory limits in a specific namespace.
%%  The value represents the current observed total usage of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_MEMORY_LIMIT_USED, 'k8s.resourcequota.memory.limit.used').


%% The memory requests in a specific namespace.
%%  The value represents the configured quota limit of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_MEMORY_REQUEST_HARD, 'k8s.resourcequota.memory.request.hard').


%% The memory requests in a specific namespace.
%%  The value represents the current observed total usage of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_MEMORY_REQUEST_USED, 'k8s.resourcequota.memory.request.used').


%% The object count limits in a specific namespace.
%%  The value represents the configured quota limit of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_OBJECT_COUNT_HARD, 'k8s.resourcequota.object_count.hard').


%% The object count limits in a specific namespace.
%%  The value represents the current observed total usage of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_OBJECT_COUNT_USED, 'k8s.resourcequota.object_count.used').


%% The total number of PersistentVolumeClaims that can exist in the namespace.
%%  The value represents the configured quota limit of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_PERSISTENTVOLUMECLAIM_COUNT_HARD, 'k8s.resourcequota.persistentvolumeclaim_count.hard').


%% The total number of PersistentVolumeClaims that can exist in the namespace.
%%  The value represents the current observed total usage of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_PERSISTENTVOLUMECLAIM_COUNT_USED, 'k8s.resourcequota.persistentvolumeclaim_count.used').


%% The storage requests in a specific namespace.
%%  The value represents the configured quota limit of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_STORAGE_REQUEST_HARD, 'k8s.resourcequota.storage.request.hard').


%% The storage requests in a specific namespace.
%%  The value represents the current observed total usage of the resource in the namespace.
%%  
-define(K8S_RESOURCEQUOTA_STORAGE_REQUEST_USED, 'k8s.resourcequota.storage.request.used').


%% Number of endpoints for a service by condition and address type.
-define(K8S_SERVICE_ENDPOINT_COUNT, 'k8s.service.endpoint.count').


%% Number of load balancer ingress points (external IPs/hostnames) assigned to the service.
-define(K8S_SERVICE_LOAD_BALANCER_INGRESS_COUNT, 'k8s.service.load_balancer.ingress.count').

%% @deprecated Replaced by `k8s.statefulset.pod.current`.
%% Deprecated, use `k8s.statefulset.pod.current` instead.
-define(K8S_STATEFULSET_CURRENT_PODS, 'k8s.statefulset.current_pods').

%% @deprecated Replaced by `k8s.statefulset.pod.desired`.
%% Deprecated, use `k8s.statefulset.pod.desired` instead.
-define(K8S_STATEFULSET_DESIRED_PODS, 'k8s.statefulset.desired_pods').


%% The number of replica pods created by the statefulset controller from the statefulset version indicated by currentRevision.
-define(K8S_STATEFULSET_POD_CURRENT, 'k8s.statefulset.pod.current').


%% Number of desired replica pods in this statefulset.
-define(K8S_STATEFULSET_POD_DESIRED, 'k8s.statefulset.pod.desired').


%% The number of replica pods created for this statefulset with a Ready Condition.
-define(K8S_STATEFULSET_POD_READY, 'k8s.statefulset.pod.ready').


%% Number of replica pods created by the statefulset controller from the statefulset version indicated by updateRevision.
-define(K8S_STATEFULSET_POD_UPDATED, 'k8s.statefulset.pod.updated').

%% @deprecated Replaced by `k8s.statefulset.pod.ready`.
%% Deprecated, use `k8s.statefulset.pod.ready` instead.
-define(K8S_STATEFULSET_READY_PODS, 'k8s.statefulset.ready_pods').

%% @deprecated Replaced by `k8s.statefulset.pod.updated`.
%% Deprecated, use `k8s.statefulset.pod.updated` instead.
-define(K8S_STATEFULSET_UPDATED_PODS, 'k8s.statefulset.updated_pods').
