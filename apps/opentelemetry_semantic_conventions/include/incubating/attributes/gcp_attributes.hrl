
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

%% The container within GCP where the AppHub application is defined.
%%  
-define(GCP_APPHUB_APPLICATION_CONTAINER, 'gcp.apphub.application.container').


%% The name of the application as configured in AppHub.
%%  
-define(GCP_APPHUB_APPLICATION_ID, 'gcp.apphub.application.id').


%% The GCP zone or region where the application is defined.
%%  
-define(GCP_APPHUB_APPLICATION_LOCATION, 'gcp.apphub.application.location').


%% Criticality of a service indicates its importance to the business.
%%  
-define(GCP_APPHUB_SERVICE_CRITICALITY_TYPE, 'gcp.apphub.service.criticality_type').

-define(GCP_APPHUB_SERVICE_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL, 'MISSION_CRITICAL').

-define(GCP_APPHUB_SERVICE_CRITICALITY_TYPE_VALUES_HIGH, 'HIGH').

-define(GCP_APPHUB_SERVICE_CRITICALITY_TYPE_VALUES_MEDIUM, 'MEDIUM').

-define(GCP_APPHUB_SERVICE_CRITICALITY_TYPE_VALUES_LOW, 'LOW').



%% Environment of a service is the stage of a software lifecycle.
%%  
-define(GCP_APPHUB_SERVICE_ENVIRONMENT_TYPE, 'gcp.apphub.service.environment_type').

-define(GCP_APPHUB_SERVICE_ENVIRONMENT_TYPE_VALUES_PRODUCTION, 'PRODUCTION').

-define(GCP_APPHUB_SERVICE_ENVIRONMENT_TYPE_VALUES_STAGING, 'STAGING').

-define(GCP_APPHUB_SERVICE_ENVIRONMENT_TYPE_VALUES_TEST, 'TEST').

-define(GCP_APPHUB_SERVICE_ENVIRONMENT_TYPE_VALUES_DEVELOPMENT, 'DEVELOPMENT').



%% The name of the service as configured in AppHub.
%%  
-define(GCP_APPHUB_SERVICE_ID, 'gcp.apphub.service.id').


%% Criticality of a workload indicates its importance to the business.
%%  
-define(GCP_APPHUB_WORKLOAD_CRITICALITY_TYPE, 'gcp.apphub.workload.criticality_type').

-define(GCP_APPHUB_WORKLOAD_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL, 'MISSION_CRITICAL').

-define(GCP_APPHUB_WORKLOAD_CRITICALITY_TYPE_VALUES_HIGH, 'HIGH').

-define(GCP_APPHUB_WORKLOAD_CRITICALITY_TYPE_VALUES_MEDIUM, 'MEDIUM').

-define(GCP_APPHUB_WORKLOAD_CRITICALITY_TYPE_VALUES_LOW, 'LOW').



%% Environment of a workload is the stage of a software lifecycle.
%%  
-define(GCP_APPHUB_WORKLOAD_ENVIRONMENT_TYPE, 'gcp.apphub.workload.environment_type').

-define(GCP_APPHUB_WORKLOAD_ENVIRONMENT_TYPE_VALUES_PRODUCTION, 'PRODUCTION').

-define(GCP_APPHUB_WORKLOAD_ENVIRONMENT_TYPE_VALUES_STAGING, 'STAGING').

-define(GCP_APPHUB_WORKLOAD_ENVIRONMENT_TYPE_VALUES_TEST, 'TEST').

-define(GCP_APPHUB_WORKLOAD_ENVIRONMENT_TYPE_VALUES_DEVELOPMENT, 'DEVELOPMENT').



%% The name of the workload as configured in AppHub.
%%  
-define(GCP_APPHUB_WORKLOAD_ID, 'gcp.apphub.workload.id').


%% The container within GCP where the AppHub destination application is defined.
%%  
-define(GCP_APPHUB_DESTINATION_APPLICATION_CONTAINER, 'gcp.apphub_destination.application.container').


%% The name of the destination application as configured in AppHub.
%%  
-define(GCP_APPHUB_DESTINATION_APPLICATION_ID, 'gcp.apphub_destination.application.id').


%% The GCP zone or region where the destination application is defined.
%%  
-define(GCP_APPHUB_DESTINATION_APPLICATION_LOCATION, 'gcp.apphub_destination.application.location').


%% Criticality of a destination workload indicates its importance to the business as specified in [AppHub type enum](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type)
%%  
-define(GCP_APPHUB_DESTINATION_SERVICE_CRITICALITY_TYPE, 'gcp.apphub_destination.service.criticality_type').

-define(GCP_APPHUB_DESTINATION_SERVICE_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL, 'MISSION_CRITICAL').

-define(GCP_APPHUB_DESTINATION_SERVICE_CRITICALITY_TYPE_VALUES_HIGH, 'HIGH').

-define(GCP_APPHUB_DESTINATION_SERVICE_CRITICALITY_TYPE_VALUES_MEDIUM, 'MEDIUM').

-define(GCP_APPHUB_DESTINATION_SERVICE_CRITICALITY_TYPE_VALUES_LOW, 'LOW').



%% Software lifecycle stage of a destination service as defined [AppHub environment type](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type_1)
%%  
-define(GCP_APPHUB_DESTINATION_SERVICE_ENVIRONMENT_TYPE, 'gcp.apphub_destination.service.environment_type').

-define(GCP_APPHUB_DESTINATION_SERVICE_ENVIRONMENT_TYPE_VALUES_PRODUCTION, 'PRODUCTION').

-define(GCP_APPHUB_DESTINATION_SERVICE_ENVIRONMENT_TYPE_VALUES_STAGING, 'STAGING').

-define(GCP_APPHUB_DESTINATION_SERVICE_ENVIRONMENT_TYPE_VALUES_TEST, 'TEST').

-define(GCP_APPHUB_DESTINATION_SERVICE_ENVIRONMENT_TYPE_VALUES_DEVELOPMENT, 'DEVELOPMENT').



%% The name of the destination service as configured in AppHub.
%%  
-define(GCP_APPHUB_DESTINATION_SERVICE_ID, 'gcp.apphub_destination.service.id').


%% Criticality of a destination workload indicates its importance to the business as specified in [AppHub type enum](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type)
%%  
-define(GCP_APPHUB_DESTINATION_WORKLOAD_CRITICALITY_TYPE, 'gcp.apphub_destination.workload.criticality_type').

-define(GCP_APPHUB_DESTINATION_WORKLOAD_CRITICALITY_TYPE_VALUES_MISSION_CRITICAL, 'MISSION_CRITICAL').

-define(GCP_APPHUB_DESTINATION_WORKLOAD_CRITICALITY_TYPE_VALUES_HIGH, 'HIGH').

-define(GCP_APPHUB_DESTINATION_WORKLOAD_CRITICALITY_TYPE_VALUES_MEDIUM, 'MEDIUM').

-define(GCP_APPHUB_DESTINATION_WORKLOAD_CRITICALITY_TYPE_VALUES_LOW, 'LOW').



%% Environment of a destination workload is the stage of a software lifecycle as provided in the [AppHub environment type](https://cloud.google.com/app-hub/docs/reference/rest/v1/Attributes#type_1)
%%  
-define(GCP_APPHUB_DESTINATION_WORKLOAD_ENVIRONMENT_TYPE, 'gcp.apphub_destination.workload.environment_type').

-define(GCP_APPHUB_DESTINATION_WORKLOAD_ENVIRONMENT_TYPE_VALUES_PRODUCTION, 'PRODUCTION').

-define(GCP_APPHUB_DESTINATION_WORKLOAD_ENVIRONMENT_TYPE_VALUES_STAGING, 'STAGING').

-define(GCP_APPHUB_DESTINATION_WORKLOAD_ENVIRONMENT_TYPE_VALUES_TEST, 'TEST').

-define(GCP_APPHUB_DESTINATION_WORKLOAD_ENVIRONMENT_TYPE_VALUES_DEVELOPMENT, 'DEVELOPMENT').



%% The name of the destination workload as configured in AppHub.
%%  
-define(GCP_APPHUB_DESTINATION_WORKLOAD_ID, 'gcp.apphub_destination.workload.id').


%% Identifies the Google Cloud service for which the official client library is intended.
-define(GCP_CLIENT_SERVICE, 'gcp.client.service').


%% The name of the Cloud Run [execution](https://cloud.google.com/run/docs/managing/job-executions) being run for the Job, as set by the [`CLOUD_RUN_EXECUTION`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable.
%%  
-define(GCP_CLOUD_RUN_JOB_EXECUTION, 'gcp.cloud_run.job.execution').


%% The index for a task within an execution as provided by the [`CLOUD_RUN_TASK_INDEX`](https://cloud.google.com/run/docs/container-contract#jobs-env-vars) environment variable.
%%  
-define(GCP_CLOUD_RUN_JOB_TASK_INDEX, 'gcp.cloud_run.job.task_index').


%% The hostname of a GCE instance. This is the full value of the default or [custom hostname](https://cloud.google.com/compute/docs/instances/custom-hostname-vm).
%%  
-define(GCP_GCE_INSTANCE_HOSTNAME, 'gcp.gce.instance.hostname').


%% The instance name of a GCE instance. This is the value provided by `host.name`, the visible name of the instance in the Cloud Console UI, and the prefix for the default hostname of the instance as defined by the [default internal DNS name](https://cloud.google.com/compute/docs/internal-dns#instance-fully-qualified-domain-names).
%%  
-define(GCP_GCE_INSTANCE_NAME, 'gcp.gce.instance.name').


%% The name of the Instance Group Manager (IGM) that manages this VM, if any.
%%  
-define(GCP_GCE_INSTANCE_GROUP_MANAGER_NAME, 'gcp.gce.instance_group_manager.name').


%% The region of a **regional** Instance Group Manager (e.g., `us-central1`). Set this **only** when the IGM is regional.
%%  
-define(GCP_GCE_INSTANCE_GROUP_MANAGER_REGION, 'gcp.gce.instance_group_manager.region').


%% The zone of a **zonal** Instance Group Manager (e.g., `us-central1-a`). Set this **only** when the IGM is zonal.
%%  
-define(GCP_GCE_INSTANCE_GROUP_MANAGER_ZONE, 'gcp.gce.instance_group_manager.zone').
