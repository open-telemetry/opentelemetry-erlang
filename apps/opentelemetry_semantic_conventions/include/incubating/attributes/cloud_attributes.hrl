
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

%% The cloud account ID the resource is assigned to.
%%  
-define(CLOUD_ACCOUNT_ID, 'cloud.account.id').


%% Cloud regions often have multiple, isolated locations known as zones to increase availability. Availability zone represents the zone where the resource is running.
%%  
-define(CLOUD_AVAILABILITY_ZONE, 'cloud.availability_zone').


%% The cloud platform in use.
%%  
-define(CLOUD_PLATFORM, 'cloud.platform').

-define(CLOUD_PLATFORM_VALUES_ALIBABA_CLOUD_ECS, 'alibaba_cloud_ecs').

-define(CLOUD_PLATFORM_VALUES_ALIBABA_CLOUD_FC, 'alibaba_cloud_fc').

-define(CLOUD_PLATFORM_VALUES_ALIBABA_CLOUD_OPENSHIFT, 'alibaba_cloud_openshift').

-define(CLOUD_PLATFORM_VALUES_AWS_EC2, 'aws_ec2').

-define(CLOUD_PLATFORM_VALUES_AWS_ECS, 'aws_ecs').

-define(CLOUD_PLATFORM_VALUES_AWS_EKS, 'aws_eks').

-define(CLOUD_PLATFORM_VALUES_AWS_LAMBDA, 'aws_lambda').

-define(CLOUD_PLATFORM_VALUES_AWS_ELASTIC_BEANSTALK, 'aws_elastic_beanstalk').

-define(CLOUD_PLATFORM_VALUES_AWS_APP_RUNNER, 'aws_app_runner').

-define(CLOUD_PLATFORM_VALUES_AWS_OPENSHIFT, 'aws_openshift').

-define(CLOUD_PLATFORM_VALUES_AZURE_VM, 'azure_vm').

-define(CLOUD_PLATFORM_VALUES_AZURE_CONTAINER_APPS, 'azure_container_apps').

-define(CLOUD_PLATFORM_VALUES_AZURE_CONTAINER_INSTANCES, 'azure_container_instances').

-define(CLOUD_PLATFORM_VALUES_AZURE_AKS, 'azure_aks').

-define(CLOUD_PLATFORM_VALUES_AZURE_FUNCTIONS, 'azure_functions').

-define(CLOUD_PLATFORM_VALUES_AZURE_APP_SERVICE, 'azure_app_service').

-define(CLOUD_PLATFORM_VALUES_AZURE_OPENSHIFT, 'azure_openshift').

-define(CLOUD_PLATFORM_VALUES_GCP_BARE_METAL_SOLUTION, 'gcp_bare_metal_solution').

-define(CLOUD_PLATFORM_VALUES_GCP_COMPUTE_ENGINE, 'gcp_compute_engine').

-define(CLOUD_PLATFORM_VALUES_GCP_CLOUD_RUN, 'gcp_cloud_run').

-define(CLOUD_PLATFORM_VALUES_GCP_KUBERNETES_ENGINE, 'gcp_kubernetes_engine').

-define(CLOUD_PLATFORM_VALUES_GCP_CLOUD_FUNCTIONS, 'gcp_cloud_functions').

-define(CLOUD_PLATFORM_VALUES_GCP_APP_ENGINE, 'gcp_app_engine').

-define(CLOUD_PLATFORM_VALUES_GCP_OPENSHIFT, 'gcp_openshift').

-define(CLOUD_PLATFORM_VALUES_IBM_CLOUD_OPENSHIFT, 'ibm_cloud_openshift').

-define(CLOUD_PLATFORM_VALUES_TENCENT_CLOUD_CVM, 'tencent_cloud_cvm').

-define(CLOUD_PLATFORM_VALUES_TENCENT_CLOUD_EKS, 'tencent_cloud_eks').

-define(CLOUD_PLATFORM_VALUES_TENCENT_CLOUD_SCF, 'tencent_cloud_scf').



%% Name of the cloud provider.
%%  
-define(CLOUD_PROVIDER, 'cloud.provider').

-define(CLOUD_PROVIDER_VALUES_ALIBABA_CLOUD, 'alibaba_cloud').

-define(CLOUD_PROVIDER_VALUES_AWS, 'aws').

-define(CLOUD_PROVIDER_VALUES_AZURE, 'azure').

-define(CLOUD_PROVIDER_VALUES_GCP, 'gcp').

-define(CLOUD_PROVIDER_VALUES_HEROKU, 'heroku').

-define(CLOUD_PROVIDER_VALUES_IBM_CLOUD, 'ibm_cloud').

-define(CLOUD_PROVIDER_VALUES_TENCENT_CLOUD, 'tencent_cloud').



%% The geographical region the resource is running.
%%  
-define(CLOUD_REGION, 'cloud.region').


%% Cloud provider-specific native identifier of the monitored cloud resource (e.g. an [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html) on AWS, a [fully qualified resource ID](https://learn.microsoft.com/rest/api/resources/resources/get-by-id) on Azure, a [full resource name](https://cloud.google.com/apis/design/resource_names#full_resource_name) on GCP)
%%  
-define(CLOUD_RESOURCE_ID, 'cloud.resource_id').
