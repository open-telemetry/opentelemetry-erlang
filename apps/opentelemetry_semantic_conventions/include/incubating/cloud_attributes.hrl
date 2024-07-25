
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

-define('CLOUD_PLATFORM_VALUES.alibaba_cloud_ecs', 'alibaba_cloud_ecs').

-define('CLOUD_PLATFORM_VALUES.alibaba_cloud_fc', 'alibaba_cloud_fc').

-define('CLOUD_PLATFORM_VALUES.alibaba_cloud_openshift', 'alibaba_cloud_openshift').

-define('CLOUD_PLATFORM_VALUES.aws_ec2', 'aws_ec2').

-define('CLOUD_PLATFORM_VALUES.aws_ecs', 'aws_ecs').

-define('CLOUD_PLATFORM_VALUES.aws_eks', 'aws_eks').

-define('CLOUD_PLATFORM_VALUES.aws_lambda', 'aws_lambda').

-define('CLOUD_PLATFORM_VALUES.aws_elastic_beanstalk', 'aws_elastic_beanstalk').

-define('CLOUD_PLATFORM_VALUES.aws_app_runner', 'aws_app_runner').

-define('CLOUD_PLATFORM_VALUES.aws_openshift', 'aws_openshift').

-define('CLOUD_PLATFORM_VALUES.azure_vm', 'azure_vm').

-define('CLOUD_PLATFORM_VALUES.azure_container_apps', 'azure_container_apps').

-define('CLOUD_PLATFORM_VALUES.azure_container_instances', 'azure_container_instances').

-define('CLOUD_PLATFORM_VALUES.azure_aks', 'azure_aks').

-define('CLOUD_PLATFORM_VALUES.azure_functions', 'azure_functions').

-define('CLOUD_PLATFORM_VALUES.azure_app_service', 'azure_app_service').

-define('CLOUD_PLATFORM_VALUES.azure_openshift', 'azure_openshift').

-define('CLOUD_PLATFORM_VALUES.gcp_bare_metal_solution', 'gcp_bare_metal_solution').

-define('CLOUD_PLATFORM_VALUES.gcp_compute_engine', 'gcp_compute_engine').

-define('CLOUD_PLATFORM_VALUES.gcp_cloud_run', 'gcp_cloud_run').

-define('CLOUD_PLATFORM_VALUES.gcp_kubernetes_engine', 'gcp_kubernetes_engine').

-define('CLOUD_PLATFORM_VALUES.gcp_cloud_functions', 'gcp_cloud_functions').

-define('CLOUD_PLATFORM_VALUES.gcp_app_engine', 'gcp_app_engine').

-define('CLOUD_PLATFORM_VALUES.gcp_openshift', 'gcp_openshift').

-define('CLOUD_PLATFORM_VALUES.ibm_cloud_openshift', 'ibm_cloud_openshift').

-define('CLOUD_PLATFORM_VALUES.tencent_cloud_cvm', 'tencent_cloud_cvm').

-define('CLOUD_PLATFORM_VALUES.tencent_cloud_eks', 'tencent_cloud_eks').

-define('CLOUD_PLATFORM_VALUES.tencent_cloud_scf', 'tencent_cloud_scf').



%% Name of the cloud provider.
%%  
-define(CLOUD_PROVIDER, 'cloud.provider').

-define('CLOUD_PROVIDER_VALUES.alibaba_cloud', 'alibaba_cloud').

-define('CLOUD_PROVIDER_VALUES.aws', 'aws').

-define('CLOUD_PROVIDER_VALUES.azure', 'azure').

-define('CLOUD_PROVIDER_VALUES.gcp', 'gcp').

-define('CLOUD_PROVIDER_VALUES.heroku', 'heroku').

-define('CLOUD_PROVIDER_VALUES.ibm_cloud', 'ibm_cloud').

-define('CLOUD_PROVIDER_VALUES.tencent_cloud', 'tencent_cloud').



%% The geographical region the resource is running.
%%  
-define(CLOUD_REGION, 'cloud.region').


%% Cloud provider-specific native identifier of the monitored cloud resource (e.g. an [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html) on AWS, a [fully qualified resource ID](https://learn.microsoft.com/rest/api/resources/resources/get-by-id) on Azure, a [full resource name](https://cloud.google.com/apis/design/resource_names#full_resource_name) on GCP)
%%  
-define(CLOUD_RESOURCE_ID, 'cloud.resource_id').
