
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
-define(CLOUD_AVAILABILITYZONE, 'cloud.availability_zone').


%% The cloud platform in use.
%%  

-define('cloud_platform.alibaba_cloud_ecs', 'alibaba_cloud_ecs').

-define('cloud_platform.alibaba_cloud_fc', 'alibaba_cloud_fc').

-define('cloud_platform.alibaba_cloud_openshift', 'alibaba_cloud_openshift').

-define('cloud_platform.aws_ec2', 'aws_ec2').

-define('cloud_platform.aws_ecs', 'aws_ecs').

-define('cloud_platform.aws_eks', 'aws_eks').

-define('cloud_platform.aws_lambda', 'aws_lambda').

-define('cloud_platform.aws_elastic_beanstalk', 'aws_elastic_beanstalk').

-define('cloud_platform.aws_app_runner', 'aws_app_runner').

-define('cloud_platform.aws_openshift', 'aws_openshift').

-define('cloud_platform.azure_vm', 'azure_vm').

-define('cloud_platform.azure_container_apps', 'azure_container_apps').

-define('cloud_platform.azure_container_instances', 'azure_container_instances').

-define('cloud_platform.azure_aks', 'azure_aks').

-define('cloud_platform.azure_functions', 'azure_functions').

-define('cloud_platform.azure_app_service', 'azure_app_service').

-define('cloud_platform.azure_openshift', 'azure_openshift').

-define('cloud_platform.gcp_bare_metal_solution', 'gcp_bare_metal_solution').

-define('cloud_platform.gcp_compute_engine', 'gcp_compute_engine').

-define('cloud_platform.gcp_cloud_run', 'gcp_cloud_run').

-define('cloud_platform.gcp_kubernetes_engine', 'gcp_kubernetes_engine').

-define('cloud_platform.gcp_cloud_functions', 'gcp_cloud_functions').

-define('cloud_platform.gcp_app_engine', 'gcp_app_engine').

-define('cloud_platform.gcp_openshift', 'gcp_openshift').

-define('cloud_platform.ibm_cloud_openshift', 'ibm_cloud_openshift').

-define('cloud_platform.tencent_cloud_cvm', 'tencent_cloud_cvm').

-define('cloud_platform.tencent_cloud_eks', 'tencent_cloud_eks').

-define('cloud_platform.tencent_cloud_scf', 'tencent_cloud_scf').

-define(cloud_platform(Custom), Custom).


%% Name of the cloud provider.
%%  

-define('cloud_provider.alibaba_cloud', 'alibaba_cloud').

-define('cloud_provider.aws', 'aws').

-define('cloud_provider.azure', 'azure').

-define('cloud_provider.gcp', 'gcp').

-define('cloud_provider.heroku', 'heroku').

-define('cloud_provider.ibm_cloud', 'ibm_cloud').

-define('cloud_provider.tencent_cloud', 'tencent_cloud').

-define(cloud_provider(Custom), Custom).


%% The geographical region the resource is running.
%%  
-define(CLOUD_REGION, 'cloud.region').


%% Cloud provider-specific native identifier of the monitored cloud resource (e.g. an [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html) on AWS, a [fully qualified resource ID](https://learn.microsoft.com/rest/api/resources/resources/get-by-id) on Azure, a [full resource name](https://cloud.google.com/apis/design/resource_names#full_resource_name) on GCP)
%%  
-define(CLOUD_RESOURCEID, 'cloud.resource_id').
