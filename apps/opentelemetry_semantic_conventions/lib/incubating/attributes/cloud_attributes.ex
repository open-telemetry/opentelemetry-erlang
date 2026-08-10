defmodule OpenTelemetry.SemConv.Incubating.CloudAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cloud attributes.
  """

  @doc """
  The cloud account ID the resource is assigned to.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["111111111111", "opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_account_id()
      :"cloud.account.id"

  ### Erlang

  ```erlang
  ?CLOUD_ACCOUNT_ID.
  'cloud.account.id'
  ```

  <!-- tabs-close -->
  """
  @spec cloud_account_id :: :"cloud.account.id"
  def cloud_account_id do
    :"cloud.account.id"
  end

  @doc """
  Cloud regions often have multiple, isolated locations known as zones to increase availability. Availability zone represents the zone where the resource is running.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Availability zones are called "zones" on Alibaba Cloud and Google Cloud.

  ### Examples

  ```
  ["us-east-1c"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_availability_zone()
      :"cloud.availability_zone"

  ### Erlang

  ```erlang
  ?CLOUD_AVAILABILITY_ZONE.
  'cloud.availability_zone'
  ```

  <!-- tabs-close -->
  """
  @spec cloud_availability_zone :: :"cloud.availability_zone"
  def cloud_availability_zone do
    :"cloud.availability_zone"
  end

  @typedoc """
  The cloud platform in use.


  ### Enum Values
  * `:"akamai_cloud.compute"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Akamai Cloud Compute
  * `:alibaba_cloud_ecs` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Alibaba Cloud Elastic Compute Service
  * `:alibaba_cloud_fc` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Alibaba Cloud Function Compute
  * `:alibaba_cloud_openshift` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Red Hat OpenShift on Alibaba Cloud
  * `:aws_ec2` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - AWS Elastic Compute Cloud
  * `:aws_ecs` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - AWS Elastic Container Service
  * `:aws_eks` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - AWS Elastic Kubernetes Service
  * `:aws_lambda` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - AWS Lambda
  * `:aws_elastic_beanstalk` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - AWS Elastic Beanstalk
  * `:aws_app_runner` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - AWS App Runner
  * `:aws_openshift` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Red Hat OpenShift on AWS (ROSA)
  * `:"azure.vm"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure Virtual Machines
  * `:"azure.container_apps"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure Container Apps
  * `:"azure.container_instances"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure Container Instances
  * `:"azure.aks"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure Kubernetes Service
  * `:"azure.functions"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure Functions
  * `:"azure.app_service"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure App Service
  * `:"azure.openshift"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure Red Hat OpenShift
  * `:azure_vm` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Azure Virtual Machines~~
  * `:azure_container_apps` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Azure Container Apps~~
  * `:azure_container_instances` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Azure Container Instances~~
  * `:azure_aks` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Azure Kubernetes Service~~
  * `:azure_functions` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Azure Functions~~
  * `:azure_app_service` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Azure App Service~~
  * `:azure_openshift` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Azure Red Hat OpenShift~~
  * `:"gcp.agent_engine"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Google Vertex AI Agent Engine
  * `:gcp_bare_metal_solution` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Google Bare Metal Solution (BMS)
  * `:gcp_compute_engine` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Google Cloud Compute Engine (GCE)
  * `:gcp_cloud_run` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Google Cloud Run
  * `:gcp_kubernetes_engine` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Google Cloud Kubernetes Engine (GKE)
  * `:gcp_cloud_functions` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Google Cloud Functions (GCF)
  * `:gcp_app_engine` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Google Cloud App Engine (GAE)
  * `:gcp_openshift` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Red Hat OpenShift on Google Cloud
  * `:"hetzner.cloud_server"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Server on Hetzner Cloud
  * `:ibm_cloud_openshift` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Red Hat OpenShift on IBM Cloud
  * `:oracle_cloud_compute` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Compute on Oracle Cloud Infrastructure (OCI)
  * `:oracle_cloud_oke` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Kubernetes Engine (OKE) on Oracle Cloud Infrastructure (OCI)
  * `:tencent_cloud_cvm` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Tencent Cloud Cloud Virtual Machine (CVM)
  * `:tencent_cloud_eks` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Tencent Cloud Elastic Kubernetes Service (EKS)
  * `:tencent_cloud_scf` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Tencent Cloud Serverless Cloud Function (SCF)
  * `:"vultr.cloud_compute"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Vultr Cloud Compute
  """
  @type cloud_platform_values() :: %{
          :"akamai_cloud.compute" => :"akamai_cloud.compute",
          :alibaba_cloud_ecs => :alibaba_cloud_ecs,
          :alibaba_cloud_fc => :alibaba_cloud_fc,
          :alibaba_cloud_openshift => :alibaba_cloud_openshift,
          :aws_ec2 => :aws_ec2,
          :aws_ecs => :aws_ecs,
          :aws_eks => :aws_eks,
          :aws_lambda => :aws_lambda,
          :aws_elastic_beanstalk => :aws_elastic_beanstalk,
          :aws_app_runner => :aws_app_runner,
          :aws_openshift => :aws_openshift,
          :"azure.vm" => :"azure.vm",
          :"azure.container_apps" => :"azure.container_apps",
          :"azure.container_instances" => :"azure.container_instances",
          :"azure.aks" => :"azure.aks",
          :"azure.functions" => :"azure.functions",
          :"azure.app_service" => :"azure.app_service",
          :"azure.openshift" => :"azure.openshift",
          :azure_vm => :azure_vm,
          :azure_container_apps => :azure_container_apps,
          :azure_container_instances => :azure_container_instances,
          :azure_aks => :azure_aks,
          :azure_functions => :azure_functions,
          :azure_app_service => :azure_app_service,
          :azure_openshift => :azure_openshift,
          :"gcp.agent_engine" => :"gcp.agent_engine",
          :gcp_bare_metal_solution => :gcp_bare_metal_solution,
          :gcp_compute_engine => :gcp_compute_engine,
          :gcp_cloud_run => :gcp_cloud_run,
          :gcp_kubernetes_engine => :gcp_kubernetes_engine,
          :gcp_cloud_functions => :gcp_cloud_functions,
          :gcp_app_engine => :gcp_app_engine,
          :gcp_openshift => :gcp_openshift,
          :"hetzner.cloud_server" => :"hetzner.cloud_server",
          :ibm_cloud_openshift => :ibm_cloud_openshift,
          :oracle_cloud_compute => :oracle_cloud_compute,
          :oracle_cloud_oke => :oracle_cloud_oke,
          :tencent_cloud_cvm => :tencent_cloud_cvm,
          :tencent_cloud_eks => :tencent_cloud_eks,
          :tencent_cloud_scf => :tencent_cloud_scf,
          :"vultr.cloud_compute" => :"vultr.cloud_compute"
        }
  @doc """
  The cloud platform in use.


  ### Notes

  The prefix of the service **SHOULD** match the one specified in `cloud.provider`.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_platform()
      :"cloud.platform"

      iex> OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_platform_values()[:"akamai_cloud.compute"]
      :"akamai_cloud.compute"

      iex> %{OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_platform() => OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_platform_values()[:"akamai_cloud.compute"]}
      %{:"cloud.platform" => :"akamai_cloud.compute"}

  ### Erlang

  ```erlang
  ?CLOUD_PLATFORM.
  'cloud.platform'

  ?CLOUD_PLATFORM_VALUES_AKAMAI_CLOUD_COMPUTE.
  'akamai_cloud.compute'

  \#{?CLOUD_PLATFORM => ?CLOUD_PLATFORM_VALUES_AKAMAI_CLOUD_COMPUTE}.
  \#{'cloud.platform' => 'akamai_cloud.compute'}
  ```

  <!-- tabs-close -->
  """
  @spec cloud_platform :: :"cloud.platform"
  def cloud_platform do
    :"cloud.platform"
  end

  @spec cloud_platform_values() :: cloud_platform_values()
  def cloud_platform_values() do
    %{
      :"akamai_cloud.compute" => :"akamai_cloud.compute",
      :alibaba_cloud_ecs => :alibaba_cloud_ecs,
      :alibaba_cloud_fc => :alibaba_cloud_fc,
      :alibaba_cloud_openshift => :alibaba_cloud_openshift,
      :aws_ec2 => :aws_ec2,
      :aws_ecs => :aws_ecs,
      :aws_eks => :aws_eks,
      :aws_lambda => :aws_lambda,
      :aws_elastic_beanstalk => :aws_elastic_beanstalk,
      :aws_app_runner => :aws_app_runner,
      :aws_openshift => :aws_openshift,
      :"azure.vm" => :"azure.vm",
      :"azure.container_apps" => :"azure.container_apps",
      :"azure.container_instances" => :"azure.container_instances",
      :"azure.aks" => :"azure.aks",
      :"azure.functions" => :"azure.functions",
      :"azure.app_service" => :"azure.app_service",
      :"azure.openshift" => :"azure.openshift",
      :azure_vm => :azure_vm,
      :azure_container_apps => :azure_container_apps,
      :azure_container_instances => :azure_container_instances,
      :azure_aks => :azure_aks,
      :azure_functions => :azure_functions,
      :azure_app_service => :azure_app_service,
      :azure_openshift => :azure_openshift,
      :"gcp.agent_engine" => :"gcp.agent_engine",
      :gcp_bare_metal_solution => :gcp_bare_metal_solution,
      :gcp_compute_engine => :gcp_compute_engine,
      :gcp_cloud_run => :gcp_cloud_run,
      :gcp_kubernetes_engine => :gcp_kubernetes_engine,
      :gcp_cloud_functions => :gcp_cloud_functions,
      :gcp_app_engine => :gcp_app_engine,
      :gcp_openshift => :gcp_openshift,
      :"hetzner.cloud_server" => :"hetzner.cloud_server",
      :ibm_cloud_openshift => :ibm_cloud_openshift,
      :oracle_cloud_compute => :oracle_cloud_compute,
      :oracle_cloud_oke => :oracle_cloud_oke,
      :tencent_cloud_cvm => :tencent_cloud_cvm,
      :tencent_cloud_eks => :tencent_cloud_eks,
      :tencent_cloud_scf => :tencent_cloud_scf,
      :"vultr.cloud_compute" => :"vultr.cloud_compute"
    }
  end

  @typedoc """
  Name of the cloud provider.


  ### Enum Values
  * `:akamai_cloud` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Akamai Cloud
  * `:alibaba_cloud` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Alibaba Cloud
  * `:aws` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Amazon Web Services
  * `:azure` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Microsoft Azure
  * `:gcp` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Google Cloud Platform
  * `:heroku` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Heroku Platform as a Service
  * `:hetzner` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Hetzner
  * `:ibm_cloud` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IBM Cloud
  * `:oracle_cloud` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Oracle Cloud Infrastructure (OCI)
  * `:tencent_cloud` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Tencent Cloud
  * `:vultr` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Vultr
  """
  @type cloud_provider_values() :: %{
          :akamai_cloud => :akamai_cloud,
          :alibaba_cloud => :alibaba_cloud,
          :aws => :aws,
          :azure => :azure,
          :gcp => :gcp,
          :heroku => :heroku,
          :hetzner => :hetzner,
          :ibm_cloud => :ibm_cloud,
          :oracle_cloud => :oracle_cloud,
          :tencent_cloud => :tencent_cloud,
          :vultr => :vultr
        }
  @doc """
  Name of the cloud provider.



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_provider()
      :"cloud.provider"

      iex> OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_provider_values().akamai_cloud
      :akamai_cloud

      iex> %{OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_provider() => OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_provider_values().akamai_cloud}
      %{:"cloud.provider" => :akamai_cloud}

  ### Erlang

  ```erlang
  ?CLOUD_PROVIDER.
  'cloud.provider'

  ?CLOUD_PROVIDER_VALUES_AKAMAI_CLOUD.
  'akamai_cloud'

  \#{?CLOUD_PROVIDER => ?CLOUD_PROVIDER_VALUES_AKAMAI_CLOUD}.
  \#{'cloud.provider' => 'akamai_cloud'}
  ```

  <!-- tabs-close -->
  """
  @spec cloud_provider :: :"cloud.provider"
  def cloud_provider do
    :"cloud.provider"
  end

  @spec cloud_provider_values() :: cloud_provider_values()
  def cloud_provider_values() do
    %{
      :akamai_cloud => :akamai_cloud,
      :alibaba_cloud => :alibaba_cloud,
      :aws => :aws,
      :azure => :azure,
      :gcp => :gcp,
      :heroku => :heroku,
      :hetzner => :hetzner,
      :ibm_cloud => :ibm_cloud,
      :oracle_cloud => :oracle_cloud,
      :tencent_cloud => :tencent_cloud,
      :vultr => :vultr
    }
  end

  @doc """
  The geographical region within a cloud provider. When associated with a resource, this attribute specifies the region where the resource operates. When calling services or APIs deployed on a cloud, this attribute identifies the region where the called destination is deployed.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Refer to your provider's docs to see the available regions, for example [Alibaba Cloud regions](https://www.alibabacloud.com/help/doc-detail/40654.htm), [AWS regions](https://aws.amazon.com/about-aws/global-infrastructure/regions_az/), [Azure regions](https://azure.microsoft.com/global-infrastructure/geographies/), [Google Cloud regions](https://cloud.google.com/about/locations), or [Tencent Cloud regions](https://www.tencentcloud.com/document/product/213/6091).

  ### Examples

  ```
  ["us-central1", "us-east-1"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_region()
      :"cloud.region"

  ### Erlang

  ```erlang
  ?CLOUD_REGION.
  'cloud.region'
  ```

  <!-- tabs-close -->
  """
  @spec cloud_region :: :"cloud.region"
  def cloud_region do
    :"cloud.region"
  end

  @doc """
  Cloud provider-specific native identifier of the monitored cloud resource (e.g. an [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html) on AWS, a [fully qualified resource ID](https://learn.microsoft.com/rest/api/resources/resources/get-by-id) on Azure, a [full resource name](https://google.aip.dev/122#full-resource-names) on GCP)

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  On some cloud providers, it may not be possible to determine the full ID at startup,
  so it may be necessary to set `cloud.resource_id` as a span attribute instead.

  The exact value to use for `cloud.resource_id` depends on the cloud provider.
  The following well-known definitions **MUST** be used if you set this attribute and they apply:

  - **AWS Lambda:** The function [ARN](https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html).
    Take care not to use the "invoked ARN" directly but replace any
    [alias suffix](https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html)
    with the resolved function version, as the same runtime instance may be invocable with
    multiple different aliases.
  - **GCP:** The [URI of the resource](https://cloud.google.com/iam/docs/full-resource-names)
  - **Azure:** The [Fully Qualified Resource ID](https://learn.microsoft.com/rest/api/resources/resources/get-by-id) of the invoked function,
    *not* the function app, having the form
    `/subscriptions/<SUBSCRIPTION_GUID>/resourceGroups/<RG>/providers/Microsoft.Web/sites/<FUNCAPP>/functions/<FUNC>`.
    This means that a span attribute **MUST** be used, as an Azure function app can host multiple functions that would usually share
    a TracerProvider.

  ### Examples

  ```
  ["arn:aws:lambda:REGION:ACCOUNT_ID:function:my-function", "//run.googleapis.com/projects/PROJECT_ID/locations/LOCATION_ID/services/SERVICE_ID", "/subscriptions/<SUBSCRIPTION_GUID>/resourceGroups/<RG>/providers/Microsoft.Web/sites/<FUNCAPP>/functions/<FUNC>"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudAttributes.cloud_resource_id()
      :"cloud.resource_id"

  ### Erlang

  ```erlang
  ?CLOUD_RESOURCE_ID.
  'cloud.resource_id'
  ```

  <!-- tabs-close -->
  """
  @spec cloud_resource_id :: :"cloud.resource_id"
  def cloud_resource_id do
    :"cloud.resource_id"
  end
end
