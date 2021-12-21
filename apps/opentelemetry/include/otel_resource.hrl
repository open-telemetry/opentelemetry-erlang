%% The schema url for telemetry resources
-define(SCHEMA_URL, <<"https://opentelemetry.io/schemas/1.8.0">>).

%% if any `service' attribute is defined then
%% `service.name' and `service.instance.id' are required

%% The name of the telemetry library.
-define(SERVICE_NAME, <<"service.name">>).
%% The string ID of the service instance. MUST be unique for each instance of the same
%% `service.namespace,service.name` pair.
-define(SERVICE_INSTANCE, <<"service.instance.id">>).
%% A namespace for `service.name`. A string value having a meaning that helps to distinguish a
%% group of services,
-define(SERVICE_NAMESPACE, <<"service.namespace">>).
%% The version string of the service API or implementation.
-define(SERVICE_VERSION, <<"service.version">>).

%% Process attributes

%% The name of the process executable.
-define(PROCESS_EXECUTABLE_NAME, <<"process.executable.name">>).

%% The name of the Erlang runtime being used. Usually will be BEAM.
-define(PROCESS_RUNTIME_NAME, <<"process.runtime.name">>).
%% The ERTS (Erlang Runtime System) version. For BEAM this is found with application:get_key(erts, vsn).
-define(PROCESS_RUNTIME_VERSION, <<"process.runtime.version">>).
%% The OTP version erlang:system_info(otp_release) and ERTS version combined.
-define(PROCESS_RUNTIME_DESCRIPTION, <<"process.runtime.description">>).

%% Library attributes

%% The name of the telemetry library.
-define(LIBRARY_NAME, <<"library.name">>).
%% The language of telemetry library and of the code instrumented with it.
-define(LIBRARY_LANGUAGE, <<"library.language">>).
%% The version string of the library.
-define(LIBRARY_VERSION, <<"library.version">>).

%% Container attributes

%% Container name.
-define(CONTAINER_NAME, <<"container.name">>).
%% Name of the image the container was built on.
-define(CONTAINER_IMAGE_NAME, <<"container.image.name">>).
%% Container image tag.
-define(CONTAINER_IMAGE_TAG, <<"container.image.tag">>).

%% K8s attributes

%% The name of the cluster that the pod is running in.
-define(K8S_CLUSTER, <<"k8s.cluster.name">>).
%% The name of the namespace that the pod is running in.
-define(K8S_NAMESPACE, <<"k8s.namespace.name">>).
%% The name of the pod.
-define(K8S_POD, <<"k8s.pod.name">>).
%% The name of the deployment.
-define(K8S_DEPLOYMENT, <<"k8s.deployment.name">>).


%% Host attributes

%% Hostname of the host. It contains what the `hostname` command returns on the host machine.
-define(HOST_HOSTNAME, <<"host.hostname">>).
%% Unique host id. For Cloud this must be the instance_id assigned by the cloud provider.
-define(HOST_ID, <<"host.id">>).
%% Name of the host. It may contain what `hostname` returns on Unix systems, the fully qualified,
%% or a name specified by the user.
-define(HOST_NAME, <<"host.name">>).
%% Type of host. For Cloud this must be the machine type.
-define(HOST_TYPE, <<"host.type">>).
%% Name of the VM image or OS install the host was instantiated from.
-define(HOST_IMAGE_NAME, <<"host.image.name">>).
%% VM image id. For Cloud, this value is from the provider.
-define(HOST_IMAGE_ID, <<"host.image.id">>).
%% The version string of the VM image.
-define(HOST_IMAGE_VERSION, <<"host.image.version">>).

%% Cloud attributes

%% Name of the cloud provider.
-define(CLOUD_PROVIDER, <<"cloud.provider">>).
%% The cloud account id used to identify different entities.
-define(CLOUD_ACCOUNT, <<"cloud.account.id">>).
%% A specific geographical location where different entities can run.
-define(CLOUD_REGION, <<"cloud.region">>).
%% Zones are a sub set of the region connected through low-latency links.
-define(CLOUD_ZONE, <<"cloud.zone">>).
