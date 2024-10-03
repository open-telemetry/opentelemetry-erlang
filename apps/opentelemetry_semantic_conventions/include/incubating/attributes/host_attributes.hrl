
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

%% The CPU architecture the host system is running on.
%%  
-define(HOST_ARCH, 'host.arch').

-define(HOST_ARCH_VALUES_AMD64, 'amd64').

-define(HOST_ARCH_VALUES_ARM32, 'arm32').

-define(HOST_ARCH_VALUES_ARM64, 'arm64').

-define(HOST_ARCH_VALUES_IA64, 'ia64').

-define(HOST_ARCH_VALUES_PPC32, 'ppc32').

-define(HOST_ARCH_VALUES_PPC64, 'ppc64').

-define(HOST_ARCH_VALUES_S390X, 's390x').

-define(HOST_ARCH_VALUES_X86, 'x86').



%% The amount of level 2 memory cache available to the processor (in Bytes).
%%  
-define(HOST_CPU_CACHE_L2_SIZE, 'host.cpu.cache.l2.size').


%% Family or generation of the CPU.
%%  
-define(HOST_CPU_FAMILY, 'host.cpu.family').


%% Model identifier. It provides more granular information about the CPU, distinguishing it from other CPUs within the same family.
%%  
-define(HOST_CPU_MODEL_ID, 'host.cpu.model.id').


%% Model designation of the processor.
%%  
-define(HOST_CPU_MODEL_NAME, 'host.cpu.model.name').


%% Stepping or core revisions.
%%  
-define(HOST_CPU_STEPPING, 'host.cpu.stepping').


%% Processor manufacturer identifier. A maximum 12-character string.
%%  
-define(HOST_CPU_VENDOR_ID, 'host.cpu.vendor.id').


%% Unique host ID. For Cloud, this must be the instance_id assigned by the cloud provider. For non-containerized systems, this should be the `machine-id`. See the table below for the sources to use to determine the `machine-id` based on operating system.
%%  
-define(HOST_ID, 'host.id').


%% VM image ID or host OS image ID. For Cloud, this value is from the provider.
%%  
-define(HOST_IMAGE_ID, 'host.image.id').


%% Name of the VM image or OS install the host was instantiated from.
%%  
-define(HOST_IMAGE_NAME, 'host.image.name').


%% The version string of the VM image or host OS as defined in [Version Attributes](/docs/resource/README.md#version-attributes).
%%  
-define(HOST_IMAGE_VERSION, 'host.image.version').


%% Available IP addresses of the host, excluding loopback interfaces.
%%  
-define(HOST_IP, 'host.ip').


%% Available MAC addresses of the host, excluding loopback interfaces.
%%  
-define(HOST_MAC, 'host.mac').


%% Name of the host. On Unix systems, it may contain what the hostname command returns, or the fully qualified hostname, or another name specified by the user.
%%  
-define(HOST_NAME, 'host.name').


%% Type of host. For Cloud, this must be the machine type.
%%  
-define(HOST_TYPE, 'host.type').
