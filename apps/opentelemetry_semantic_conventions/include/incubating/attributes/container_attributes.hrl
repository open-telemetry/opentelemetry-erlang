
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

%% The command used to run the container (i.e. the command name).
%%  
-define(CONTAINER_COMMAND, 'container.command').


%% All the command arguments (including the command/executable itself) run by the container. [2]
%%  
-define(CONTAINER_COMMAND_ARGS, 'container.command_args').


%% The full command run by the container as a single string representing the full command. [2]
%%  
-define(CONTAINER_COMMAND_LINE, 'container.command_line').

%% @deprecated Replaced by `cpu.mode`
%% Deprecated, use `cpu.mode` instead.
-define(CONTAINER_CPU_STATE, 'container.cpu.state').

-define(CONTAINER_CPU_STATE_VALUES_USER, 'user').

-define(CONTAINER_CPU_STATE_VALUES_SYSTEM, 'system').

-define(CONTAINER_CPU_STATE_VALUES_KERNEL, 'kernel').



%% Container ID. Usually a UUID, as for example used to [identify Docker containers](https://docs.docker.com/engine/reference/run/#container-identification). The UUID might be abbreviated.
%%  
-define(CONTAINER_ID, 'container.id').


%% Runtime specific image identifier. Usually a hash algorithm followed by a UUID.
%%  
-define(CONTAINER_IMAGE_ID, 'container.image.id').


%% Name of the image the container was built on.
%%  
-define(CONTAINER_IMAGE_NAME, 'container.image.name').


%% Repo digests of the container image as provided by the container runtime.
%%  
-define(CONTAINER_IMAGE_REPO_DIGESTS, 'container.image.repo_digests').


%% Container image tags. An example can be found in [Docker Image Inspect](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect). Should be only the `<tag>` section of the full name for example from `registry.example.com/my-org/my-image:<tag>`.
%%  
-define(CONTAINER_IMAGE_TAGS, 'container.image.tags').


%% Container labels, `<key>` being the label name, the value being the label value.
%%  
-define(CONTAINER_LABEL, 'container.label').

%% @deprecated Replaced by `container.label`.
%% Deprecated, use `container.label` instead.
-define(CONTAINER_LABELS, 'container.labels').


%% Container name used by container runtime.
%%  
-define(CONTAINER_NAME, 'container.name').


%% The container runtime managing this container.
%%  
-define(CONTAINER_RUNTIME, 'container.runtime').
