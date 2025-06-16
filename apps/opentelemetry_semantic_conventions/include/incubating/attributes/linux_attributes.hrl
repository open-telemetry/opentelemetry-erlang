
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/linux_attributes.hrl").


%% The Linux Slab memory state
-define(LINUX_MEMORY_SLAB_STATE, 'linux.memory.slab.state').

-define(LINUX_MEMORY_SLAB_STATE_VALUES_RECLAIMABLE, 'reclaimable').

-define(LINUX_MEMORY_SLAB_STATE_VALUES_UNRECLAIMABLE, 'unreclaimable').

