
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/service_attributes.hrl").


%% The operational criticality of the service.
%%  
-define(SERVICE_CRITICALITY, 'service.criticality').

-define(SERVICE_CRITICALITY_VALUES_CRITICAL, 'critical').

-define(SERVICE_CRITICALITY_VALUES_HIGH, 'high').

-define(SERVICE_CRITICALITY_VALUES_MEDIUM, 'medium').

-define(SERVICE_CRITICALITY_VALUES_LOW, 'low').



%% Logical name of the service on the other side of the connection. SHOULD be equal to the actual [`service.name`](/docs/resource/README.md#service) resource attribute of the remote service if any.
%%  
-define(SERVICE_PEER_NAME, 'service.peer.name').


%% Logical namespace of the service on the other side of the connection. SHOULD be equal to the actual [`service.namespace`](/docs/resource/README.md#service) resource attribute of the remote service if any.
%%  
-define(SERVICE_PEER_NAMESPACE, 'service.peer.namespace').
