
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

%% Local address of the network connection - IP address or Unix domain socket name.
-define(NETWORK_LOCAL_ADDRESS, 'network.local.address').


%% Local port number of the network connection.
-define(NETWORK_LOCAL_PORT, 'network.local.port').


%% Peer address of the network connection - IP address or Unix domain socket name.
-define(NETWORK_PEER_ADDRESS, 'network.peer.address').


%% Peer port number of the network connection.
-define(NETWORK_PEER_PORT, 'network.peer.port').


%% [OSI application layer](https://osi-model.com/application-layer/) or non-OSI equivalent.
-define(NETWORK_PROTOCOL_NAME, 'network.protocol.name').


%% The actual version of the protocol used for network communication.
-define(NETWORK_PROTOCOL_VERSION, 'network.protocol.version').


%% [OSI transport layer](https://osi-model.com/transport-layer/) or [inter-process communication method](https://wikipedia.org/wiki/Inter-process_communication).
%%  
-define(NETWORK_TRANSPORT, 'network.transport').

-define(NETWORK_TRANSPORT_VALUES_TCP, 'tcp').

-define(NETWORK_TRANSPORT_VALUES_UDP, 'udp').

-define(NETWORK_TRANSPORT_VALUES_PIPE, 'pipe').

-define(NETWORK_TRANSPORT_VALUES_UNIX, 'unix').

-define(NETWORK_TRANSPORT_VALUES_QUIC, 'quic').



%% [OSI network layer](https://osi-model.com/network-layer/) or non-OSI equivalent.
-define(NETWORK_TYPE, 'network.type').

-define(NETWORK_TYPE_VALUES_IPV4, 'ipv4').

-define(NETWORK_TYPE_VALUES_IPV6, 'ipv6').

