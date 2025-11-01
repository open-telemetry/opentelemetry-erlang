
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/net_attributes.hrl").

%% @deprecated Replaced by `network.local.address`.
%% Deprecated, use `network.local.address`.
-define(NET_HOST_IP, 'net.host.ip').

%% @deprecated Replaced by `server.address`.
%% Deprecated, use `server.address`.
-define(NET_HOST_NAME, 'net.host.name').

%% @deprecated Replaced by `server.port`.
%% Deprecated, use `server.port`.
-define(NET_HOST_PORT, 'net.host.port').

%% @deprecated Replaced by `network.peer.address`.
%% Deprecated, use `network.peer.address`.
-define(NET_PEER_IP, 'net.peer.ip').

%% @deprecated Replaced by `server.address` on client spans and `client.address` on server spans.
%% Deprecated, use `server.address` on client spans and `client.address` on server spans.
-define(NET_PEER_NAME, 'net.peer.name').

%% @deprecated Replaced by `server.port` on client spans and `client.port` on server spans.
%% Deprecated, use `server.port` on client spans and `client.port` on server spans.
-define(NET_PEER_PORT, 'net.peer.port').

%% @deprecated Replaced by `network.protocol.name`.
%% Deprecated, use `network.protocol.name`.
-define(NET_PROTOCOL_NAME, 'net.protocol.name').

%% @deprecated Replaced by `network.protocol.version`.
%% Deprecated, use `network.protocol.version`.
-define(NET_PROTOCOL_VERSION, 'net.protocol.version').

%% @deprecated Split to `network.transport` and `network.type`.
%% Deprecated, use `network.transport` and `network.type`.
-define(NET_SOCK_FAMILY, 'net.sock.family').

-define(NET_SOCK_FAMILY_VALUES_INET, 'inet').

-define(NET_SOCK_FAMILY_VALUES_INET6, 'inet6').

-define(NET_SOCK_FAMILY_VALUES_UNIX, 'unix').


%% @deprecated Replaced by `network.local.address`.
%% Deprecated, use `network.local.address`.
-define(NET_SOCK_HOST_ADDR, 'net.sock.host.addr').

%% @deprecated Replaced by `network.local.port`.
%% Deprecated, use `network.local.port`.
-define(NET_SOCK_HOST_PORT, 'net.sock.host.port').

%% @deprecated Replaced by `network.peer.address`.
%% Deprecated, use `network.peer.address`.
-define(NET_SOCK_PEER_ADDR, 'net.sock.peer.addr').

%% @deprecated Removed.
%% Deprecated, no replacement at this time.
-define(NET_SOCK_PEER_NAME, 'net.sock.peer.name').

%% @deprecated Replaced by `network.peer.port`.
%% Deprecated, use `network.peer.port`.
-define(NET_SOCK_PEER_PORT, 'net.sock.peer.port').

%% @deprecated Replaced by `network.transport`.
%% Deprecated, use `network.transport`.
-define(NET_TRANSPORT, 'net.transport').

-define(NET_TRANSPORT_VALUES_IP_TCP, 'ip_tcp').

-define(NET_TRANSPORT_VALUES_IP_UDP, 'ip_udp').

-define(NET_TRANSPORT_VALUES_PIPE, 'pipe').

-define(NET_TRANSPORT_VALUES_INPROC, 'inproc').

-define(NET_TRANSPORT_VALUES_OTHER, 'other').

