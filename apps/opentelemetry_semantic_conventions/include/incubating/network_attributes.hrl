
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

-define('net_sock_family.inet', 'inet').

-define('net_sock_family.inet6', 'inet6').

-define('net_sock_family.unix', 'unix').

-define(net_sock_family(Custom), Custom).

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

-define('net_transport.ip_tcp', 'ip_tcp').

-define('net_transport.ip_udp', 'ip_udp').

-define('net_transport.pipe', 'pipe').

-define('net_transport.inproc', 'inproc').

-define('net_transport.other', 'other').

-define(net_transport(Custom), Custom).


%% The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network.
-define(NETWORK_CARRIER_ICC, 'network.carrier.icc').


%% The mobile carrier country code.
-define(NETWORK_CARRIER_MCC, 'network.carrier.mcc').


%% The mobile carrier network code.
-define(NETWORK_CARRIER_MNC, 'network.carrier.mnc').


%% The name of the mobile carrier.
-define(NETWORK_CARRIER_NAME, 'network.carrier.name').


%% This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection.

-define('network_connection_subtype.gprs', 'gprs').

-define('network_connection_subtype.edge', 'edge').

-define('network_connection_subtype.umts', 'umts').

-define('network_connection_subtype.cdma', 'cdma').

-define('network_connection_subtype.evdo_0', 'evdo_0').

-define('network_connection_subtype.evdo_a', 'evdo_a').

-define('network_connection_subtype.cdma2000_1xrtt', 'cdma2000_1xrtt').

-define('network_connection_subtype.hsdpa', 'hsdpa').

-define('network_connection_subtype.hsupa', 'hsupa').

-define('network_connection_subtype.hspa', 'hspa').

-define('network_connection_subtype.iden', 'iden').

-define('network_connection_subtype.evdo_b', 'evdo_b').

-define('network_connection_subtype.lte', 'lte').

-define('network_connection_subtype.ehrpd', 'ehrpd').

-define('network_connection_subtype.hspap', 'hspap').

-define('network_connection_subtype.gsm', 'gsm').

-define('network_connection_subtype.td_scdma', 'td_scdma').

-define('network_connection_subtype.iwlan', 'iwlan').

-define('network_connection_subtype.nr', 'nr').

-define('network_connection_subtype.nrnsa', 'nrnsa').

-define('network_connection_subtype.lte_ca', 'lte_ca').

-define(network_connection_subtype(Custom), Custom).


%% The internet connection type.

-define('network_connection_type.wifi', 'wifi').

-define('network_connection_type.wired', 'wired').

-define('network_connection_type.cell', 'cell').

-define('network_connection_type.unavailable', 'unavailable').

-define('network_connection_type.unknown', 'unknown').

-define(network_connection_type(Custom), Custom).


%% The network IO operation direction.

-define('network_io_direction.transmit', 'transmit').

-define('network_io_direction.receive', 'receive').

-define(network_io_direction(Custom), Custom).
