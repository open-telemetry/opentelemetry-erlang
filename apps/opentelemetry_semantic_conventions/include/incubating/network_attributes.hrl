
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
-define(NET_SOCK_FAMILY, 'net.sock.family').

-define('NET_SOCK_FAMILY_VALUES.inet', 'inet').

-define('NET_SOCK_FAMILY_VALUES.inet6', 'inet6').

-define('NET_SOCK_FAMILY_VALUES.unix', 'unix').


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

-define('NET_TRANSPORT_VALUES.ip_tcp', 'ip_tcp').

-define('NET_TRANSPORT_VALUES.ip_udp', 'ip_udp').

-define('NET_TRANSPORT_VALUES.pipe', 'pipe').

-define('NET_TRANSPORT_VALUES.inproc', 'inproc').

-define('NET_TRANSPORT_VALUES.other', 'other').



%% The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network.
-define(NETWORK_CARRIER_ICC, 'network.carrier.icc').


%% The mobile carrier country code.
-define(NETWORK_CARRIER_MCC, 'network.carrier.mcc').


%% The mobile carrier network code.
-define(NETWORK_CARRIER_MNC, 'network.carrier.mnc').


%% The name of the mobile carrier.
-define(NETWORK_CARRIER_NAME, 'network.carrier.name').


%% This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection.
-define(NETWORK_CONNECTION_SUBTYPE, 'network.connection.subtype').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.gprs', 'gprs').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.edge', 'edge').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.umts', 'umts').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.cdma', 'cdma').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.evdo_0', 'evdo_0').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.evdo_a', 'evdo_a').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.cdma2000_1xrtt', 'cdma2000_1xrtt').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.hsdpa', 'hsdpa').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.hsupa', 'hsupa').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.hspa', 'hspa').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.iden', 'iden').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.evdo_b', 'evdo_b').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.lte', 'lte').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.ehrpd', 'ehrpd').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.hspap', 'hspap').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.gsm', 'gsm').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.td_scdma', 'td_scdma').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.iwlan', 'iwlan').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.nr', 'nr').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.nrnsa', 'nrnsa').

-define('NETWORK_CONNECTION_SUBTYPE_VALUES.lte_ca', 'lte_ca').



%% The internet connection type.
-define(NETWORK_CONNECTION_TYPE, 'network.connection.type').

-define('NETWORK_CONNECTION_TYPE_VALUES.wifi', 'wifi').

-define('NETWORK_CONNECTION_TYPE_VALUES.wired', 'wired').

-define('NETWORK_CONNECTION_TYPE_VALUES.cell', 'cell').

-define('NETWORK_CONNECTION_TYPE_VALUES.unavailable', 'unavailable').

-define('NETWORK_CONNECTION_TYPE_VALUES.unknown', 'unknown').



%% The network IO operation direction.
-define(NETWORK_IO_DIRECTION, 'network.io.direction').

-define('NETWORK_IO_DIRECTION_VALUES.transmit', 'transmit').

-define('NETWORK_IO_DIRECTION_VALUES.receive', 'receive').

