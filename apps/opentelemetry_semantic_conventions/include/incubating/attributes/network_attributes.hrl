
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/network_attributes.hrl").


%% The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network.
-define(NETWORK_CARRIER_ICC, 'network.carrier.icc').


%% The mobile carrier country code.
-define(NETWORK_CARRIER_MCC, 'network.carrier.mcc').


%% The mobile carrier network code.
-define(NETWORK_CARRIER_MNC, 'network.carrier.mnc').


%% The name of the mobile carrier.
-define(NETWORK_CARRIER_NAME, 'network.carrier.name').


%% The state of network connection
-define(NETWORK_CONNECTION_STATE, 'network.connection.state').

-define(NETWORK_CONNECTION_STATE_VALUES_CLOSED, 'closed').

-define(NETWORK_CONNECTION_STATE_VALUES_CLOSE_WAIT, 'close_wait').

-define(NETWORK_CONNECTION_STATE_VALUES_CLOSING, 'closing').

-define(NETWORK_CONNECTION_STATE_VALUES_ESTABLISHED, 'established').

-define(NETWORK_CONNECTION_STATE_VALUES_FIN_WAIT_1, 'fin_wait_1').

-define(NETWORK_CONNECTION_STATE_VALUES_FIN_WAIT_2, 'fin_wait_2').

-define(NETWORK_CONNECTION_STATE_VALUES_LAST_ACK, 'last_ack').

-define(NETWORK_CONNECTION_STATE_VALUES_LISTEN, 'listen').

-define(NETWORK_CONNECTION_STATE_VALUES_SYN_RECEIVED, 'syn_received').

-define(NETWORK_CONNECTION_STATE_VALUES_SYN_SENT, 'syn_sent').

-define(NETWORK_CONNECTION_STATE_VALUES_TIME_WAIT, 'time_wait').



%% This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection.
-define(NETWORK_CONNECTION_SUBTYPE, 'network.connection.subtype').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_GPRS, 'gprs').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_EDGE, 'edge').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_UMTS, 'umts').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_CDMA, 'cdma').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_EVDO_0, 'evdo_0').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_EVDO_A, 'evdo_a').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_CDMA2000_1XRTT, 'cdma2000_1xrtt').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_HSDPA, 'hsdpa').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_HSUPA, 'hsupa').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_HSPA, 'hspa').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_IDEN, 'iden').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_EVDO_B, 'evdo_b').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_LTE, 'lte').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_EHRPD, 'ehrpd').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_HSPAP, 'hspap').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_GSM, 'gsm').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_TD_SCDMA, 'td_scdma').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_IWLAN, 'iwlan').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_NR, 'nr').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_NRNSA, 'nrnsa').

-define(NETWORK_CONNECTION_SUBTYPE_VALUES_LTE_CA, 'lte_ca').



%% The internet connection type.
-define(NETWORK_CONNECTION_TYPE, 'network.connection.type').

-define(NETWORK_CONNECTION_TYPE_VALUES_WIFI, 'wifi').

-define(NETWORK_CONNECTION_TYPE_VALUES_WIRED, 'wired').

-define(NETWORK_CONNECTION_TYPE_VALUES_CELL, 'cell').

-define(NETWORK_CONNECTION_TYPE_VALUES_UNAVAILABLE, 'unavailable').

-define(NETWORK_CONNECTION_TYPE_VALUES_UNKNOWN, 'unknown').



%% The network interface name.
-define(NETWORK_INTERFACE_NAME, 'network.interface.name').


%% The network IO operation direction.
-define(NETWORK_IO_DIRECTION, 'network.io.direction').

-define(NETWORK_IO_DIRECTION_VALUES_TRANSMIT, 'transmit').

-define(NETWORK_IO_DIRECTION_VALUES_RECEIVE, 'receive').

