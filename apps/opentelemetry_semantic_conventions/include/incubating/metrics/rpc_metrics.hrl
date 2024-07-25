
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

%% Measures the duration of outbound RPC.
-define(RPC_CLIENT_DURATION, 'rpc.client.duration').


%% Measures the size of RPC request messages (uncompressed).
-define(RPC_CLIENT_REQUEST_SIZE, 'rpc.client.request.size').


%% Measures the number of messages received per RPC.
-define(RPC_CLIENT_REQUESTS_PER_RPC, 'rpc.client.requests_per_rpc').


%% Measures the size of RPC response messages (uncompressed).
-define(RPC_CLIENT_RESPONSE_SIZE, 'rpc.client.response.size').


%% Measures the number of messages sent per RPC.
-define(RPC_CLIENT_RESPONSES_PER_RPC, 'rpc.client.responses_per_rpc').


%% Measures the duration of inbound RPC.
-define(RPC_SERVER_DURATION, 'rpc.server.duration').


%% Measures the size of RPC request messages (uncompressed).
-define(RPC_SERVER_REQUEST_SIZE, 'rpc.server.request.size').


%% Measures the number of messages received per RPC.
-define(RPC_SERVER_REQUESTS_PER_RPC, 'rpc.server.requests_per_rpc').


%% Measures the size of RPC response messages (uncompressed).
-define(RPC_SERVER_RESPONSE_SIZE, 'rpc.server.response.size').


%% Measures the number of messages sent per RPC.
-define(RPC_SERVER_RESPONSES_PER_RPC, 'rpc.server.responses_per_rpc').
