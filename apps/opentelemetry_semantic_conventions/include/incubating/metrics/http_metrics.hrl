
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

%% Number of active HTTP requests.
-define(HTTP_CLIENT_ACTIVE_REQUESTS, 'http.client.active_requests').


%% The duration of the successfully established outbound HTTP connections.
-define(HTTP_CLIENT_CONNECTION_DURATION, 'http.client.connection.duration').


%% Number of outbound HTTP connections that are currently active or idle on the client.
-define(HTTP_CLIENT_OPEN_CONNECTIONS, 'http.client.open_connections').


%% Size of HTTP client request bodies.
-define(HTTP_CLIENT_REQUEST_BODY_SIZE, 'http.client.request.body.size').


%% Size of HTTP client response bodies.
-define(HTTP_CLIENT_RESPONSE_BODY_SIZE, 'http.client.response.body.size').


%% Number of active HTTP server requests.
-define(HTTP_SERVER_ACTIVE_REQUESTS, 'http.server.active_requests').


%% Size of HTTP server request bodies.
-define(HTTP_SERVER_REQUEST_BODY_SIZE, 'http.server.request.body.size').


%% Size of HTTP server response bodies.
-define(HTTP_SERVER_RESPONSE_BODY_SIZE, 'http.server.response.body.size').
