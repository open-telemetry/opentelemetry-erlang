
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/http_attributes.hrl").

%% @deprecated Replaced by `client.address`.
%% Deprecated, use `client.address` instead.
-define(HTTP_CLIENT_IP, 'http.client_ip').


%% State of the HTTP connection in the HTTP connection pool.
-define(HTTP_CONNECTION_STATE, 'http.connection.state').

-define(HTTP_CONNECTION_STATE_VALUES_ACTIVE, 'active').

-define(HTTP_CONNECTION_STATE_VALUES_IDLE, 'idle').


%% @deprecated Replaced by `network.protocol.name`.
%% Deprecated, use `network.protocol.name` instead.
-define(HTTP_FLAVOR, 'http.flavor').

-define(HTTP_FLAVOR_VALUES_HTTP_1_0, '1.0').

-define(HTTP_FLAVOR_VALUES_HTTP_1_1, '1.1').

-define(HTTP_FLAVOR_VALUES_HTTP_2_0, '2.0').

-define(HTTP_FLAVOR_VALUES_HTTP_3_0, '3.0').

-define(HTTP_FLAVOR_VALUES_SPDY, 'SPDY').

-define(HTTP_FLAVOR_VALUES_QUIC, 'QUIC').


%% @deprecated Replaced by one of `server.address`, `client.address` or `http.request.header.host`, depending on the usage.
%% Deprecated, use one of `server.address`, `client.address` or `http.request.header.host` instead, depending on the usage.
-define(HTTP_HOST, 'http.host').

%% @deprecated Replaced by `http.request.method`.
%% Deprecated, use `http.request.method` instead.
-define(HTTP_METHOD, 'http.method').


%% The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size.
%%  
-define(HTTP_REQUEST_BODY_SIZE, 'http.request.body.size').


%% The total size of the request in bytes. This should be the total number of bytes sent over the wire, including the request line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and request body if any.
%%  
-define(HTTP_REQUEST_SIZE, 'http.request.size').

%% @deprecated Replaced by `http.request.header.content-length`.
%% Deprecated, use `http.request.header.content-length` instead.
-define(HTTP_REQUEST_CONTENT_LENGTH, 'http.request_content_length').

%% @deprecated Replaced by `http.request.body.size`.
%% Deprecated, use `http.request.body.size` instead.
-define(HTTP_REQUEST_CONTENT_LENGTH_UNCOMPRESSED, 'http.request_content_length_uncompressed').


%% The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size.
%%  
-define(HTTP_RESPONSE_BODY_SIZE, 'http.response.body.size').


%% The total size of the response in bytes. This should be the total number of bytes sent over the wire, including the status line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and response body and trailers if any.
%%  
-define(HTTP_RESPONSE_SIZE, 'http.response.size').

%% @deprecated Replaced by `http.response.header.content-length`.
%% Deprecated, use `http.response.header.content-length` instead.
-define(HTTP_RESPONSE_CONTENT_LENGTH, 'http.response_content_length').

%% @deprecated Replace by `http.response.body.size`.
%% Deprecated, use `http.response.body.size` instead.
-define(HTTP_RESPONSE_CONTENT_LENGTH_UNCOMPRESSED, 'http.response_content_length_uncompressed').

%% @deprecated Replaced by `url.scheme` instead.
%% Deprecated, use `url.scheme` instead.
-define(HTTP_SCHEME, 'http.scheme').

%% @deprecated Replaced by `server.address`.
%% Deprecated, use `server.address` instead.
-define(HTTP_SERVER_NAME, 'http.server_name').

%% @deprecated Replaced by `http.response.status_code`.
%% Deprecated, use `http.response.status_code` instead.
-define(HTTP_STATUS_CODE, 'http.status_code').

%% @deprecated Split to `url.path` and `url.query.
%% Deprecated, use `url.path` and `url.query` instead.
-define(HTTP_TARGET, 'http.target').

%% @deprecated Replaced by `url.full`.
%% Deprecated, use `url.full` instead.
-define(HTTP_URL, 'http.url').

%% @deprecated Replaced by `user_agent.original`.
%% Deprecated, use `user_agent.original` instead.
-define(HTTP_USER_AGENT, 'http.user_agent').
