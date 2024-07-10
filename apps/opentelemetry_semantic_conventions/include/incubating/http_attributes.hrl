
%% @deprecated Replaced by `client.address`.
%% Deprecated, use `client.address` instead.
-define(HTTP_CLIENTIP, 'http.client_ip').


%% State of the HTTP connection in the HTTP connection pool.

-define('http_connection_state.active', 'active').

-define('http_connection_state.idle', 'idle').

-define(http_connection_state(Custom), Custom).

%% @deprecated Replaced by `network.protocol.name`.
%% Deprecated, use `network.protocol.name` instead.

-define('http_flavor.http_1_0', '1.0').

-define('http_flavor.http_1_1', '1.1').

-define('http_flavor.http_2_0', '2.0').

-define('http_flavor.http_3_0', '3.0').

-define('http_flavor.spdy', 'SPDY').

-define('http_flavor.quic', 'QUIC').

-define(http_flavor(Custom), Custom).

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
-define(HTTP_REQUESTCONTENTLENGTH, 'http.request_content_length').

%% @deprecated Replaced by `http.request.body.size`.
%% Deprecated, use `http.request.body.size` instead.
-define(HTTP_REQUESTCONTENTLENGTHUNCOMPRESSED, 'http.request_content_length_uncompressed').


%% The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size.
%%  
-define(HTTP_RESPONSE_BODY_SIZE, 'http.response.body.size').


%% The total size of the response in bytes. This should be the total number of bytes sent over the wire, including the status line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and response body and trailers if any.
%%  
-define(HTTP_RESPONSE_SIZE, 'http.response.size').

%% @deprecated Replaced by `http.response.header.content-length`.
%% Deprecated, use `http.response.header.content-length` instead.
-define(HTTP_RESPONSECONTENTLENGTH, 'http.response_content_length').

%% @deprecated Replace by `http.response.body.size`.
%% Deprecated, use `http.response.body.size` instead.
-define(HTTP_RESPONSECONTENTLENGTHUNCOMPRESSED, 'http.response_content_length_uncompressed').

%% @deprecated Replaced by `url.scheme` instead.
%% Deprecated, use `url.scheme` instead.
-define(HTTP_SCHEME, 'http.scheme').

%% @deprecated Replaced by `server.address`.
%% Deprecated, use `server.address` instead.
-define(HTTP_SERVERNAME, 'http.server_name').

%% @deprecated Replaced by `http.response.status_code`.
%% Deprecated, use `http.response.status_code` instead.
-define(HTTP_STATUSCODE, 'http.status_code').

%% @deprecated Split to `url.path` and `url.query.
%% Deprecated, use `url.path` and `url.query` instead.
-define(HTTP_TARGET, 'http.target').

%% @deprecated Replaced by `url.full`.
%% Deprecated, use `url.full` instead.
-define(HTTP_URL, 'http.url').

%% @deprecated Replaced by `user_agent.original`.
%% Deprecated, use `user_agent.original` instead.
-define(HTTP_USERAGENT, 'http.user_agent').