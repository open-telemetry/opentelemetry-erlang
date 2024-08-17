
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

%% HTTP request headers, `<key>` being the normalized HTTP Header name (lowercase), the value being the header values.
%%  
-define(HTTP_REQUEST_HEADER, 'http.request.header').


%% HTTP request method.
-define(HTTP_REQUEST_METHOD, 'http.request.method').

-define(HTTP_REQUEST_METHOD_VALUES_CONNECT, 'CONNECT').

-define(HTTP_REQUEST_METHOD_VALUES_DELETE, 'DELETE').

-define(HTTP_REQUEST_METHOD_VALUES_GET, 'GET').

-define(HTTP_REQUEST_METHOD_VALUES_HEAD, 'HEAD').

-define(HTTP_REQUEST_METHOD_VALUES_OPTIONS, 'OPTIONS').

-define(HTTP_REQUEST_METHOD_VALUES_PATCH, 'PATCH').

-define(HTTP_REQUEST_METHOD_VALUES_POST, 'POST').

-define(HTTP_REQUEST_METHOD_VALUES_PUT, 'PUT').

-define(HTTP_REQUEST_METHOD_VALUES_TRACE, 'TRACE').

-define(HTTP_REQUEST_METHOD_VALUES_OTHER, '_OTHER').



%% Original HTTP method sent by the client in the request line.
-define(HTTP_REQUEST_METHOD_ORIGINAL, 'http.request.method_original').


%% The ordinal number of request resending attempt (for any reason, including redirects).
%%  
-define(HTTP_REQUEST_RESEND_COUNT, 'http.request.resend_count').


%% HTTP response headers, `<key>` being the normalized HTTP Header name (lowercase), the value being the header values.
%%  
-define(HTTP_RESPONSE_HEADER, 'http.response.header').


%% [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6).
-define(HTTP_RESPONSE_STATUS_CODE, 'http.response.status_code').


%% The matched route, that is, the path template in the format used by the respective server framework.
%%  
-define(HTTP_ROUTE, 'http.route').
