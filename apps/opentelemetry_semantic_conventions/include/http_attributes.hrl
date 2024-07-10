

%% HTTP request headers, `<key>` being the normalized HTTP Header name (lowercase), the value being the header values.
%%  
-define(HTTP_REQUEST_HEADER, 'http.request.header').


%% HTTP request method.

-define('http_request_method.connect', 'CONNECT').

-define('http_request_method.delete', 'DELETE').

-define('http_request_method.get', 'GET').

-define('http_request_method.head', 'HEAD').

-define('http_request_method.options', 'OPTIONS').

-define('http_request_method.patch', 'PATCH').

-define('http_request_method.post', 'POST').

-define('http_request_method.put', 'PUT').

-define('http_request_method.trace', 'TRACE').

-define('http_request_method.other', '_OTHER').

-define(http_request_method(Custom), Custom).


%% Original HTTP method sent by the client in the request line.
-define(HTTP_REQUEST_METHODORIGINAL, 'http.request.method_original').


%% The ordinal number of request resending attempt (for any reason, including redirects).
%%  
-define(HTTP_REQUEST_RESENDCOUNT, 'http.request.resend_count').


%% HTTP response headers, `<key>` being the normalized HTTP Header name (lowercase), the value being the header values.
%%  
-define(HTTP_RESPONSE_HEADER, 'http.response.header').


%% [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6).
-define(HTTP_RESPONSE_STATUSCODE, 'http.response.status_code').


%% The matched route, that is, the path template in the format used by the respective server framework.
%%  
-define(HTTP_ROUTE, 'http.route').
