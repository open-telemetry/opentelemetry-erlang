defmodule OpenTelemetry.SemanticConventions.HttpAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Http attributes.
  """

  @deprecated """
  Replaced by `client.address`.
  """

  @spec http_clientip :: :"http.client_ip"
  def http_clientip do
    :"http.client_ip"
  end

  @typedoc """
  State of the HTTP connection in the HTTP connection pool.

  ### Options
  * `:active` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - active state.
  * `:idle` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - idle state.

  """
  @type http_connection_state() :: :active | :idle | atom()

  @doc """
  State of the HTTP connection in the HTTP connection pool.


  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_connection_state(:active)
      :active
      
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_connection_state(:custom_value)
      :custom_value
  """
  @spec http_connection_state(http_connection_state()) :: :active | :idle | atom()
  def http_connection_state(option) do
    case option do
      :active -> :active
      :idle -> :idle
      _ -> option
    end
  end

  @type http_1_0() :: :"1.0"

  @type http_1_1() :: :"1.1"

  @type http_2_0() :: :"2.0"

  @type http_3_0() :: :"3.0"

  @type spdy() :: :SPDY

  @type quic() :: :QUIC

  @typedoc """
  Deprecated, use `network.protocol.name` instead.

  ### Options
  * `:http_1_0` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HTTP/1.0
  * `:http_1_1` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HTTP/1.1
  * `:http_2_0` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HTTP/2
  * `:http_3_0` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - HTTP/3
  * `:spdy` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - SPDY protocol.
  * `:quic` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - QUIC protocol.

  """
  @type http_flavor() ::
          http_1_0() | http_1_1() | http_2_0() | http_3_0() | spdy() | quic() | atom()

  @deprecated """
  Replaced by `network.protocol.name`.
  """

  @spec http_flavor(http_flavor()) ::
          http_1_0() | http_1_1() | http_2_0() | http_3_0() | spdy() | quic() | atom()
  def http_flavor(option) do
    case option do
      :http_1_0 -> :"1.0"
      :http_1_1 -> :"1.1"
      :http_2_0 -> :"2.0"
      :http_3_0 -> :"3.0"
      :spdy -> :SPDY
      :quic -> :QUIC
      _ -> option
    end
  end

  @deprecated """
  Replaced by one of `server.address`, `client.address` or `http.request.header.host`, depending on the usage.
  """

  @spec http_host :: :"http.host"
  def http_host do
    :"http.host"
  end

  @deprecated """
  Replaced by `http.request.method`.
  """

  @spec http_method :: :"http.method"
  def http_method do
    :"http.method"
  end

  @doc """
  The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_request_body_size()
      :"http.request.body.size"
  """
  @spec http_request_body_size :: :"http.request.body.size"
  def http_request_body_size do
    :"http.request.body.size"
  end

  @doc """
  HTTP request headers, `<key>` being the normalized HTTP Header name (lowercase), the value being the header values.

  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which headers are to be captured. Including all request headers can be a security risk - explicit configuration helps avoid leaking sensitive information.
  The `User-Agent` header is already captured in the `user_agent.original` attribute. Users **MAY** explicitly configure instrumentations to capture them even though it is not recommended.
  The attribute value **MUST** consist of either multiple header values as an array of strings or a single-item array containing a possibly comma-concatenated string, depending on the way the HTTP library provides access to headers.


  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_request_header()
      :"http.request.header"
  """
  @spec http_request_header :: :"http.request.header"
  def http_request_header do
    :"http.request.header"
  end

  @type connect() :: :CONNECT

  @type delete() :: :DELETE

  @type get() :: :GET

  @type head() :: :HEAD

  @type options() :: :OPTIONS

  @type patch() :: :PATCH

  @type post() :: :POST

  @type put() :: :PUT

  @type trace() :: :TRACE

  @type other() :: :_OTHER

  @typedoc """
  HTTP request method.

  ### Options
  * `:connect` - CONNECT method.
  * `:delete` - DELETE method.
  * `:get` - GET method.
  * `:head` - HEAD method.
  * `:options` - OPTIONS method.
  * `:patch` - PATCH method.
  * `:post` - POST method.
  * `:put` - PUT method.
  * `:trace` - TRACE method.
  * `:other` - Any HTTP method that the instrumentation has no prior knowledge of.

  """
  @type http_request_method() ::
          connect()
          | delete()
          | get()
          | head()
          | options()
          | patch()
          | post()
          | put()
          | trace()
          | other()
          | atom()

  @doc """
  HTTP request method.
  ### Notes

  HTTP request method value **SHOULD** be "known" to the instrumentation.
  By default, this convention defines "known" methods as the ones listed in [RFC9110](https://www.rfc-editor.org/rfc/rfc9110.html#name-methods)
  and the PATCH method defined in [RFC5789](https://www.rfc-editor.org/rfc/rfc5789.html).

  If the HTTP request method is not known to instrumentation, it **MUST** set the `http.request.method` attribute to `_OTHER`.

  If the HTTP instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it **MUST** provide a way to override
  the list of known HTTP methods. If this override is done via environment variable, then the environment variable **MUST** be named
  OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated list of case-sensitive known HTTP methods
  (this list **MUST** be a full override of the default known method, it is not a list of known methods in addition to the defaults).

  HTTP method names are case-sensitive and `http.request.method` attribute value **MUST** match a known HTTP method name exactly.
  Instrumentations for specific web frameworks that consider HTTP methods to be case insensitive, **SHOULD** populate a canonical equivalent.
  Tracing instrumentations that do so, **MUST** also set `http.request.method_original` to the original value.


  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_request_method(:connect)
      :CONNECT
      
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_request_method(:custom_value)
      :custom_value
  """
  @spec http_request_method(http_request_method()) ::
          connect()
          | delete()
          | get()
          | head()
          | options()
          | patch()
          | post()
          | put()
          | trace()
          | other()
          | atom()
  def http_request_method(option) do
    case option do
      :connect -> :CONNECT
      :delete -> :DELETE
      :get -> :GET
      :head -> :HEAD
      :options -> :OPTIONS
      :patch -> :PATCH
      :post -> :POST
      :put -> :PUT
      :trace -> :TRACE
      :other -> :_OTHER
      _ -> option
    end
  end

  @doc """
  Original HTTP method sent by the client in the request line.


  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_request_methodoriginal()
      :"http.request.method_original"
  """
  @spec http_request_methodoriginal :: :"http.request.method_original"
  def http_request_methodoriginal do
    :"http.request.method_original"
  end

  @doc """
  The ordinal number of request resending attempt (for any reason, including redirects).

  ### Notes

  The resend count **SHOULD** be updated each time an HTTP request gets resent by the client, regardless of what was the cause of the resending (e.g. redirection, authorization failure, 503 Server Unavailable, network issues, or any other).


  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_request_resendcount()
      :"http.request.resend_count"
  """
  @spec http_request_resendcount :: :"http.request.resend_count"
  def http_request_resendcount do
    :"http.request.resend_count"
  end

  @doc """
  The total size of the request in bytes. This should be the total number of bytes sent over the wire, including the request line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and request body if any.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_request_size()
      :"http.request.size"
  """
  @spec http_request_size :: :"http.request.size"
  def http_request_size do
    :"http.request.size"
  end

  @deprecated """
  Replaced by `http.request.header.content-length`.
  """

  @spec http_requestcontentlength :: :"http.request_content_length"
  def http_requestcontentlength do
    :"http.request_content_length"
  end

  @deprecated """
  Replaced by `http.request.body.size`.
  """

  @spec http_requestcontentlengthuncompressed :: :"http.request_content_length_uncompressed"
  def http_requestcontentlengthuncompressed do
    :"http.request_content_length_uncompressed"
  end

  @doc """
  The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_response_body_size()
      :"http.response.body.size"
  """
  @spec http_response_body_size :: :"http.response.body.size"
  def http_response_body_size do
    :"http.response.body.size"
  end

  @doc """
  HTTP response headers, `<key>` being the normalized HTTP Header name (lowercase), the value being the header values.

  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which headers are to be captured. Including all response headers can be a security risk - explicit configuration helps avoid leaking sensitive information.
  Users **MAY** explicitly configure instrumentations to capture them even though it is not recommended.
  The attribute value **MUST** consist of either multiple header values as an array of strings or a single-item array containing a possibly comma-concatenated string, depending on the way the HTTP library provides access to headers.


  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_response_header()
      :"http.response.header"
  """
  @spec http_response_header :: :"http.response.header"
  def http_response_header do
    :"http.response.header"
  end

  @doc """
  The total size of the response in bytes. This should be the total number of bytes sent over the wire, including the status line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and response body and trailers if any.



  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_response_size()
      :"http.response.size"
  """
  @spec http_response_size :: :"http.response.size"
  def http_response_size do
    :"http.response.size"
  end

  @doc """
  [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6).


  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_response_statuscode()
      :"http.response.status_code"
  """
  @spec http_response_statuscode :: :"http.response.status_code"
  def http_response_statuscode do
    :"http.response.status_code"
  end

  @deprecated """
  Replaced by `http.response.header.content-length`.
  """

  @spec http_responsecontentlength :: :"http.response_content_length"
  def http_responsecontentlength do
    :"http.response_content_length"
  end

  @deprecated """
  Replace by `http.response.body.size`.
  """

  @spec http_responsecontentlengthuncompressed :: :"http.response_content_length_uncompressed"
  def http_responsecontentlengthuncompressed do
    :"http.response_content_length_uncompressed"
  end

  @doc """
  The matched route, that is, the path template in the format used by the respective server framework.

  ### Notes

  MUST **NOT** be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can **NOT** substitute it.
  **SHOULD** include the [application root](/docs/http/http-spans.md#http-server-definitions) if there is one.


  ### Example
      iex> OpenTelemetry.SemanticConventions.HttpAttributes.http_route()
      :"http.route"
  """
  @spec http_route :: :"http.route"
  def http_route do
    :"http.route"
  end

  @deprecated """
  Replaced by `url.scheme` instead.
  """

  @spec http_scheme :: :"http.scheme"
  def http_scheme do
    :"http.scheme"
  end

  @deprecated """
  Replaced by `server.address`.
  """

  @spec http_servername :: :"http.server_name"
  def http_servername do
    :"http.server_name"
  end

  @deprecated """
  Replaced by `http.response.status_code`.
  """

  @spec http_statuscode :: :"http.status_code"
  def http_statuscode do
    :"http.status_code"
  end

  @deprecated """
  Split to `url.path` and `url.query.
  """

  @spec http_target :: :"http.target"
  def http_target do
    :"http.target"
  end

  @deprecated """
  Replaced by `url.full`.
  """

  @spec http_url :: :"http.url"
  def http_url do
    :"http.url"
  end

  @deprecated """
  Replaced by `user_agent.original`.
  """

  @spec http_useragent :: :"http.user_agent"
  def http_useragent do
    :"http.user_agent"
  end
end
