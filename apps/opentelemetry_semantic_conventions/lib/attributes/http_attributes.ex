defmodule OpenTelemetry.SemConv.HTTPAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for HTTP attributes.
  """

  @doc """
  HTTP request headers, `<key>` being the normalized HTTP Header name (lowercase), the value being the header values.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which headers are to be captured. Including all request headers can be a security risk - explicit configuration helps avoid leaking sensitive information.
  The `User-Agent` header is already captured in the `user_agent.original` attribute. Users **MAY** explicitly configure instrumentations to capture them even though it is not recommended.
  The attribute value **MUST** consist of either multiple header values as an array of strings or a single-item array containing a possibly comma-concatenated string, depending on the way the HTTP library provides access to headers.

  ### Examples

  ```
  ["http.request.header.content-type=[\"application/json\"]", "http.request.header.x-forwarded-for=[\"1.2.3.4\", \"1.2.3.5\"]"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.HTTPAttributes.http_request_header()
      :"http.request.header"

  ### Erlang

  ```erlang
  ?HTTP_REQUEST_HEADER.
  'http.request.header'
  ```

  <!-- tabs-close -->
  """
  @spec http_request_header :: :"http.request.header"
  def http_request_header do
    :"http.request.header"
  end

  @typedoc """
  HTTP request method.

  ### Enum Values
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
  @type http_request_method_values() :: %{
          :connect => :CONNECT,
          :delete => :DELETE,
          :get => :GET,
          :head => :HEAD,
          :options => :OPTIONS,
          :patch => :PATCH,
          :post => :POST,
          :put => :PUT,
          :trace => :TRACE,
          :other => :_OTHER
        }
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

  ### Examples

  ```
  ["GET", "POST", "HEAD"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.HTTPAttributes.http_request_method()
      :"http.request.method"

      iex> OpenTelemetry.SemConv.HTTPAttributes.http_request_method_values().connect
      :CONNECT

      iex> %{OpenTelemetry.SemConv.HTTPAttributes.http_request_method() => OpenTelemetry.SemConv.HTTPAttributes.http_request_method_values().connect}
      %{:"http.request.method" => :CONNECT}

  ### Erlang

  ```erlang
  ?HTTP_REQUEST_METHOD.
  'http.request.method'

  ?HTTP_REQUEST_METHOD_VALUES_CONNECT.
  'CONNECT'

  \#{?HTTP_REQUEST_METHOD => ?HTTP_REQUEST_METHOD_VALUES_CONNECT}.
  \#{'http.request.method' => 'CONNECT'}
  ```

  <!-- tabs-close -->
  """
  @spec http_request_method :: :"http.request.method"
  def http_request_method do
    :"http.request.method"
  end

  @spec http_request_method_values() :: http_request_method_values()
  def http_request_method_values() do
    %{
      :connect => :CONNECT,
      :delete => :DELETE,
      :get => :GET,
      :head => :HEAD,
      :options => :OPTIONS,
      :patch => :PATCH,
      :post => :POST,
      :put => :PUT,
      :trace => :TRACE,
      :other => :_OTHER
    }
  end

  @doc """
  Original HTTP method sent by the client in the request line.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["GeT", "ACL", "foo"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.HTTPAttributes.http_request_method_original()
      :"http.request.method_original"

  ### Erlang

  ```erlang
  ?HTTP_REQUEST_METHOD_ORIGINAL.
  'http.request.method_original'
  ```

  <!-- tabs-close -->
  """
  @spec http_request_method_original :: :"http.request.method_original"
  def http_request_method_original do
    :"http.request.method_original"
  end

  @doc """
  The ordinal number of request resending attempt (for any reason, including redirects).

  ### Value type

  Value must be of type `integer()`.
  ### Notes

  The resend count **SHOULD** be updated each time an HTTP request gets resent by the client, regardless of what was the cause of the resending (e.g. redirection, authorization failure, 503 Server Unavailable, network issues, or any other).

  ### Examples

  ```
  3
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.HTTPAttributes.http_request_resend_count()
      :"http.request.resend_count"

  ### Erlang

  ```erlang
  ?HTTP_REQUEST_RESEND_COUNT.
  'http.request.resend_count'
  ```

  <!-- tabs-close -->
  """
  @spec http_request_resend_count :: :"http.request.resend_count"
  def http_request_resend_count do
    :"http.request.resend_count"
  end

  @doc """
  HTTP response headers, `<key>` being the normalized HTTP Header name (lowercase), the value being the header values.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  Instrumentations **SHOULD** require an explicit configuration of which headers are to be captured. Including all response headers can be a security risk - explicit configuration helps avoid leaking sensitive information.
  Users **MAY** explicitly configure instrumentations to capture them even though it is not recommended.
  The attribute value **MUST** consist of either multiple header values as an array of strings or a single-item array containing a possibly comma-concatenated string, depending on the way the HTTP library provides access to headers.

  ### Examples

  ```
  ["http.response.header.content-type=[\"application/json\"]", "http.response.header.my-custom-header=[\"abc\", \"def\"]"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.HTTPAttributes.http_response_header()
      :"http.response.header"

  ### Erlang

  ```erlang
  ?HTTP_RESPONSE_HEADER.
  'http.response.header'
  ```

  <!-- tabs-close -->
  """
  @spec http_response_header :: :"http.response.header"
  def http_response_header do
    :"http.response.header"
  end

  @doc """
  [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6).
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [200]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.HTTPAttributes.http_response_status_code()
      :"http.response.status_code"

  ### Erlang

  ```erlang
  ?HTTP_RESPONSE_STATUS_CODE.
  'http.response.status_code'
  ```

  <!-- tabs-close -->
  """
  @spec http_response_status_code :: :"http.response.status_code"
  def http_response_status_code do
    :"http.response.status_code"
  end

  @doc """
  The matched route, that is, the path template in the format used by the respective server framework.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  MUST **NOT** be populated when this is not supported by the HTTP server framework as the route attribute should have low-cardinality and the URI path can **NOT** substitute it.
  **SHOULD** include the [application root](/docs/http/http-spans.md#http-server-definitions) if there is one.

  ### Examples

  ```
  ["/users/:userID?", "{controller}/{action}/{id?}"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.HTTPAttributes.http_route()
      :"http.route"

  ### Erlang

  ```erlang
  ?HTTP_ROUTE.
  'http.route'
  ```

  <!-- tabs-close -->
  """
  @spec http_route :: :"http.route"
  def http_route do
    :"http.route"
  end
end
