defmodule OpenTelemetry.SemConv.Incubating.Metrics.HTTPMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for HTTP metrics.
  """
  @doc """
  Number of active HTTP requests.

  Instrument: `updowncounter`
  Unit: `{request}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HTTPMetrics.http_client_active_requests()
      :"http.client.active_requests"

  ### Erlang

  ```erlang
  ?HTTP_CLIENT_ACTIVE_REQUESTS.
  'http.client.active_requests'
  ```

  <!-- tabs-close -->
  """

  @spec http_client_active_requests :: :"http.client.active_requests"
  def http_client_active_requests do
    :"http.client.active_requests"
  end

  @doc """
  The duration of the successfully established outbound HTTP connections.

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HTTPMetrics.http_client_connection_duration()
      :"http.client.connection.duration"

  ### Erlang

  ```erlang
  ?HTTP_CLIENT_CONNECTION_DURATION.
  'http.client.connection.duration'
  ```

  <!-- tabs-close -->
  """

  @spec http_client_connection_duration :: :"http.client.connection.duration"
  def http_client_connection_duration do
    :"http.client.connection.duration"
  end

  @doc """
  Number of outbound HTTP connections that are currently active or idle on the client.

  Instrument: `updowncounter`
  Unit: `{connection}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HTTPMetrics.http_client_open_connections()
      :"http.client.open_connections"

  ### Erlang

  ```erlang
  ?HTTP_CLIENT_OPEN_CONNECTIONS.
  'http.client.open_connections'
  ```

  <!-- tabs-close -->
  """

  @spec http_client_open_connections :: :"http.client.open_connections"
  def http_client_open_connections do
    :"http.client.open_connections"
  end

  @doc """
  Size of HTTP client request bodies.

  Instrument: `histogram`
  Unit: `By`
  ### Notes

  The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HTTPMetrics.http_client_request_body_size()
      :"http.client.request.body.size"

  ### Erlang

  ```erlang
  ?HTTP_CLIENT_REQUEST_BODY_SIZE.
  'http.client.request.body.size'
  ```

  <!-- tabs-close -->
  """

  @spec http_client_request_body_size :: :"http.client.request.body.size"
  def http_client_request_body_size do
    :"http.client.request.body.size"
  end

  @doc """
  Size of HTTP client response bodies.

  Instrument: `histogram`
  Unit: `By`
  ### Notes

  The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HTTPMetrics.http_client_response_body_size()
      :"http.client.response.body.size"

  ### Erlang

  ```erlang
  ?HTTP_CLIENT_RESPONSE_BODY_SIZE.
  'http.client.response.body.size'
  ```

  <!-- tabs-close -->
  """

  @spec http_client_response_body_size :: :"http.client.response.body.size"
  def http_client_response_body_size do
    :"http.client.response.body.size"
  end

  @doc """
  Number of active HTTP server requests.

  Instrument: `updowncounter`
  Unit: `{request}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HTTPMetrics.http_server_active_requests()
      :"http.server.active_requests"

  ### Erlang

  ```erlang
  ?HTTP_SERVER_ACTIVE_REQUESTS.
  'http.server.active_requests'
  ```

  <!-- tabs-close -->
  """

  @spec http_server_active_requests :: :"http.server.active_requests"
  def http_server_active_requests do
    :"http.server.active_requests"
  end

  @doc """
  Size of HTTP server request bodies.

  Instrument: `histogram`
  Unit: `By`
  ### Notes

  The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HTTPMetrics.http_server_request_body_size()
      :"http.server.request.body.size"

  ### Erlang

  ```erlang
  ?HTTP_SERVER_REQUEST_BODY_SIZE.
  'http.server.request.body.size'
  ```

  <!-- tabs-close -->
  """

  @spec http_server_request_body_size :: :"http.server.request.body.size"
  def http_server_request_body_size do
    :"http.server.request.body.size"
  end

  @doc """
  Size of HTTP server response bodies.

  Instrument: `histogram`
  Unit: `By`
  ### Notes

  The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.HTTPMetrics.http_server_response_body_size()
      :"http.server.response.body.size"

  ### Erlang

  ```erlang
  ?HTTP_SERVER_RESPONSE_BODY_SIZE.
  'http.server.response.body.size'
  ```

  <!-- tabs-close -->
  """

  @spec http_server_response_body_size :: :"http.server.response.body.size"
  def http_server_response_body_size do
    :"http.server.response.body.size"
  end
end
