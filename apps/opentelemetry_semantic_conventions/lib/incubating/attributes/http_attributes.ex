defmodule OpenTelemetry.SemConv.Incubating.HTTPAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for HTTP attributes.
  """
  defdelegate http_request_header(), to: OpenTelemetry.SemConv.HTTPAttributes

  defdelegate http_request_method(), to: OpenTelemetry.SemConv.HTTPAttributes

  defdelegate http_request_method_values(), to: OpenTelemetry.SemConv.HTTPAttributes

  defdelegate http_request_method_original(), to: OpenTelemetry.SemConv.HTTPAttributes

  defdelegate http_request_resend_count(), to: OpenTelemetry.SemConv.HTTPAttributes

  defdelegate http_response_header(), to: OpenTelemetry.SemConv.HTTPAttributes

  defdelegate http_response_status_code(), to: OpenTelemetry.SemConv.HTTPAttributes

  defdelegate http_route(), to: OpenTelemetry.SemConv.HTTPAttributes

  @deprecated """
  Replaced by `client.address`.
  """
  @spec http_client_ip :: :"http.client_ip"
  def http_client_ip do
    :"http.client_ip"
  end

  @typedoc """
  State of the HTTP connection in the HTTP connection pool.

  ### Enum Values
  * `:active` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - active state.
  * `:idle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - idle state.
  """
  @type http_connection_state_values() :: %{
          :active => :active,
          :idle => :idle
        }
  @doc """
  State of the HTTP connection in the HTTP connection pool.

  ### Examples

  ```
  ["active", "idle"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HTTPAttributes.http_connection_state()
      :"http.connection.state"

      iex> OpenTelemetry.SemConv.Incubating.HTTPAttributes.http_connection_state_values().active
      :active

      iex> %{OpenTelemetry.SemConv.Incubating.HTTPAttributes.http_connection_state() => OpenTelemetry.SemConv.Incubating.HTTPAttributes.http_connection_state_values().active}
      %{:"http.connection.state" => :active}

  ### Erlang

  ```erlang
  ?HTTP_CONNECTION_STATE.
  'http.connection.state'

  ?HTTP_CONNECTION_STATE_VALUES_ACTIVE.
  'active'

  \#{?HTTP_CONNECTION_STATE => ?HTTP_CONNECTION_STATE_VALUES_ACTIVE}.
  \#{'http.connection.state' => 'active'}
  ```

  <!-- tabs-close -->
  """
  @spec http_connection_state :: :"http.connection.state"
  def http_connection_state do
    :"http.connection.state"
  end

  @spec http_connection_state_values() :: http_connection_state_values()
  def http_connection_state_values() do
    %{
      :active => :active,
      :idle => :idle
    }
  end

  @typedoc """
  Deprecated, use `network.protocol.name` instead.

  ### Enum Values
  * `:http_1_0` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HTTP/1.0
  * `:http_1_1` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HTTP/1.1
  * `:http_2_0` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HTTP/2
  * `:http_3_0` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - HTTP/3
  * `:spdy` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - SPDY protocol.
  * `:quic` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - QUIC protocol.
  """
  @type http_flavor_values() :: %{
          :http_1_0 => :"1.0",
          :http_1_1 => :"1.1",
          :http_2_0 => :"2.0",
          :http_3_0 => :"3.0",
          :spdy => :SPDY,
          :quic => :QUIC
        }
  @deprecated """
  Replaced by `network.protocol.name`.
  """
  @spec http_flavor :: :"http.flavor"
  def http_flavor do
    :"http.flavor"
  end

  @spec http_flavor_values() :: http_flavor_values()
  def http_flavor_values() do
    %{
      :http_1_0 => :"1.0",
      :http_1_1 => :"1.1",
      :http_2_0 => :"2.0",
      :http_3_0 => :"3.0",
      :spdy => :SPDY,
      :quic => :QUIC
    }
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

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  3495
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HTTPAttributes.http_request_body_size()
      :"http.request.body.size"

  ### Erlang

  ```erlang
  ?HTTP_REQUEST_BODY_SIZE.
  'http.request.body.size'
  ```

  <!-- tabs-close -->
  """
  @spec http_request_body_size :: :"http.request.body.size"
  def http_request_body_size do
    :"http.request.body.size"
  end

  @doc """
  The total size of the request in bytes. This should be the total number of bytes sent over the wire, including the request line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and request body if any.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  1437
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HTTPAttributes.http_request_size()
      :"http.request.size"

  ### Erlang

  ```erlang
  ?HTTP_REQUEST_SIZE.
  'http.request.size'
  ```

  <!-- tabs-close -->
  """
  @spec http_request_size :: :"http.request.size"
  def http_request_size do
    :"http.request.size"
  end

  @deprecated """
  Replaced by `http.request.header.content-length`.
  """
  @spec http_request_content_length :: :"http.request_content_length"
  def http_request_content_length do
    :"http.request_content_length"
  end

  @deprecated """
  Replaced by `http.request.body.size`.
  """
  @spec http_request_content_length_uncompressed :: :"http.request_content_length_uncompressed"
  def http_request_content_length_uncompressed do
    :"http.request_content_length_uncompressed"
  end

  @doc """
  The size of the response payload body in bytes. This is the number of bytes transferred excluding headers and is often, but not always, present as the [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length) header. For requests using transport encoding, this should be the compressed size.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  3495
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HTTPAttributes.http_response_body_size()
      :"http.response.body.size"

  ### Erlang

  ```erlang
  ?HTTP_RESPONSE_BODY_SIZE.
  'http.response.body.size'
  ```

  <!-- tabs-close -->
  """
  @spec http_response_body_size :: :"http.response.body.size"
  def http_response_body_size do
    :"http.response.body.size"
  end

  @doc """
  The total size of the response in bytes. This should be the total number of bytes sent over the wire, including the status line (HTTP/1.1), framing (HTTP/2 and HTTP/3), headers, and response body and trailers if any.

  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  1437
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HTTPAttributes.http_response_size()
      :"http.response.size"

  ### Erlang

  ```erlang
  ?HTTP_RESPONSE_SIZE.
  'http.response.size'
  ```

  <!-- tabs-close -->
  """
  @spec http_response_size :: :"http.response.size"
  def http_response_size do
    :"http.response.size"
  end

  @deprecated """
  Replaced by `http.response.header.content-length`.
  """
  @spec http_response_content_length :: :"http.response_content_length"
  def http_response_content_length do
    :"http.response_content_length"
  end

  @deprecated """
  Replace by `http.response.body.size`.
  """
  @spec http_response_content_length_uncompressed :: :"http.response_content_length_uncompressed"
  def http_response_content_length_uncompressed do
    :"http.response_content_length_uncompressed"
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
  @spec http_server_name :: :"http.server_name"
  def http_server_name do
    :"http.server_name"
  end

  @deprecated """
  Replaced by `http.response.status_code`.
  """
  @spec http_status_code :: :"http.status_code"
  def http_status_code do
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
  @spec http_user_agent :: :"http.user_agent"
  def http_user_agent do
    :"http.user_agent"
  end
end
