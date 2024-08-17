defmodule OpenTelemetry.SemConv.Metrics.HTTPMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for HTTP metrics.
  """
  @doc """
  Duration of HTTP client requests.

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Metrics.HTTPMetrics.http_client_request_duration()
      :"http.client.request.duration"

  ### Erlang

  ```erlang
  ?HTTP_CLIENT_REQUEST_DURATION.
  'http.client.request.duration'
  ```

  <!-- tabs-close -->
  """

  @spec http_client_request_duration :: :"http.client.request.duration"
  def http_client_request_duration do
    :"http.client.request.duration"
  end

  @doc """
  Duration of HTTP server requests.

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Metrics.HTTPMetrics.http_server_request_duration()
      :"http.server.request.duration"

  ### Erlang

  ```erlang
  ?HTTP_SERVER_REQUEST_DURATION.
  'http.server.request.duration'
  ```

  <!-- tabs-close -->
  """

  @spec http_server_request_duration :: :"http.server.request.duration"
  def http_server_request_duration do
    :"http.server.request.duration"
  end
end
