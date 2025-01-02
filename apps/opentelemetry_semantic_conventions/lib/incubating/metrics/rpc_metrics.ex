defmodule OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for RPC metrics.
  """
  @doc """
  Measures the duration of outbound RPC.

  Instrument: `histogram`
  Unit: `ms`
  ### Notes

  While streaming RPCs may record this metric as start-of-batch
  to end-of-batch, it's hard to interpret in practice.

  **Streaming**: N/A.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_client_duration()
      :"rpc.client.duration"

  ### Erlang

  ```erlang
  ?RPC_CLIENT_DURATION.
  'rpc.client.duration'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_client_duration :: :"rpc.client.duration"
  def rpc_client_duration do
    :"rpc.client.duration"
  end

  @doc """
  Measures the size of RPC request messages (uncompressed).

  Instrument: `histogram`
  Unit: `By`
  ### Notes

  **Streaming**: Recorded per message in a streaming batch


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_client_request_size()
      :"rpc.client.request.size"

  ### Erlang

  ```erlang
  ?RPC_CLIENT_REQUEST_SIZE.
  'rpc.client.request.size'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_client_request_size :: :"rpc.client.request.size"
  def rpc_client_request_size do
    :"rpc.client.request.size"
  end

  @doc """
  Measures the number of messages received per RPC.

  Instrument: `histogram`
  Unit: `{count}`
  ### Notes

  Should be 1 for all non-streaming RPCs.

  **Streaming**: This metric is required for server and client streaming RPCs


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_client_requests_per_rpc()
      :"rpc.client.requests_per_rpc"

  ### Erlang

  ```erlang
  ?RPC_CLIENT_REQUESTS_PER_RPC.
  'rpc.client.requests_per_rpc'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_client_requests_per_rpc :: :"rpc.client.requests_per_rpc"
  def rpc_client_requests_per_rpc do
    :"rpc.client.requests_per_rpc"
  end

  @doc """
  Measures the size of RPC response messages (uncompressed).

  Instrument: `histogram`
  Unit: `By`
  ### Notes

  **Streaming**: Recorded per response in a streaming batch


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_client_response_size()
      :"rpc.client.response.size"

  ### Erlang

  ```erlang
  ?RPC_CLIENT_RESPONSE_SIZE.
  'rpc.client.response.size'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_client_response_size :: :"rpc.client.response.size"
  def rpc_client_response_size do
    :"rpc.client.response.size"
  end

  @doc """
  Measures the number of messages sent per RPC.

  Instrument: `histogram`
  Unit: `{count}`
  ### Notes

  Should be 1 for all non-streaming RPCs.

  **Streaming**: This metric is required for server and client streaming RPCs


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_client_responses_per_rpc()
      :"rpc.client.responses_per_rpc"

  ### Erlang

  ```erlang
  ?RPC_CLIENT_RESPONSES_PER_RPC.
  'rpc.client.responses_per_rpc'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_client_responses_per_rpc :: :"rpc.client.responses_per_rpc"
  def rpc_client_responses_per_rpc do
    :"rpc.client.responses_per_rpc"
  end

  @doc """
  Measures the duration of inbound RPC.

  Instrument: `histogram`
  Unit: `ms`
  ### Notes

  While streaming RPCs may record this metric as start-of-batch
  to end-of-batch, it's hard to interpret in practice.

  **Streaming**: N/A.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_server_duration()
      :"rpc.server.duration"

  ### Erlang

  ```erlang
  ?RPC_SERVER_DURATION.
  'rpc.server.duration'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_server_duration :: :"rpc.server.duration"
  def rpc_server_duration do
    :"rpc.server.duration"
  end

  @doc """
  Measures the size of RPC request messages (uncompressed).

  Instrument: `histogram`
  Unit: `By`
  ### Notes

  **Streaming**: Recorded per message in a streaming batch


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_server_request_size()
      :"rpc.server.request.size"

  ### Erlang

  ```erlang
  ?RPC_SERVER_REQUEST_SIZE.
  'rpc.server.request.size'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_server_request_size :: :"rpc.server.request.size"
  def rpc_server_request_size do
    :"rpc.server.request.size"
  end

  @doc """
  Measures the number of messages received per RPC.

  Instrument: `histogram`
  Unit: `{count}`
  ### Notes

  Should be 1 for all non-streaming RPCs.

  **Streaming** : This metric is required for server and client streaming RPCs


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_server_requests_per_rpc()
      :"rpc.server.requests_per_rpc"

  ### Erlang

  ```erlang
  ?RPC_SERVER_REQUESTS_PER_RPC.
  'rpc.server.requests_per_rpc'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_server_requests_per_rpc :: :"rpc.server.requests_per_rpc"
  def rpc_server_requests_per_rpc do
    :"rpc.server.requests_per_rpc"
  end

  @doc """
  Measures the size of RPC response messages (uncompressed).

  Instrument: `histogram`
  Unit: `By`
  ### Notes

  **Streaming**: Recorded per response in a streaming batch


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_server_response_size()
      :"rpc.server.response.size"

  ### Erlang

  ```erlang
  ?RPC_SERVER_RESPONSE_SIZE.
  'rpc.server.response.size'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_server_response_size :: :"rpc.server.response.size"
  def rpc_server_response_size do
    :"rpc.server.response.size"
  end

  @doc """
  Measures the number of messages sent per RPC.

  Instrument: `histogram`
  Unit: `{count}`
  ### Notes

  Should be 1 for all non-streaming RPCs.

  **Streaming**: This metric is required for server and client streaming RPCs


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_server_responses_per_rpc()
      :"rpc.server.responses_per_rpc"

  ### Erlang

  ```erlang
  ?RPC_SERVER_RESPONSES_PER_RPC.
  'rpc.server.responses_per_rpc'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_server_responses_per_rpc :: :"rpc.server.responses_per_rpc"
  def rpc_server_responses_per_rpc do
    :"rpc.server.responses_per_rpc"
  end
end
