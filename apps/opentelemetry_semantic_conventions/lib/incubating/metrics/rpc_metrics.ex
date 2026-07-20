defmodule OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for RPC metrics.
  """
  @doc """
  Measures the duration of an outgoing Remote Procedure Call (RPC).

  Instrument: `histogram`
  Unit: `s`
  ### Notes

  When this metric is reported alongside an RPC client span, the metric value
  **SHOULD** be the same as the RPC client span duration.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_client_call_duration()
      :"rpc.client.call.duration"

  ### Erlang

  ```erlang
  ?RPC_CLIENT_CALL_DURATION.
  'rpc.client.call.duration'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_client_call_duration :: :"rpc.client.call.duration"
  def rpc_client_call_duration do
    :"rpc.client.call.duration"
  end

  @deprecated """
  Replaced by `rpc.client.call.duration` with unit `s`.
  """

  @spec rpc_client_duration :: :"rpc.client.duration"
  def rpc_client_duration do
    :"rpc.client.duration"
  end

  @deprecated """
  Removed, no replacement at this time.
  """

  @spec rpc_client_request_size :: :"rpc.client.request.size"
  def rpc_client_request_size do
    :"rpc.client.request.size"
  end

  @deprecated """
  Removed, no replacement at this time.
  """

  @spec rpc_client_requests_per_rpc :: :"rpc.client.requests_per_rpc"
  def rpc_client_requests_per_rpc do
    :"rpc.client.requests_per_rpc"
  end

  @deprecated """
  Removed, no replacement at this time.
  """

  @spec rpc_client_response_size :: :"rpc.client.response.size"
  def rpc_client_response_size do
    :"rpc.client.response.size"
  end

  @deprecated """
  Removed, no replacement at this time.
  """

  @spec rpc_client_responses_per_rpc :: :"rpc.client.responses_per_rpc"
  def rpc_client_responses_per_rpc do
    :"rpc.client.responses_per_rpc"
  end

  @doc """
  Measures the duration of an incoming Remote Procedure Call (RPC).

  Instrument: `histogram`
  Unit: `s`
  ### Notes

  When this metric is reported alongside an RPC server span, the metric value
  **SHOULD** be the same as the RPC server span duration.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.RPCMetrics.rpc_server_call_duration()
      :"rpc.server.call.duration"

  ### Erlang

  ```erlang
  ?RPC_SERVER_CALL_DURATION.
  'rpc.server.call.duration'
  ```

  <!-- tabs-close -->
  """

  @spec rpc_server_call_duration :: :"rpc.server.call.duration"
  def rpc_server_call_duration do
    :"rpc.server.call.duration"
  end

  @deprecated """
  Replaced by `rpc.server.call.duration` with unit `s`.
  """

  @spec rpc_server_duration :: :"rpc.server.duration"
  def rpc_server_duration do
    :"rpc.server.duration"
  end

  @deprecated """
  Removed, no replacement at this time.
  """

  @spec rpc_server_request_size :: :"rpc.server.request.size"
  def rpc_server_request_size do
    :"rpc.server.request.size"
  end

  @deprecated """
  Removed, no replacement at this time.
  """

  @spec rpc_server_requests_per_rpc :: :"rpc.server.requests_per_rpc"
  def rpc_server_requests_per_rpc do
    :"rpc.server.requests_per_rpc"
  end

  @deprecated """
  Removed, no replacement at this time.
  """

  @spec rpc_server_response_size :: :"rpc.server.response.size"
  def rpc_server_response_size do
    :"rpc.server.response.size"
  end

  @deprecated """
  Removed, no replacement at this time.
  """

  @spec rpc_server_responses_per_rpc :: :"rpc.server.responses_per_rpc"
  def rpc_server_responses_per_rpc do
    :"rpc.server.responses_per_rpc"
  end
end
