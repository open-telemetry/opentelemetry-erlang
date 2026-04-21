defmodule OpenTelemetry.SemConv.Incubating.Metrics.McpMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Mcp metrics.
  """
  @doc """
  The duration of the MCP request or notification as observed on the sender from the time it was sent until the response or ack is received.


  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.McpMetrics.mcp_client_operation_duration()
      :"mcp.client.operation.duration"

  ### Erlang

  ```erlang
  ?MCP_CLIENT_OPERATION_DURATION.
  'mcp.client.operation.duration'
  ```

  <!-- tabs-close -->
  """

  @spec mcp_client_operation_duration :: :"mcp.client.operation.duration"
  def mcp_client_operation_duration do
    :"mcp.client.operation.duration"
  end

  @doc """
  The duration of the MCP session as observed on the MCP client.

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.McpMetrics.mcp_client_session_duration()
      :"mcp.client.session.duration"

  ### Erlang

  ```erlang
  ?MCP_CLIENT_SESSION_DURATION.
  'mcp.client.session.duration'
  ```

  <!-- tabs-close -->
  """

  @spec mcp_client_session_duration :: :"mcp.client.session.duration"
  def mcp_client_session_duration do
    :"mcp.client.session.duration"
  end

  @doc """
  MCP request or notification duration as observed on the receiver from the time it was received until the result or ack is sent.


  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.McpMetrics.mcp_server_operation_duration()
      :"mcp.server.operation.duration"

  ### Erlang

  ```erlang
  ?MCP_SERVER_OPERATION_DURATION.
  'mcp.server.operation.duration'
  ```

  <!-- tabs-close -->
  """

  @spec mcp_server_operation_duration :: :"mcp.server.operation.duration"
  def mcp_server_operation_duration do
    :"mcp.server.operation.duration"
  end

  @doc """
  The duration of the MCP session as observed on the MCP server.

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.McpMetrics.mcp_server_session_duration()
      :"mcp.server.session.duration"

  ### Erlang

  ```erlang
  ?MCP_SERVER_SESSION_DURATION.
  'mcp.server.session.duration'
  ```

  <!-- tabs-close -->
  """

  @spec mcp_server_session_duration :: :"mcp.server.session.duration"
  def mcp_server_session_duration do
    :"mcp.server.session.duration"
  end
end
