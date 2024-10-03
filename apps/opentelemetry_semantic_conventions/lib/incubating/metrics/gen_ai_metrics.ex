defmodule OpenTelemetry.SemConv.Incubating.Metrics.GenAiMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Gen_Ai metrics.
  """
  @doc """
  GenAI operation duration

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.GenAiMetrics.gen_ai_client_operation_duration()
      :"gen_ai.client.operation.duration"

  ### Erlang

  ```erlang
  ?GEN_AI_CLIENT_OPERATION_DURATION.
  'gen_ai.client.operation.duration'
  ```

  <!-- tabs-close -->
  """

  @spec gen_ai_client_operation_duration :: :"gen_ai.client.operation.duration"
  def gen_ai_client_operation_duration do
    :"gen_ai.client.operation.duration"
  end

  @doc """
  Measures number of input and output tokens used

  Instrument: `histogram`
  Unit: `{token}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.GenAiMetrics.gen_ai_client_token_usage()
      :"gen_ai.client.token.usage"

  ### Erlang

  ```erlang
  ?GEN_AI_CLIENT_TOKEN_USAGE.
  'gen_ai.client.token.usage'
  ```

  <!-- tabs-close -->
  """

  @spec gen_ai_client_token_usage :: :"gen_ai.client.token.usage"
  def gen_ai_client_token_usage do
    :"gen_ai.client.token.usage"
  end

  @doc """
  Generative AI server request duration such as time-to-last byte or last output token

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.GenAiMetrics.gen_ai_server_request_duration()
      :"gen_ai.server.request.duration"

  ### Erlang

  ```erlang
  ?GEN_AI_SERVER_REQUEST_DURATION.
  'gen_ai.server.request.duration'
  ```

  <!-- tabs-close -->
  """

  @spec gen_ai_server_request_duration :: :"gen_ai.server.request.duration"
  def gen_ai_server_request_duration do
    :"gen_ai.server.request.duration"
  end

  @doc """
  Time per output token generated after the first token for successful responses

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.GenAiMetrics.gen_ai_server_time_per_output_token()
      :"gen_ai.server.time_per_output_token"

  ### Erlang

  ```erlang
  ?GEN_AI_SERVER_TIME_PER_OUTPUT_TOKEN.
  'gen_ai.server.time_per_output_token'
  ```

  <!-- tabs-close -->
  """

  @spec gen_ai_server_time_per_output_token :: :"gen_ai.server.time_per_output_token"
  def gen_ai_server_time_per_output_token do
    :"gen_ai.server.time_per_output_token"
  end

  @doc """
  Time to generate first token for successful responses

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.GenAiMetrics.gen_ai_server_time_to_first_token()
      :"gen_ai.server.time_to_first_token"

  ### Erlang

  ```erlang
  ?GEN_AI_SERVER_TIME_TO_FIRST_TOKEN.
  'gen_ai.server.time_to_first_token'
  ```

  <!-- tabs-close -->
  """

  @spec gen_ai_server_time_to_first_token :: :"gen_ai.server.time_to_first_token"
  def gen_ai_server_time_to_first_token do
    :"gen_ai.server.time_to_first_token"
  end
end
