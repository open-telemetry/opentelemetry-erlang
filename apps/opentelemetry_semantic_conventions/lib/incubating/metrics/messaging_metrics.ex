defmodule OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Messaging metrics.
  """
  @doc """
  Measures the duration of process operation.

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics.messaging_process_duration()
      :"messaging.process.duration"

  ### Erlang

  ```erlang
  ?'MESSAGING_PROCESS_DURATION'.
  'messaging.process.duration'
  ```

  <!-- tabs-close -->
  """

  @spec messaging_process_duration :: :"messaging.process.duration"
  def messaging_process_duration do
    :"messaging.process.duration"
  end

  @doc """
  Measures the number of processed messages.

  Instrument: `counter`
  Unit: `{message}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics.messaging_process_messages()
      :"messaging.process.messages"

  ### Erlang

  ```erlang
  ?'MESSAGING_PROCESS_MESSAGES'.
  'messaging.process.messages'
  ```

  <!-- tabs-close -->
  """

  @spec messaging_process_messages :: :"messaging.process.messages"
  def messaging_process_messages do
    :"messaging.process.messages"
  end

  @doc """
  Measures the duration of publish operation.

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics.messaging_publish_duration()
      :"messaging.publish.duration"

  ### Erlang

  ```erlang
  ?'MESSAGING_PUBLISH_DURATION'.
  'messaging.publish.duration'
  ```

  <!-- tabs-close -->
  """

  @spec messaging_publish_duration :: :"messaging.publish.duration"
  def messaging_publish_duration do
    :"messaging.publish.duration"
  end

  @doc """
  Measures the number of published messages.

  Instrument: `counter`
  Unit: `{message}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics.messaging_publish_messages()
      :"messaging.publish.messages"

  ### Erlang

  ```erlang
  ?'MESSAGING_PUBLISH_MESSAGES'.
  'messaging.publish.messages'
  ```

  <!-- tabs-close -->
  """

  @spec messaging_publish_messages :: :"messaging.publish.messages"
  def messaging_publish_messages do
    :"messaging.publish.messages"
  end

  @doc """
  Measures the duration of receive operation.

  Instrument: `histogram`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics.messaging_receive_duration()
      :"messaging.receive.duration"

  ### Erlang

  ```erlang
  ?'MESSAGING_RECEIVE_DURATION'.
  'messaging.receive.duration'
  ```

  <!-- tabs-close -->
  """

  @spec messaging_receive_duration :: :"messaging.receive.duration"
  def messaging_receive_duration do
    :"messaging.receive.duration"
  end

  @doc """
  Measures the number of received messages.

  Instrument: `counter`
  Unit: `{message}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics.messaging_receive_messages()
      :"messaging.receive.messages"

  ### Erlang

  ```erlang
  ?'MESSAGING_RECEIVE_MESSAGES'.
  'messaging.receive.messages'
  ```

  <!-- tabs-close -->
  """

  @spec messaging_receive_messages :: :"messaging.receive.messages"
  def messaging_receive_messages do
    :"messaging.receive.messages"
  end
end
