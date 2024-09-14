defmodule OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Messaging metrics.
  """
  @doc """
  Number of messages that were delivered to the application.

  Instrument: `counter`
  Unit: `{message}`
  ### Notes

  Records the number of messages pulled from the broker or number of messages dispatched to the application in push-based scenarios.
  The metric **SHOULD** be reported once per message delivery. For example, if receiving and processing operations are both instrumented for a single message delivery, this counter is incremented when the message is received and not reported when it is processed.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics.messaging_client_consumed_messages()
      :"messaging.client.consumed.messages"

  ### Erlang

  ```erlang
  ?MESSAGING_CLIENT_CONSUMED_MESSAGES.
  'messaging.client.consumed.messages'
  ```

  <!-- tabs-close -->
  """

  @spec messaging_client_consumed_messages :: :"messaging.client.consumed.messages"
  def messaging_client_consumed_messages do
    :"messaging.client.consumed.messages"
  end

  @doc """
  Duration of messaging operation initiated by a producer or consumer client.

  Instrument: `histogram`
  Unit: `s`
  ### Notes

  This metric **SHOULD** **NOT** be used to report processing duration - processing duration is reported in `messaging.process.duration` metric.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics.messaging_client_operation_duration()
      :"messaging.client.operation.duration"

  ### Erlang

  ```erlang
  ?MESSAGING_CLIENT_OPERATION_DURATION.
  'messaging.client.operation.duration'
  ```

  <!-- tabs-close -->
  """

  @spec messaging_client_operation_duration :: :"messaging.client.operation.duration"
  def messaging_client_operation_duration do
    :"messaging.client.operation.duration"
  end

  @doc """
  Number of messages producer attempted to publish to the broker.

  Instrument: `counter`
  Unit: `{message}`
  ### Notes

  This metric **MUST** **NOT** count messages that were created haven't yet been attempted to be published.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics.messaging_client_published_messages()
      :"messaging.client.published.messages"

  ### Erlang

  ```erlang
  ?MESSAGING_CLIENT_PUBLISHED_MESSAGES.
  'messaging.client.published.messages'
  ```

  <!-- tabs-close -->
  """

  @spec messaging_client_published_messages :: :"messaging.client.published.messages"
  def messaging_client_published_messages do
    :"messaging.client.published.messages"
  end

  @doc """
  Duration of processing operation.

  Instrument: `histogram`
  Unit: `s`
  ### Notes

  This metric **MUST** be reported for operations with `messaging.operation.type` that matches `process`.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.MessagingMetrics.messaging_process_duration()
      :"messaging.process.duration"

  ### Erlang

  ```erlang
  ?MESSAGING_PROCESS_DURATION.
  'messaging.process.duration'
  ```

  <!-- tabs-close -->
  """

  @spec messaging_process_duration :: :"messaging.process.duration"
  def messaging_process_duration do
    :"messaging.process.duration"
  end

  @deprecated """
  Replaced by `messaging.client.consumed.messages`.
  """

  @spec messaging_process_messages :: :"messaging.process.messages"
  def messaging_process_messages do
    :"messaging.process.messages"
  end

  @deprecated """
  Replaced by `messaging.client.operation.duration`.
  """

  @spec messaging_publish_duration :: :"messaging.publish.duration"
  def messaging_publish_duration do
    :"messaging.publish.duration"
  end

  @deprecated """
  Replaced by `messaging.client.produced.messages`.
  """

  @spec messaging_publish_messages :: :"messaging.publish.messages"
  def messaging_publish_messages do
    :"messaging.publish.messages"
  end

  @deprecated """
  Replaced by `messaging.client.operation.duration`.
  """

  @spec messaging_receive_duration :: :"messaging.receive.duration"
  def messaging_receive_duration do
    :"messaging.receive.duration"
  end

  @deprecated """
  Replaced by `messaging.client.consumed.messages`.
  """

  @spec messaging_receive_messages :: :"messaging.receive.messages"
  def messaging_receive_messages do
    :"messaging.receive.messages"
  end
end
