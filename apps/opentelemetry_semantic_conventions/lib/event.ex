defmodule OpenTelemetry.SemanticConventions.Event do
  @moduledoc """
  OpenTelemetry Semantic Conventions for Attributes.
  """

  @doc namespace: :message
  @typedoc """
  Whether this is a received or sent message

  ### Options


  * `:SENT`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - sent

  * `:RECEIVED`^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - received



  """
  @type message_type() :: :SENT | :RECEIVED

  @doc """
  The URL of the OpenTelemetry schema for these keys and values.

      iex> OpenTelemetry.SemanticConventions.Event.schema_url()
      "https://opentelemetry.io/schemas/1.25.0"
  """
  @spec schema_url :: String.t()
  def schema_url do
    "https://opentelemetry.io/schemas/1.25.0"
  end

  @doc namespace: :message

  @doc """


  Whether this is a received or sent message

      iex> OpenTelemetry.SemanticConventions.Event.message_type()
      :"message.type"
  """

  @spec message_type :: :"message.type"
  def message_type do
    :"message.type"
  end
end
