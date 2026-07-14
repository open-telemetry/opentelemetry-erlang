defmodule OpenTelemetry.SemConv.Incubating.MessageAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Message attributes.
  """

  @deprecated """
  Replaced by `rpc.message.compressed_size`.
  """
  @spec message_compressed_size :: :"message.compressed_size"
  def message_compressed_size do
    :"message.compressed_size"
  end

  @deprecated """
  Replaced by `rpc.message.id`.
  """
  @spec message_id :: :"message.id"
  def message_id do
    :"message.id"
  end

  @typedoc """
  Deprecated, use `rpc.message.type` instead.

  ### Enum Values
  * `:sent` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:received` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type message_type_values() :: %{
          :sent => :SENT,
          :received => :RECEIVED
        }
  @deprecated """
  Replaced by `rpc.message.type`.
  """
  @spec message_type :: :"message.type"
  def message_type do
    :"message.type"
  end

  @spec message_type_values() :: message_type_values()
  def message_type_values() do
    %{
      :sent => :SENT,
      :received => :RECEIVED
    }
  end

  @deprecated """
  Replaced by `rpc.message.uncompressed_size`.
  """
  @spec message_uncompressed_size :: :"message.uncompressed_size"
  def message_uncompressed_size do
    :"message.uncompressed_size"
  end
end
