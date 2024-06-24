defmodule OpenTelemetry.SemanticConventions.DiskAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Disk attributes.
  """

  @typedoc """
  The disk IO operation direction.

  ### Options
  * `:read` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:write` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^

  """
  @type disk_io_direction() :: :read | :write | atom()

  @doc """
  The disk IO operation direction.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DiskAttributes.disk_io_direction(:read)
      :read
      
      iex> OpenTelemetry.SemanticConventions.DiskAttributes.disk_io_direction(:custom_value)
      :custom_value
  """
  @spec disk_io_direction(disk_io_direction()) :: :read | :write | atom()
  def disk_io_direction(option) do
    :"disk.io.direction"

    case option do
      :read -> :read
      :write -> :write
      _ -> option
    end
  end
end
