defmodule OpenTelemetry.SemanticConventions.DiskAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Disk attributes.
  """

  @typedoc """
  The disk IO operation direction.

  ### Enum Values
  * `:read` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  * `:write` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^
  """
  @type disk_io_direction() :: %{
          :read => :read,
          :write => :write
        }
  @doc """
  The disk IO operation direction.


  ### Example
      iex> OpenTelemetry.SemanticConventions.DiskAttributes.disk_io_direction().read
      :read
      
      iex> OpenTelemetry.SemanticConventions.DiskAttributes.disk_io_direction(:custom_value)
      :custom_value
  """
  @spec disk_io_direction() :: disk_io_direction()
  def disk_io_direction() do
    %{
      :read => :read,
      :write => :write
    }
  end

  @spec disk_io_direction(atom() | String.t()) :: atom() | String.t()
  def disk_io_direction(custom_value) do
    custom_value
  end
end
