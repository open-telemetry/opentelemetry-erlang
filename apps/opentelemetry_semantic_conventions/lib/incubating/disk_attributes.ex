defmodule OpenTelemetry.SemConv.Incubating.DiskAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Disk attributes.
  """

  @typedoc """
  The disk IO operation direction.

  ### Enum Values
  * `:read` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:write` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type disk_io_direction() :: %{
          :read => :read,
          :write => :write
        }
  @doc """
  The disk IO operation direction.

  ### Examples

  ```
  ["read"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.DiskAttributes.disk_io_direction().read
      :read
      
      iex> OpenTelemetry.SemConv.Incubating.DiskAttributes.disk_io_direction(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'disk_io_direction.read'.
  read

  ?disk_io_direction.(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
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
