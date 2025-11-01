defmodule OpenTelemetry.SemConv.Incubating.OtherAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Other attributes.
  """

  @typedoc """
  Deprecated, use `db.client.connection.state` instead.

  ### Enum Values
  * `:idle` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  * `:used` ^[e](`m:OpenTelemetry.SemConv#experimental`)^
  """
  @type state_values() :: %{
          :idle => :idle,
          :used => :used
        }
  @deprecated """
  Replaced by `db.client.connection.state`.
  """
  @spec state :: :state
  def state do
    :state
  end

  @spec state_values() :: state_values()
  def state_values() do
    %{
      :idle => :idle,
      :used => :used
    }
  end
end
