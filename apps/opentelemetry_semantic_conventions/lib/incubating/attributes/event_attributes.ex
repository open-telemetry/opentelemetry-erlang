defmodule OpenTelemetry.SemConv.Incubating.EventAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Event attributes.
  """

  @deprecated """
  The value of this attribute MUST now be set as the value of the EventName field on the LogRecord to indicate that the LogRecord represents an Event.

  """
  @spec event_name :: :"event.name"
  def event_name do
    :"event.name"
  end
end
