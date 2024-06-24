defmodule OpenTelemetry.SemanticConventions.EventAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Event attributes.
  """

  @doc """
  Identifies the class / type of event.

  ### Notes

  Event names are subject to the same rules as [attribute names](https://github.com/open-telemetry/opentelemetry-specification/tree/v1.33.0/specification/common/attribute-naming.md). Notably, event names are namespaced to avoid collisions and provide a clean separation of semantics for events in separate domains like browser, mobile, and kubernetes.


  ### Example
      iex> OpenTelemetry.SemanticConventions.EventAttributes.event_name()
      :"event.name"
  """
  @spec event_name :: :"event.name"
  def event_name do
    :"event.name"
  end
end
