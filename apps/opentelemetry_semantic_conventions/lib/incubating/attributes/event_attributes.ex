defmodule OpenTelemetry.SemConv.Incubating.EventAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Event attributes.
  """

  @doc """
  Identifies the class / type of event.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Event names are subject to the same rules as [attribute names](/docs/general/attribute-naming.md). Notably, event names are namespaced to avoid collisions and provide a clean separation of semantics for events in separate domains like browser, mobile, and kubernetes.

  ### Examples

  ```
  ["browser.mouse.click", "device.app.lifecycle"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.EventAttributes.event_name()
      :"event.name"

  ### Erlang

  ```erlang
  ?EVENT_NAME.
  'event.name'
  ```

  <!-- tabs-close -->
  """
  @spec event_name :: :"event.name"
  def event_name do
    :"event.name"
  end
end
