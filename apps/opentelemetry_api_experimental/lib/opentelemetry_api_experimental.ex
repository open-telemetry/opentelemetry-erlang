defmodule OpenTelemetryExperimental do
  @moduledoc """
  """

  @spec register_application_meter(atom()) :: boolean()
  defdelegate register_application_meter(name), to: :opentelemetry_experimental
end
