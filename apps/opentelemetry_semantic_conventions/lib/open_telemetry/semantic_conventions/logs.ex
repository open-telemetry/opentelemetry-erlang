defmodule OpenTelemetry.SemanticConventions.Logs do
  @doc """
  The schema url for telemetry resources.

      iex> OpenTelemetry.SemanticConventions.Logs.logs_schema_url()
      "https://opentelemetry.io/schemas/1.13.0"
  """
  @spec logs_schema_url :: String.t()
  def logs_schema_url do
    "https://opentelemetry.io/schemas/1.13.0"
  end
  @doc """
  The name identifies the event

      iex> OpenTelemetry.SemanticConventions.Logs.event_name()
      :"event.name"
  """
  @spec event_name :: :"event.name"
  def event_name do
    :"event.name"
  end
end
