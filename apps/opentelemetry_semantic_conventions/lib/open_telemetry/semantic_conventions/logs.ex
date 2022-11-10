defmodule OpenTelemetry.SemanticConventions.Logs do
  @doc """
  The schema url for telemetry resources.

      iex> require OpenTelemetry.SemanticConventions.Logs
      ...> OpenTelemetry.SemanticConventions.Logs.logs_schema_url()
      "https://opentelemetry.io/schemas/1.13.0"
  """
  @spec logs_schema_url :: String.t()
  defmacro logs_schema_url do
    "https://opentelemetry.io/schemas/1.13.0"
  end

  @doc """
  The name identifies the event

      iex> require OpenTelemetry.SemanticConventions.Logs
      ...> OpenTelemetry.SemanticConventions.Logs.event_name()
      :"event.name"
  """
  @spec event_name :: :"event.name"
  defmacro event_name do
    :"event.name"
  end

  @doc """
  The domain identifies the context in which an event happened. An event name is unique only within a domain

  ### Notes

  An `event.name` is supposed to be unique only in the context of an
  `event.domain`, so this allows for two events in different domains to
  have same `event.name`, yet be unrelated events

      iex> require OpenTelemetry.SemanticConventions.Logs
      ...> OpenTelemetry.SemanticConventions.Logs.event_domain()
      :"event.domain"
  """
  @spec event_domain :: :"event.domain"
  defmacro event_domain do
    :"event.domain"
  end
end
