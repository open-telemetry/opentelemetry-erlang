defmodule OpenTelemetry.SemanticConventions.Logs do
  @moduledoc """
  WARNING: This module is deprecated and will be removed in a future release.
  Migrate to >= v1.27.0 semantic conventions.
  """

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec logs_schema_url :: String.t()
  def logs_schema_url do
    "https://opentelemetry.io/schemas/1.13.0"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec event_name :: :"event.name"
  def event_name do
    :"event.name"
  end

  @deprecated "Upgrade to >= v1.27.0 semantic conventions"
  @spec event_domain :: :"event.domain"
  def event_domain do
    :"event.domain"
  end
end
