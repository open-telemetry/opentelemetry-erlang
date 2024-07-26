defmodule OpenTelemetry.SemConv.Incubating.OtelAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Otel attributes.
  """

  @deprecated """
  use the `otel.scope.name` attribute.
  """
  @spec otel_library_name :: :"otel.library.name"
  def otel_library_name do
    :"otel.library.name"
  end

  @deprecated """
  use the `otel.scope.version` attribute.
  """
  @spec otel_library_version :: :"otel.library.version"
  def otel_library_version do
    :"otel.library.version"
  end
end
