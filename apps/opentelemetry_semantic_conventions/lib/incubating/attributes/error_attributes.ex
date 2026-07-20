defmodule OpenTelemetry.SemConv.Incubating.ErrorAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Error attributes.
  """

  @deprecated """
  Use domain-specific error message attribute. For example, use `feature_flag.error.message` for feature flag errors.
  """
  @spec error_message :: :"error.message"
  def error_message do
    :"error.message"
  end
end
