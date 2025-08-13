defmodule OpenTelemetry.SemConv.Incubating.ExceptionAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Exception attributes.
  """
  defdelegate exception_escaped(), to: OpenTelemetry.SemConv.ExceptionAttributes

  defdelegate exception_message(), to: OpenTelemetry.SemConv.ExceptionAttributes

  defdelegate exception_stacktrace(), to: OpenTelemetry.SemConv.ExceptionAttributes

  defdelegate exception_type(), to: OpenTelemetry.SemConv.ExceptionAttributes
end
