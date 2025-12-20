defmodule OpenTelemetry.SemConv.Incubating.CodeAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Code attributes.
  """
  defdelegate code_column_number(), to: OpenTelemetry.SemConv.CodeAttributes

  defdelegate code_file_path(), to: OpenTelemetry.SemConv.CodeAttributes

  defdelegate code_function_name(), to: OpenTelemetry.SemConv.CodeAttributes

  defdelegate code_line_number(), to: OpenTelemetry.SemConv.CodeAttributes

  defdelegate code_stacktrace(), to: OpenTelemetry.SemConv.CodeAttributes

  @deprecated """
  Replaced by `code.column.number`
  """
  @spec code_column :: :"code.column"
  def code_column do
    :"code.column"
  end

  @deprecated """
  Replaced by `code.file.path`
  """
  @spec code_filepath :: :"code.filepath"
  def code_filepath do
    :"code.filepath"
  end

  @deprecated """
  Replaced by `code.function.name`
  """
  @spec code_function :: :"code.function"
  def code_function do
    :"code.function"
  end

  @deprecated """
  Replaced by `code.line.number`
  """
  @spec code_lineno :: :"code.lineno"
  def code_lineno do
    :"code.lineno"
  end

  @deprecated """
  Value should be included in `code.function.name` which is expected to be a fully-qualified name.
  """
  @spec code_namespace :: :"code.namespace"
  def code_namespace do
    :"code.namespace"
  end
end
