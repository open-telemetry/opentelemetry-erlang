defmodule OpenTelemetry.SemanticConventions.CodeAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Code attributes.
  """

  @doc """
  The column number in `code.filepath` best representing the operation. It **SHOULD** point within the code unit named in `code.function`.



  ### Example
      iex> OpenTelemetry.SemanticConventions.CodeAttributes.code_column()
      :"code.column"
  """
  @spec code_column :: :"code.column"
  def code_column do
    :"code.column"
  end

  @doc """
  The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path).



  ### Example
      iex> OpenTelemetry.SemanticConventions.CodeAttributes.code_filepath()
      :"code.filepath"
  """
  @spec code_filepath :: :"code.filepath"
  def code_filepath do
    :"code.filepath"
  end

  @doc """
  The method or function name, or equivalent (usually rightmost part of the code unit's name).



  ### Example
      iex> OpenTelemetry.SemanticConventions.CodeAttributes.code_function()
      :"code.function"
  """
  @spec code_function :: :"code.function"
  def code_function do
    :"code.function"
  end

  @doc """
  The line number in `code.filepath` best representing the operation. It **SHOULD** point within the code unit named in `code.function`.



  ### Example
      iex> OpenTelemetry.SemanticConventions.CodeAttributes.code_lineno()
      :"code.lineno"
  """
  @spec code_lineno :: :"code.lineno"
  def code_lineno do
    :"code.lineno"
  end

  @doc """
  The "namespace" within which `code.function` is defined. Usually the qualified class or module name, such that `code.namespace` + some separator + `code.function` form a unique identifier for the code unit.



  ### Example
      iex> OpenTelemetry.SemanticConventions.CodeAttributes.code_namespace()
      :"code.namespace"
  """
  @spec code_namespace :: :"code.namespace"
  def code_namespace do
    :"code.namespace"
  end

  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG.



  ### Example
      iex> OpenTelemetry.SemanticConventions.CodeAttributes.code_stacktrace()
      :"code.stacktrace"
  """
  @spec code_stacktrace :: :"code.stacktrace"
  def code_stacktrace do
    :"code.stacktrace"
  end
end
