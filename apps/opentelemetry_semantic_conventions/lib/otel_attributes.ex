defmodule OpenTelemetry.SemanticConventions.OtelAttributes do
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

  @doc """
  The name of the instrumentation scope - (`InstrumentationScope.Name` in OTLP).


  ### Example
      iex> OpenTelemetry.SemanticConventions.OtelAttributes.otel_scope_name()
      :"otel.scope.name"
  """
  @spec otel_scope_name :: :"otel.scope.name"
  def otel_scope_name do
    :"otel.scope.name"
  end

  @doc """
  The version of the instrumentation scope - (`InstrumentationScope.Version` in OTLP).


  ### Example
      iex> OpenTelemetry.SemanticConventions.OtelAttributes.otel_scope_version()
      :"otel.scope.version"
  """
  @spec otel_scope_version :: :"otel.scope.version"
  def otel_scope_version do
    :"otel.scope.version"
  end

  @type ok() :: :OK

  @type error() :: :ERROR

  @typedoc """
  Name of the code, either "OK" or "ERROR". **MUST** **NOT** be set if the status code is UNSET.

  ### Options
  * `:ok` - The operation has been validated by an Application developer or Operator to have completed successfully.
  * `:error` - The operation contains an error.

  """
  @type otel_statuscode() :: ok() | error() | atom()

  @doc """
  Name of the code, either "OK" or "ERROR". **MUST** **NOT** be set if the status code is UNSET.


  ### Example
      iex> OpenTelemetry.SemanticConventions.OtelAttributes.otel_statuscode(:ok)
      :OK
      
      iex> OpenTelemetry.SemanticConventions.OtelAttributes.otel_statuscode(:custom_value)
      :custom_value
  """
  @spec otel_statuscode(otel_statuscode()) :: ok() | error() | atom()
  def otel_statuscode(option) do
    case option do
      :ok -> :OK
      :error -> :ERROR
      _ -> option
    end
  end

  @doc """
  Description of the Status if it has a value, otherwise not set.


  ### Example
      iex> OpenTelemetry.SemanticConventions.OtelAttributes.otel_statusdescription()
      :"otel.status_description"
  """
  @spec otel_statusdescription :: :"otel.status_description"
  def otel_statusdescription do
    :"otel.status_description"
  end
end
