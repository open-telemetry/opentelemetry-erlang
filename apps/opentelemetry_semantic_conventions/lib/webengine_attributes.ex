defmodule OpenTelemetry.SemanticConventions.WebengineAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Webengine attributes.
  """

  @doc """
  Additional description of the web engine (e.g. detailed version and edition information).



  ### Example
      iex> OpenTelemetry.SemanticConventions.WebengineAttributes.webengine_description()
      :"webengine.description"
  """
  @spec webengine_description :: :"webengine.description"
  def webengine_description do
    :"webengine.description"
  end

  @doc """
  The name of the web engine.



  ### Example
      iex> OpenTelemetry.SemanticConventions.WebengineAttributes.webengine_name()
      :"webengine.name"
  """
  @spec webengine_name :: :"webengine.name"
  def webengine_name do
    :"webengine.name"
  end

  @doc """
  The version of the web engine.



  ### Example
      iex> OpenTelemetry.SemanticConventions.WebengineAttributes.webengine_version()
      :"webengine.version"
  """
  @spec webengine_version :: :"webengine.version"
  def webengine_version do
    :"webengine.version"
  end
end
