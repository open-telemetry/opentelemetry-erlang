defmodule OpenTelemetry.SemConv.Incubating.WebengineAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Webengine attributes.
  """

  @doc """
  Additional description of the web engine (e.g. detailed version and edition information).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["WildFly Full 21.0.0.Final (WildFly Core 13.0.1.Final) - 2.2.2.Final"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.WebengineAttributes.webengine_description()
      :"webengine.description"

  ### Erlang

  ```erlang
  ?'WEBENGINE_DESCRIPTION'.
  'webengine.description'
  ```

  <!-- tabs-close -->
  """
  @spec webengine_description :: :"webengine.description"
  def webengine_description do
    :"webengine.description"
  end

  @doc """
  The name of the web engine.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["WildFly"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.WebengineAttributes.webengine_name()
      :"webengine.name"

  ### Erlang

  ```erlang
  ?'WEBENGINE_NAME'.
  'webengine.name'
  ```

  <!-- tabs-close -->
  """
  @spec webengine_name :: :"webengine.name"
  def webengine_name do
    :"webengine.name"
  end

  @doc """
  The version of the web engine.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["21.0.0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.WebengineAttributes.webengine_version()
      :"webengine.version"

  ### Erlang

  ```erlang
  ?'WEBENGINE_VERSION'.
  'webengine.version'
  ```

  <!-- tabs-close -->
  """
  @spec webengine_version :: :"webengine.version"
  def webengine_version do
    :"webengine.version"
  end
end
