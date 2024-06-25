defmodule OpenTelemetry.SemanticConventions.TelemetryAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Telemetry attributes.
  """

  @doc """
  The name of the auto instrumentation agent or distribution, if used.

  ### Notes

  Official auto instrumentation agents and distributions **SHOULD** set the `telemetry.distro.name` attribute to
  a string starting with `opentelemetry-`, e.g. `opentelemetry-java-instrumentation`.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TelemetryAttributes.telemetry_distro_name()
      :"telemetry.distro.name"
  """
  @spec telemetry_distro_name :: :"telemetry.distro.name"
  def telemetry_distro_name do
    :"telemetry.distro.name"
  end

  @doc """
  The version string of the auto instrumentation agent or distribution, if used.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TelemetryAttributes.telemetry_distro_version()
      :"telemetry.distro.version"
  """
  @spec telemetry_distro_version :: :"telemetry.distro.version"
  def telemetry_distro_version do
    :"telemetry.distro.version"
  end

  @typedoc """
  The language of the telemetry SDK.


  ### Enum Values
  * `:cpp`
  * `:dotnet`
  * `:erlang`
  * `:go`
  * `:java`
  * `:nodejs`
  * `:php`
  * `:python`
  * `:ruby`
  * `:rust`
  * `:swift`
  * `:webjs`
  """
  @type telemetry_sdk_language() :: %{
          :cpp => :cpp,
          :dotnet => :dotnet,
          :erlang => :erlang,
          :go => :go,
          :java => :java,
          :nodejs => :nodejs,
          :php => :php,
          :python => :python,
          :ruby => :ruby,
          :rust => :rust,
          :swift => :swift,
          :webjs => :webjs
        }
  @doc """
  The language of the telemetry SDK.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TelemetryAttributes.telemetry_sdk_language().cpp
      :cpp
      
      iex> OpenTelemetry.SemanticConventions.TelemetryAttributes.telemetry_sdk_language(:custom_value)
      :custom_value
  """
  @spec telemetry_sdk_language() :: telemetry_sdk_language()
  def telemetry_sdk_language() do
    %{
      :cpp => :cpp,
      :dotnet => :dotnet,
      :erlang => :erlang,
      :go => :go,
      :java => :java,
      :nodejs => :nodejs,
      :php => :php,
      :python => :python,
      :ruby => :ruby,
      :rust => :rust,
      :swift => :swift,
      :webjs => :webjs
    }
  end

  @spec telemetry_sdk_language(atom() | String.t()) :: atom() | String.t()
  def telemetry_sdk_language(custom_value) do
    custom_value
  end

  @doc """
  The name of the telemetry SDK as defined above.

  ### Notes

  The OpenTelemetry SDK **MUST** set the `telemetry.sdk.name` attribute to `opentelemetry`.
  If another SDK, like a fork or a vendor-provided implementation, is used, this SDK **MUST** set the
  `telemetry.sdk.name` attribute to the fully-qualified class or module name of this SDK's main entry point
  or another suitable identifier depending on the language.
  The identifier `opentelemetry` is reserved and **MUST** **NOT** be used in this case.
  All custom identifiers **SHOULD** be stable across different versions of an implementation.


  ### Example
      iex> OpenTelemetry.SemanticConventions.TelemetryAttributes.telemetry_sdk_name()
      :"telemetry.sdk.name"
  """
  @spec telemetry_sdk_name :: :"telemetry.sdk.name"
  def telemetry_sdk_name do
    :"telemetry.sdk.name"
  end

  @doc """
  The version string of the telemetry SDK.



  ### Example
      iex> OpenTelemetry.SemanticConventions.TelemetryAttributes.telemetry_sdk_version()
      :"telemetry.sdk.version"
  """
  @spec telemetry_sdk_version :: :"telemetry.sdk.version"
  def telemetry_sdk_version do
    :"telemetry.sdk.version"
  end
end
