defmodule OpenTelemetry.SemConv.TelemetryAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Telemetry attributes.
  """

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
  @type telemetry_sdk_language_values() :: %{
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



  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.TelemetryAttributes.telemetry_sdk_language()
      :"telemetry.sdk.language"

      iex> OpenTelemetry.SemConv.TelemetryAttributes.telemetry_sdk_language_values().cpp
      :cpp

      iex> %{OpenTelemetry.SemConv.TelemetryAttributes.telemetry_sdk_language() => OpenTelemetry.SemConv.TelemetryAttributes.telemetry_sdk_language_values().cpp}
      %{:"telemetry.sdk.language" => :cpp}

  ### Erlang

  ```erlang
  ?TELEMETRY_SDK_LANGUAGE.
  'telemetry.sdk.language'

  ?TELEMETRY_SDK_LANGUAGE_VALUES_CPP.
  'cpp'

  \#{?TELEMETRY_SDK_LANGUAGE => ?TELEMETRY_SDK_LANGUAGE_VALUES_CPP}.
  \#{'telemetry.sdk.language' => 'cpp'}
  ```

  <!-- tabs-close -->
  """
  @spec telemetry_sdk_language :: :"telemetry.sdk.language"
  def telemetry_sdk_language do
    :"telemetry.sdk.language"
  end

  @spec telemetry_sdk_language_values() :: telemetry_sdk_language_values()
  def telemetry_sdk_language_values() do
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

  @doc """
  The name of the telemetry SDK as defined above.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The OpenTelemetry SDK **MUST** set the `telemetry.sdk.name` attribute to `opentelemetry`.
  If another SDK, like a fork or a vendor-provided implementation, is used, this SDK **MUST** set the
  `telemetry.sdk.name` attribute to the fully-qualified class or module name of this SDK's main entry point
  or another suitable identifier depending on the language.
  The identifier `opentelemetry` is reserved and **MUST** **NOT** be used in this case.
  All custom identifiers **SHOULD** be stable across different versions of an implementation.

  ### Examples

  ```
  ["opentelemetry"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.TelemetryAttributes.telemetry_sdk_name()
      :"telemetry.sdk.name"

  ### Erlang

  ```erlang
  ?TELEMETRY_SDK_NAME.
  'telemetry.sdk.name'
  ```

  <!-- tabs-close -->
  """
  @spec telemetry_sdk_name :: :"telemetry.sdk.name"
  def telemetry_sdk_name do
    :"telemetry.sdk.name"
  end

  @doc """
  The version string of the telemetry SDK.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1.2.3"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.TelemetryAttributes.telemetry_sdk_version()
      :"telemetry.sdk.version"

  ### Erlang

  ```erlang
  ?TELEMETRY_SDK_VERSION.
  'telemetry.sdk.version'
  ```

  <!-- tabs-close -->
  """
  @spec telemetry_sdk_version :: :"telemetry.sdk.version"
  def telemetry_sdk_version do
    :"telemetry.sdk.version"
  end
end
