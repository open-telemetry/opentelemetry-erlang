defmodule OpenTelemetry.SemConv.Incubating.TelemetryAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Telemetry attributes.
  """
  defdelegate telemetry_sdk_language(), to: OpenTelemetry.SemConv.TelemetryAttributes

  defdelegate telemetry_sdk_language_values(), to: OpenTelemetry.SemConv.TelemetryAttributes

  defdelegate telemetry_sdk_name(), to: OpenTelemetry.SemConv.TelemetryAttributes

  defdelegate telemetry_sdk_version(), to: OpenTelemetry.SemConv.TelemetryAttributes

  @doc """
  The name of the auto instrumentation agent or distribution, if used.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Official auto instrumentation agents and distributions **SHOULD** set the `telemetry.distro.name` attribute to
  a string starting with `opentelemetry-`, e.g. `opentelemetry-java-instrumentation`.

  ### Examples

  ```
  ["parts-unlimited-java"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TelemetryAttributes.telemetry_distro_name()
      :"telemetry.distro.name"

  ### Erlang

  ```erlang
  ?TELEMETRY_DISTRO_NAME.
  'telemetry.distro.name'
  ```

  <!-- tabs-close -->
  """
  @spec telemetry_distro_name :: :"telemetry.distro.name"
  def telemetry_distro_name do
    :"telemetry.distro.name"
  end

  @doc """
  The version string of the auto instrumentation agent or distribution, if used.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1.2.3"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.TelemetryAttributes.telemetry_distro_version()
      :"telemetry.distro.version"

  ### Erlang

  ```erlang
  ?TELEMETRY_DISTRO_VERSION.
  'telemetry.distro.version'
  ```

  <!-- tabs-close -->
  """
  @spec telemetry_distro_version :: :"telemetry.distro.version"
  def telemetry_distro_version do
    :"telemetry.distro.version"
  end
end
