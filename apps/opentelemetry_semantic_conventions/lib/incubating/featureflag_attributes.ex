defmodule OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Feature_Flag attributes.
  """

  @doc """
  The unique identifier of the feature flag.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["logo-color"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_key()
      :"feature_flag.key"

  ### Erlang

  ```erlang
  ?FEATURE_FLAG_KEY.
  'feature_flag.key'
  ```

  <!-- tabs-close -->
  """
  @spec feature_flag_key :: :"feature_flag.key"
  def feature_flag_key do
    :"feature_flag.key"
  end

  @doc """
  The name of the service provider that performs the flag evaluation.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Flag Manager"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_provider_name()
      :"feature_flag.provider_name"

  ### Erlang

  ```erlang
  ?FEATURE_FLAG_PROVIDER_NAME.
  'feature_flag.provider_name'
  ```

  <!-- tabs-close -->
  """
  @spec feature_flag_provider_name :: :"feature_flag.provider_name"
  def feature_flag_provider_name do
    :"feature_flag.provider_name"
  end

  @doc """
  SHOULD be a semantic identifier for a value. If one is unavailable, a stringified version of the value can be used.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  A semantic identifier, commonly referred to as a variant, provides a means
  for referring to a value without including the value itself. This can
  provide additional context for understanding the meaning behind a value.
  For example, the variant `red` maybe be used for the value `#c05543`.

  A stringified version of the value can be used in situations where a
  semantic identifier is unavailable. String representation of the value
  should be determined by the implementer.
  ### Examples

  ```
  ["red", "true", "on"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_variant()
      :"feature_flag.variant"

  ### Erlang

  ```erlang
  ?FEATURE_FLAG_VARIANT.
  'feature_flag.variant'
  ```

  <!-- tabs-close -->
  """
  @spec feature_flag_variant :: :"feature_flag.variant"
  def feature_flag_variant do
    :"feature_flag.variant"
  end
end
