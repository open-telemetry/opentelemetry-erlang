defmodule OpenTelemetry.SemanticConventions.FeatureFlagAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Feature_Flag attributes.
  """

  @doc """
  The unique identifier of the feature flag.


  ### Example
      iex> OpenTelemetry.SemanticConventions.FeatureFlagAttributes.featureflag_key()
      :"feature_flag.key"
  """
  @spec featureflag_key :: :"feature_flag.key"
  def featureflag_key do
    :"feature_flag.key"
  end

  @doc """
  The name of the service provider that performs the flag evaluation.


  ### Example
      iex> OpenTelemetry.SemanticConventions.FeatureFlagAttributes.featureflag_providername()
      :"feature_flag.provider_name"
  """
  @spec featureflag_providername :: :"feature_flag.provider_name"
  def featureflag_providername do
    :"feature_flag.provider_name"
  end

  @doc """
  SHOULD be a semantic identifier for a value. If one is unavailable, a stringified version of the value can be used.

  ### Notes

  A semantic identifier, commonly referred to as a variant, provides a means
  for referring to a value without including the value itself. This can
  provide additional context for understanding the meaning behind a value.
  For example, the variant `red` maybe be used for the value `#c05543`.

  A stringified version of the value can be used in situations where a
  semantic identifier is unavailable. String representation of the value
  should be determined by the implementer.

  ### Example
      iex> OpenTelemetry.SemanticConventions.FeatureFlagAttributes.featureflag_variant()
      :"feature_flag.variant"
  """
  @spec featureflag_variant :: :"feature_flag.variant"
  def featureflag_variant do
    :"feature_flag.variant"
  end
end
