defmodule OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Feature_Flag attributes.
  """

  @doc """
  The unique identifier for the flag evaluation context. For example, the targeting key.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["5157782b-2203-4c80-a857-dbbd5e7761db"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_context_id()
      :"feature_flag.context.id"

  ### Erlang

  ```erlang
  ?FEATURE_FLAG_CONTEXT_ID.
  'feature_flag.context.id'
  ```

  <!-- tabs-close -->
  """
  @spec feature_flag_context_id :: :"feature_flag.context.id"
  def feature_flag_context_id do
    :"feature_flag.context.id"
  end

  @deprecated """
  Replaced by `error.message`.
  """
  @spec feature_flag_evaluation_error_message :: :"feature_flag.evaluation.error.message"
  def feature_flag_evaluation_error_message do
    :"feature_flag.evaluation.error.message"
  end

  @typedoc """
  Deprecated, use `feature_flag.result.reason` instead.

  ### Enum Values
  * `:static` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value is static (no dynamic evaluation).
  * `:default` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value fell back to a pre-configured value (no dynamic evaluation occurred or dynamic evaluation yielded no result).
  * `:targeting_match` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value was the result of a dynamic evaluation, such as a rule or specific user-targeting.
  * `:split` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value was the result of pseudorandom assignment.
  * `:cached` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value was retrieved from cache.
  * `:disabled` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value was the result of the flag being disabled in the management system.
  * `:unknown` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The reason for the resolved value could not be determined.
  * `:stale` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value is non-authoritative or possibly out of date
  * `:error` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value was the result of an error.
  """
  @type feature_flag_evaluation_reason_values() :: %{
          :static => :static,
          :default => :default,
          :targeting_match => :targeting_match,
          :split => :split,
          :cached => :cached,
          :disabled => :disabled,
          :unknown => :unknown,
          :stale => :stale,
          :error => :error
        }
  @deprecated """
  Replaced by `feature_flag.result.reason`.
  """
  @spec feature_flag_evaluation_reason :: :"feature_flag.evaluation.reason"
  def feature_flag_evaluation_reason do
    :"feature_flag.evaluation.reason"
  end

  @spec feature_flag_evaluation_reason_values() :: feature_flag_evaluation_reason_values()
  def feature_flag_evaluation_reason_values() do
    %{
      :static => :static,
      :default => :default,
      :targeting_match => :targeting_match,
      :split => :split,
      :cached => :cached,
      :disabled => :disabled,
      :unknown => :unknown,
      :stale => :stale,
      :error => :error
    }
  end

  @doc """
  The lookup key of the feature flag.
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
  Identifies the feature flag provider.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Flag Manager"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_provider_name()
      :"feature_flag.provider.name"

  ### Erlang

  ```erlang
  ?FEATURE_FLAG_PROVIDER_NAME.
  'feature_flag.provider.name'
  ```

  <!-- tabs-close -->
  """
  @spec feature_flag_provider_name :: :"feature_flag.provider.name"
  def feature_flag_provider_name do
    :"feature_flag.provider.name"
  end

  @typedoc """
  The reason code which shows how a feature flag value was determined.


  ### Enum Values
  * `:static` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value is static (no dynamic evaluation).
  * `:default` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value fell back to a pre-configured value (no dynamic evaluation occurred or dynamic evaluation yielded no result).
  * `:targeting_match` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value was the result of a dynamic evaluation, such as a rule or specific user-targeting.
  * `:split` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value was the result of pseudorandom assignment.
  * `:cached` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value was retrieved from cache.
  * `:disabled` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value was the result of the flag being disabled in the management system.
  * `:unknown` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The reason for the resolved value could not be determined.
  * `:stale` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value is non-authoritative or possibly out of date
  * `:error` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The resolved value was the result of an error.
  """
  @type feature_flag_result_reason_values() :: %{
          :static => :static,
          :default => :default,
          :targeting_match => :targeting_match,
          :split => :split,
          :cached => :cached,
          :disabled => :disabled,
          :unknown => :unknown,
          :stale => :stale,
          :error => :error
        }
  @doc """
  The reason code which shows how a feature flag value was determined.


  ### Examples

  ```
  ["static", "targeting_match", "error", "default"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_result_reason()
      :"feature_flag.result.reason"

      iex> OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_result_reason_values().static
      :static

      iex> %{OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_result_reason() => OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_result_reason_values().static}
      %{:"feature_flag.result.reason" => :static}

  ### Erlang

  ```erlang
  ?FEATURE_FLAG_RESULT_REASON.
  'feature_flag.result.reason'

  ?FEATURE_FLAG_RESULT_REASON_VALUES_STATIC.
  'static'

  \#{?FEATURE_FLAG_RESULT_REASON => ?FEATURE_FLAG_RESULT_REASON_VALUES_STATIC}.
  \#{'feature_flag.result.reason' => 'static'}
  ```

  <!-- tabs-close -->
  """
  @spec feature_flag_result_reason :: :"feature_flag.result.reason"
  def feature_flag_result_reason do
    :"feature_flag.result.reason"
  end

  @spec feature_flag_result_reason_values() :: feature_flag_result_reason_values()
  def feature_flag_result_reason_values() do
    %{
      :static => :static,
      :default => :default,
      :targeting_match => :targeting_match,
      :split => :split,
      :cached => :cached,
      :disabled => :disabled,
      :unknown => :unknown,
      :stale => :stale,
      :error => :error
    }
  end

  @doc """
  A semantic identifier for an evaluated flag value.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  A semantic identifier, commonly referred to as a variant, provides a means
  for referring to a value without including the value itself. This can
  provide additional context for understanding the meaning behind a value.
  For example, the variant `red` maybe be used for the value `#c05543`.
  ### Examples

  ```
  ["red", "true", "on"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_result_variant()
      :"feature_flag.result.variant"

  ### Erlang

  ```erlang
  ?FEATURE_FLAG_RESULT_VARIANT.
  'feature_flag.result.variant'
  ```

  <!-- tabs-close -->
  """
  @spec feature_flag_result_variant :: :"feature_flag.result.variant"
  def feature_flag_result_variant do
    :"feature_flag.result.variant"
  end

  @doc """
  The identifier of the [flag set](https://openfeature.dev/specification/glossary/#flag-set) to which the feature flag belongs.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["proj-1", "ab98sgs", "service1/dev"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_set_id()
      :"feature_flag.set.id"

  ### Erlang

  ```erlang
  ?FEATURE_FLAG_SET_ID.
  'feature_flag.set.id'
  ```

  <!-- tabs-close -->
  """
  @spec feature_flag_set_id :: :"feature_flag.set.id"
  def feature_flag_set_id do
    :"feature_flag.set.id"
  end

  @deprecated """
  Replaced by `feature_flag.result.variant`.
  """
  @spec feature_flag_variant :: :"feature_flag.variant"
  def feature_flag_variant do
    :"feature_flag.variant"
  end

  @doc """
  The version of the ruleset used during the evaluation. This may be any stable value which uniquely identifies the ruleset.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1", "01ABCDEF"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.FeatureFlagAttributes.feature_flag_version()
      :"feature_flag.version"

  ### Erlang

  ```erlang
  ?FEATURE_FLAG_VERSION.
  'feature_flag.version'
  ```

  <!-- tabs-close -->
  """
  @spec feature_flag_version :: :"feature_flag.version"
  def feature_flag_version do
    :"feature_flag.version"
  end
end
