defmodule OpenTelemetry.SemConv.Incubating.SecurityRuleAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Security_Rule attributes.
  """

  @doc """
  A categorization value keyword used by the entity using the rule for detection of this event

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Attempted Information Leak"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SecurityRuleAttributes.security_rule_category()
      :"security_rule.category"

  ### Erlang

  ```erlang
  ?SECURITY_RULE_CATEGORY.
  'security_rule.category'
  ```

  <!-- tabs-close -->
  """
  @spec security_rule_category :: :"security_rule.category"
  def security_rule_category do
    :"security_rule.category"
  end

  @doc """
  The description of the rule generating the event.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Block requests to public DNS over HTTPS / TLS protocols"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SecurityRuleAttributes.security_rule_description()
      :"security_rule.description"

  ### Erlang

  ```erlang
  ?SECURITY_RULE_DESCRIPTION.
  'security_rule.description'
  ```

  <!-- tabs-close -->
  """
  @spec security_rule_description :: :"security_rule.description"
  def security_rule_description do
    :"security_rule.description"
  end

  @doc """
  Name of the license under which the rule used to generate this event is made available.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Apache 2.0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SecurityRuleAttributes.security_rule_license()
      :"security_rule.license"

  ### Erlang

  ```erlang
  ?SECURITY_RULE_LICENSE.
  'security_rule.license'
  ```

  <!-- tabs-close -->
  """
  @spec security_rule_license :: :"security_rule.license"
  def security_rule_license do
    :"security_rule.license"
  end

  @doc """
  The name of the rule or signature generating the event.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["BLOCK_DNS_over_TLS"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SecurityRuleAttributes.security_rule_name()
      :"security_rule.name"

  ### Erlang

  ```erlang
  ?SECURITY_RULE_NAME.
  'security_rule.name'
  ```

  <!-- tabs-close -->
  """
  @spec security_rule_name :: :"security_rule.name"
  def security_rule_name do
    :"security_rule.name"
  end

  @doc """
  Reference URL to additional information about the rule used to generate this event.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  The URL can point to the vendor’s documentation about the rule. If that’s not available, it can also be a link to a more general page describing this type of alert.

  ### Examples

  ```
  ["https://en.wikipedia.org/wiki/DNS_over_TLS"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SecurityRuleAttributes.security_rule_reference()
      :"security_rule.reference"

  ### Erlang

  ```erlang
  ?SECURITY_RULE_REFERENCE.
  'security_rule.reference'
  ```

  <!-- tabs-close -->
  """
  @spec security_rule_reference :: :"security_rule.reference"
  def security_rule_reference do
    :"security_rule.reference"
  end

  @doc """
  Name of the ruleset, policy, group, or parent category in which the rule used to generate this event is a member.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Standard_Protocol_Filters"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SecurityRuleAttributes.security_rule_ruleset_name()
      :"security_rule.ruleset.name"

  ### Erlang

  ```erlang
  ?SECURITY_RULE_RULESET_NAME.
  'security_rule.ruleset.name'
  ```

  <!-- tabs-close -->
  """
  @spec security_rule_ruleset_name :: :"security_rule.ruleset.name"
  def security_rule_ruleset_name do
    :"security_rule.ruleset.name"
  end

  @doc """
  A rule ID that is unique within the scope of a set or group of agents, observers, or other entities using the rule for detection of this event.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["550e8400-e29b-41d4-a716-446655440000", "1100110011"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SecurityRuleAttributes.security_rule_uuid()
      :"security_rule.uuid"

  ### Erlang

  ```erlang
  ?SECURITY_RULE_UUID.
  'security_rule.uuid'
  ```

  <!-- tabs-close -->
  """
  @spec security_rule_uuid :: :"security_rule.uuid"
  def security_rule_uuid do
    :"security_rule.uuid"
  end

  @doc """
  The version / revision of the rule being used for analysis.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1.0.0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SecurityRuleAttributes.security_rule_version()
      :"security_rule.version"

  ### Erlang

  ```erlang
  ?SECURITY_RULE_VERSION.
  'security_rule.version'
  ```

  <!-- tabs-close -->
  """
  @spec security_rule_version :: :"security_rule.version"
  def security_rule_version do
    :"security_rule.version"
  end
end
