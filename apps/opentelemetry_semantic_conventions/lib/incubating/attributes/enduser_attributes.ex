defmodule OpenTelemetry.SemConv.Incubating.EnduserAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Enduser attributes.
  """

  @doc """
  Unique identifier of an end user in the system. It maybe a username, email address, or other identifier.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Unique identifier of an end user in the system.

  > [!Warning]
  > This field contains sensitive (PII) information.

  ### Examples

  ```
  ["username"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.EnduserAttributes.enduser_id()
      :"enduser.id"

  ### Erlang

  ```erlang
  ?ENDUSER_ID.
  'enduser.id'
  ```

  <!-- tabs-close -->
  """
  @spec enduser_id :: :"enduser.id"
  def enduser_id do
    :"enduser.id"
  end

  @doc """
  Pseudonymous identifier of an end user. This identifier should be a random value that is not directly linked or associated with the end user's actual identity.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Pseudonymous identifier of an end user.

  > [!Warning]
  > This field contains sensitive (linkable PII) information.

  ### Examples

  ```
  ["QdH5CAWJgqVT4rOr0qtumf"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.EnduserAttributes.enduser_pseudo_id()
      :"enduser.pseudo.id"

  ### Erlang

  ```erlang
  ?ENDUSER_PSEUDO_ID.
  'enduser.pseudo.id'
  ```

  <!-- tabs-close -->
  """
  @spec enduser_pseudo_id :: :"enduser.pseudo.id"
  def enduser_pseudo_id do
    :"enduser.pseudo.id"
  end

  @deprecated """
  Replaced by `user.roles` attribute.
  """
  @spec enduser_role :: :"enduser.role"
  def enduser_role do
    :"enduser.role"
  end

  @deprecated """
  Removed.
  """
  @spec enduser_scope :: :"enduser.scope"
  def enduser_scope do
    :"enduser.scope"
  end
end
