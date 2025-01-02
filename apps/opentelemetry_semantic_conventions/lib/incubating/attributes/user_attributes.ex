defmodule OpenTelemetry.SemConv.Incubating.UserAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for User attributes.
  """

  @doc """
  User email address.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["a.einstein@example.com"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.UserAttributes.user_email()
      :"user.email"

  ### Erlang

  ```erlang
  ?USER_EMAIL.
  'user.email'
  ```

  <!-- tabs-close -->
  """
  @spec user_email :: :"user.email"
  def user_email do
    :"user.email"
  end

  @doc """
  User's full name

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Albert Einstein"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.UserAttributes.user_full_name()
      :"user.full_name"

  ### Erlang

  ```erlang
  ?USER_FULL_NAME.
  'user.full_name'
  ```

  <!-- tabs-close -->
  """
  @spec user_full_name :: :"user.full_name"
  def user_full_name do
    :"user.full_name"
  end

  @doc """
  Unique user hash to correlate information for a user in anonymized form.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Useful if `user.id` or `user.name` contain confidential information and cannot be used.

  ### Examples

  ```
  ["364fc68eaf4c8acec74a4e52d7d1feaa"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.UserAttributes.user_hash()
      :"user.hash"

  ### Erlang

  ```erlang
  ?USER_HASH.
  'user.hash'
  ```

  <!-- tabs-close -->
  """
  @spec user_hash :: :"user.hash"
  def user_hash do
    :"user.hash"
  end

  @doc """
  Unique identifier of the user.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["S-1-5-21-202424912787-2692429404-2351956786-1000"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.UserAttributes.user_id()
      :"user.id"

  ### Erlang

  ```erlang
  ?USER_ID.
  'user.id'
  ```

  <!-- tabs-close -->
  """
  @spec user_id :: :"user.id"
  def user_id do
    :"user.id"
  end

  @doc """
  Short name or login/username of the user.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["a.einstein"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.UserAttributes.user_name()
      :"user.name"

  ### Erlang

  ```erlang
  ?USER_NAME.
  'user.name'
  ```

  <!-- tabs-close -->
  """
  @spec user_name :: :"user.name"
  def user_name do
    :"user.name"
  end

  @doc """
  Array of user roles at the time of the event.

  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["admin", "reporting_user"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.UserAttributes.user_roles()
      :"user.roles"

  ### Erlang

  ```erlang
  ?USER_ROLES.
  'user.roles'
  ```

  <!-- tabs-close -->
  """
  @spec user_roles :: :"user.roles"
  def user_roles do
    :"user.roles"
  end
end
