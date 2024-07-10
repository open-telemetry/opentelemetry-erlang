defmodule OpenTelemetry.SemConv.Incubating.EnduserAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Enduser attributes.
  """

  @doc """
  Username or client_id extracted from the access token or [Authorization](https://tools.ietf.org/html/rfc7235#section-4.2) header in the inbound request from outside the system.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  username
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
  Actual/assumed role the client is making the request under extracted from token or application security context.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  admin
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.EnduserAttributes.enduser_role()
      :"enduser.role"

  ### Erlang

  ```erlang
  ?ENDUSER_ROLE.
  'enduser.role'
  ```

  <!-- tabs-close -->
  """
  @spec enduser_role :: :"enduser.role"
  def enduser_role do
    :"enduser.role"
  end

  @doc """
  Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an [OAuth 2.0 Access Token](https://tools.ietf.org/html/rfc6749#section-3.3) or an attribute value in a [SAML 2.0 Assertion](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  read:message, write:files
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.EnduserAttributes.enduser_scope()
      :"enduser.scope"

  ### Erlang

  ```erlang
  ?ENDUSER_SCOPE.
  'enduser.scope'
  ```

  <!-- tabs-close -->
  """
  @spec enduser_scope :: :"enduser.scope"
  def enduser_scope do
    :"enduser.scope"
  end
end
