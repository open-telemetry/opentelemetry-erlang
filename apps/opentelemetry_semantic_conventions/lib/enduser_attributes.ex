defmodule OpenTelemetry.SemanticConventions.EnduserAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Enduser attributes.
  """

  @doc """
  Username or client_id extracted from the access token or [Authorization](https://tools.ietf.org/html/rfc7235#section-4.2) header in the inbound request from outside the system.



  ### Example
      iex> OpenTelemetry.SemanticConventions.EnduserAttributes.enduser_id()
      :"enduser.id"
  """
  @spec enduser_id :: :"enduser.id"
  def enduser_id do
    :"enduser.id"
  end

  @doc """
  Actual/assumed role the client is making the request under extracted from token or application security context.


  ### Example
      iex> OpenTelemetry.SemanticConventions.EnduserAttributes.enduser_role()
      :"enduser.role"
  """
  @spec enduser_role :: :"enduser.role"
  def enduser_role do
    :"enduser.role"
  end

  @doc """
  Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an [OAuth 2.0 Access Token](https://tools.ietf.org/html/rfc6749#section-3.3) or an attribute value in a [SAML 2.0 Assertion](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html).



  ### Example
      iex> OpenTelemetry.SemanticConventions.EnduserAttributes.enduser_scope()
      :"enduser.scope"
  """
  @spec enduser_scope :: :"enduser.scope"
  def enduser_scope do
    :"enduser.scope"
  end
end
