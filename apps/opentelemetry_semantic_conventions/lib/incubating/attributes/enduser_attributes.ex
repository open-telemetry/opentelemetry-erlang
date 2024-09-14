defmodule OpenTelemetry.SemConv.Incubating.EnduserAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Enduser attributes.
  """

  @deprecated """
  Replaced by `user.id` attribute.
  """
  @spec enduser_id :: :"enduser.id"
  def enduser_id do
    :"enduser.id"
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
