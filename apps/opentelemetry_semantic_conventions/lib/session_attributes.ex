defmodule OpenTelemetry.SemanticConventions.SessionAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Session attributes.
  """

  @doc """
  A unique id to identify a session.


  ### Example
      iex> OpenTelemetry.SemanticConventions.SessionAttributes.session_id()
      :"session.id"
  """
  @spec session_id :: :"session.id"
  def session_id do
    :"session.id"
  end

  @doc """
  The previous `session.id` for this user, when known.


  ### Example
      iex> OpenTelemetry.SemanticConventions.SessionAttributes.session_previousid()
      :"session.previous_id"
  """
  @spec session_previousid :: :"session.previous_id"
  def session_previousid do
    :"session.previous_id"
  end
end
