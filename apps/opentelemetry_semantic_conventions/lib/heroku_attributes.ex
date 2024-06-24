defmodule OpenTelemetry.SemanticConventions.HerokuAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Heroku attributes.
  """

  @doc """
  Unique identifier for the application



  ### Example
      iex> OpenTelemetry.SemanticConventions.HerokuAttributes.heroku_app_id()
      :"heroku.app.id"
  """
  @spec heroku_app_id :: :"heroku.app.id"
  def heroku_app_id do
    :"heroku.app.id"
  end

  @doc """
  Commit hash for the current release



  ### Example
      iex> OpenTelemetry.SemanticConventions.HerokuAttributes.heroku_release_commit()
      :"heroku.release.commit"
  """
  @spec heroku_release_commit :: :"heroku.release.commit"
  def heroku_release_commit do
    :"heroku.release.commit"
  end

  @doc """
  Time and date the release was created



  ### Example
      iex> OpenTelemetry.SemanticConventions.HerokuAttributes.heroku_release_creationtimestamp()
      :"heroku.release.creation_timestamp"
  """
  @spec heroku_release_creationtimestamp :: :"heroku.release.creation_timestamp"
  def heroku_release_creationtimestamp do
    :"heroku.release.creation_timestamp"
  end
end
