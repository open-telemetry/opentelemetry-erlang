defmodule OpenTelemetry.SemConv.Incubating.HerokuAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Heroku attributes.
  """

  @doc """
  Unique identifier for the application

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2daa2797-e42b-4624-9322-ec3f968df4da"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HerokuAttributes.heroku_app_id()
      :"heroku.app.id"

  ### Erlang

  ```erlang
  ?HEROKU_APP_ID.
  'heroku.app.id'
  ```

  <!-- tabs-close -->
  """
  @spec heroku_app_id :: :"heroku.app.id"
  def heroku_app_id do
    :"heroku.app.id"
  end

  @doc """
  Commit hash for the current release

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["e6134959463efd8966b20e75b913cafe3f5ec"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HerokuAttributes.heroku_release_commit()
      :"heroku.release.commit"

  ### Erlang

  ```erlang
  ?HEROKU_RELEASE_COMMIT.
  'heroku.release.commit'
  ```

  <!-- tabs-close -->
  """
  @spec heroku_release_commit :: :"heroku.release.commit"
  def heroku_release_commit do
    :"heroku.release.commit"
  end

  @doc """
  Time and date the release was created

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["2022-10-23T18:00:42Z"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.HerokuAttributes.heroku_release_creation_timestamp()
      :"heroku.release.creation_timestamp"

  ### Erlang

  ```erlang
  ?HEROKU_RELEASE_CREATION_TIMESTAMP.
  'heroku.release.creation_timestamp'
  ```

  <!-- tabs-close -->
  """
  @spec heroku_release_creation_timestamp :: :"heroku.release.creation_timestamp"
  def heroku_release_creation_timestamp do
    :"heroku.release.creation_timestamp"
  end
end
