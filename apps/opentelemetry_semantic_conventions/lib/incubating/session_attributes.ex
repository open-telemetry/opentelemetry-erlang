defmodule OpenTelemetry.SemConv.Incubating.SessionAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Session attributes.
  """

  @doc """
  A unique id to identify a session.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  00112233-4455-6677-8899-aabbccddeeff
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SessionAttributes.session_id()
      :"session.id"

  ### Erlang

  ```erlang
  ?SESSION_ID.
  'session.id'
  ```

  <!-- tabs-close -->
  """
  @spec session_id :: :"session.id"
  def session_id do
    :"session.id"
  end

  @doc """
  The previous `session.id` for this user, when known.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  00112233-4455-6677-8899-aabbccddeeff
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.SessionAttributes.session_previousid()
      :"session.previous_id"

  ### Erlang

  ```erlang
  ?SESSION_PREVIOUSID.
  'session.previous_id'
  ```

  <!-- tabs-close -->
  """
  @spec session_previousid :: :"session.previous_id"
  def session_previousid do
    :"session.previous_id"
  end
end
