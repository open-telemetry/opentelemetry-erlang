defmodule OpenTelemetry.SemConv.Incubating.UserAgentAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for User_Agent attributes.
  """
  defdelegate user_agent_original(), to: OpenTelemetry.SemConv.UserAgentAttributes

  @doc """
  Name of the user-agent extracted from original. Usually refers to the browser's name.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  [Example](https://www.whatsmyua.info) of extracting browser's name from original string. In the case of using a user-agent for non-browser products, such as microservices with multiple names/versions inside the `user_agent.original`, the most significant name **SHOULD** be selected. In such a scenario it should align with `user_agent.version`

  ### Examples

  ```
  ["Safari", "YourApp"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.UserAgentAttributes.user_agent_name()
      :"user_agent.name"

  ### Erlang

  ```erlang
  ?USER_AGENT_NAME.
  'user_agent.name'
  ```

  <!-- tabs-close -->
  """
  @spec user_agent_name :: :"user_agent.name"
  def user_agent_name do
    :"user_agent.name"
  end

  @doc """
  Version of the user-agent extracted from original. Usually refers to the browser's version

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  [Example](https://www.whatsmyua.info) of extracting browser's version from original string. In the case of using a user-agent for non-browser products, such as microservices with multiple names/versions inside the `user_agent.original`, the most significant version **SHOULD** be selected. In such a scenario it should align with `user_agent.name`

  ### Examples

  ```
  ["14.1.2", "1.0.0"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.UserAgentAttributes.user_agent_version()
      :"user_agent.version"

  ### Erlang

  ```erlang
  ?USER_AGENT_VERSION.
  'user_agent.version'
  ```

  <!-- tabs-close -->
  """
  @spec user_agent_version :: :"user_agent.version"
  def user_agent_version do
    :"user_agent.version"
  end
end
