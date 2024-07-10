defmodule OpenTelemetry.SemConv.Incubating.UserAgentAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for User_Agent attributes.
  """

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

      iex> OpenTelemetry.SemConv.Incubating.UserAgentAttributes.useragent_name()
      :"user_agent.name"

  ### Erlang

  ```erlang
  ?USERAGENT_NAME.
  'user_agent.name'
  ```

  <!-- tabs-close -->
  """
  @spec useragent_name :: :"user_agent.name"
  def useragent_name do
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

      iex> OpenTelemetry.SemConv.Incubating.UserAgentAttributes.useragent_version()
      :"user_agent.version"

  ### Erlang

  ```erlang
  ?USERAGENT_VERSION.
  'user_agent.version'
  ```

  <!-- tabs-close -->
  """
  @spec useragent_version :: :"user_agent.version"
  def useragent_version do
    :"user_agent.version"
  end
end
