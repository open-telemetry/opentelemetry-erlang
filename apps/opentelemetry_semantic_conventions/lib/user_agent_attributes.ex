defmodule OpenTelemetry.SemConv.UserAgentAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for User_Agent attributes.
  """

  @doc """
  Value of the [HTTP User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#field.user-agent) header sent by the client.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["CERN-LineMode/2.15 libwww/2.17b3", "Mozilla/5.0 (iPhone; CPU iPhone OS 14_7_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.2 Mobile/15E148 Safari/604.1", "YourApp/1.0.0 grpc-java-okhttp/1.27.2"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.UserAgentAttributes.user_agent_original()
      :"user_agent.original"

  ### Erlang

  ```erlang
  ?USER_AGENT_ORIGINAL.
  'user_agent.original'
  ```

  <!-- tabs-close -->
  """
  @spec user_agent_original :: :"user_agent.original"
  def user_agent_original do
    :"user_agent.original"
  end
end
