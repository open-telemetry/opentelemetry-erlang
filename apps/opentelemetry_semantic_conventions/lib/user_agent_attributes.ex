defmodule OpenTelemetry.SemanticConventions.UserAgentAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for User_Agent attributes.
  """

  @doc """
  Name of the user-agent extracted from original. Usually refers to the browser's name.

  ### Notes

  [Example](https://www.whatsmyua.info) of extracting browser's name from original string. In the case of using a user-agent for non-browser products, such as microservices with multiple names/versions inside the `user_agent.original`, the most significant name **SHOULD** be selected. In such a scenario it should align with `user_agent.version`


  ### Example
      iex> OpenTelemetry.SemanticConventions.UserAgentAttributes.useragent_name()
      :"user_agent.name"
  """
  @spec useragent_name :: :"user_agent.name"
  def useragent_name do
    :"user_agent.name"
  end

  @doc """
  Value of the [HTTP User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#field.user-agent) header sent by the client.



  ### Example
      iex> OpenTelemetry.SemanticConventions.UserAgentAttributes.useragent_original()
      :"user_agent.original"
  """
  @spec useragent_original :: :"user_agent.original"
  def useragent_original do
    :"user_agent.original"
  end

  @doc """
  Version of the user-agent extracted from original. Usually refers to the browser's version

  ### Notes

  [Example](https://www.whatsmyua.info) of extracting browser's version from original string. In the case of using a user-agent for non-browser products, such as microservices with multiple names/versions inside the `user_agent.original`, the most significant version **SHOULD** be selected. In such a scenario it should align with `user_agent.name`


  ### Example
      iex> OpenTelemetry.SemanticConventions.UserAgentAttributes.useragent_version()
      :"user_agent.version"
  """
  @spec useragent_version :: :"user_agent.version"
  def useragent_version do
    :"user_agent.version"
  end
end
