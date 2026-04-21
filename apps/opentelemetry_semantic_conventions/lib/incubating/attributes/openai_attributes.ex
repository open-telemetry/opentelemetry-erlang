defmodule OpenTelemetry.SemConv.Incubating.OpenaiAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Openai attributes.
  """

  @typedoc """
  The type of OpenAI API being used.

  ### Enum Values
  * `:chat_completions` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The OpenAI [Chat Completions API](https://developers.openai.com/api/reference/chat-completions/overview).
  * `:responses` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The OpenAI [Responses API](https://developers.openai.com/api/reference/responses/overview).
  """
  @type openai_api_type_values() :: %{
          :chat_completions => :chat_completions,
          :responses => :responses
        }
  @doc """
  The type of OpenAI API being used.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OpenaiAttributes.openai_api_type()
      :"openai.api.type"

      iex> OpenTelemetry.SemConv.Incubating.OpenaiAttributes.openai_api_type_values().chat_completions
      :chat_completions

      iex> %{OpenTelemetry.SemConv.Incubating.OpenaiAttributes.openai_api_type() => OpenTelemetry.SemConv.Incubating.OpenaiAttributes.openai_api_type_values().chat_completions}
      %{:"openai.api.type" => :chat_completions}

  ### Erlang

  ```erlang
  ?OPENAI_API_TYPE.
  'openai.api.type'

  ?OPENAI_API_TYPE_VALUES_CHAT_COMPLETIONS.
  'chat_completions'

  \#{?OPENAI_API_TYPE => ?OPENAI_API_TYPE_VALUES_CHAT_COMPLETIONS}.
  \#{'openai.api.type' => 'chat_completions'}
  ```

  <!-- tabs-close -->
  """
  @spec openai_api_type :: :"openai.api.type"
  def openai_api_type do
    :"openai.api.type"
  end

  @spec openai_api_type_values() :: openai_api_type_values()
  def openai_api_type_values() do
    %{
      :chat_completions => :chat_completions,
      :responses => :responses
    }
  end

  @typedoc """
  The service tier requested. May be a specific tier, default, or auto.

  ### Enum Values
  * `:auto` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The system will utilize scale tier credits until they are exhausted.
  * `:default` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The system will utilize the default scale tier.
  """
  @type openai_request_service_tier_values() :: %{
          :auto => :auto,
          :default => :default
        }
  @doc """
  The service tier requested. May be a specific tier, default, or auto.

  ### Examples

  ```
  ["auto", "default"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OpenaiAttributes.openai_request_service_tier()
      :"openai.request.service_tier"

      iex> OpenTelemetry.SemConv.Incubating.OpenaiAttributes.openai_request_service_tier_values().auto
      :auto

      iex> %{OpenTelemetry.SemConv.Incubating.OpenaiAttributes.openai_request_service_tier() => OpenTelemetry.SemConv.Incubating.OpenaiAttributes.openai_request_service_tier_values().auto}
      %{:"openai.request.service_tier" => :auto}

  ### Erlang

  ```erlang
  ?OPENAI_REQUEST_SERVICE_TIER.
  'openai.request.service_tier'

  ?OPENAI_REQUEST_SERVICE_TIER_VALUES_AUTO.
  'auto'

  \#{?OPENAI_REQUEST_SERVICE_TIER => ?OPENAI_REQUEST_SERVICE_TIER_VALUES_AUTO}.
  \#{'openai.request.service_tier' => 'auto'}
  ```

  <!-- tabs-close -->
  """
  @spec openai_request_service_tier :: :"openai.request.service_tier"
  def openai_request_service_tier do
    :"openai.request.service_tier"
  end

  @spec openai_request_service_tier_values() :: openai_request_service_tier_values()
  def openai_request_service_tier_values() do
    %{
      :auto => :auto,
      :default => :default
    }
  end

  @doc """
  The service tier used for the response.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["scale", "default"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OpenaiAttributes.openai_response_service_tier()
      :"openai.response.service_tier"

  ### Erlang

  ```erlang
  ?OPENAI_RESPONSE_SERVICE_TIER.
  'openai.response.service_tier'
  ```

  <!-- tabs-close -->
  """
  @spec openai_response_service_tier :: :"openai.response.service_tier"
  def openai_response_service_tier do
    :"openai.response.service_tier"
  end

  @doc """
  A fingerprint to track any eventual change in the Generative AI environment.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["fp_44709d6fcb"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.OpenaiAttributes.openai_response_system_fingerprint()
      :"openai.response.system_fingerprint"

  ### Erlang

  ```erlang
  ?OPENAI_RESPONSE_SYSTEM_FINGERPRINT.
  'openai.response.system_fingerprint'
  ```

  <!-- tabs-close -->
  """
  @spec openai_response_system_fingerprint :: :"openai.response.system_fingerprint"
  def openai_response_system_fingerprint do
    :"openai.response.system_fingerprint"
  end
end
