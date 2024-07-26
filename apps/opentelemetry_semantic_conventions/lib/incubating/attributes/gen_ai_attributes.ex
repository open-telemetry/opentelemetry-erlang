defmodule OpenTelemetry.SemConv.Incubating.GenAiAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Gen_Ai attributes.
  """

  @doc """
  The full response received from the LLM.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  It's RECOMMENDED to format completions as JSON string matching [OpenAI messages format](https://platform.openai.com/docs/guides/text-generation)
  ### Examples

  ```
  ["[{'role': 'assistant', 'content': 'The capital of France is Paris.'}]"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_completion()
      :"gen_ai.completion"

  ### Erlang

  ```erlang
  ?'GEN_AI_COMPLETION'.
  'gen_ai.completion'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_completion :: :"gen_ai.completion"
  def gen_ai_completion do
    :"gen_ai.completion"
  end

  @doc """
  The full prompt sent to an LLM.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  It's RECOMMENDED to format prompts as JSON string matching [OpenAI messages format](https://platform.openai.com/docs/guides/text-generation)
  ### Examples

  ```
  ["[{'role': 'user', 'content': 'What is the capital of France?'}]"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_prompt()
      :"gen_ai.prompt"

  ### Erlang

  ```erlang
  ?'GEN_AI_PROMPT'.
  'gen_ai.prompt'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_prompt :: :"gen_ai.prompt"
  def gen_ai_prompt do
    :"gen_ai.prompt"
  end

  @doc """
  The maximum number of tokens the LLM generates for a request.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [100]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_max_tokens()
      :"gen_ai.request.max_tokens"

  ### Erlang

  ```erlang
  ?'GEN_AI_REQUEST_MAX_TOKENS'.
  'gen_ai.request.max_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_max_tokens :: :"gen_ai.request.max_tokens"
  def gen_ai_request_max_tokens do
    :"gen_ai.request.max_tokens"
  end

  @doc """
  The name of the LLM a request is being made to.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  gpt-4
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_model()
      :"gen_ai.request.model"

  ### Erlang

  ```erlang
  ?'GEN_AI_REQUEST_MODEL'.
  'gen_ai.request.model'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_model :: :"gen_ai.request.model"
  def gen_ai_request_model do
    :"gen_ai.request.model"
  end

  @doc """
  The temperature setting for the LLM request.
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [0.0]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_temperature()
      :"gen_ai.request.temperature"

  ### Erlang

  ```erlang
  ?'GEN_AI_REQUEST_TEMPERATURE'.
  'gen_ai.request.temperature'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_temperature :: :"gen_ai.request.temperature"
  def gen_ai_request_temperature do
    :"gen_ai.request.temperature"
  end

  @doc """
  The top_p sampling setting for the LLM request.
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [1.0]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_top_p()
      :"gen_ai.request.top_p"

  ### Erlang

  ```erlang
  ?'GEN_AI_REQUEST_TOP_P'.
  'gen_ai.request.top_p'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_top_p :: :"gen_ai.request.top_p"
  def gen_ai_request_top_p do
    :"gen_ai.request.top_p"
  end

  @doc """
  Array of reasons the model stopped generating tokens, corresponding to each generation received.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["stop"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_response_finish_reasons()
      :"gen_ai.response.finish_reasons"

  ### Erlang

  ```erlang
  ?'GEN_AI_RESPONSE_FINISH_REASONS'.
  'gen_ai.response.finish_reasons'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_response_finish_reasons :: :"gen_ai.response.finish_reasons"
  def gen_ai_response_finish_reasons do
    :"gen_ai.response.finish_reasons"
  end

  @doc """
  The unique identifier for the completion.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["chatcmpl-123"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_response_id()
      :"gen_ai.response.id"

  ### Erlang

  ```erlang
  ?'GEN_AI_RESPONSE_ID'.
  'gen_ai.response.id'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_response_id :: :"gen_ai.response.id"
  def gen_ai_response_id do
    :"gen_ai.response.id"
  end

  @doc """
  The name of the LLM a response was generated from.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["gpt-4-0613"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_response_model()
      :"gen_ai.response.model"

  ### Erlang

  ```erlang
  ?'GEN_AI_RESPONSE_MODEL'.
  'gen_ai.response.model'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_response_model :: :"gen_ai.response.model"
  def gen_ai_response_model do
    :"gen_ai.response.model"
  end

  @typedoc """
  The Generative AI product as identified by the client instrumentation.

  ### Enum Values
  * `:openai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OpenAI
  """
  @type gen_ai_system_values() :: %{
          :openai => :openai
        }
  @doc """
  The Generative AI product as identified by the client instrumentation.

  ### Notes

  The actual GenAI product may differ from the one identified by the client. For example, when using OpenAI client libraries to communicate with Mistral, the `gen_ai.system` is set to `openai` based on the instrumentation's best knowledge.

  ### Examples

  ```
  openai
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_system()
      :"gen_ai.system"
      
      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_system_values().openai
      :openai
      
      iex> %{OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_system() => OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_system_values().openai}
      %{:"gen_ai.system" => :openai}

  ### Erlang

  ```erlang
  ?'GEN_AI_SYSTEM'.
  'gen_ai.system'

  ?'GEN_AI_SYSTEM_VALUES_OPENAI'.
  openai

  \#{?GEN_AI_SYSTEM => ?'GEN_AI_SYSTEM_VALUES_OPENAI'}.
  \#{'gen_ai.system' => openai}
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_system :: :"gen_ai.system"
  def gen_ai_system do
    :"gen_ai.system"
  end

  @spec gen_ai_system_values() :: gen_ai_system_values()
  def gen_ai_system_values() do
    %{
      :openai => :openai
    }
  end

  @doc """
  The number of tokens used in the LLM response (completion).
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [180]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_usage_completion_tokens()
      :"gen_ai.usage.completion_tokens"

  ### Erlang

  ```erlang
  ?'GEN_AI_USAGE_COMPLETION_TOKENS'.
  'gen_ai.usage.completion_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_usage_completion_tokens :: :"gen_ai.usage.completion_tokens"
  def gen_ai_usage_completion_tokens do
    :"gen_ai.usage.completion_tokens"
  end

  @doc """
  The number of tokens used in the LLM prompt.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [100]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_usage_prompt_tokens()
      :"gen_ai.usage.prompt_tokens"

  ### Erlang

  ```erlang
  ?'GEN_AI_USAGE_PROMPT_TOKENS'.
  'gen_ai.usage.prompt_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_usage_prompt_tokens :: :"gen_ai.usage.prompt_tokens"
  def gen_ai_usage_prompt_tokens do
    :"gen_ai.usage.prompt_tokens"
  end
end
