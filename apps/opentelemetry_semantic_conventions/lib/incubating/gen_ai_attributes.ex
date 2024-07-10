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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_completion()
      :"gen_ai.completion"

  ### Erlang

  ```erlang
  ?GENAI_COMPLETION.
  'gen_ai.completion'
  ```

  <!-- tabs-close -->
  """
  @spec genai_completion :: :"gen_ai.completion"
  def genai_completion do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_prompt()
      :"gen_ai.prompt"

  ### Erlang

  ```erlang
  ?GENAI_PROMPT.
  'gen_ai.prompt'
  ```

  <!-- tabs-close -->
  """
  @spec genai_prompt :: :"gen_ai.prompt"
  def genai_prompt do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_request_maxtokens()
      :"gen_ai.request.max_tokens"

  ### Erlang

  ```erlang
  ?GENAI_REQUEST_MAXTOKENS.
  'gen_ai.request.max_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec genai_request_maxtokens :: :"gen_ai.request.max_tokens"
  def genai_request_maxtokens do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_request_model()
      :"gen_ai.request.model"

  ### Erlang

  ```erlang
  ?GENAI_REQUEST_MODEL.
  'gen_ai.request.model'
  ```

  <!-- tabs-close -->
  """
  @spec genai_request_model :: :"gen_ai.request.model"
  def genai_request_model do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_request_temperature()
      :"gen_ai.request.temperature"

  ### Erlang

  ```erlang
  ?GENAI_REQUEST_TEMPERATURE.
  'gen_ai.request.temperature'
  ```

  <!-- tabs-close -->
  """
  @spec genai_request_temperature :: :"gen_ai.request.temperature"
  def genai_request_temperature do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_request_topp()
      :"gen_ai.request.top_p"

  ### Erlang

  ```erlang
  ?GENAI_REQUEST_TOPP.
  'gen_ai.request.top_p'
  ```

  <!-- tabs-close -->
  """
  @spec genai_request_topp :: :"gen_ai.request.top_p"
  def genai_request_topp do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_response_finishreasons()
      :"gen_ai.response.finish_reasons"

  ### Erlang

  ```erlang
  ?GENAI_RESPONSE_FINISHREASONS.
  'gen_ai.response.finish_reasons'
  ```

  <!-- tabs-close -->
  """
  @spec genai_response_finishreasons :: :"gen_ai.response.finish_reasons"
  def genai_response_finishreasons do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_response_id()
      :"gen_ai.response.id"

  ### Erlang

  ```erlang
  ?GENAI_RESPONSE_ID.
  'gen_ai.response.id'
  ```

  <!-- tabs-close -->
  """
  @spec genai_response_id :: :"gen_ai.response.id"
  def genai_response_id do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_response_model()
      :"gen_ai.response.model"

  ### Erlang

  ```erlang
  ?GENAI_RESPONSE_MODEL.
  'gen_ai.response.model'
  ```

  <!-- tabs-close -->
  """
  @spec genai_response_model :: :"gen_ai.response.model"
  def genai_response_model do
    :"gen_ai.response.model"
  end

  @typedoc """
  The Generative AI product as identified by the client instrumentation.

  ### Enum Values
  * `:openai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OpenAI
  """
  @type genai_system() :: %{
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_system().openai
      :openai
      
      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_system(:custom_value)
      :custom_value

  ### Erlang

  ```erlang
  ?'genai_system.openai'.
  openai

  ?genai_system.(custom_value).
  custom_value
  ```

  <!-- tabs-close -->
  """
  @spec genai_system() :: genai_system()
  def genai_system() do
    %{
      :openai => :openai
    }
  end

  @spec genai_system(atom() | String.t()) :: atom() | String.t()
  def genai_system(custom_value) do
    custom_value
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_usage_completiontokens()
      :"gen_ai.usage.completion_tokens"

  ### Erlang

  ```erlang
  ?GENAI_USAGE_COMPLETIONTOKENS.
  'gen_ai.usage.completion_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec genai_usage_completiontokens :: :"gen_ai.usage.completion_tokens"
  def genai_usage_completiontokens do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.genai_usage_prompttokens()
      :"gen_ai.usage.prompt_tokens"

  ### Erlang

  ```erlang
  ?GENAI_USAGE_PROMPTTOKENS.
  'gen_ai.usage.prompt_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec genai_usage_prompttokens :: :"gen_ai.usage.prompt_tokens"
  def genai_usage_prompttokens do
    :"gen_ai.usage.prompt_tokens"
  end
end
