defmodule OpenTelemetry.SemConv.Incubating.GenAiAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Gen_Ai attributes.
  """

  @doc """
  The full response received from the GenAI model.
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
  ?GEN_AI_COMPLETION.
  'gen_ai.completion'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_completion :: :"gen_ai.completion"
  def gen_ai_completion do
    :"gen_ai.completion"
  end

  @typedoc """
  The name of the operation being performed.

  ### Enum Values
  * `:chat` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Chat completion operation such as [OpenAI Chat API](https://platform.openai.com/docs/api-reference/chat)
  * `:text_completion` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Text completions operation such as [OpenAI Completions API (Legacy)](https://platform.openai.com/docs/api-reference/completions)
  """
  @type gen_ai_operation_name_values() :: %{
          :chat => :chat,
          :text_completion => :text_completion
        }
  @doc """
  The name of the operation being performed.

  ### Notes

  If one of the predefined values applies, but specific system uses a different name it's RECOMMENDED to document it in the semantic conventions for specific GenAI system and use system-specific name in the instrumentation. If a different name is not documented, instrumentation libraries **SHOULD** use applicable predefined value.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_operation_name()
      :"gen_ai.operation.name"

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_operation_name_values().chat
      :chat

      iex> %{OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_operation_name() => OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_operation_name_values().chat}
      %{:"gen_ai.operation.name" => :chat}

  ### Erlang

  ```erlang
  ?GEN_AI_OPERATION_NAME.
  'gen_ai.operation.name'

  ?GEN_AI_OPERATION_NAME_VALUES_CHAT.
  'chat'

  \#{?GEN_AI_OPERATION_NAME => ?GEN_AI_OPERATION_NAME_VALUES_CHAT}.
  \#{'gen_ai.operation.name' => 'chat'}
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_operation_name :: :"gen_ai.operation.name"
  def gen_ai_operation_name do
    :"gen_ai.operation.name"
  end

  @spec gen_ai_operation_name_values() :: gen_ai_operation_name_values()
  def gen_ai_operation_name_values() do
    %{
      :chat => :chat,
      :text_completion => :text_completion
    }
  end

  @doc """
  The full prompt sent to the GenAI model.
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
  ?GEN_AI_PROMPT.
  'gen_ai.prompt'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_prompt :: :"gen_ai.prompt"
  def gen_ai_prompt do
    :"gen_ai.prompt"
  end

  @doc """
  The frequency penalty setting for the GenAI request.
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [0.1]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_frequency_penalty()
      :"gen_ai.request.frequency_penalty"

  ### Erlang

  ```erlang
  ?GEN_AI_REQUEST_FREQUENCY_PENALTY.
  'gen_ai.request.frequency_penalty'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_frequency_penalty :: :"gen_ai.request.frequency_penalty"
  def gen_ai_request_frequency_penalty do
    :"gen_ai.request.frequency_penalty"
  end

  @doc """
  The maximum number of tokens the model generates for a request.
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
  ?GEN_AI_REQUEST_MAX_TOKENS.
  'gen_ai.request.max_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_max_tokens :: :"gen_ai.request.max_tokens"
  def gen_ai_request_max_tokens do
    :"gen_ai.request.max_tokens"
  end

  @doc """
  The name of the GenAI model a request is being made to.
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
  ?GEN_AI_REQUEST_MODEL.
  'gen_ai.request.model'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_model :: :"gen_ai.request.model"
  def gen_ai_request_model do
    :"gen_ai.request.model"
  end

  @doc """
  The presence penalty setting for the GenAI request.
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [0.1]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_presence_penalty()
      :"gen_ai.request.presence_penalty"

  ### Erlang

  ```erlang
  ?GEN_AI_REQUEST_PRESENCE_PENALTY.
  'gen_ai.request.presence_penalty'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_presence_penalty :: :"gen_ai.request.presence_penalty"
  def gen_ai_request_presence_penalty do
    :"gen_ai.request.presence_penalty"
  end

  @doc """
  List of sequences that the model will use to stop generating further tokens.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  ["forest", "lived"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_stop_sequences()
      :"gen_ai.request.stop_sequences"

  ### Erlang

  ```erlang
  ?GEN_AI_REQUEST_STOP_SEQUENCES.
  'gen_ai.request.stop_sequences'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_stop_sequences :: :"gen_ai.request.stop_sequences"
  def gen_ai_request_stop_sequences do
    :"gen_ai.request.stop_sequences"
  end

  @doc """
  The temperature setting for the GenAI request.
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
  ?GEN_AI_REQUEST_TEMPERATURE.
  'gen_ai.request.temperature'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_temperature :: :"gen_ai.request.temperature"
  def gen_ai_request_temperature do
    :"gen_ai.request.temperature"
  end

  @doc """
  The top_k sampling setting for the GenAI request.
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [1.0]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_top_k()
      :"gen_ai.request.top_k"

  ### Erlang

  ```erlang
  ?GEN_AI_REQUEST_TOP_K.
  'gen_ai.request.top_k'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_top_k :: :"gen_ai.request.top_k"
  def gen_ai_request_top_k do
    :"gen_ai.request.top_k"
  end

  @doc """
  The top_p sampling setting for the GenAI request.
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
  ?GEN_AI_REQUEST_TOP_P.
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
  ?GEN_AI_RESPONSE_FINISH_REASONS.
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
  ?GEN_AI_RESPONSE_ID.
  'gen_ai.response.id'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_response_id :: :"gen_ai.response.id"
  def gen_ai_response_id do
    :"gen_ai.response.id"
  end

  @doc """
  The name of the model that generated the response.
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
  ?GEN_AI_RESPONSE_MODEL.
  'gen_ai.response.model'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_response_model :: :"gen_ai.response.model"
  def gen_ai_response_model do
    :"gen_ai.response.model"
  end

  @typedoc """
  The Generative AI product as identified by the client or server instrumentation.

  ### Enum Values
  * `:openai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OpenAI
  * `:vertex_ai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Vertex AI
  * `:anthropic` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Anthropic
  * `:cohere` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Cohere
  """
  @type gen_ai_system_values() :: %{
          :openai => :openai,
          :vertex_ai => :vertex_ai,
          :anthropic => :anthropic,
          :cohere => :cohere
        }
  @doc """
  The Generative AI product as identified by the client or server instrumentation.

  ### Notes

  The `gen_ai.system` describes a family of GenAI models with specific model identified
  by `gen_ai.request.model` and `gen_ai.response.model` attributes.

  The actual GenAI product may differ from the one identified by the client.
  For example, when using OpenAI client libraries to communicate with Mistral, the `gen_ai.system`
  is set to `openai` based on the instrumentation's best knowledge.

  For custom model, a custom friendly name **SHOULD** be used.
  If none of these options apply, the `gen_ai.system` **SHOULD** be set to `_OTHER`.

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
  ?GEN_AI_SYSTEM.
  'gen_ai.system'

  ?GEN_AI_SYSTEM_VALUES_OPENAI.
  'openai'

  \#{?GEN_AI_SYSTEM => ?GEN_AI_SYSTEM_VALUES_OPENAI}.
  \#{'gen_ai.system' => 'openai'}
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
      :openai => :openai,
      :vertex_ai => :vertex_ai,
      :anthropic => :anthropic,
      :cohere => :cohere
    }
  end

  @typedoc """
  The type of token being counted.

  ### Enum Values
  * `:input` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Input tokens (prompt, input, etc.)
  * `:completion` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Output tokens (completion, response, etc.)
  """
  @type gen_ai_token_type_values() :: %{
          :input => :input,
          :completion => :output
        }
  @doc """
  The type of token being counted.

  ### Examples

  ```
  ["input", "output"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_token_type()
      :"gen_ai.token.type"

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_token_type_values().input
      :input

      iex> %{OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_token_type() => OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_token_type_values().input}
      %{:"gen_ai.token.type" => :input}

  ### Erlang

  ```erlang
  ?GEN_AI_TOKEN_TYPE.
  'gen_ai.token.type'

  ?GEN_AI_TOKEN_TYPE_VALUES_INPUT.
  'input'

  \#{?GEN_AI_TOKEN_TYPE => ?GEN_AI_TOKEN_TYPE_VALUES_INPUT}.
  \#{'gen_ai.token.type' => 'input'}
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_token_type :: :"gen_ai.token.type"
  def gen_ai_token_type do
    :"gen_ai.token.type"
  end

  @spec gen_ai_token_type_values() :: gen_ai_token_type_values()
  def gen_ai_token_type_values() do
    %{
      :input => :input,
      :completion => :output
    }
  end

  @deprecated """
  Replaced by `gen_ai.usage.output_tokens` attribute.
  """
  @spec gen_ai_usage_completion_tokens :: :"gen_ai.usage.completion_tokens"
  def gen_ai_usage_completion_tokens do
    :"gen_ai.usage.completion_tokens"
  end

  @doc """
  The number of tokens used in the GenAI input (prompt).
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [100]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_usage_input_tokens()
      :"gen_ai.usage.input_tokens"

  ### Erlang

  ```erlang
  ?GEN_AI_USAGE_INPUT_TOKENS.
  'gen_ai.usage.input_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_usage_input_tokens :: :"gen_ai.usage.input_tokens"
  def gen_ai_usage_input_tokens do
    :"gen_ai.usage.input_tokens"
  end

  @doc """
  The number of tokens used in the GenAI response (completion).
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [180]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_usage_output_tokens()
      :"gen_ai.usage.output_tokens"

  ### Erlang

  ```erlang
  ?GEN_AI_USAGE_OUTPUT_TOKENS.
  'gen_ai.usage.output_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_usage_output_tokens :: :"gen_ai.usage.output_tokens"
  def gen_ai_usage_output_tokens do
    :"gen_ai.usage.output_tokens"
  end

  @deprecated """
  Replaced by `gen_ai.usage.input_tokens` attribute.
  """
  @spec gen_ai_usage_prompt_tokens :: :"gen_ai.usage.prompt_tokens"
  def gen_ai_usage_prompt_tokens do
    :"gen_ai.usage.prompt_tokens"
  end
end
