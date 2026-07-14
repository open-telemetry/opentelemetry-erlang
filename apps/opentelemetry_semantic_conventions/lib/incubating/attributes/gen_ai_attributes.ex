defmodule OpenTelemetry.SemConv.Incubating.GenAIAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Gen_Ai attributes.
  """

  @doc """
  Free-form description of the GenAI agent provided by the application.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Helps with math problems", "Generates fiction stories"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_agent_description()
      :"gen_ai.agent.description"

  ### Erlang

  ```erlang
  ?GEN_AI_AGENT_DESCRIPTION.
  'gen_ai.agent.description'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_agent_description :: :"gen_ai.agent.description"
  def gen_ai_agent_description do
    :"gen_ai.agent.description"
  end

  @doc """
  The unique identifier of the GenAI agent.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["asst_5j66UpCpwteGg4YSxUnt7lPY"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_agent_id()
      :"gen_ai.agent.id"

  ### Erlang

  ```erlang
  ?GEN_AI_AGENT_ID.
  'gen_ai.agent.id'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_agent_id :: :"gen_ai.agent.id"
  def gen_ai_agent_id do
    :"gen_ai.agent.id"
  end

  @doc """
  Human-readable name of the GenAI agent provided by the application.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Math Tutor", "Fiction Writer"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_agent_name()
      :"gen_ai.agent.name"

  ### Erlang

  ```erlang
  ?GEN_AI_AGENT_NAME.
  'gen_ai.agent.name'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_agent_name :: :"gen_ai.agent.name"
  def gen_ai_agent_name do
    :"gen_ai.agent.name"
  end

  @deprecated """
  Removed, no replacement at this time.
  """
  @spec gen_ai_completion :: :"gen_ai.completion"
  def gen_ai_completion do
    :"gen_ai.completion"
  end

  @typedoc """
  Deprecated, use `gen_ai.output.type`.


  ### Enum Values
  * `:text` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Text response format
  * `:json_object` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - JSON object response format
  * `:json_schema` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - JSON schema response format
  """
  @type gen_ai_openai_request_response_format_values() :: %{
          :text => :text,
          :json_object => :json_object,
          :json_schema => :json_schema
        }
  @deprecated """
  Replaced by `gen_ai.output.type`.
  """
  @spec gen_ai_openai_request_response_format :: :"gen_ai.openai.request.response_format"
  def gen_ai_openai_request_response_format do
    :"gen_ai.openai.request.response_format"
  end

  @spec gen_ai_openai_request_response_format_values() ::
          gen_ai_openai_request_response_format_values()
  def gen_ai_openai_request_response_format_values() do
    %{
      :text => :text,
      :json_object => :json_object,
      :json_schema => :json_schema
    }
  end

  @deprecated """
  Replaced by `gen_ai.request.seed` attribute.
  """
  @spec gen_ai_openai_request_seed :: :"gen_ai.openai.request.seed"
  def gen_ai_openai_request_seed do
    :"gen_ai.openai.request.seed"
  end

  @typedoc """
  The service tier requested. May be a specific tier, default, or auto.

  ### Enum Values
  * `:auto` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The system will utilize scale tier credits until they are exhausted.
  * `:default` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The system will utilize the default scale tier.
  """
  @type gen_ai_openai_request_service_tier_values() :: %{
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_openai_request_service_tier()
      :"gen_ai.openai.request.service_tier"

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_openai_request_service_tier_values().auto
      :auto

      iex> %{OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_openai_request_service_tier() => OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_openai_request_service_tier_values().auto}
      %{:"gen_ai.openai.request.service_tier" => :auto}

  ### Erlang

  ```erlang
  ?GEN_AI_OPENAI_REQUEST_SERVICE_TIER.
  'gen_ai.openai.request.service_tier'

  ?GEN_AI_OPENAI_REQUEST_SERVICE_TIER_VALUES_AUTO.
  'auto'

  \#{?GEN_AI_OPENAI_REQUEST_SERVICE_TIER => ?GEN_AI_OPENAI_REQUEST_SERVICE_TIER_VALUES_AUTO}.
  \#{'gen_ai.openai.request.service_tier' => 'auto'}
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_openai_request_service_tier :: :"gen_ai.openai.request.service_tier"
  def gen_ai_openai_request_service_tier do
    :"gen_ai.openai.request.service_tier"
  end

  @spec gen_ai_openai_request_service_tier_values() :: gen_ai_openai_request_service_tier_values()
  def gen_ai_openai_request_service_tier_values() do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_openai_response_service_tier()
      :"gen_ai.openai.response.service_tier"

  ### Erlang

  ```erlang
  ?GEN_AI_OPENAI_RESPONSE_SERVICE_TIER.
  'gen_ai.openai.response.service_tier'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_openai_response_service_tier :: :"gen_ai.openai.response.service_tier"
  def gen_ai_openai_response_service_tier do
    :"gen_ai.openai.response.service_tier"
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_openai_response_system_fingerprint()
      :"gen_ai.openai.response.system_fingerprint"

  ### Erlang

  ```erlang
  ?GEN_AI_OPENAI_RESPONSE_SYSTEM_FINGERPRINT.
  'gen_ai.openai.response.system_fingerprint'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_openai_response_system_fingerprint :: :"gen_ai.openai.response.system_fingerprint"
  def gen_ai_openai_response_system_fingerprint do
    :"gen_ai.openai.response.system_fingerprint"
  end

  @typedoc """
  The name of the operation being performed.

  ### Enum Values
  * `:chat` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Chat completion operation such as [OpenAI Chat API](https://platform.openai.com/docs/api-reference/chat)
  * `:generate_content` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Multimodal content generation operation such as [Gemini Generate Content](https://ai.google.dev/api/generate-content)
  * `:text_completion` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Text completions operation such as [OpenAI Completions API (Legacy)](https://platform.openai.com/docs/api-reference/completions)
  * `:embeddings` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Embeddings operation such as [OpenAI Create embeddings API](https://platform.openai.com/docs/api-reference/embeddings/create)
  * `:create_agent` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Create GenAI agent
  * `:invoke_agent` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Invoke GenAI agent
  * `:execute_tool` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Execute a tool
  """
  @type gen_ai_operation_name_values() :: %{
          :chat => :chat,
          :generate_content => :generate_content,
          :text_completion => :text_completion,
          :embeddings => :embeddings,
          :create_agent => :create_agent,
          :invoke_agent => :invoke_agent,
          :execute_tool => :execute_tool
        }
  @doc """
  The name of the operation being performed.

  ### Notes

  If one of the predefined values applies, but specific system uses a different name it's RECOMMENDED to document it in the semantic conventions for specific GenAI system and use system-specific name in the instrumentation. If a different name is not documented, instrumentation libraries **SHOULD** use applicable predefined value.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_operation_name()
      :"gen_ai.operation.name"

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_operation_name_values().chat
      :chat

      iex> %{OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_operation_name() => OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_operation_name_values().chat}
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
      :generate_content => :generate_content,
      :text_completion => :text_completion,
      :embeddings => :embeddings,
      :create_agent => :create_agent,
      :invoke_agent => :invoke_agent,
      :execute_tool => :execute_tool
    }
  end

  @typedoc """
  Represents the content type requested by the client.

  ### Enum Values
  * `:text` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Plain text
  * `:json` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - JSON object with known or unknown schema
  * `:image` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Image
  * `:speech` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Speech
  """
  @type gen_ai_output_type_values() :: %{
          :text => :text,
          :json => :json,
          :image => :image,
          :speech => :speech
        }
  @doc """
  Represents the content type requested by the client.

  ### Notes

  This attribute **SHOULD** be used when the client requests output of a specific type. The model may return zero or more outputs of this type.
  This attribute specifies the output modality and not the actual output format. For example, if an image is requested, the actual output could be a URL pointing to an image file.
  Additional output format details may be recorded in the future in the `gen_ai.output.{type}.*` attributes.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_output_type()
      :"gen_ai.output.type"

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_output_type_values().text
      :text

      iex> %{OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_output_type() => OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_output_type_values().text}
      %{:"gen_ai.output.type" => :text}

  ### Erlang

  ```erlang
  ?GEN_AI_OUTPUT_TYPE.
  'gen_ai.output.type'

  ?GEN_AI_OUTPUT_TYPE_VALUES_TEXT.
  'text'

  \#{?GEN_AI_OUTPUT_TYPE => ?GEN_AI_OUTPUT_TYPE_VALUES_TEXT}.
  \#{'gen_ai.output.type' => 'text'}
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_output_type :: :"gen_ai.output.type"
  def gen_ai_output_type do
    :"gen_ai.output.type"
  end

  @spec gen_ai_output_type_values() :: gen_ai_output_type_values()
  def gen_ai_output_type_values() do
    %{
      :text => :text,
      :json => :json,
      :image => :image,
      :speech => :speech
    }
  end

  @deprecated """
  Removed, no replacement at this time.
  """
  @spec gen_ai_prompt :: :"gen_ai.prompt"
  def gen_ai_prompt do
    :"gen_ai.prompt"
  end

  @doc """
  The target number of candidate completions to return.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [3]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_choice_count()
      :"gen_ai.request.choice.count"

  ### Erlang

  ```erlang
  ?GEN_AI_REQUEST_CHOICE_COUNT.
  'gen_ai.request.choice.count'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_choice_count :: :"gen_ai.request.choice.count"
  def gen_ai_request_choice_count do
    :"gen_ai.request.choice.count"
  end

  @doc """
  The encoding formats requested in an embeddings operation, if specified.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Notes

  In some GenAI systems the encoding formats are called embedding types. Also, some GenAI systems only accept a single format per request.

  ### Examples

  ```
  [["base64"], ["float", "binary"]]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_encoding_formats()
      :"gen_ai.request.encoding_formats"

  ### Erlang

  ```erlang
  ?GEN_AI_REQUEST_ENCODING_FORMATS.
  'gen_ai.request.encoding_formats'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_encoding_formats :: :"gen_ai.request.encoding_formats"
  def gen_ai_request_encoding_formats do
    :"gen_ai.request.encoding_formats"
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_frequency_penalty()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_max_tokens()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_model()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_presence_penalty()
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
  Requests with same seed value more likely to return same result.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [100]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_seed()
      :"gen_ai.request.seed"

  ### Erlang

  ```erlang
  ?GEN_AI_REQUEST_SEED.
  'gen_ai.request.seed'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_request_seed :: :"gen_ai.request.seed"
  def gen_ai_request_seed do
    :"gen_ai.request.seed"
  end

  @doc """
  List of sequences that the model will use to stop generating further tokens.
  ### Value type

  Value must be of type `[atom() | String.t()]`.
  ### Examples

  ```
  [["forest", "lived"]]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_stop_sequences()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_temperature()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_top_k()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_request_top_p()
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
  [["stop"], ["stop", "length"]]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_response_finish_reasons()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_response_id()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_response_model()
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
  * `:"gcp.gen_ai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Any Google generative AI endpoint
  * `:"gcp.vertex_ai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Vertex AI
  * `:"gcp.gemini"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Gemini
  * `:vertex_ai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Vertex AI
  * `:gemini` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Gemini
  * `:anthropic` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Anthropic
  * `:cohere` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Cohere
  * `:"az.ai.inference"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure AI Inference
  * `:"az.ai.openai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure OpenAI
  * `:"ibm.watsonx.ai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - IBM Watsonx AI
  * `:"aws.bedrock"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - AWS Bedrock
  * `:perplexity` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Perplexity
  * `:xai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - xAI
  * `:deepseek` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - DeepSeek
  * `:groq` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Groq
  * `:mistral_ai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Mistral AI
  """
  @type gen_ai_system_values() :: %{
          :openai => :openai,
          :"gcp.gen_ai" => :"gcp.gen_ai",
          :"gcp.vertex_ai" => :"gcp.vertex_ai",
          :"gcp.gemini" => :"gcp.gemini",
          :vertex_ai => :vertex_ai,
          :gemini => :gemini,
          :anthropic => :anthropic,
          :cohere => :cohere,
          :"az.ai.inference" => :"az.ai.inference",
          :"az.ai.openai" => :"az.ai.openai",
          :"ibm.watsonx.ai" => :"ibm.watsonx.ai",
          :"aws.bedrock" => :"aws.bedrock",
          :perplexity => :perplexity,
          :xai => :xai,
          :deepseek => :deepseek,
          :groq => :groq,
          :mistral_ai => :mistral_ai
        }
  @doc """
  The Generative AI product as identified by the client or server instrumentation.

  ### Notes

  The `gen_ai.system` describes a family of GenAI models with specific model identified
  by `gen_ai.request.model` and `gen_ai.response.model` attributes.

  The actual GenAI product may differ from the one identified by the client.
  Multiple systems, including Azure OpenAI and Gemini, are accessible by OpenAI client
  libraries. In such cases, the `gen_ai.system` is set to `openai` based on the
  instrumentation's best knowledge, instead of the actual system. The `server.address`
  attribute may help identify the actual system in use for `openai`.

  For custom model, a custom friendly name **SHOULD** be used.
  If none of these options apply, the `gen_ai.system` **SHOULD** be set to `_OTHER`.

  ### Examples

  ```
  openai
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_system()
      :"gen_ai.system"

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_system_values().openai
      :openai

      iex> %{OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_system() => OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_system_values().openai}
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
      :"gcp.gen_ai" => :"gcp.gen_ai",
      :"gcp.vertex_ai" => :"gcp.vertex_ai",
      :"gcp.gemini" => :"gcp.gemini",
      :vertex_ai => :vertex_ai,
      :gemini => :gemini,
      :anthropic => :anthropic,
      :cohere => :cohere,
      :"az.ai.inference" => :"az.ai.inference",
      :"az.ai.openai" => :"az.ai.openai",
      :"ibm.watsonx.ai" => :"ibm.watsonx.ai",
      :"aws.bedrock" => :"aws.bedrock",
      :perplexity => :perplexity,
      :xai => :xai,
      :deepseek => :deepseek,
      :groq => :groq,
      :mistral_ai => :mistral_ai
    }
  end

  @typedoc """
  The type of token being counted.

  ### Enum Values
  * `:input` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Input tokens (prompt, input, etc.)
  * `:completion` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Output tokens (completion, response, etc.)
  * `:output` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Output tokens (completion, response, etc.)
  """
  @type gen_ai_token_type_values() :: %{
          :input => :input,
          :completion => :output,
          :output => :output
        }
  @doc """
  The type of token being counted.

  ### Examples

  ```
  ["input", "output"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_token_type()
      :"gen_ai.token.type"

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_token_type_values().input
      :input

      iex> %{OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_token_type() => OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_token_type_values().input}
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
      :completion => :output,
      :output => :output
    }
  end

  @doc """
  The tool call identifier.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["call_mszuSIzqtI65i1wAUOE8w5H4"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_tool_call_id()
      :"gen_ai.tool.call.id"

  ### Erlang

  ```erlang
  ?GEN_AI_TOOL_CALL_ID.
  'gen_ai.tool.call.id'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_tool_call_id :: :"gen_ai.tool.call.id"
  def gen_ai_tool_call_id do
    :"gen_ai.tool.call.id"
  end

  @doc """
  The tool description.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Multiply two numbers"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_tool_description()
      :"gen_ai.tool.description"

  ### Erlang

  ```erlang
  ?GEN_AI_TOOL_DESCRIPTION.
  'gen_ai.tool.description'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_tool_description :: :"gen_ai.tool.description"
  def gen_ai_tool_description do
    :"gen_ai.tool.description"
  end

  @doc """
  Name of the tool utilized by the agent.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Flights"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_tool_name()
      :"gen_ai.tool.name"

  ### Erlang

  ```erlang
  ?GEN_AI_TOOL_NAME.
  'gen_ai.tool.name'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_tool_name :: :"gen_ai.tool.name"
  def gen_ai_tool_name do
    :"gen_ai.tool.name"
  end

  @doc """
  Type of the tool utilized by the agent
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Extension: A tool executed on the agent-side to directly call external APIs, bridging the gap between the agent and real-world systems.
    Agent-side operations involve actions that are performed by the agent on the server or within the agent's controlled environment.
  Function: A tool executed on the client-side, where the agent generates parameters for a predefined function, and the client executes the logic.
    Client-side operations are actions taken on the user's end or within the client application.
  Datastore: A tool used by the agent to access and query structured or unstructured external data for retrieval-augmented tasks or knowledge updates.

  ### Examples

  ```
  ["function", "extension", "datastore"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_tool_type()
      :"gen_ai.tool.type"

  ### Erlang

  ```erlang
  ?GEN_AI_TOOL_TYPE.
  'gen_ai.tool.type'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_tool_type :: :"gen_ai.tool.type"
  def gen_ai_tool_type do
    :"gen_ai.tool.type"
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_usage_input_tokens()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAIAttributes.gen_ai_usage_output_tokens()
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
