defmodule OpenTelemetry.SemConv.Incubating.GenAiAttributes do
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_agent_description()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_agent_id()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_agent_name()
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

  @doc """
  The version of the GenAI agent.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["1.0.0", "2025-05-01"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_agent_version()
      :"gen_ai.agent.version"

  ### Erlang

  ```erlang
  ?GEN_AI_AGENT_VERSION.
  'gen_ai.agent.version'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_agent_version :: :"gen_ai.agent.version"
  def gen_ai_agent_version do
    :"gen_ai.agent.version"
  end

  @deprecated """
  Removed, no replacement at this time.
  """
  @spec gen_ai_completion :: :"gen_ai.completion"
  def gen_ai_completion do
    :"gen_ai.completion"
  end

  @doc """
  The unique identifier for a conversation (session, thread), used to store and correlate messages within this conversation.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["conv_5j66UpCpwteGg4YSxUnt7lPY"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_conversation_id()
      :"gen_ai.conversation.id"

  ### Erlang

  ```erlang
  ?GEN_AI_CONVERSATION_ID.
  'gen_ai.conversation.id'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_conversation_id :: :"gen_ai.conversation.id"
  def gen_ai_conversation_id do
    :"gen_ai.conversation.id"
  end

  @doc """
  The data source identifier.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  Data sources are used by AI agents and RAG applications to store grounding data. A data source may be an external database, object store, document collection, website, or any other storage system used by the GenAI agent or application. The `gen_ai.data_source.id` **SHOULD** match the identifier used by the GenAI system rather than a name specific to the external storage, such as a database or object store. Semantic conventions referencing `gen_ai.data_source.id` **MAY** also leverage additional attributes, such as `db.*`, to further identify and describe the data source.

  ### Examples

  ```
  ["H7STPQYOND"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_data_source_id()
      :"gen_ai.data_source.id"

  ### Erlang

  ```erlang
  ?GEN_AI_DATA_SOURCE_ID.
  'gen_ai.data_source.id'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_data_source_id :: :"gen_ai.data_source.id"
  def gen_ai_data_source_id do
    :"gen_ai.data_source.id"
  end

  @doc """
  The number of dimensions the resulting output embeddings should have.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [512, 1024]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_embeddings_dimension_count()
      :"gen_ai.embeddings.dimension.count"

  ### Erlang

  ```erlang
  ?GEN_AI_EMBEDDINGS_DIMENSION_COUNT.
  'gen_ai.embeddings.dimension.count'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_embeddings_dimension_count :: :"gen_ai.embeddings.dimension.count"
  def gen_ai_embeddings_dimension_count do
    :"gen_ai.embeddings.dimension.count"
  end

  @doc """
  A free-form explanation for the assigned score provided by the evaluator.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["The response is factually accurate but lacks sufficient detail to fully address the question."]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_evaluation_explanation()
      :"gen_ai.evaluation.explanation"

  ### Erlang

  ```erlang
  ?GEN_AI_EVALUATION_EXPLANATION.
  'gen_ai.evaluation.explanation'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_evaluation_explanation :: :"gen_ai.evaluation.explanation"
  def gen_ai_evaluation_explanation do
    :"gen_ai.evaluation.explanation"
  end

  @doc """
  The name of the evaluation metric used for the GenAI response.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["Relevance", "IntentResolution"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_evaluation_name()
      :"gen_ai.evaluation.name"

  ### Erlang

  ```erlang
  ?GEN_AI_EVALUATION_NAME.
  'gen_ai.evaluation.name'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_evaluation_name :: :"gen_ai.evaluation.name"
  def gen_ai_evaluation_name do
    :"gen_ai.evaluation.name"
  end

  @doc """
  Human readable label for evaluation.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  This attribute provides a human-readable interpretation of the evaluation score produced by an evaluator. For example, a score value of 1 could mean "relevant" in one evaluation system and "not relevant" in another, depending on the scoring range and evaluator. The label **SHOULD** have low cardinality. Possible values depend on the evaluation metric and evaluator used; implementations **SHOULD** document the possible values.

  ### Examples

  ```
  ["relevant", "not_relevant", "correct", "incorrect", "pass", "fail"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_evaluation_score_label()
      :"gen_ai.evaluation.score.label"

  ### Erlang

  ```erlang
  ?GEN_AI_EVALUATION_SCORE_LABEL.
  'gen_ai.evaluation.score.label'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_evaluation_score_label :: :"gen_ai.evaluation.score.label"
  def gen_ai_evaluation_score_label do
    :"gen_ai.evaluation.score.label"
  end

  @doc """
  The evaluation score returned by the evaluator.
  ### Value type

  Value must be of type `float()`.
  ### Examples

  ```
  [4.0]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_evaluation_score_value()
      :"gen_ai.evaluation.score.value"

  ### Erlang

  ```erlang
  ?GEN_AI_EVALUATION_SCORE_VALUE.
  'gen_ai.evaluation.score.value'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_evaluation_score_value :: :"gen_ai.evaluation.score.value"
  def gen_ai_evaluation_score_value do
    :"gen_ai.evaluation.score.value"
  end

  @doc """
  The chat history provided to the model as an input.


  ### Notes

  Instrumentations **MUST** follow [Input messages JSON schema](/docs/gen-ai/gen-ai-input-messages.json).
  When the attribute is recorded on events, it **MUST** be recorded in structured
  form. When recorded on spans, it **MAY** be recorded as a JSON string if structured
  format is not supported and **SHOULD** be recorded in structured form otherwise.

  Messages **MUST** be provided in the order they were sent to the model.
  Instrumentations **MAY** provide a way for users to filter or truncate
  input messages.

  > [!Warning]
  > This attribute is likely to contain sensitive information including user/PII data.

  See [Recording content on attributes](/docs/gen-ai/gen-ai-spans.md#recording-content-on-attributes)
  section for more details.

  ### Examples

  ```
  ["[\n  {\n    \"role\": \"user\",\n    \"parts\": [\n      {\n        \"type\": \"text\",\n        \"content\": \"Weather in Paris?\"\n      }\n    ]\n  },\n  {\n    \"role\": \"assistant\",\n    \"parts\": [\n      {\n        \"type\": \"tool_call\",\n        \"id\": \"call_VSPygqKTWdrhaFErNvMV18Yl\",\n        \"name\": \"get_weather\",\n        \"arguments\": {\n          \"location\": \"Paris\"\n        }\n      }\n    ]\n  },\n  {\n    \"role\": \"tool\",\n    \"parts\": [\n      {\n        \"type\": \"tool_call_response\",\n        \"id\": \" call_VSPygqKTWdrhaFErNvMV18Yl\",\n        \"result\": \"rainy, 57°F\"\n      }\n    ]\n  }\n]\n"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_input_messages()
      :"gen_ai.input.messages"

  ### Erlang

  ```erlang
  ?GEN_AI_INPUT_MESSAGES.
  'gen_ai.input.messages'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_input_messages :: :"gen_ai.input.messages"
  def gen_ai_input_messages do
    :"gen_ai.input.messages"
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
  Replaced by `gen_ai.request.seed`.
  """
  @spec gen_ai_openai_request_seed :: :"gen_ai.openai.request.seed"
  def gen_ai_openai_request_seed do
    :"gen_ai.openai.request.seed"
  end

  @typedoc """
  Deprecated, use `openai.request.service_tier`.

  ### Enum Values
  * `:auto` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The system will utilize scale tier credits until they are exhausted.
  * `:default` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - The system will utilize the default scale tier.
  """
  @type gen_ai_openai_request_service_tier_values() :: %{
          :auto => :auto,
          :default => :default
        }
  @deprecated """
  Replaced by `openai.request.service_tier`.
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

  @deprecated """
  Replaced by `openai.response.service_tier`.
  """
  @spec gen_ai_openai_response_service_tier :: :"gen_ai.openai.response.service_tier"
  def gen_ai_openai_response_service_tier do
    :"gen_ai.openai.response.service_tier"
  end

  @deprecated """
  Replaced by `openai.response.system_fingerprint`.
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
  * `:retrieval` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Retrieval operation such as [OpenAI Search Vector Store API](https://platform.openai.com/docs/api-reference/vector-stores/search)
  * `:create_agent` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Create GenAI agent
  * `:invoke_agent` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Invoke GenAI agent
  * `:execute_tool` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Execute a tool
  """
  @type gen_ai_operation_name_values() :: %{
          :chat => :chat,
          :generate_content => :generate_content,
          :text_completion => :text_completion,
          :embeddings => :embeddings,
          :retrieval => :retrieval,
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
      :generate_content => :generate_content,
      :text_completion => :text_completion,
      :embeddings => :embeddings,
      :retrieval => :retrieval,
      :create_agent => :create_agent,
      :invoke_agent => :invoke_agent,
      :execute_tool => :execute_tool
    }
  end

  @doc """
  Messages returned by the model where each message represents a specific model response (choice, candidate).

  ### Notes

  Instrumentations **MUST** follow [Output messages JSON schema](/docs/gen-ai/gen-ai-output-messages.json)

  Each message represents a single output choice/candidate generated by
  the model. Each message corresponds to exactly one generation
  (choice/candidate) and vice versa - one choice cannot be split across
  multiple messages or one message cannot contain parts from multiple choices.

  When the attribute is recorded on events, it **MUST** be recorded in structured
  form. When recorded on spans, it **MAY** be recorded as a JSON string if structured
  format is not supported and **SHOULD** be recorded in structured form otherwise.

  Instrumentations **MAY** provide a way for users to filter or truncate
  output messages.

  > [!Warning]
  > This attribute is likely to contain sensitive information including user/PII data.

  See [Recording content on attributes](/docs/gen-ai/gen-ai-spans.md#recording-content-on-attributes)
  section for more details.

  ### Examples

  ```
  ["[\n  {\n    \"role\": \"assistant\",\n    \"parts\": [\n      {\n        \"type\": \"text\",\n        \"content\": \"The weather in Paris is currently rainy with a temperature of 57°F.\"\n      }\n    ],\n    \"finish_reason\": \"stop\"\n  }\n]\n"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_output_messages()
      :"gen_ai.output.messages"

  ### Erlang

  ```erlang
  ?GEN_AI_OUTPUT_MESSAGES.
  'gen_ai.output.messages'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_output_messages :: :"gen_ai.output.messages"
  def gen_ai_output_messages do
    :"gen_ai.output.messages"
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_output_type()
      :"gen_ai.output.type"

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_output_type_values().text
      :text

      iex> %{OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_output_type() => OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_output_type_values().text}
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
  The name of the prompt that uniquely identifies it.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["analyze-code"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_prompt_name()
      :"gen_ai.prompt.name"

  ### Erlang

  ```erlang
  ?GEN_AI_PROMPT_NAME.
  'gen_ai.prompt.name'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_prompt_name :: :"gen_ai.prompt.name"
  def gen_ai_prompt_name do
    :"gen_ai.prompt.name"
  end

  @typedoc """
  The Generative AI provider as identified by the client or server instrumentation.

  ### Enum Values
  * `:openai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [OpenAI](https://openai.com/)
  * `:"gcp.gen_ai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Any Google generative AI endpoint
  * `:"gcp.vertex_ai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Vertex AI](https://cloud.google.com/vertex-ai)
  * `:"gcp.gemini"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Gemini](https://cloud.google.com/products/gemini)
  * `:anthropic` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Anthropic](https://www.anthropic.com/)
  * `:cohere` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Cohere](https://cohere.com/)
  * `:"azure.ai.inference"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure AI Inference
  * `:"azure.ai.openai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Azure OpenAI](https://azure.microsoft.com/products/ai-services/openai-service/)
  * `:"ibm.watsonx.ai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [IBM Watsonx AI](https://www.ibm.com/products/watsonx-ai)
  * `:"aws.bedrock"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [AWS Bedrock](https://aws.amazon.com/bedrock)
  * `:perplexity` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Perplexity](https://www.perplexity.ai/)
  * `:x_ai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [xAI](https://x.ai/)
  * `:deepseek` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [DeepSeek](https://www.deepseek.com/)
  * `:groq` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Groq](https://groq.com/)
  * `:mistral_ai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Mistral AI](https://mistral.ai/)
  """
  @type gen_ai_provider_name_values() :: %{
          :openai => :openai,
          :"gcp.gen_ai" => :"gcp.gen_ai",
          :"gcp.vertex_ai" => :"gcp.vertex_ai",
          :"gcp.gemini" => :"gcp.gemini",
          :anthropic => :anthropic,
          :cohere => :cohere,
          :"azure.ai.inference" => :"azure.ai.inference",
          :"azure.ai.openai" => :"azure.ai.openai",
          :"ibm.watsonx.ai" => :"ibm.watsonx.ai",
          :"aws.bedrock" => :"aws.bedrock",
          :perplexity => :perplexity,
          :x_ai => :x_ai,
          :deepseek => :deepseek,
          :groq => :groq,
          :mistral_ai => :mistral_ai
        }
  @doc """
  The Generative AI provider as identified by the client or server instrumentation.

  ### Notes

  The attribute **SHOULD** be set based on the instrumentation's best
  knowledge and may differ from the actual model provider.

  Multiple providers, including Azure OpenAI, Gemini, and AI hosting platforms
  are accessible using the OpenAI REST API and corresponding client libraries,
  but may proxy or host models from different providers.

  The `gen_ai.request.model`, `gen_ai.response.model`, and `server.address`
  attributes may help identify the actual system in use.

  The `gen_ai.provider.name` attribute acts as a discriminator that
  identifies the GenAI telemetry format flavor specific to that provider
  within GenAI semantic conventions.
  It **SHOULD** be set consistently with provider-specific attributes and signals.
  For example, GenAI spans, metrics, and events related to AWS Bedrock
  should have the `gen_ai.provider.name` set to `aws.bedrock` and include
  applicable `aws.bedrock.*` attributes and are not expected to include
  `openai.*` attributes.


  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_provider_name()
      :"gen_ai.provider.name"

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_provider_name_values().openai
      :openai

      iex> %{OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_provider_name() => OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_provider_name_values().openai}
      %{:"gen_ai.provider.name" => :openai}

  ### Erlang

  ```erlang
  ?GEN_AI_PROVIDER_NAME.
  'gen_ai.provider.name'

  ?GEN_AI_PROVIDER_NAME_VALUES_OPENAI.
  'openai'

  \#{?GEN_AI_PROVIDER_NAME => ?GEN_AI_PROVIDER_NAME_VALUES_OPENAI}.
  \#{'gen_ai.provider.name' => 'openai'}
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_provider_name :: :"gen_ai.provider.name"
  def gen_ai_provider_name do
    :"gen_ai.provider.name"
  end

  @spec gen_ai_provider_name_values() :: gen_ai_provider_name_values()
  def gen_ai_provider_name_values() do
    %{
      :openai => :openai,
      :"gcp.gen_ai" => :"gcp.gen_ai",
      :"gcp.vertex_ai" => :"gcp.vertex_ai",
      :"gcp.gemini" => :"gcp.gemini",
      :anthropic => :anthropic,
      :cohere => :cohere,
      :"azure.ai.inference" => :"azure.ai.inference",
      :"azure.ai.openai" => :"azure.ai.openai",
      :"ibm.watsonx.ai" => :"ibm.watsonx.ai",
      :"aws.bedrock" => :"aws.bedrock",
      :perplexity => :perplexity,
      :x_ai => :x_ai,
      :deepseek => :deepseek,
      :groq => :groq,
      :mistral_ai => :mistral_ai
    }
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_choice_count()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_encoding_formats()
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
  Requests with same seed value more likely to return same result.
  ### Value type

  Value must be of type `integer()`.
  ### Examples

  ```
  [100]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_request_seed()
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
  [["stop"], ["stop", "length"]]
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

  @doc """
  The documents retrieved.

  ### Notes

  Instrumentations **MUST** follow [Retrieval documents JSON schema](/docs/gen-ai/gen-ai-retrieval-documents.json).
  When the attribute is recorded on events, it **MUST** be recorded in structured
  form. When recorded on spans, it **MAY** be recorded as a JSON string if structured
  format is not supported and **SHOULD** be recorded in structured form otherwise.

  Each document object **SHOULD** contain at least the following properties:
  `id` (string): A unique identifier for the document, `score` (double): The relevance score of the document

  ### Examples

  ```
  ["[\n  {\n    \"id\": \"doc_123\",\n    \"score\": 0.95\n  },\n  {\n    \"id\": \"doc_456\",\n    \"score\": 0.87\n  },\n  {\n    \"id\": \"doc_789\",\n    \"score\": 0.82\n  }\n]\n"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_retrieval_documents()
      :"gen_ai.retrieval.documents"

  ### Erlang

  ```erlang
  ?GEN_AI_RETRIEVAL_DOCUMENTS.
  'gen_ai.retrieval.documents'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_retrieval_documents :: :"gen_ai.retrieval.documents"
  def gen_ai_retrieval_documents do
    :"gen_ai.retrieval.documents"
  end

  @doc """
  The query text used for retrieval.
  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes

  > [!Warning]
  > This attribute may contain sensitive information.

  ### Examples

  ```
  ["What is the capital of France?", "weather in Paris"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_retrieval_query_text()
      :"gen_ai.retrieval.query.text"

  ### Erlang

  ```erlang
  ?GEN_AI_RETRIEVAL_QUERY_TEXT.
  'gen_ai.retrieval.query.text'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_retrieval_query_text :: :"gen_ai.retrieval.query.text"
  def gen_ai_retrieval_query_text do
    :"gen_ai.retrieval.query.text"
  end

  @typedoc """
  Deprecated, use `gen_ai.provider.name` instead.

  ### Enum Values
  * `:openai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - OpenAI
  * `:"gcp.gen_ai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Any Google generative AI endpoint
  * `:"gcp.vertex_ai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Vertex AI
  * `:"gcp.gemini"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Gemini
  * `:vertex_ai` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Vertex AI~~
  * `:gemini` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Gemini~~
  * `:anthropic` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Anthropic
  * `:cohere` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Cohere
  * `:"az.ai.inference"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Azure AI Inference~~
  * `:"az.ai.openai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Azure OpenAI~~
  * `:"azure.ai.inference"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure AI Inference
  * `:"azure.ai.openai"` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Azure OpenAI
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
          :"azure.ai.inference" => :"azure.ai.inference",
          :"azure.ai.openai" => :"azure.ai.openai",
          :"ibm.watsonx.ai" => :"ibm.watsonx.ai",
          :"aws.bedrock" => :"aws.bedrock",
          :perplexity => :perplexity,
          :xai => :xai,
          :deepseek => :deepseek,
          :groq => :groq,
          :mistral_ai => :mistral_ai
        }
  @deprecated """
  Replaced by `gen_ai.provider.name`.
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
      :"azure.ai.inference" => :"azure.ai.inference",
      :"azure.ai.openai" => :"azure.ai.openai",
      :"ibm.watsonx.ai" => :"ibm.watsonx.ai",
      :"aws.bedrock" => :"aws.bedrock",
      :perplexity => :perplexity,
      :xai => :xai,
      :deepseek => :deepseek,
      :groq => :groq,
      :mistral_ai => :mistral_ai
    }
  end

  @doc """
  The system message or instructions provided to the GenAI model separately from the chat history.

  ### Notes

  This attribute **SHOULD** be used when the corresponding provider or API
  allows to provide system instructions or messages separately from the
  chat history.

  Instructions that are part of the chat history **SHOULD** be recorded in
  `gen_ai.input.messages` attribute instead.

  Instrumentations **MUST** follow [System instructions JSON schema](/docs/gen-ai/gen-ai-system-instructions.json).

  When recorded on spans, it **MAY** be recorded as a JSON string if structured
  format is not supported and **SHOULD** be recorded in structured form otherwise.

  Instrumentations **MAY** provide a way for users to filter or truncate
  system instructions.

  > [!Warning]
  > This attribute may contain sensitive information.

  See [Recording content on attributes](/docs/gen-ai/gen-ai-spans.md#recording-content-on-attributes)
  section for more details.

  ### Examples

  ```
  ["[\n  {\n    \"type\": \"text\",\n    \"content\": \"You are an Agent that greet users, always use greetings tool to respond\"\n  }\n]\n", "[\n  {\n    \"type\": \"text\",\n    \"content\": \"You are a language translator.\"\n  },\n  {\n    \"type\": \"text\",\n    \"content\": \"Your mission is to translate text in English to French.\"\n  }\n]\n"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_system_instructions()
      :"gen_ai.system_instructions"

  ### Erlang

  ```erlang
  ?GEN_AI_SYSTEM_INSTRUCTIONS.
  'gen_ai.system_instructions'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_system_instructions :: :"gen_ai.system_instructions"
  def gen_ai_system_instructions do
    :"gen_ai.system_instructions"
  end

  @typedoc """
  The type of token being counted.

  ### Enum Values
  * `:input` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Input tokens (prompt, input, etc.)
  * `:completion` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - **deprecated** ~~Output tokens (completion, response, etc.)~~
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
      :completion => :output,
      :output => :output
    }
  end

  @doc """
  Parameters passed to the tool call.

  ### Notes

  > [!WARNING]
  > This attribute may contain sensitive information.

  It's expected to be an object - in case a serialized string is available
  to the instrumentation, the instrumentation **SHOULD** do the best effort to
  deserialize it to an object. When recorded on spans, it **MAY** be recorded as a JSON string if structured format is not supported and **SHOULD** be recorded in structured form otherwise.

  ### Examples

  ```
  ["{\n    \"location\": \"San Francisco?\",\n    \"date\": \"2025-10-01\"\n}\n"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_tool_call_arguments()
      :"gen_ai.tool.call.arguments"

  ### Erlang

  ```erlang
  ?GEN_AI_TOOL_CALL_ARGUMENTS.
  'gen_ai.tool.call.arguments'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_tool_call_arguments :: :"gen_ai.tool.call.arguments"
  def gen_ai_tool_call_arguments do
    :"gen_ai.tool.call.arguments"
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_tool_call_id()
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
  The result returned by the tool call (if any and if execution was successful).

  ### Notes

  > [!WARNING]
  > This attribute may contain sensitive information.

  It's expected to be an object - in case a serialized string is available
  to the instrumentation, the instrumentation **SHOULD** do the best effort to
  deserialize it to an object. When recorded on spans, it **MAY** be recorded as a JSON string if structured format is not supported and **SHOULD** be recorded in structured form otherwise.

  ### Examples

  ```
  ["{\n  \"temperature_range\": {\n    \"high\": 75,\n    \"low\": 60\n  },\n  \"conditions\": \"sunny\"\n}\n"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_tool_call_result()
      :"gen_ai.tool.call.result"

  ### Erlang

  ```erlang
  ?GEN_AI_TOOL_CALL_RESULT.
  'gen_ai.tool.call.result'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_tool_call_result :: :"gen_ai.tool.call.result"
  def gen_ai_tool_call_result do
    :"gen_ai.tool.call.result"
  end

  @doc """
  The list of source system tool definitions available to the GenAI agent or model.

  ### Notes

  The value of this attribute matches source system tool definition format.

  It's expected to be an array of objects where each object represents a tool definition. In case a serialized string is available
  to the instrumentation, the instrumentation **SHOULD** do the best effort to
  deserialize it to an array. When recorded on spans, it **MAY** be recorded as a JSON string if structured format is not supported and **SHOULD** be recorded in structured form otherwise.

  Since this attribute could be large, it's **NOT** RECOMMENDED to populate
  it by default. Instrumentations **MAY** provide a way to enable
  populating this attribute.

  ### Examples

  ```
  ["[\n  {\n    \"type\": \"function\",\n    \"name\": \"get_current_weather\",\n    \"description\": \"Get the current weather in a given location\",\n    \"parameters\": {\n      \"type\": \"object\",\n      \"properties\": {\n        \"location\": {\n          \"type\": \"string\",\n          \"description\": \"The city and state, e.g. San Francisco, CA\"\n        },\n        \"unit\": {\n          \"type\": \"string\",\n          \"enum\": [\n            \"celsius\",\n            \"fahrenheit\"\n          ]\n        }\n      },\n      \"required\": [\n        \"location\",\n        \"unit\"\n      ]\n    }\n  }\n]\n"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_tool_definitions()
      :"gen_ai.tool.definitions"

  ### Erlang

  ```erlang
  ?GEN_AI_TOOL_DEFINITIONS.
  'gen_ai.tool.definitions'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_tool_definitions :: :"gen_ai.tool.definitions"
  def gen_ai_tool_definitions do
    :"gen_ai.tool.definitions"
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_tool_description()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_tool_name()
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

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_tool_type()
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

  @doc """
  The number of input tokens written to a provider-managed cache.
  ### Value type

  Value must be of type `integer()`.
  ### Notes

  The value **SHOULD** be included in `gen_ai.usage.input_tokens`.

  ### Examples

  ```
  [25]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_usage_cache_creation_input_tokens()
      :"gen_ai.usage.cache_creation.input_tokens"

  ### Erlang

  ```erlang
  ?GEN_AI_USAGE_CACHE_CREATION_INPUT_TOKENS.
  'gen_ai.usage.cache_creation.input_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_usage_cache_creation_input_tokens :: :"gen_ai.usage.cache_creation.input_tokens"
  def gen_ai_usage_cache_creation_input_tokens do
    :"gen_ai.usage.cache_creation.input_tokens"
  end

  @doc """
  The number of input tokens served from a provider-managed cache.
  ### Value type

  Value must be of type `integer()`.
  ### Notes

  The value **SHOULD** be included in `gen_ai.usage.input_tokens`.

  ### Examples

  ```
  [50]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.GenAiAttributes.gen_ai_usage_cache_read_input_tokens()
      :"gen_ai.usage.cache_read.input_tokens"

  ### Erlang

  ```erlang
  ?GEN_AI_USAGE_CACHE_READ_INPUT_TOKENS.
  'gen_ai.usage.cache_read.input_tokens'
  ```

  <!-- tabs-close -->
  """
  @spec gen_ai_usage_cache_read_input_tokens :: :"gen_ai.usage.cache_read.input_tokens"
  def gen_ai_usage_cache_read_input_tokens do
    :"gen_ai.usage.cache_read.input_tokens"
  end

  @deprecated """
  Replaced by `gen_ai.usage.output_tokens`.
  """
  @spec gen_ai_usage_completion_tokens :: :"gen_ai.usage.completion_tokens"
  def gen_ai_usage_completion_tokens do
    :"gen_ai.usage.completion_tokens"
  end

  @doc """
  The number of tokens used in the GenAI input (prompt).
  ### Value type

  Value must be of type `integer()`.
  ### Notes

  This value **SHOULD** include all types of input tokens, including cached tokens.
  Instrumentations **SHOULD** make a best effort to populate this value, using a total
  provided by the provider when available or, depending on the provider API,
  by summing different token types parsed from the provider output.

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
  Replaced by `gen_ai.usage.input_tokens`.
  """
  @spec gen_ai_usage_prompt_tokens :: :"gen_ai.usage.prompt_tokens"
  def gen_ai_usage_prompt_tokens do
    :"gen_ai.usage.prompt_tokens"
  end
end
