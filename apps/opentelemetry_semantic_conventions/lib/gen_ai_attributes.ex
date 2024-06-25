defmodule OpenTelemetry.SemanticConventions.GenAiAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Gen_Ai attributes.
  """

  @doc """
  The full response received from the LLM.
  ### Notes

  It's RECOMMENDED to format completions as JSON string matching [OpenAI messages format](https://platform.openai.com/docs/guides/text-generation)

  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_completion()
      :"gen_ai.completion"
  """
  @spec genai_completion :: :"gen_ai.completion"
  def genai_completion do
    :"gen_ai.completion"
  end

  @doc """
  The full prompt sent to an LLM.
  ### Notes

  It's RECOMMENDED to format prompts as JSON string matching [OpenAI messages format](https://platform.openai.com/docs/guides/text-generation)

  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_prompt()
      :"gen_ai.prompt"
  """
  @spec genai_prompt :: :"gen_ai.prompt"
  def genai_prompt do
    :"gen_ai.prompt"
  end

  @doc """
  The maximum number of tokens the LLM generates for a request.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_request_maxtokens()
      :"gen_ai.request.max_tokens"
  """
  @spec genai_request_maxtokens :: :"gen_ai.request.max_tokens"
  def genai_request_maxtokens do
    :"gen_ai.request.max_tokens"
  end

  @doc """
  The name of the LLM a request is being made to.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_request_model()
      :"gen_ai.request.model"
  """
  @spec genai_request_model :: :"gen_ai.request.model"
  def genai_request_model do
    :"gen_ai.request.model"
  end

  @doc """
  The temperature setting for the LLM request.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_request_temperature()
      :"gen_ai.request.temperature"
  """
  @spec genai_request_temperature :: :"gen_ai.request.temperature"
  def genai_request_temperature do
    :"gen_ai.request.temperature"
  end

  @doc """
  The top_p sampling setting for the LLM request.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_request_topp()
      :"gen_ai.request.top_p"
  """
  @spec genai_request_topp :: :"gen_ai.request.top_p"
  def genai_request_topp do
    :"gen_ai.request.top_p"
  end

  @doc """
  Array of reasons the model stopped generating tokens, corresponding to each generation received.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_response_finishreasons()
      :"gen_ai.response.finish_reasons"
  """
  @spec genai_response_finishreasons :: :"gen_ai.response.finish_reasons"
  def genai_response_finishreasons do
    :"gen_ai.response.finish_reasons"
  end

  @doc """
  The unique identifier for the completion.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_response_id()
      :"gen_ai.response.id"
  """
  @spec genai_response_id :: :"gen_ai.response.id"
  def genai_response_id do
    :"gen_ai.response.id"
  end

  @doc """
  The name of the LLM a response was generated from.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_response_model()
      :"gen_ai.response.model"
  """
  @spec genai_response_model :: :"gen_ai.response.model"
  def genai_response_model do
    :"gen_ai.response.model"
  end

  @typedoc """
  The Generative AI product as identified by the client instrumentation.

  ### Enum Values
  * `:openai` ^[e](`m:OpenTelemetry.SemanticConventions#experimental`)^ - OpenAI
  """
  @type genai_system() :: %{
          :openai => :openai
        }
  @doc """
  The Generative AI product as identified by the client instrumentation.
  ### Notes

  The actual GenAI product may differ from the one identified by the client. For example, when using OpenAI client libraries to communicate with Mistral, the `gen_ai.system` is set to `openai` based on the instrumentation's best knowledge.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_system().openai
      :openai
      
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_system(:custom_value)
      :custom_value
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


  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_usage_completiontokens()
      :"gen_ai.usage.completion_tokens"
  """
  @spec genai_usage_completiontokens :: :"gen_ai.usage.completion_tokens"
  def genai_usage_completiontokens do
    :"gen_ai.usage.completion_tokens"
  end

  @doc """
  The number of tokens used in the LLM prompt.


  ### Example
      iex> OpenTelemetry.SemanticConventions.GenAiAttributes.genai_usage_prompttokens()
      :"gen_ai.usage.prompt_tokens"
  """
  @spec genai_usage_prompttokens :: :"gen_ai.usage.prompt_tokens"
  def genai_usage_prompttokens do
    :"gen_ai.usage.prompt_tokens"
  end
end
