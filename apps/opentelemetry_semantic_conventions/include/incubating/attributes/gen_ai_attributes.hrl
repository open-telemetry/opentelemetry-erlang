
%%%------------------------------------------------------------------------
%% Copyright The OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%-------------------------------------------------------------------------

%% Free-form description of the GenAI agent provided by the application.
-define(GEN_AI_AGENT_DESCRIPTION, 'gen_ai.agent.description').


%% The unique identifier of the GenAI agent.
-define(GEN_AI_AGENT_ID, 'gen_ai.agent.id').


%% Human-readable name of the GenAI agent provided by the application.
-define(GEN_AI_AGENT_NAME, 'gen_ai.agent.name').


%% The version of the GenAI agent.
-define(GEN_AI_AGENT_VERSION, 'gen_ai.agent.version').

%% @deprecated Removed, no replacement at this time.
%% Deprecated, use Event API to report completions contents.
-define(GEN_AI_COMPLETION, 'gen_ai.completion').


%% The unique identifier for a conversation (session, thread), used to store and correlate messages within this conversation.
-define(GEN_AI_CONVERSATION_ID, 'gen_ai.conversation.id').


%% The data source identifier.
-define(GEN_AI_DATA_SOURCE_ID, 'gen_ai.data_source.id').


%% The number of dimensions the resulting output embeddings should have.
-define(GEN_AI_EMBEDDINGS_DIMENSION_COUNT, 'gen_ai.embeddings.dimension.count').


%% A free-form explanation for the assigned score provided by the evaluator.
-define(GEN_AI_EVALUATION_EXPLANATION, 'gen_ai.evaluation.explanation').


%% The name of the evaluation metric used for the GenAI response.
-define(GEN_AI_EVALUATION_NAME, 'gen_ai.evaluation.name').


%% Human readable label for evaluation.
-define(GEN_AI_EVALUATION_SCORE_LABEL, 'gen_ai.evaluation.score.label').


%% The evaluation score returned by the evaluator.
-define(GEN_AI_EVALUATION_SCORE_VALUE, 'gen_ai.evaluation.score.value').


%% The chat history provided to the model as an input.
%%  
-define(GEN_AI_INPUT_MESSAGES, 'gen_ai.input.messages').

%% @deprecated Replaced by `gen_ai.output.type`.
%% Deprecated, use `gen_ai.output.type`.
%%  
-define(GEN_AI_OPENAI_REQUEST_RESPONSE_FORMAT, 'gen_ai.openai.request.response_format').

-define(GEN_AI_OPENAI_REQUEST_RESPONSE_FORMAT_VALUES_TEXT, 'text').

-define(GEN_AI_OPENAI_REQUEST_RESPONSE_FORMAT_VALUES_JSON_OBJECT, 'json_object').

-define(GEN_AI_OPENAI_REQUEST_RESPONSE_FORMAT_VALUES_JSON_SCHEMA, 'json_schema').


%% @deprecated Replaced by `gen_ai.request.seed`.
%% Deprecated, use `gen_ai.request.seed`.
-define(GEN_AI_OPENAI_REQUEST_SEED, 'gen_ai.openai.request.seed').

%% @deprecated Replaced by `openai.request.service_tier`.
%% Deprecated, use `openai.request.service_tier`.
-define(GEN_AI_OPENAI_REQUEST_SERVICE_TIER, 'gen_ai.openai.request.service_tier').

-define(GEN_AI_OPENAI_REQUEST_SERVICE_TIER_VALUES_AUTO, 'auto').

-define(GEN_AI_OPENAI_REQUEST_SERVICE_TIER_VALUES_DEFAULT, 'default').


%% @deprecated Replaced by `openai.response.service_tier`.
%% Deprecated, use `openai.response.service_tier`.
-define(GEN_AI_OPENAI_RESPONSE_SERVICE_TIER, 'gen_ai.openai.response.service_tier').

%% @deprecated Replaced by `openai.response.system_fingerprint`.
%% Deprecated, use `openai.response.system_fingerprint`.
-define(GEN_AI_OPENAI_RESPONSE_SYSTEM_FINGERPRINT, 'gen_ai.openai.response.system_fingerprint').


%% The name of the operation being performed.
-define(GEN_AI_OPERATION_NAME, 'gen_ai.operation.name').

-define(GEN_AI_OPERATION_NAME_VALUES_CHAT, 'chat').

-define(GEN_AI_OPERATION_NAME_VALUES_GENERATE_CONTENT, 'generate_content').

-define(GEN_AI_OPERATION_NAME_VALUES_TEXT_COMPLETION, 'text_completion').

-define(GEN_AI_OPERATION_NAME_VALUES_EMBEDDINGS, 'embeddings').

-define(GEN_AI_OPERATION_NAME_VALUES_RETRIEVAL, 'retrieval').

-define(GEN_AI_OPERATION_NAME_VALUES_CREATE_AGENT, 'create_agent').

-define(GEN_AI_OPERATION_NAME_VALUES_INVOKE_AGENT, 'invoke_agent').

-define(GEN_AI_OPERATION_NAME_VALUES_EXECUTE_TOOL, 'execute_tool').



%% Messages returned by the model where each message represents a specific model response (choice, candidate).
-define(GEN_AI_OUTPUT_MESSAGES, 'gen_ai.output.messages').


%% Represents the content type requested by the client.
-define(GEN_AI_OUTPUT_TYPE, 'gen_ai.output.type').

-define(GEN_AI_OUTPUT_TYPE_VALUES_TEXT, 'text').

-define(GEN_AI_OUTPUT_TYPE_VALUES_JSON, 'json').

-define(GEN_AI_OUTPUT_TYPE_VALUES_IMAGE, 'image').

-define(GEN_AI_OUTPUT_TYPE_VALUES_SPEECH, 'speech').


%% @deprecated Removed, no replacement at this time.
%% Deprecated, use Event API to report prompt contents.
-define(GEN_AI_PROMPT, 'gen_ai.prompt').


%% The name of the prompt that uniquely identifies it.
-define(GEN_AI_PROMPT_NAME, 'gen_ai.prompt.name').


%% The Generative AI provider as identified by the client or server instrumentation.
-define(GEN_AI_PROVIDER_NAME, 'gen_ai.provider.name').

-define(GEN_AI_PROVIDER_NAME_VALUES_OPENAI, 'openai').

-define(GEN_AI_PROVIDER_NAME_VALUES_GCP_GEN_AI, 'gcp.gen_ai').

-define(GEN_AI_PROVIDER_NAME_VALUES_GCP_VERTEX_AI, 'gcp.vertex_ai').

-define(GEN_AI_PROVIDER_NAME_VALUES_GCP_GEMINI, 'gcp.gemini').

-define(GEN_AI_PROVIDER_NAME_VALUES_ANTHROPIC, 'anthropic').

-define(GEN_AI_PROVIDER_NAME_VALUES_COHERE, 'cohere').

-define(GEN_AI_PROVIDER_NAME_VALUES_AZURE_AI_INFERENCE, 'azure.ai.inference').

-define(GEN_AI_PROVIDER_NAME_VALUES_AZURE_AI_OPENAI, 'azure.ai.openai').

-define(GEN_AI_PROVIDER_NAME_VALUES_IBM_WATSONX_AI, 'ibm.watsonx.ai').

-define(GEN_AI_PROVIDER_NAME_VALUES_AWS_BEDROCK, 'aws.bedrock').

-define(GEN_AI_PROVIDER_NAME_VALUES_PERPLEXITY, 'perplexity').

-define(GEN_AI_PROVIDER_NAME_VALUES_X_AI, 'x_ai').

-define(GEN_AI_PROVIDER_NAME_VALUES_DEEPSEEK, 'deepseek').

-define(GEN_AI_PROVIDER_NAME_VALUES_GROQ, 'groq').

-define(GEN_AI_PROVIDER_NAME_VALUES_MISTRAL_AI, 'mistral_ai').



%% The target number of candidate completions to return.
-define(GEN_AI_REQUEST_CHOICE_COUNT, 'gen_ai.request.choice.count').


%% The encoding formats requested in an embeddings operation, if specified.
-define(GEN_AI_REQUEST_ENCODING_FORMATS, 'gen_ai.request.encoding_formats').


%% The frequency penalty setting for the GenAI request.
-define(GEN_AI_REQUEST_FREQUENCY_PENALTY, 'gen_ai.request.frequency_penalty').


%% The maximum number of tokens the model generates for a request.
-define(GEN_AI_REQUEST_MAX_TOKENS, 'gen_ai.request.max_tokens').


%% The name of the GenAI model a request is being made to.
-define(GEN_AI_REQUEST_MODEL, 'gen_ai.request.model').


%% The presence penalty setting for the GenAI request.
-define(GEN_AI_REQUEST_PRESENCE_PENALTY, 'gen_ai.request.presence_penalty').


%% Requests with same seed value more likely to return same result.
-define(GEN_AI_REQUEST_SEED, 'gen_ai.request.seed').


%% List of sequences that the model will use to stop generating further tokens.
-define(GEN_AI_REQUEST_STOP_SEQUENCES, 'gen_ai.request.stop_sequences').


%% The temperature setting for the GenAI request.
-define(GEN_AI_REQUEST_TEMPERATURE, 'gen_ai.request.temperature').


%% The top_k sampling setting for the GenAI request.
-define(GEN_AI_REQUEST_TOP_K, 'gen_ai.request.top_k').


%% The top_p sampling setting for the GenAI request.
-define(GEN_AI_REQUEST_TOP_P, 'gen_ai.request.top_p').


%% Array of reasons the model stopped generating tokens, corresponding to each generation received.
-define(GEN_AI_RESPONSE_FINISH_REASONS, 'gen_ai.response.finish_reasons').


%% The unique identifier for the completion.
-define(GEN_AI_RESPONSE_ID, 'gen_ai.response.id').


%% The name of the model that generated the response.
-define(GEN_AI_RESPONSE_MODEL, 'gen_ai.response.model').


%% The documents retrieved.
-define(GEN_AI_RETRIEVAL_DOCUMENTS, 'gen_ai.retrieval.documents').


%% The query text used for retrieval.
-define(GEN_AI_RETRIEVAL_QUERY_TEXT, 'gen_ai.retrieval.query.text').

%% @deprecated Replaced by `gen_ai.provider.name`.
%% Deprecated, use `gen_ai.provider.name` instead.
-define(GEN_AI_SYSTEM, 'gen_ai.system').

-define(GEN_AI_SYSTEM_VALUES_OPENAI, 'openai').

-define(GEN_AI_SYSTEM_VALUES_GCP_GEN_AI, 'gcp.gen_ai').

-define(GEN_AI_SYSTEM_VALUES_GCP_VERTEX_AI, 'gcp.vertex_ai').

-define(GEN_AI_SYSTEM_VALUES_GCP_GEMINI, 'gcp.gemini').

-define(GEN_AI_SYSTEM_VALUES_VERTEX_AI, 'vertex_ai').

-define(GEN_AI_SYSTEM_VALUES_GEMINI, 'gemini').

-define(GEN_AI_SYSTEM_VALUES_ANTHROPIC, 'anthropic').

-define(GEN_AI_SYSTEM_VALUES_COHERE, 'cohere').

-define(GEN_AI_SYSTEM_VALUES_AZ_AI_INFERENCE, 'az.ai.inference').

-define(GEN_AI_SYSTEM_VALUES_AZ_AI_OPENAI, 'az.ai.openai').

-define(GEN_AI_SYSTEM_VALUES_AZURE_AI_INFERENCE, 'azure.ai.inference').

-define(GEN_AI_SYSTEM_VALUES_AZURE_AI_OPENAI, 'azure.ai.openai').

-define(GEN_AI_SYSTEM_VALUES_IBM_WATSONX_AI, 'ibm.watsonx.ai').

-define(GEN_AI_SYSTEM_VALUES_AWS_BEDROCK, 'aws.bedrock').

-define(GEN_AI_SYSTEM_VALUES_PERPLEXITY, 'perplexity').

-define(GEN_AI_SYSTEM_VALUES_XAI, 'xai').

-define(GEN_AI_SYSTEM_VALUES_DEEPSEEK, 'deepseek').

-define(GEN_AI_SYSTEM_VALUES_GROQ, 'groq').

-define(GEN_AI_SYSTEM_VALUES_MISTRAL_AI, 'mistral_ai').



%% The system message or instructions provided to the GenAI model separately from the chat history.
-define(GEN_AI_SYSTEM_INSTRUCTIONS, 'gen_ai.system_instructions').


%% The type of token being counted.
-define(GEN_AI_TOKEN_TYPE, 'gen_ai.token.type').

-define(GEN_AI_TOKEN_TYPE_VALUES_INPUT, 'input').

-define(GEN_AI_TOKEN_TYPE_VALUES_COMPLETION, 'output').

-define(GEN_AI_TOKEN_TYPE_VALUES_OUTPUT, 'output').



%% Parameters passed to the tool call.
-define(GEN_AI_TOOL_CALL_ARGUMENTS, 'gen_ai.tool.call.arguments').


%% The tool call identifier.
-define(GEN_AI_TOOL_CALL_ID, 'gen_ai.tool.call.id').


%% The result returned by the tool call (if any and if execution was successful).
-define(GEN_AI_TOOL_CALL_RESULT, 'gen_ai.tool.call.result').


%% The list of source system tool definitions available to the GenAI agent or model.
-define(GEN_AI_TOOL_DEFINITIONS, 'gen_ai.tool.definitions').


%% The tool description.
-define(GEN_AI_TOOL_DESCRIPTION, 'gen_ai.tool.description').


%% Name of the tool utilized by the agent.
-define(GEN_AI_TOOL_NAME, 'gen_ai.tool.name').


%% Type of the tool utilized by the agent
-define(GEN_AI_TOOL_TYPE, 'gen_ai.tool.type').


%% The number of input tokens written to a provider-managed cache.
-define(GEN_AI_USAGE_CACHE_CREATION_INPUT_TOKENS, 'gen_ai.usage.cache_creation.input_tokens').


%% The number of input tokens served from a provider-managed cache.
-define(GEN_AI_USAGE_CACHE_READ_INPUT_TOKENS, 'gen_ai.usage.cache_read.input_tokens').

%% @deprecated Replaced by `gen_ai.usage.output_tokens`.
%% Deprecated, use `gen_ai.usage.output_tokens` instead.
-define(GEN_AI_USAGE_COMPLETION_TOKENS, 'gen_ai.usage.completion_tokens').


%% The number of tokens used in the GenAI input (prompt).
-define(GEN_AI_USAGE_INPUT_TOKENS, 'gen_ai.usage.input_tokens').


%% The number of tokens used in the GenAI response (completion).
-define(GEN_AI_USAGE_OUTPUT_TOKENS, 'gen_ai.usage.output_tokens').

%% @deprecated Replaced by `gen_ai.usage.input_tokens`.
%% Deprecated, use `gen_ai.usage.input_tokens` instead.
-define(GEN_AI_USAGE_PROMPT_TOKENS, 'gen_ai.usage.prompt_tokens').
