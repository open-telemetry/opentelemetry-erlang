
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

%% The full response received from the GenAI model.
-define(GEN_AI_COMPLETION, 'gen_ai.completion').


%% The name of the operation being performed.
-define(GEN_AI_OPERATION_NAME, 'gen_ai.operation.name').

-define(GEN_AI_OPERATION_NAME_VALUES_CHAT, 'chat').

-define(GEN_AI_OPERATION_NAME_VALUES_TEXT_COMPLETION, 'text_completion').



%% The full prompt sent to the GenAI model.
-define(GEN_AI_PROMPT, 'gen_ai.prompt').


%% The frequency penalty setting for the GenAI request.
-define(GEN_AI_REQUEST_FREQUENCY_PENALTY, 'gen_ai.request.frequency_penalty').


%% The maximum number of tokens the model generates for a request.
-define(GEN_AI_REQUEST_MAX_TOKENS, 'gen_ai.request.max_tokens').


%% The name of the GenAI model a request is being made to.
-define(GEN_AI_REQUEST_MODEL, 'gen_ai.request.model').


%% The presence penalty setting for the GenAI request.
-define(GEN_AI_REQUEST_PRESENCE_PENALTY, 'gen_ai.request.presence_penalty').


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


%% The Generative AI product as identified by the client or server instrumentation.
-define(GEN_AI_SYSTEM, 'gen_ai.system').

-define(GEN_AI_SYSTEM_VALUES_OPENAI, 'openai').

-define(GEN_AI_SYSTEM_VALUES_VERTEX_AI, 'vertex_ai').

-define(GEN_AI_SYSTEM_VALUES_ANTHROPIC, 'anthropic').

-define(GEN_AI_SYSTEM_VALUES_COHERE, 'cohere').



%% The type of token being counted.
-define(GEN_AI_TOKEN_TYPE, 'gen_ai.token.type').

-define(GEN_AI_TOKEN_TYPE_VALUES_INPUT, 'input').

-define(GEN_AI_TOKEN_TYPE_VALUES_COMPLETION, 'output').


%% @deprecated Replaced by `gen_ai.usage.output_tokens` attribute.
%% Deprecated, use `gen_ai.usage.output_tokens` instead.
-define(GEN_AI_USAGE_COMPLETION_TOKENS, 'gen_ai.usage.completion_tokens').


%% The number of tokens used in the GenAI input (prompt).
-define(GEN_AI_USAGE_INPUT_TOKENS, 'gen_ai.usage.input_tokens').


%% The number of tokens used in the GenAI response (completion).
-define(GEN_AI_USAGE_OUTPUT_TOKENS, 'gen_ai.usage.output_tokens').

%% @deprecated Replaced by `gen_ai.usage.input_tokens` attribute.
%% Deprecated, use `gen_ai.usage.input_tokens` instead.
-define(GEN_AI_USAGE_PROMPT_TOKENS, 'gen_ai.usage.prompt_tokens').
