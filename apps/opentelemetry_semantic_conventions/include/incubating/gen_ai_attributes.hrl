
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

%% The full response received from the LLM.
-define(GENAI_COMPLETION, 'gen_ai.completion').


%% The full prompt sent to an LLM.
-define(GENAI_PROMPT, 'gen_ai.prompt').


%% The maximum number of tokens the LLM generates for a request.
-define(GENAI_REQUEST_MAXTOKENS, 'gen_ai.request.max_tokens').


%% The name of the LLM a request is being made to.
-define(GENAI_REQUEST_MODEL, 'gen_ai.request.model').


%% The temperature setting for the LLM request.
-define(GENAI_REQUEST_TEMPERATURE, 'gen_ai.request.temperature').


%% The top_p sampling setting for the LLM request.
-define(GENAI_REQUEST_TOPP, 'gen_ai.request.top_p').


%% Array of reasons the model stopped generating tokens, corresponding to each generation received.
-define(GENAI_RESPONSE_FINISHREASONS, 'gen_ai.response.finish_reasons').


%% The unique identifier for the completion.
-define(GENAI_RESPONSE_ID, 'gen_ai.response.id').


%% The name of the LLM a response was generated from.
-define(GENAI_RESPONSE_MODEL, 'gen_ai.response.model').


%% The Generative AI product as identified by the client instrumentation.
-define(GENAI_SYSTEM, 'gen_ai.system').

-define('GENAI_SYSTEM_VALUES.openai', 'openai').



%% The number of tokens used in the LLM response (completion).
-define(GENAI_USAGE_COMPLETIONTOKENS, 'gen_ai.usage.completion_tokens').


%% The number of tokens used in the LLM prompt.
-define(GENAI_USAGE_PROMPTTOKENS, 'gen_ai.usage.prompt_tokens').
