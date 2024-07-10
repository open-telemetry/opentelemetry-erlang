

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

-define('genai_system.openai', 'openai').

-define(genai_system.(Custom), Custom).


%% The number of tokens used in the LLM response (completion).
-define(GENAI_USAGE_COMPLETIONTOKENS, 'gen_ai.usage.completion_tokens').


%% The number of tokens used in the LLM prompt.
-define(GENAI_USAGE_PROMPTTOKENS, 'gen_ai.usage.prompt_tokens').
