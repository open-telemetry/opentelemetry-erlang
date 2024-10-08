
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

%% GenAI operation duration
-define(GEN_AI_CLIENT_OPERATION_DURATION, 'gen_ai.client.operation.duration').


%% Measures number of input and output tokens used
-define(GEN_AI_CLIENT_TOKEN_USAGE, 'gen_ai.client.token.usage').


%% Generative AI server request duration such as time-to-last byte or last output token
-define(GEN_AI_SERVER_REQUEST_DURATION, 'gen_ai.server.request.duration').


%% Time per output token generated after the first token for successful responses
-define(GEN_AI_SERVER_TIME_PER_OUTPUT_TOKEN, 'gen_ai.server.time_per_output_token').


%% Time to generate first token for successful responses
-define(GEN_AI_SERVER_TIME_TO_FIRST_TOKEN, 'gen_ai.server.time_to_first_token').
