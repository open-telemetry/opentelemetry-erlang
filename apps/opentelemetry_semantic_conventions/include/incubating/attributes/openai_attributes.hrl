
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

%% The type of OpenAI API being used.
-define(OPENAI_API_TYPE, 'openai.api.type').

-define(OPENAI_API_TYPE_VALUES_CHAT_COMPLETIONS, 'chat_completions').

-define(OPENAI_API_TYPE_VALUES_RESPONSES, 'responses').



%% The service tier requested. May be a specific tier, default, or auto.
-define(OPENAI_REQUEST_SERVICE_TIER, 'openai.request.service_tier').

-define(OPENAI_REQUEST_SERVICE_TIER_VALUES_AUTO, 'auto').

-define(OPENAI_REQUEST_SERVICE_TIER_VALUES_DEFAULT, 'default').



%% The service tier used for the response.
-define(OPENAI_RESPONSE_SERVICE_TIER, 'openai.response.service_tier').


%% A fingerprint to track any eventual change in the Generative AI environment.
-define(OPENAI_RESPONSE_SYSTEM_FINGERPRINT, 'openai.response.system_fingerprint').
