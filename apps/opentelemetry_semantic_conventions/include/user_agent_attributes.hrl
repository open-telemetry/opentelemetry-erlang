
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

%% Value of the [HTTP User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#field.user-agent) header sent by the client.
%%  
{"brief": "Value of the [HTTP User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#field.user-agent) header sent by the client.\n", "examples": ["CERN-LineMode/2.15 libwww/2.17b3", "Mozilla/5.0 (iPhone; CPU iPhone OS 14_7_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.2 Mobile/15E148 Safari/604.1", "YourApp/1.0.0 grpc-java-okhttp/1.27.2"], "name": "user_agent.original", "requirement_level": "recommended", "stability": "stable", "type": "string"}
user_agent_original
-define(USER_AGENT_ORIGINAL, 'user_agent.original').
