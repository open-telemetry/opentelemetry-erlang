
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

%% The name of the request or notification method.
-define(MCP_METHOD_NAME, 'mcp.method.name').

-define(MCP_METHOD_NAME_VALUES_NOTIFICATIONS_CANCELLED, 'notifications/cancelled').

-define(MCP_METHOD_NAME_VALUES_INITIALIZE, 'initialize').

-define(MCP_METHOD_NAME_VALUES_NOTIFICATIONS_INITIALIZED, 'notifications/initialized').

-define(MCP_METHOD_NAME_VALUES_NOTIFICATIONS_PROGRESS, 'notifications/progress').

-define(MCP_METHOD_NAME_VALUES_PING, 'ping').

-define(MCP_METHOD_NAME_VALUES_RESOURCES_LIST, 'resources/list').

-define(MCP_METHOD_NAME_VALUES_RESOURCES_TEMPLATES_LIST, 'resources/templates/list').

-define(MCP_METHOD_NAME_VALUES_RESOURCES_READ, 'resources/read').

-define(MCP_METHOD_NAME_VALUES_NOTIFICATIONS_RESOURCES_LIST_CHANGED, 'notifications/resources/list_changed').

-define(MCP_METHOD_NAME_VALUES_RESOURCES_SUBSCRIBE, 'resources/subscribe').

-define(MCP_METHOD_NAME_VALUES_RESOURCES_UNSUBSCRIBE, 'resources/unsubscribe').

-define(MCP_METHOD_NAME_VALUES_NOTIFICATIONS_RESOURCES_UPDATED, 'notifications/resources/updated').

-define(MCP_METHOD_NAME_VALUES_PROMPTS_LIST, 'prompts/list').

-define(MCP_METHOD_NAME_VALUES_PROMPTS_GET, 'prompts/get').

-define(MCP_METHOD_NAME_VALUES_NOTIFICATIONS_PROMPTS_LIST_CHANGED, 'notifications/prompts/list_changed').

-define(MCP_METHOD_NAME_VALUES_TOOLS_LIST, 'tools/list').

-define(MCP_METHOD_NAME_VALUES_TOOLS_CALL, 'tools/call').

-define(MCP_METHOD_NAME_VALUES_NOTIFICATIONS_TOOLS_LIST_CHANGED, 'notifications/tools/list_changed').

-define(MCP_METHOD_NAME_VALUES_LOGGING_SET_LEVEL, 'logging/setLevel').

-define(MCP_METHOD_NAME_VALUES_NOTIFICATIONS_MESSAGE, 'notifications/message').

-define(MCP_METHOD_NAME_VALUES_SAMPLING_CREATE_MESSAGE, 'sampling/createMessage').

-define(MCP_METHOD_NAME_VALUES_COMPLETION_COMPLETE, 'completion/complete').

-define(MCP_METHOD_NAME_VALUES_ROOTS_LIST, 'roots/list').

-define(MCP_METHOD_NAME_VALUES_NOTIFICATIONS_ROOTS_LIST_CHANGED, 'notifications/roots/list_changed').

-define(MCP_METHOD_NAME_VALUES_ELICITATION_CREATE, 'elicitation/create').



%% The [version](https://modelcontextprotocol.io/specification/versioning) of the Model Context Protocol used.
-define(MCP_PROTOCOL_VERSION, 'mcp.protocol.version').


%% The value of the resource uri.
-define(MCP_RESOURCE_URI, 'mcp.resource.uri').


%% Identifies [MCP session](https://modelcontextprotocol.io/specification/2025-06-18/basic/transports#session-management).
-define(MCP_SESSION_ID, 'mcp.session.id').
