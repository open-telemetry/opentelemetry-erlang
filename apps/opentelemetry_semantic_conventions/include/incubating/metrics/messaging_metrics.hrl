
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

%% Number of messages that were delivered to the application.
-define(MESSAGING_CLIENT_CONSUMED_MESSAGES, 'messaging.client.consumed.messages').


%% Duration of messaging operation initiated by a producer or consumer client.
-define(MESSAGING_CLIENT_OPERATION_DURATION, 'messaging.client.operation.duration').


%% Number of messages producer attempted to publish to the broker.
-define(MESSAGING_CLIENT_PUBLISHED_MESSAGES, 'messaging.client.published.messages').


%% Duration of processing operation.
-define(MESSAGING_PROCESS_DURATION, 'messaging.process.duration').

%% @deprecated Replaced by `messaging.client.consumed.messages`.
%% Deprecated. Use `messaging.client.consumed.messages` instead.
-define(MESSAGING_PROCESS_MESSAGES, 'messaging.process.messages').

%% @deprecated Replaced by `messaging.client.operation.duration`.
%% Deprecated. Use `messaging.client.operation.duration` instead.
-define(MESSAGING_PUBLISH_DURATION, 'messaging.publish.duration').

%% @deprecated Replaced by `messaging.client.produced.messages`.
%% Deprecated. Use `messaging.client.produced.messages` instead.
-define(MESSAGING_PUBLISH_MESSAGES, 'messaging.publish.messages').

%% @deprecated Replaced by `messaging.client.operation.duration`.
%% Deprecated. Use `messaging.client.operation.duration` instead.
-define(MESSAGING_RECEIVE_DURATION, 'messaging.receive.duration').

%% @deprecated Replaced by `messaging.client.consumed.messages`.
%% Deprecated. Use `messaging.client.consumed.messages` instead.
-define(MESSAGING_RECEIVE_MESSAGES, 'messaging.receive.messages').
