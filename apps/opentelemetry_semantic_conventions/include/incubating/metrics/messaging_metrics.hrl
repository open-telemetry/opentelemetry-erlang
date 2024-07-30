
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

%% Measures the duration of process operation.
-define('MESSAGING_PROCESS_DURATION', 'messaging.process.duration').


%% Measures the number of processed messages.
-define('MESSAGING_PROCESS_MESSAGES', 'messaging.process.messages').


%% Measures the duration of publish operation.
-define('MESSAGING_PUBLISH_DURATION', 'messaging.publish.duration').


%% Measures the number of published messages.
-define('MESSAGING_PUBLISH_MESSAGES', 'messaging.publish.messages').


%% Measures the duration of receive operation.
-define('MESSAGING_RECEIVE_DURATION', 'messaging.receive.duration').


%% Measures the number of received messages.
-define('MESSAGING_RECEIVE_MESSAGES', 'messaging.receive.messages').
