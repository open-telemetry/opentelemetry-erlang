
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
-include_lib("opentelemetry_semantic_conventions/include/attributes/other_attributes.hrl").

%% @deprecated Replaced by `db.client.connection.state`.
%% Deprecated, use `db.client.connection.state` instead.
-define(STATE, 'state').

-define(STATE_VALUES_IDLE, 'idle').

-define(STATE_VALUES_USED, 'used').

