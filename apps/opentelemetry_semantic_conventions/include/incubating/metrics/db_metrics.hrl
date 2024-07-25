
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

%% The number of connections that are currently in state described by the `state` attribute
-define(DB_CLIENT_CONNECTION_COUNT, 'db.client.connection.count').


%% The time it took to create a new connection
-define(DB_CLIENT_CONNECTION_CREATE_TIME, 'db.client.connection.create_time').


%% The maximum number of idle open connections allowed
-define(DB_CLIENT_CONNECTION_IDLE_MAX, 'db.client.connection.idle.max').


%% The minimum number of idle open connections allowed
-define(DB_CLIENT_CONNECTION_IDLE_MIN, 'db.client.connection.idle.min').


%% The maximum number of open connections allowed
-define(DB_CLIENT_CONNECTION_MAX, 'db.client.connection.max').


%% The number of pending requests for an open connection, cumulative for the entire pool
-define(DB_CLIENT_CONNECTION_PENDING_REQUESTS, 'db.client.connection.pending_requests').


%% The number of connection timeouts that have occurred trying to obtain a connection from the pool
-define(DB_CLIENT_CONNECTION_TIMEOUTS, 'db.client.connection.timeouts').


%% The time between borrowing a connection and returning it to the pool
-define(DB_CLIENT_CONNECTION_USE_TIME, 'db.client.connection.use_time').


%% The time it took to obtain an open connection from the pool
-define(DB_CLIENT_CONNECTION_WAIT_TIME, 'db.client.connection.wait_time').

%% @deprecated Replaced by `db.client.connection.create_time`. Note: the unit also changed from `ms` to `s`.
%% Deprecated, use `db.client.connection.create_time` instead. Note: the unit also changed from `ms` to `s`.
-define(DB_CLIENT_CONNECTIONS_CREATE_TIME, 'db.client.connections.create_time').

%% @deprecated Replaced by `db.client.connection.idle.max`.
%% Deprecated, use `db.client.connection.idle.max` instead.
-define(DB_CLIENT_CONNECTIONS_IDLE_MAX, 'db.client.connections.idle.max').

%% @deprecated Replaced by `db.client.connection.idle.min`.
%% Deprecated, use `db.client.connection.idle.min` instead.
-define(DB_CLIENT_CONNECTIONS_IDLE_MIN, 'db.client.connections.idle.min').

%% @deprecated Replaced by `db.client.connection.max`.
%% Deprecated, use `db.client.connection.max` instead.
-define(DB_CLIENT_CONNECTIONS_MAX, 'db.client.connections.max').

%% @deprecated Replaced by `db.client.connection.pending_requests`.
%% Deprecated, use `db.client.connection.pending_requests` instead.
-define(DB_CLIENT_CONNECTIONS_PENDING_REQUESTS, 'db.client.connections.pending_requests').

%% @deprecated Replaced by `db.client.connection.timeouts`.
%% Deprecated, use `db.client.connection.timeouts` instead.
-define(DB_CLIENT_CONNECTIONS_TIMEOUTS, 'db.client.connections.timeouts').

%% @deprecated Replaced by `db.client.connection.count`.
%% Deprecated, use `db.client.connection.count` instead.
-define(DB_CLIENT_CONNECTIONS_USAGE, 'db.client.connections.usage').

%% @deprecated Replaced by `db.client.connection.use_time`. Note: the unit also changed from `ms` to `s`.
%% Deprecated, use `db.client.connection.use_time` instead. Note: the unit also changed from `ms` to `s`.
-define(DB_CLIENT_CONNECTIONS_USE_TIME, 'db.client.connections.use_time').

%% @deprecated Replaced by `db.client.connection.wait_time`. Note: the unit also changed from `ms` to `s`.
%% Deprecated, use `db.client.connection.wait_time` instead. Note: the unit also changed from `ms` to `s`.
-define(DB_CLIENT_CONNECTIONS_WAIT_TIME, 'db.client.connections.wait_time').


%% Duration of database client operations.
-define(DB_CLIENT_OPERATION_DURATION, 'db.client.operation.duration').
