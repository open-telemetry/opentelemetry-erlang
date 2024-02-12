%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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
%%
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(otel_meter_server_sup).

-behaviour(supervisor).

-export([start_link/3,
         provider_pid/1]).

-export([init/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

start_link(Name, Resource, Opts) ->
    supervisor:start_link(?MODULE, [Name, Resource, Opts]).

%% eqwalizer:ignore waiting on sup_ref to be exported https://github.com/erlang/otp/pull/7205
-spec provider_pid(supervisor:sup_ref()) -> pid() | restarting | undefined.
provider_pid(SupPid) ->
    Children = supervisor:which_children(SupPid),
    case lists:keyfind(otel_meter_server, 1, Children) of
        {_, Child, _, _} ->
            Child;
        false ->
            undefined
    end.

init([Name, Resource, Opts]) ->
    SupFlags = #{strategy => rest_for_one, %% restart the readers if the server dies
                 intensity => 1,
                 period => 5},

    MeterServerRegName = list_to_atom(lists:concat([otel_meter_provider, "_", Name])),

    MetricReaderSup = #{id => otel_metric_reader_sup,
                        start => {otel_metric_reader_sup, start_link, [self(), Opts]},
                        restart => permanent,
                        shutdown => infinity,
                        type => supervisor,
                        modules => [otel_metric_reader_sup]},

    ProviderShutdownTimeout = maps:get(meter_provider_shutdown_timeout, Opts, 5000),
    MeterServer = #{id => otel_meter_server,
                    start => {otel_meter_server, start_link, [Name, MeterServerRegName, Resource, Opts]},
                    restart => permanent,
                    shutdown => ProviderShutdownTimeout,
                    type => worker,
                    modules => [otel_meter_provider, otel_meter_server]},

    ChildSpecs = [MeterServer, MetricReaderSup],

    {ok, {SupFlags, ChildSpecs}}.
