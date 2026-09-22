%%%------------------------------------------------------------------------
%% Copyright 2026, OpenTelemetry Authors
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
%% @private
%%%------------------------------------------------------------------------
-module(opentelemetry_sdk_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link(otel_configuration_sdk:configuration()) ->
          {ok, pid()} | ignore | {error, term()}.
start_link(Configuration) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Configuration).

-spec init(otel_configuration_sdk:configuration()) ->
          {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Configuration) ->
    case otel_configuration_sdk:disabled(Configuration) of
        true ->
            {ok, {#{}, []}};
        false ->
            init_enabled(Configuration)
    end.

init_enabled(Configuration) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Detectors = #{id => otel_resource_detector,
                  start => {otel_resource_detector, start_link, [Configuration]},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [otel_resource_detector]},

    TracerProviderSup = #{id => otel_tracer_provider_sup,
                          start => {otel_tracer_provider_sup, start_link, []},
                          restart => permanent,
                          shutdown => 5000,
                          type => supervisor,
                          modules => [otel_tracer_provider_sup]},

    SpanSup = #{id => otel_span_sup,
                start => {otel_span_sup, start_link, [Configuration]},
                type => supervisor,
                restart => permanent,
                shutdown => infinity,
                modules => [otel_span_sup]},

    %% `SpanSup' starts first so it shuts down last. Its ETS table must remain
    %% alive until all tracer provider processes have stopped.
    ChildSpecs = [SpanSup, Detectors, TracerProviderSup],

    {ok, {SupFlags, ChildSpecs}}.
