%%%------------------------------------------------------------------------
%% Copyright 2019, OpenTelemetry Authors
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
%%%-------------------------------------------------------------------------
-module(opentelemetry_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).

init([#{sdk_disabled := true}]) ->
    {ok, {#{}, []}};
init([Opts]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Detectors =  #{id => otel_resource_detector,
                   start => {otel_resource_detector, start_link, [Opts]},
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
                start => {otel_span_sup, start_link, [Opts]},
                type => supervisor,
                restart => permanent,
                shutdown => infinity,
                modules => [otel_span_sup]},

    %% `SpanSup' should be the last to shutdown so the ETS table lives until the end
    %% the `TracerServer' process
    ChildSpecs = [SpanSup, Detectors, TracerProviderSup],

    {ok, {SupFlags, ChildSpecs}}.
