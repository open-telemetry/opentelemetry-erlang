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
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(opentelemetry_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Opts = application:get_all_env(opentelemetry),

    %% if the span impl needs to have a process supervised it must be
    %% setup after the supervision tree has started.
    {sampler, {Sampler, SamplerOpts}} = lists:keyfind(sampler, 1, Opts),
    SamplerFun = ot_sampler:setup(Sampler, SamplerOpts),

    %% if the span impl needs to have a process supervised it must be
    %% setup after the supervision tree has started.
    {tracer, {Tracer, TracerOpts}} = lists:keyfind(tracer, 1, Opts),
    TracerChildren = ot_tracer:setup(Tracer, TracerOpts, SamplerFun),

    opentelemetry_sup:start_link(TracerChildren, Opts).

stop(_State) ->
    ok.

%% internal functions
