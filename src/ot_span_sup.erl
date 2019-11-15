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
-module(ot_span_sup).

-behaviour(supervisor).

-export([start_link/1,
         start_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).

start_child(ChildSpec) ->
    supervisor:start_child(?SERVER, ChildSpec).

init([Opts]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    SweeperOpts = proplists:get_value(sweeper, Opts, #{}),
    Sweeper = #{id => ot_span_sweeper,
                start => {ot_span_sweeper, start_link, [SweeperOpts]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [ot_span_sweeper]},

    SpanHandler = #{id => ot_span_ets,
                    start => {ot_span_ets, start_link, [[]]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [ot_span_ets]},

    ChildSpecs = [SpanHandler, Sweeper],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
