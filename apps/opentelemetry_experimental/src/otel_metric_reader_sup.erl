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
-module(otel_metric_reader_sup).

-behaviour(supervisor).

-export([start_link/1,
         start_child/5]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]).

start_child(SupRef, ChildId, ViewAggregationTable, MetricsTable, ReaderConfig) ->
    supervisor:start_child(SupRef, #{id => ChildId,
                                     start => {otel_metric_reader, start_link, [ChildId,
                                                                                ViewAggregationTable,
                                                                                MetricsTable,
                                                                                ReaderConfig]},
                                     type => worker,
                                     restart => permanent,
                                     shutdown => 1000}).

init([Opts]) ->
    Readers = maps:get(readers, Opts, []),

    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    ChildSpecs = [#{id => ChildId,
                    start => {otel_metric_reader, start_link, [ChildId, ReaderConfig]},
                    type => worker,
                    restart => permanent,
                    shutdown => 1000} || #{id := ChildId,
                                           module := otel_metric_reader,
                                           config := ReaderConfig} <- Readers],

    {ok, {SupFlags, ChildSpecs}}.
