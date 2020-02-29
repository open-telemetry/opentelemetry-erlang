%%%------------------------------------------------------------------------
%% Copyright 2020, OpenTelemetry Authors
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
-module(ot_meter_worker).

-behaviour(gen_server).

-export([start_link/1,
         record/4,
         wait/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-include("ot_meter.hrl").

-record(state, {table :: ets:tid()}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

record(Pid, Name, LabelSet, Number) ->
    gen_server:cast(Pid, {record, Name, LabelSet, Number}).

%% use for testing.
%% call this function to know that all previous `record'ings of metrics have been handled
wait(Pid) ->
    gen_server:call(Pid, wait).

init(_Opts) ->
    Tid = ets:new(?MODULE, [protected,
                            {keypos, #instrument.name}]),
    {ok, #state{table=Tid}}.

handle_call(wait, _From, State) ->
    {reply, ok, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast({record, Name, LabelSet, Number}, State) ->
    ot_metric_accumulator:record(Name, LabelSet, Number),
    {noreply, State}.
