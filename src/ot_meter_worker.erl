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
         record/3,
         record/4,
         wait/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include("ot_meter.hrl").

-record(state, {table :: ets:tab()}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% a cast is just a send wrapped in a try/catch in case the
%% atom name isn't registered. Since we are working with
%% pids we don't need that `try' and just use `!'
record(Pid, LabelSet, Measures) when is_list(Measures) ->
    Pid ! {record, LabelSet, Measures}.

record(Pid, Name, LabelSet, Number) ->
    Pid ! {record, Name, LabelSet, Number}.

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

handle_cast(_, State) ->
    {noreply, State}.

handle_info({record, LabelSet, Measures}, State) ->
    [ot_metric_accumulator:record(Name, LabelSet, Number) || {Name, Number} <- Measures],
    {noreply, State};
handle_info({record, Name, LabelSet, Number}, State) ->
    ot_metric_accumulator:record(Name, LabelSet, Number),
    {noreply, State}.
