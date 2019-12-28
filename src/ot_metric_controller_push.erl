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
%%
%% @end
%%%-------------------------------------------------------------------------
-module(ot_metric_controller_push).

-behaviour(gen_statem).

-export([start_link/1]).

-export([init/1,
         callback_mode/0,
         handle_event/4,
         terminate/3]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-record(data, {interval :: integer()}).

start_link(Opts) ->
    gen_statem:start_link(?MODULE, Opts, []).

init(Opts) ->
    erlang:process_flag(trap_exit, true),
    Interval = maps:get(pusher_interval, Opts, timer:seconds(1)),
    {ok, ready, #data{interval=Interval}, [push_timer(Interval)]}.

callback_mode() ->
    handle_event_function.

handle_event({timeout, push_timer}, push, ready, Data) ->
    do_push(),
    {keep_state_and_data, [push_timer(Data)]}.

terminate(_, _, _) ->
    %% one last push
    do_push(),
    ok.

%%

push_timer(#data{interval=Interval}) ->
    push_timer(Interval);
push_timer(Interval) ->
    {{timeout, push_timer}, Interval, push}.

do_push() ->
    ot_metric_accumulator:collect(),
    Records = ot_metric_integrator:read(),
    [ot_metric_exporter:export(R) || R <- Records],
    ok.
