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
%% @doc The Accumulator receives metric updates and when `collect' is called
%% it sweeps through the instruments calling `checkpoint' with the define
%% aggregator for each and submitting to the Integrator.
%% @end
%%%-------------------------------------------------------------------------
-module(ot_metric_accumulator).

-behaviour(gen_server).

-export([start_link/1,
         active_table/0,
         record/2,
         record/3,
         observe/3,
         collect/0,
         lookup_active/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_meter.hrl").

-define(ACTIVE_TAB, active_instrument_updates).

-define(active_ms(Name, LabelSet),
        ets:fun2ms(fun(#active_instrument{key=Key,
                                          instrument=#instrument{number_kind=InputType},
                                          aggregator=Aggregator}) when Key =:= {Name, LabelSet} ->
                           {InputType, Aggregator}
                   end)).

-record(state, {}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

active_table() ->
    ?ACTIVE_TAB.

-spec record(ot_meter:bound_instrument(), number()) -> boolean().
record({Key, {InputType, Aggregator}}, Number) ->
    Aggregator:update(?ACTIVE_TAB, Key, InputType, Number);
record(#active_instrument{key=Key,
                          instrument=#instrument{number_kind=InputType},
                          aggregator=Aggregator}, Number) ->
    Aggregator:update(?ACTIVE_TAB, Key, InputType, Number).

-spec record(ot_meter:name(), ot_meter:labels(), number()) -> boolean() | unknown_instrument.
record(Name, LabelSet, Number) ->
    case lookup_active(Name, LabelSet) of
        unknown_instrument ->
            %% an Instrument must exist to create an Active Instrument
            unknown_instrument;
        {InputType, Aggregator} ->
            Aggregator:update(?ACTIVE_TAB, {Name, LabelSet}, InputType, Number)
    end.

observe(#instrument{name=Name}, Number, LabelSet) ->
    _ = lookup_active(Name, LabelSet),
    ot_metric_aggregator_last_value:update(?ACTIVE_TAB, {Name, LabelSet}, observer, Number),
    ok.

-spec lookup_active(instrument() | ot_meter:name(), ot_meter:labels())
                   -> {ot_meter:number_kind(), module()} | unknown_instrument.
lookup_active(Instrument=#instrument{name=Name}, LabelSet) ->
    MatchSpec = ?active_ms(Name, LabelSet),
    case ets:select(?ACTIVE_TAB, MatchSpec) of
        [{InputType, Aggregator}] ->
            {InputType, Aggregator};
        [] ->
            add_active_instrument(Instrument, Name, LabelSet)
    end;
lookup_active(Name, LabelSet) ->
    MatchSpec = ?active_ms(Name, LabelSet),
    case ets:select(?ACTIVE_TAB, MatchSpec) of
        [{InputType, Aggregator}] ->
            {InputType, Aggregator};
        [] ->
            case ot_meter_default:lookup_instrument(Name) of
                unknown_instrument ->
                    unknown_instrument;
                Instrument ->
                    add_active_instrument(Instrument, Name, LabelSet)
            end
    end.

collect()->
    gen_server:call(?MODULE, collect).

init(_Opts) ->
    %% This ETS table is required for other parts to not crash so we create
    %% it in init and not in a handle_continue or whatever else.
    %% No heir is worried about since active metrics are created dynamicly
    _ = ets:new(?ACTIVE_TAB, [named_table,
                              public,
                              {keypos, #active_instrument.key},
                              {write_concurrency, true},
                              ordered_set]),

    {ok, #state{}}.

handle_call(collect, _From, State) ->
    %% TODO: should observers just checkpoint in the first place?
    %% TODO: should have a timeout on observer callbacks
    run_observers(ets:tab2list(ot_meter_default:observer_tab())),

    MS = ets:fun2ms(fun(#active_instrument{key=Key,
                                           aggregator=Aggregator}) ->
                            {Key, Aggregator}
                    end),
    run_checkpoints(ets:select(?ACTIVE_TAB, MS, 20)),

    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%

run_checkpoints('$end_of_table') ->
    ok;
run_checkpoints({Matches, Continuation}) ->
    [Aggregator:checkpoint(?ACTIVE_TAB, Key) || {Key, Aggregator} <- Matches],
    ets:select(Continuation).


run_observers([]) ->
    ok;
run_observers([Observer | Rest]) ->
    run_observer(Observer),
    run_observers(Rest).

run_observer(#observer{instrument=ObserverInstrument,
                       callback=Callback}) ->
    try Callback(ObserverInstrument)
    catch _:_ ->
            %% TODO: log an error
            ok
    end.

aggregator(#instrument{kind=ot_counter}) ->
    ot_metric_aggregator_sum;
aggregator(#instrument{kind=ot_sum_observer}) ->
    ot_metric_aggregator_last_value;
aggregator(#instrument{kind=ot_value_recorder}) ->
    ot_metric_aggregator_mmsc.

add_active_instrument(Instrument=#instrument{number_kind=InputType}, Name, LabelSet) ->
    Aggregator = aggregator(Instrument),
    InitialValue = Aggregator:initial_value(InputType),
    ActiveInstrument = #active_instrument{key={Name, LabelSet},
                                          instrument=Instrument,
                                          aggregator=Aggregator,
                                          current=InitialValue},
    _ = ets:insert_new(?ACTIVE_TAB, ActiveInstrument),
    {InputType, Aggregator}.
