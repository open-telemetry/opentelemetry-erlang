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
         collect/0,
         lookup_counter/2,
         lookup_observer/2,
         lookup_measure/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_meter.hrl").

-define(TAB, ?MODULE).

-record(state, {}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

active_table() ->
    ?ACTIVE_TAB.

-spec record(ot_meter:bound_instrument(), number()) -> boolean().
record(Active=#active_instrument{instrument=#instrument{input_type=InputType},
                                 aggregator=Aggregator}, Number) ->
    Aggregator:update(?ACTIVE_TAB, InputType, Active, Number).

-spec record(ot_meter:name(), ot_meter:label_set(), number()) -> boolean() | unknown_instrument.
record(Name, LabelSet, Number) ->
    case lookup(Name, LabelSet) of
        unknown_instrument ->
            unknown_instrument;
        Active=#active_instrument{instrument=#instrument{input_type=InputType},
                                  aggregator=Aggregator} ->
            Aggregator:update(?ACTIVE_TAB, InputType, Active, Number)
    end.

lookup_counter(Instrument=#instrument{name=Name}, LabelSet) ->
    case ets:lookup(?ACTIVE_TAB, {Name, LabelSet}) of
        [ActiveInstrument] ->
            ActiveInstrument;
        [] ->
            add_active_instrument(Instrument, Name, LabelSet)
    end.

lookup_observer(Instrument=#instrument{name=Name}, LabelSet) ->
    case ets:lookup(?ACTIVE_TAB, {Name, LabelSet}) of
        [ActiveInstrument] ->
            ActiveInstrument;
        [] ->
            add_active_instrument(Instrument, Name, LabelSet)
    end.

lookup_measure(Instrument=#instrument{name=Name}, LabelSet) ->
    case ets:lookup(?ACTIVE_TAB, {Name, LabelSet}) of
        [ActiveInstrument] ->
            ActiveInstrument;
        [] ->
            add_active_instrument(Instrument, Name, LabelSet)
    end.

collect()->
    gen_server:call(?MODULE, collect).

init(_Opts) ->
    %% ets table is required for other parts to not crash so we create
    %% it in init and not in a handle_continue or whatever else
    case ets:info(?TAB, name) of
        undefined ->
            _ = ets:new(?TAB, [named_table,
                               public,
                               {read_concurrency, true},
                               {keypos, #active_instrument.key}]),
            _ = ets:new(?ACTIVE_TAB, [named_table,
                                      public,
                                      {keypos, #active_instrument.key},
                                      {write_concurrency, true},
                                      ordered_set]);
        _ ->
            ok
    end,

    {ok, #state{}}.

handle_call(collect, _From, State) ->
    ot_metric_aggregator_counter:checkpoint(?ACTIVE_TAB),
    ot_metric_aggregator_observer:checkpoint(?ACTIVE_TAB),
    %% ot_metric_aggregator_measure:checkpoint(?ACTIVE_MEASURE_TAB),
    ot_metric_aggregator_mmsc:checkpoint(?ACTIVE_TAB),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%

aggregator(#instrument{kind=counter}) ->
    ot_metric_aggregator_counter;
aggregator(#instrument{kind=measure}) ->
    ot_metric_aggregator_mmsc.
    %% ot_metric_aggregator_measure.

lookup(Name, LabelSet) ->
    case ets:lookup(?ACTIVE_TAB, {Name, LabelSet}) of
        [ActiveInstrument] ->
            ActiveInstrument;
        [] ->
            case ot_meter_default:lookup(Name) of
                unknown_instrument ->
                    unknown_instrument;
                Instrument ->
                    add_active_instrument(Instrument, Name, LabelSet)
            end
    end.

add_active_instrument(Instrument=#instrument{input_type=InputType}, Name, LabelSet) ->
    Aggregator = aggregator(Instrument),
    DefaultValue = default_value(InputType),
    #active_instrument{key={Name, LabelSet},
                       instrument=Instrument,
                       aggregator=Aggregator,
                       value=DefaultValue}.

-spec default_value(ot_meter:input_type()) -> integer() | float().
default_value(integer) -> 0;
default_value(float) -> 0.0.
