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
-module(ot_meter_default).

-behaviour(ot_meter).
-behaviour(gen_server).

-export([start_link/1,
         new_instruments/2,
         lookup_instrument/1,
         record/4,
         record_batch/3,

         %% functions used for bound instruments
         record/3,
         bind/3,
         release/2,

         %% observer functions
         observer_tab/0,
         register_observer/3,
         set_observer_callback/3,
         observe/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-include("ot_meter.hrl").

-define(OBSERVER_TAB, ot_metric_accumulator_observers).

-define(TAB, ?MODULE).

-record(state, {}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec new_instruments(opentelemetry:meter(), [ot_meter:instrument_opts()]) -> boolean().
new_instruments(_Meter, List) ->
    gen_server:call(?MODULE, {new, List}).

-spec record(opentelemetry:meter(), bound_instrument(), number()) -> ok.
record(_Meter, unknown_instrument, Number) when is_number(Number) ->
    ok;
record(_Meter, BoundInstrument, Number) when is_number(Number) ->
    _ = ot_metric_accumulator:record(BoundInstrument, Number),
    ok;
record(_, _, _) ->
    ok.

-spec record(opentelemetry:meter(), ot_meter:name(), ot_meter:label_set(), number()) -> ok.
record(_Meter, Name, LabelSet, Number) when is_number(Number) ->
    _ = ot_metric_accumulator:record(Name, LabelSet, Number),
    ok;
record(_, _, _, _) ->
    ok.

-spec record_batch(opentelemetry:meter(), [{ot_meter:name(), number()}], ot_meter:label_set()) -> ok.
record_batch(_Meter, Measures, LabelSet) ->
    [ot_metric_accumulator:record(Name, LabelSet, Number) || {Name, Number} <- Measures,  is_number(Number)],
    ok.

-spec release(opentelemetry:meter(), bound_instrument()) -> ok.
release(_Meter, _BoundInstrument) ->
    ok.

-spec bind(opentelemetry:meter(), instrument() | ot_meter:name(), ot_meter:label_set())
          -> bound_instrument().
bind(_Meter, Instrument=#instrument{}, LabelSet) ->
    bind_instrument(Instrument, LabelSet);
bind(_Meter, Name, LabelSet) ->
    case lookup_instrument(Name) of
        unknown_instrument ->
            unknown_instrument;
        Instrument ->
            bind_instrument(Instrument, LabelSet)
    end.

-spec lookup_instrument(ot_meter:name()) -> instrument() | unknown_instrument.
lookup_instrument(Name) ->
    case ets:lookup(?TAB, Name) of
        [Instrument] ->
            Instrument;
        [] ->
            unknown_instrument
    end.

observer_tab() ->
    ?OBSERVER_TAB.

-spec register_observer(opentelemetry:meter(), ot_meter:name(), ot_observer:callback()) -> ok.
register_observer(_Meter, Name, Callback) ->
    case lookup_instrument(Name) of
        unknown_instrument ->
            unknown_instrument;
        Instrument ->
            gen_server:call(?MODULE, {register_observer, Name, Instrument, Callback})
    end.

-spec set_observer_callback(opentelemetry:meter(), ot_meter:name(), ot_observer:callback())
                           -> ok | unknown_instrument.
set_observer_callback(_Meter, Name, Callback) ->
    case lookup_instrument(Name) of
        unknown_instrument ->
            unknown_instrument;
        Instrument ->
            gen_server:call(?MODULE, {register_observer, Name, Instrument, Callback})
    end.

-spec observe(instrument(), number(), ot_meter:label_set()) -> ok.
observe(ObserverInstrument, Number, LabelSet) when is_number(Number) ->
    ot_metric_accumulator:observe(ObserverInstrument, Number, LabelSet),
    ok;
observe(_, _, _) ->
    ok.

init(_Opts) ->
    %% TODO: we do not want to lose instrument and observer tables ever
    %% eventually need to have an heir to take them if this process crashes.
    %% Another option is to just use persistent_term since these things
    %% don't change after creation.

    %% ets table is required for other parts to not crash so we create
    %% it in init and not in a handle_continue or whatever else
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?TAB, [named_table,
                           protected,
                           {read_concurrency, true},
                           {keypos, #instrument.name}
                          ]);
        _ ->
            ok
    end,

    %% observers are stored in a separate table from other instruments
    case ets:info(?OBSERVER_TAB, name) of
        undefined ->
            _ = ets:new(?OBSERVER_TAB, [named_table,
                                        protected,
                                        {keypos, #observer.name}]);
        _ ->
            ok
    end,

    {ok, #state{}}.

handle_call({new, List}, _From, State) ->
    Result = ets:insert_new(?TAB,
      [#instrument{name=Name,
                   description=maps:get(description, I, <<>>),
                   kind=MetricKind,
                   input_type=maps:get(input_type, I, integer),
                   unit=maps:get(unit, I, one),
                   label_keys=maps:get(label_keys, I, [])} || I=#{name := Name,
                                                                  kind := MetricKind} <- List]),
    {reply, Result, State};
handle_call({register_observer, Name, Instrument, Callback}, _From, State) ->
    _ = ets:insert(?OBSERVER_TAB, #observer{name=Name,
                                            instrument={ot_meter_default, Instrument},
                                            callback=Callback}),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% internal

%% TODO: use a counter ref for `sum' and `mmsc' aggregated
%% instruments with `input_type' `integer'?
bind_instrument(Instrument, LabelSet) ->
    ot_metric_accumulator:lookup_active(Instrument, LabelSet).
