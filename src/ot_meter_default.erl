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
         new_instrument/4,
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
-include_lib("kernel/include/logger.hrl").

-define(OBSERVER_TAB, ot_metric_accumulator_observers).

-define(TAB, ?MODULE).

-record(state, {}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec new_instrument(opentelemetry:meter(), ot_meter:name(), ot_meter:instrument_kind(), ot_meter:instrument_opts()) -> boolean().
new_instrument(_Meter, Name, InstrumentKind, Opts) ->
    gen_server:call(?MODULE, {new, Name, InstrumentKind, Opts}).

%% @doc returns `true' if any instrument in the list is successfully created
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

handle_call({new, Name, InstrumentKind, Opts}, _From, State) ->
    Result = insert_new_instrument(Name, InstrumentKind, Opts),
    {reply, Result, State};
handle_call({new, List}, _From, State) ->
    Result = insert_new_instruments(List),
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

insert_new_instrument(Name, InstrumentKind, Opts) ->
    case instrument(Name, InstrumentKind, Opts) of
        {error, kind_not_a_module} ->
            false;
        Instrument ->
            insert_new(Instrument)
    end.

%% Insert each individually so we can log more useful error messages.
%% Instruments should all be created once at the start of an application
%% so the performance isn't a concern here.
insert_new_instruments(List) when is_list(List) ->
    %% use foldl to track if any insert has failed and return true only if
    %% none fails
    lists:foldl(fun({Name, InstrumentKind, Opts}, Acc) ->
                        insert_new_instrument(Name, InstrumentKind, Opts) andalso Acc;
                   (X, _Acc) ->
                        ?LOG_INFO("Unable to create instrument from argument ~p. "
                                  "Format must be {Name, InstrumentKind, Opts}.", [X]),
                        false
                end, true, List);
insert_new_instruments(_) ->
    false.

insert_new(Instrument=#instrument{name=Name}) ->
    try
        ets:insert_new(?TAB, Instrument)
    catch
        C:T:S ->
            ?LOG_INFO("Unable to create instrument.", #{instrument_name => Name,
                                                        class => C,
                                                        exception => T,
                                                        stacktrace => S}),
            false
    end.

instrument(Name, InstrumentKind, InstrumentConfig) ->
    %% InstrumentKind must be a module that implements `ot_instrument'
    try InstrumentKind:module_info() of
        _ ->
            #instrument{name=Name,
                        description=maps:get(description, InstrumentConfig, <<>>),
                        kind=InstrumentKind,
                        number_kind=maps:get(number_kind, InstrumentConfig, integer),
                        unit=maps:get(unit, InstrumentConfig, one),
                        monotonic=maps:get(monotonic, InstrumentConfig),
                        synchronous=maps:get(synchronous, InstrumentConfig)}
    catch
        error:undef ->
            ?LOG_INFO("Unable to create instrument kind because the kind must be a module.",
                      #{instrument_name => Name,
                        instrument_kind => InstrumentKind}),
            {error, kind_not_a_module}
    end.
