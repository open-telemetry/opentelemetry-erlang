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
         labels/2,
         record/3,
         record/4,
         record_batch/3,
         release/2,
         bind/3,
         lookup/1,
         set_observer_callback/3,
         update_observer/3,
         wait/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

%% -include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_meter.hrl").

-define(TAB, ?MODULE).
-define(WORKERS, {?MODULE, workers}).
-define(WORKER, element(erlang:system_info(scheduler_id), persistent_term:get(?WORKERS))).

-record(state, {refs :: [reference()],
                worker_opts :: term()}).

-spec new_instruments(opentelemetry:meter(), [ot_meter:instrument_opts()]) -> boolean().
new_instruments(_Meter, List) ->
    gen_server:call(?MODULE, {new, List}).

-spec labels(opentelemetry:meter(), list() | map()) -> ot_meter:label_set().
labels(_Meter, Labels) ->
    Labels.

-spec record(opentelemetry:meter(), ot_meter:bound_instrument(), number()) -> boolean().
record(Meter, BoundInstrument, Number) ->
    ok.

-spec record(opentelemetry:meter(), ot_meter:name(), ot_meter:label_set(), number()) -> boolean().
record(_Meter, Name, LabelSet, Number) ->
    ot_meter_worker:record(?WORKER, Name, LabelSet, Number).
    %% ot_metric_accumulator:record(Name, LabelSet, Number).

-spec record_batch(opentelemetry:meter(), [{ot_meter:name(), number()}], ot_meter:label_set()) -> boolean().
record_batch(_Meter, Measures, LabelSet) ->
    ok.

-spec release(opentelemetry:meter(), ot_meter:bound_instrument()) -> ok.
release(Meter, BoundInstrument) ->
    ok.

-spec bind(opentelemetry:meter(), instrument(), ot_meter:label_set()) -> ot_meter:bound_instrument().
bind(Meter, Instrument, LabelSet) ->
    bind_instrument(Meter, Instrument, LabelSet).

-spec lookup(ot_meter:name()) -> instrument() | unknown_instrument.
lookup(Name) ->
    case ets:lookup(?TAB, Name) of
        [Instrument] ->
            Instrument;
        [] ->
            unknown_instrument
    end.

-spec set_observer_callback(opentelemetry:meter(), ot_meter:name(), ot_observer:callback()) -> boolean().
set_observer_callback(Meter={Module, _}, Observer, Callback) ->
    true.

-spec update_observer(ot_observer:observer_result(), number(), ot_meter:label_set()) -> ok.
update_observer(ObserverResult={Module, _}, Number, LabelSet) ->
    ok.

%% use for testing.
%% call this function to know that all previous `record'ings of metrics have been handled
wait() ->
    Workers = persistent_term:get({?MODULE, workers}),
    [ot_meter_worker:wait(W) || W <- tuple_to_list(Workers)].

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(Opts) ->
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

    erlang:process_flag(trap_exit, true),
    Refs = start_children(Opts),
    persistent_term:put(?WORKERS, list_to_tuple(Refs)),

    {ok, #state{refs=Refs,
                worker_opts=Opts}}.

handle_call({new, List}, _From, State) ->
    Result = ets:insert_new(?TAB,
      [#instrument{name=Name,
                   description=maps:get(description, I, <<>>),
                   kind=MetricKind,
                   input_type=maps:get(input_type, I, integer),
                   unit=maps:get(unit, I, one),
                   label_keys=maps:get(label_keys, I, []),
                   mode=maps:get(monotonic, I, monotonic),
                   numeric_type=maps:get(absolute, I, true)} || I=#{name := Name,
                                                                    kind := MetricKind} <- List]),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', FromPid, _Reason}, State=#state{refs=Refs}) ->
    self() ! update_workers,
    {noreply, State#state{refs=lists:delete(FromPid, Refs)}};
handle_info(update_workers, State=#state{refs=Refs,
                                         worker_opts=Opts}) ->
    flush_update_workers(),
    case erlang:system_info(schedulers) - length(Refs) of
        N when N > 0 ->
            Refs1 = start_children(N, Opts)++Refs,
            persistent_term:put(?WORKERS, list_to_tuple(Refs1)),
            {noreply, State#state{refs=Refs1}};
        _ ->
            {noreply, State}
    end.

%%

bind_instrument(_Meter, Instrument=#instrument{kind=counter}, LabelSet) ->
    bind_counter(Instrument, LabelSet);
bind_instrument(_Meter, Instrument=#instrument{kind=measure}, LabelSet) ->
    bind_measure(Instrument, LabelSet).

bind_counter(Instrument=#instrument{input_type=integer}, LabelSet) ->
    %% TODO: use a counter ref here instead
    ot_metric_accumulator:lookup_counter(Instrument, LabelSet);
bind_counter(Instrument=#instrument{input_type=float}, LabelSet) ->
    ot_metric_accumulator:lookup_counter(Instrument, LabelSet).

bind_measure(Instrument, LabelSet) ->
    ot_metric_accumulator:lookup_measure(Instrument, LabelSet).

%% internal

flush_update_workers() ->
    receive
        update_workers ->
            flush_update_workers()
    after
        0 ->
            ok
    end.

start_children(Opts) ->
    start_children(erlang:system_info(schedulers), Opts).

start_children(N, Opts) ->
    lists:map(fun(_) ->
                      {ok, Pid} = ot_meter_worker:start_link(Opts),
                      Pid
              end, lists:seq(1, N)).
