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
         lookup/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

%% -include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_meter.hrl").

-define(TAB, ?MODULE).

-record(state, {}).

-spec new_instruments(opentelemetry:meter(), [ot_meter:instrument_opts()]) -> boolean().
new_instruments(_Meter, List) ->
    gen_server:call(?MODULE, {new, List}).

-spec labels(opentelemetry:meter(), list() | map()) -> ot_meter:label_set().
labels(_Meter, Labels) ->
    Labels.

-spec record(opentelemetry:meter(), ot_meter:bound_instrument(), number()) -> ok.
record(Meter, BoundInstrument, Number) ->
    ok.

-spec record(opentelemetry:meter(), ot_meter:name(), number(), ot_meter:label_set()) -> ok.
record(_Meter, Name, Number, LabelSet) ->
    ot_metric_accumulator:record(Name, LabelSet, Number).

-spec record_batch(opentelemetry:meter(), [{ot_meter:name(), number()}], ot_meter:label_set()) -> ok.
record_batch(_Meter, Measures, LabelSet) ->
    ok.

-spec release(opentelemetry:meter(), ot_meter:bound_instrument()) -> ok.
release(Meter, BoundInstrument) ->
    ok.

-spec bind(opentelemetry:meter(), instrument(), ot_meter:label_set()) -> ot_meter:bound_instrument().
bind(Meter={Module, _}, Instrument, LabelSet) ->
    Module:bind(Meter, Instrument, LabelSet).

-spec lookup(ot_meter:name()) -> instrument() | unknown_instrument.
lookup(Name) ->
    case ets:lookup(?TAB, Name) of
        [Instrument] ->
            Instrument;
        [] ->
            unknown_instrument
    end.

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(_Opts) ->
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

    {ok, #state{}}.

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

%%
