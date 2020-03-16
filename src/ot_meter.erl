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
-module(ot_meter).

-callback new_instruments(opentelemetry:meter(), [instrument_opts()]) -> boolean().

-callback record(opentelemetry:meter(), term (), number()) -> ok.
-callback record(opentelemetry:meter(), name(), label_set(), number()) -> ok.

-callback record_batch(opentelemetry:meter(), [{instrument(), number()}], label_set()) -> ok.

-callback bind(opentelemetry:meter(), instrument(), label_set()) -> term().
-callback release(opentelemetry:meter(), term()) -> ok.

-callback register_observer(opentelemetry:meter(), ot_meter:name(), ot_observer:callback()) -> ok | unknown_instrument.
-callback set_observer_callback(opentelemetry:meter(), ot_meter:name(), ot_observer:callback()) -> ok | unknown_instrument.

-callback observe(ot_observer:observer_result(), number(), label_set()) -> ok.

-export([new_instruments/2,
         bind/3,
         release/1,
         record/2,
         record/4,
         record_batch/3,
         register_observer/3,
         set_observer_callback/3,
         observe/3]).

-type label_key() :: unicode:unicode_binary().
-type label_value() :: unicode:unicode_binary().
-type label_set() :: [{label_key(), label_value()}].

-type name() :: unicode:unicode_binary().
-type key() :: {name(), label_set()}.
-type description() :: unicode:unicode_binary().
-type instrument_kind() :: counter | observer | measure.
-type unit() :: atom().
-type input_type() :: integer | float.

-type instrument_opts() :: #{name := name(),
                             description => description(),
                             kind := instrument_kind(),
                             type => input_type(),
                             label_keys => [label_key()],
                             unit => unit()}.

-type instrument() :: term().
-type bound_instrument() :: {opentelemetry:meter(), term()}.

-type measurement() :: {bound_instrument() | name(), number()}.

-export_type([key/0,
              name/0,
              description/0,
              instrument_kind/0,
              input_type/0,
              unit/0,
              measurement/0,
              label_set/0]).

-spec new_instruments(opentelemetry:meter(), [instrument_opts()]) -> boolean().
new_instruments(Meter={Module, _}, List) ->
    Module:new_instruments(Meter, List).

-spec bind(opentelemetry:meter(), name(), label_set()) -> bound_instrument().
bind(Meter={Module, _}, Name, LabelSet) ->
    {Meter, Module:bind(Meter, Name, LabelSet)}.

-spec release(bound_instrument()) -> ok.
release({Meter={Module, _}, BoundInstrument}) ->
    Module:release(Meter, BoundInstrument).

-spec record(opentelemetry:meter(), name(), number(), label_set()) -> ok.
record(Meter={Module, _}, Name, Number, LabelSet) ->
    Module:record(Meter, Name, LabelSet, Number).

-spec record(bound_instrument(), number()) -> ok.
record({Meter={Module, _}, BoundInstrument}, Number) ->
    Module:record(Meter, BoundInstrument, Number).

-spec record_batch(opentelemetry:meter(), label_set(), [measurement()]) -> ok.
record_batch(Meter={Module, _}, LabelSet, Measurements) ->
    Module:record_batch(Meter, LabelSet, Measurements).

-spec register_observer(opentelemetry:meter(), ot_meter:name(), ot_observer:callback())
                       -> ok | unknown_instrument.
register_observer(Meter={Module, _}, Name, Callback) ->
    Module:register_observer(Meter, Name, Callback).

-spec set_observer_callback(opentelemetry:meter(), ot_meter:name(), ot_observer:callback())
                           -> ok | unknown_instrument.
set_observer_callback(Meter={Module, _}, Name, Callback) ->
    Module:set_observer_callback(Meter, Name, Callback).

-spec observe(ot_observer:observer_result(), number(), label_set()) -> ok.
observe({Module, Instrument}, Number, LabelSet) ->
    Module:observe(Instrument, Number, LabelSet).
