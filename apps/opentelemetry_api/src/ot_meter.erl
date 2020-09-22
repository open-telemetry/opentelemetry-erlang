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

-include("meter.hrl").

-callback new_instrument(opentelemetry:meter(), name(), instrument_kind(), instrument_opts()) -> boolean().
-callback new_instruments(opentelemetry:meter(), [instrument_opts()]) -> boolean().

-callback record(opentelemetry:meter(), term (), number()) -> ok.
-callback record(opentelemetry:meter(), name(), labels(), number()) -> ok.

-callback record_batch(opentelemetry:meter(), [{instrument(), number()}], labels()) -> ok.

-callback bind(opentelemetry:meter(), instrument(), labels()) -> term().
-callback release(opentelemetry:meter(), term()) -> ok.

-callback register_observer(opentelemetry:meter(), ot_meter:name(), ot_observer:callback()) -> ok | unknown_instrument.
-callback set_observer_callback(opentelemetry:meter(), ot_meter:name(), ot_observer:callback()) -> ok | unknown_instrument.

-callback observe(ot_observer:observer_result(), number(), labels()) -> ok.

-export([new_instrument/4,
         new_instruments/2,
         instrument_definition/4,
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
-type label() :: {label_key(), label_value()}.
-type labels() :: [label()].

-type name() :: unicode:unicode_binary().
-type description() :: unicode:unicode_binary().
-type instrument_kind() :: module().
-type unit() :: atom().
-type number_kind() :: integer | float.

-type instrument_config() :: #{description => description(),
                               number_kind => number_kind(),
                               unit        => unit(),
                               monotonic   := boolean(),
                               synchronous := boolean()}.

-type instrument_properties() :: #{monotonic   := boolean(),
                                   synchronous := boolean()}.

-type instrument_opts() :: #{description => description(),
                             number_kind => number_kind(),
                             unit        => unit()}.

-type instrument_definition() :: {name(), instrument_kind(), instrument_opts()}.
-type instrument() :: term().
-type bound_instrument() :: {opentelemetry:meter(), term()}.

-type measurement() :: {bound_instrument() | name(), number()}.

-export_type([name/0,
              description/0,
              instrument_kind/0,
              instrument_config/0,
              instrument_opts/0,
              number_kind/0,
              unit/0,
              measurement/0,
              labels/0]).

-spec new_instrument(opentelemetry:meter(), name(), instrument_kind(), instrument_opts()) -> boolean().
new_instrument(Meter={Module, _}, Name, InstrumentKind, InstrumentOpts) ->
    Module:new_instrument(Meter, Name, InstrumentKind, InstrumentOpts).

-spec new_instruments(opentelemetry:meter(), [instrument_definition()]) -> boolean().
new_instruments(Meter={Module, _}, List) ->
    Module:new_instruments(Meter, List).

-spec instrument_definition(module(), name(), instrument_properties(), instrument_opts()) -> instrument_definition().
instrument_definition(InstrumentModule, Name, Properties, Opts) ->
    %% instrument config values are not allowed to be overridden so in case the user
    %% attempts to pass as an optiion this merge will use the config value
    {Name, InstrumentModule, maps:merge(Opts, Properties)}.

-spec bind(opentelemetry:meter(), name(), labels()) -> bound_instrument().
bind(Meter={Module, _}, Name, Labels) ->
    {Meter, Module:bind(Meter, Name, Labels)}.

-spec release(bound_instrument()) -> ok.
release({Meter={Module, _}, BoundInstrument}) ->
    Module:release(Meter, BoundInstrument).

-spec record(opentelemetry:meter(), name(), number(), labels()) -> ok.
record(Meter={Module, _}, Name, Number, Labels) ->
    Module:record(Meter, Name, Labels, Number).

-spec record(bound_instrument(), number()) -> ok.
record({Meter={Module, _}, BoundInstrument}, Number) ->
    Module:record(Meter, BoundInstrument, Number).

-spec record_batch(opentelemetry:meter(), labels(), [measurement()]) -> ok.
record_batch(Meter={Module, _}, Labels, Measurements) ->
    Module:record_batch(Meter, Labels, Measurements).

-spec register_observer(opentelemetry:meter(), ot_meter:name(), ot_observer:callback())
                       -> ok | unknown_instrument.
register_observer(Meter={Module, _}, Name, Callback) ->
    Module:register_observer(Meter, Name, Callback).

-spec set_observer_callback(opentelemetry:meter(), ot_meter:name(), ot_observer:callback())
                           -> ok | unknown_instrument.
set_observer_callback(Meter={Module, _}, Name, Callback) ->
    Module:set_observer_callback(Meter, Name, Callback).

-spec observe(ot_observer:observer_result(), number(), labels()) -> ok.
observe({Module, Instrument}, Number, Labels) ->
    Module:observe(Instrument, Number, Labels).
