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
-module(ot_value_recorder).

-behaviour(ot_instrument).

-export([new/2,
         new/3,
         definition/1,
         definition/2,
         record/2,
         record/4,
         measurement/2]).

-include("meter.hrl").

-define(PROPERTIES, #{monotonic => false,
                      synchronous => true}).

-spec new(opentelemetry:meter(), ot_meter:name()) -> boolean().
new(Meter, Name) ->
    new(Meter, Name, #{}).

-spec new(opentelemetry:meter(), ot_meter:name(), ot_meter:instrument_opts()) -> boolean().
new(Meter, Name, Opts) ->
    ot_meter:new_instrument(Meter, Name, ?MODULE, Opts).

-spec definition(ot_meter:name()) -> ot_meter:instrument_definition().
definition(Name) ->
    definition(Name, #{}).

-spec definition(ot_meter:name(), ot_meter:instrument_opts()) -> ot_meter:instrument_definition().
definition(Name, Opts) ->
    ot_meter:instrument_definition(?MODULE, Name, ?PROPERTIES, Opts).

-spec record(ot_meter:bound_instrument(), number()) -> ok.
record(BoundInstrument, Number) ->
    ot_meter:record(BoundInstrument, Number).

-spec record(opentelemetry:meter(), ot_meter:name(), number(), ot_meter:label_set()) -> ok.
record(Meter, Name, Number, LabelSet) ->
    ot_meter:record(Meter, Name, Number, LabelSet).

-spec measurement(ot_meter:bound_instrument() | ot_meter:name(), number())
                 -> {ot_meter:bound_instrument() | ot_meter:name(), number()}.
measurement(NameOrInstrument, Number) ->
    {NameOrInstrument, Number}.
