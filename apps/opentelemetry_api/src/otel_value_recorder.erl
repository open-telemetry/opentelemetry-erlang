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
-module(otel_value_recorder).

-behaviour(otel_instrument).

-export([new/2,
         new/3,
         definition/1,
         definition/2,
         record/2,
         record/4,
         measurement/2]).

-include("otel_meter.hrl").

-define(PROPERTIES, #{monotonic => false,
                      synchronous => true}).

-spec new(opentelemetry:meter(), otel_meter:name()) -> boolean().
new(Meter, Name) ->
    new(Meter, Name, #{}).

-spec new(opentelemetry:meter(), otel_meter:name(), otel_meter:instrument_opts()) -> boolean().
new(Meter, Name, Opts) ->
    otel_meter:new_instrument(Meter, Name, ?MODULE, Opts).

-spec definition(otel_meter:name()) -> otel_meter:instrument_definition().
definition(Name) ->
    definition(Name, #{}).

-spec definition(otel_meter:name(), otel_meter:instrument_opts()) -> otel_meter:instrument_definition().
definition(Name, Opts) ->
    otel_meter:instrument_definition(?MODULE, Name, ?PROPERTIES, Opts).

-spec record(otel_meter:bound_instrument(), number()) -> ok.
record(BoundInstrument, Number) ->
    otel_meter:record(BoundInstrument, Number).

-spec record(opentelemetry:meter(), otel_meter:name(), number(), otel_meter:labels()) -> ok.
record(Meter, Name, Number, LabelSet) ->
    otel_meter:record(Meter, Name, Number, LabelSet).

-spec measurement(otel_meter:bound_instrument() | otel_meter:name(), number())
                 -> {otel_meter:bound_instrument() | otel_meter:name(), number()}.
measurement(NameOrInstrument, Number) ->
    {NameOrInstrument, Number}.
