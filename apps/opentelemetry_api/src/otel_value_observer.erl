%%%------------------------------------------------------------------------
%% Copyright 2020, OpenTelemetry Authors
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
-module(otel_value_observer).

-behaviour(otel_instrument).
-behaviour(otel_observer).

-export([new/2,
         new/3,
         definition/1,
         definition/2,
         set_callback/3,
         observe/3]).

-include("otel_meter.hrl").

-define(PROPERTIES, #{monotonic => false,
                      synchronous => false}).

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

-spec set_callback(opentelemetry:meter(), otel_meter:name(), otel_observer:callback()) -> ok.
set_callback(Meter, Observer, Callback) ->
    otel_meter:set_observer_callback(Meter, Observer, Callback).

-spec observe(otel_observer:instrument(), number(), otel_meter:labels()) -> ok.
observe(ObserverInstrument, Number, LabelSet) ->
    otel_meter:observe(ObserverInstrument, Number, LabelSet).
