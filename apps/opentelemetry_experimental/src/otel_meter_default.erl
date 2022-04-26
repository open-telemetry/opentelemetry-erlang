%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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
-module(otel_meter_default).

-behaviour(otel_meter).

-export([create_counter/4,
         create_observable_counter/5,
         create_histogram/4,
         create_observable_gauge/5,
         create_updown_counter/4,
         create_observable_updowncounter/5]).

%% also act as default version of instruments
-export([add/3,
         record/3]).

-spec create_counter(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
create_counter(Meter, Name, ValueType, Opts) ->
    default_instrument(Meter, counter, Name, ValueType, Opts).

-spec create_observable_counter(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      Callback :: otel_meter:callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
create_observable_counter(Meter, Name, _Callback, ValueType, Opts) ->
    default_instrument(Meter, observable_counter, Name, ValueType, Opts).

-spec create_histogram(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
create_histogram(Meter, Name, ValueType, Opts) ->
    default_instrument(Meter, histogram, Name, ValueType, Opts).

-spec create_observable_gauge(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      Callback :: otel_meter:callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
create_observable_gauge(Meter, Name, _Callback, ValueType, Opts) ->
    default_instrument(Meter, observable_gauge, Name, ValueType, Opts).

-spec create_updown_counter(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
create_updown_counter(Meter, Name, ValueType, Opts) ->
    default_instrument(Meter, updown_counter, Name, ValueType, Opts).

-spec create_observable_updowncounter(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      Callback :: otel_meter:callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
create_observable_updowncounter(Meter, Name, _Callback, ValueType, Opts) ->
    default_instrument(Meter, observable_updowncounter, Name, ValueType, Opts).

%%

%% handles both default counter and default updown counter
add(Instrument, Number, Attributes) ->
    otel_meter_server:record(Instrument, Number, Attributes).

record(Instrument, Number, Attributes) ->
    otel_meter_server:record(Instrument, Number, Attributes).

%%

default_instrument(Meter, Kind, Name, ValueType, Opts) ->
    Instrument = otel_instrument:new(?MODULE, Meter, Kind, Name, maps:get(description, Opts, undefined),
                                     maps:get(unit, Opts, undefined), ValueType),
    otel_meter_server:add_instrument(Instrument),
    Instrument.
