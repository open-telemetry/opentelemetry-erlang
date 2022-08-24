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
%% @doc `otel_meter' is responsible for creating Instruments. An Instrument
%% is just a record so calling the creation function has no side effects.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_meter).

-export([counter/4,
         observable_counter/5,
         histogram/4,
         observable_gauge/5,
         updown_counter/4,
         observable_updowncounter/5,

         instrument/5,
         instrument/6]).

-include("otel_metrics.hrl").

-callback instrument(Meter, Name, Kind, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Kind :: otel_instrument:kind(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
-callback instrument(Meter, Name, Kind, ValueType, Callback, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Kind :: otel_instrument:kind(),
      ValueType :: otel_instrument:value_type(),
      Callback :: otel_instrument:callback(),
      Opts :: otel_meter:opts().

-type opts() :: #{description => otel_instrument:description(),
                  unit => otel_instrument:unit()}.

-type t() :: {module(), term()}.

-export_type([t/0,
              opts/0]).

-spec counter(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
counter(Meter, Name, ValueType, Opts) ->
    instrument(Meter, Name, ?KIND_COUNTER, ValueType, Opts).

-spec observable_counter(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: otel_instrument:callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
observable_counter(Meter, Name, Callback, ValueType, Opts) ->
    instrument(Meter, Name, ?KIND_OBSERVABLE_COUNTER, Callback, ValueType, Opts).

-spec histogram(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
histogram(Meter, Name, ValueType, Opts) ->
    instrument(Meter, Name, ?KIND_HISTOGRAM, ValueType, Opts).

-spec observable_gauge(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: otel_instrument:callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
observable_gauge(Meter, Name, Callback, ValueType, Opts) ->
    instrument(Meter, Name, ?KIND_OBSERVABLE_GAUGE, Callback, ValueType, Opts).

-spec updown_counter(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
updown_counter(Meter, Name, ValueType, Opts) ->
    instrument(Meter, Name, ?KIND_UPDOWN_COUNTER, ValueType, Opts).

-spec observable_updowncounter(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: otel_instrument:callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
observable_updowncounter(Meter, Name, Callback, ValueType, Opts) ->
    instrument(Meter, Name, ?KIND_OBSERVABLE_COUNTER, Callback, ValueType, Opts).

-spec instrument(Meter, Name, Kind, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Kind :: otel_instrument:kind(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
instrument(Meter={Module, _}, Name, Kind, ValueType, Opts) ->
    Module:instrument(Meter, Name, Kind, ValueType, Opts).

-spec instrument(Meter, Name, Kind, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Kind :: otel_instrument:kind(),
      Callback :: otel_instrument:callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
instrument(Meter={Module, _}, Name, Kind, ValueType, Callback, Opts) ->
    Module:instrument(Meter, Name, Kind, ValueType, Callback, Opts).
