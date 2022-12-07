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
%% @doc `otel_meter' is responsible for creating Instruments.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_meter).

-export([create_counter/4,
         create_histogram/4,
         create_updown_counter/4,

         create_observable_counter/6,
         create_observable_gauge/6,
         create_observable_updowncounter/6,

         scope/1,

         register_callback/4,

         instrument/7,
         lookup_instrument/2,

         record/4]).

-include("otel_metrics.hrl").

-callback create_instrument(Meter, Name, Kind, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Kind :: otel_instrument:kind(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().

-callback create_instrument(Meter, Name, Kind, ValueType, Callback, CallbackArgs, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Kind :: otel_instrument:kind(),
      ValueType :: otel_instrument:value_type(),
      Callback :: otel_instrument:callback(),
      CallbackArgs :: term(),
      Opts :: otel_meter:opts().

-callback register_callback(Meter, Instruments, Callback, CallbackArgs) -> ok when
      Meter :: t(),
      Instruments :: otel_instrument:t(),
      Callback :: otel_instrument:callback(),
      CallbackArgs :: term().

-type opts() :: #{description => otel_instrument:description(),
                  unit => otel_instrument:unit()}.

-type t() :: {module(), term()}.

-export_type([t/0,
              opts/0]).

-spec create_counter(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_counter(Meter, Name, ValueType, Opts) ->
    create_instrument(Meter, Name, ?KIND_COUNTER, ValueType, Opts).

-spec create_updown_counter(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_updown_counter(Meter, Name, ValueType, Opts) ->
    create_instrument(Meter, Name, ?KIND_UPDOWN_COUNTER, ValueType, Opts).

-spec create_histogram(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_histogram(Meter, Name, ValueType, Opts) ->
    create_instrument(Meter, Name, ?KIND_HISTOGRAM, ValueType, Opts).

-spec create_observable_counter(Meter, Name, Callback, CallbackArgs, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: otel_instrument:callback(),
      CallbackArgs :: term(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_observable_counter(Meter, Name, Callback, CallbackArgs, ValueType, Opts) ->
    instrument(Meter, Name, ?KIND_OBSERVABLE_COUNTER, Callback, CallbackArgs, ValueType, Opts).

-spec create_observable_gauge(Meter, Name, Callback, CallbackArgs, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: otel_instrument:callback(),
      CallbackArgs :: term(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_observable_gauge(Meter, Name, Callback, CallbackArgs, ValueType, Opts) ->
    instrument(Meter, Name, ?KIND_OBSERVABLE_GAUGE, Callback, CallbackArgs, ValueType, Opts).

-spec create_observable_updowncounter(Meter, Name, Callback, CallbackArgs, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: otel_instrument:callback(),
      CallbackArgs :: term(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_observable_updowncounter(Meter, Name, Callback, CallbackArgs, ValueType, Opts) ->
    instrument(Meter, Name, ?KIND_OBSERVABLE_UPDOWNCOUNTER, Callback, CallbackArgs, ValueType, Opts).

-spec scope(Meter) -> Scope when
      Meter :: t(),
      Scope :: opentelemetry:instrumentation_scope().
scope(Meter={Module, _}) ->
    Module:scope(Meter).

-spec create_instrument(Meter, Name, Kind, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Kind :: otel_instrument:kind(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_instrument(Meter={Module, _}, Name, Kind, ValueType, Opts) ->
    Module:create_instrument(Meter, Name, Kind, ValueType, Opts).

-spec lookup_instrument(Meter, Name) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name().
lookup_instrument(Meter={Module, _}, Name) ->
    Module:lookup_instrument(Meter, Name).

-spec instrument(Meter, Name, Kind, Callback, CallbackArgs, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Kind :: otel_instrument:kind(),
      Callback :: otel_instrument:callback(),
      CallbackArgs :: term(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
instrument(Meter={Module, _}, Name, Kind, ValueType, Callback, CallbackArgs, Opts) ->
    Module:create_instrument(Meter, Name, Kind, ValueType, Callback, CallbackArgs, Opts).

-spec register_callback(Meter, Instruments, Callback, CallbackArgs) -> ok when
      Meter :: t(),
      Instruments :: [otel_instrument:t()],
      Callback :: otel_instrument:callback(),
      CallbackArgs :: term().
register_callback(Meter={Module, _}, Instruments, Callback, CallbackArgs) ->
    Module:register_callback(Meter, Instruments, Callback, CallbackArgs).

record(Meter={Module, _}, Name, Number, Attributes) ->
    Module:record(Meter, Name, Number, Attributes).
