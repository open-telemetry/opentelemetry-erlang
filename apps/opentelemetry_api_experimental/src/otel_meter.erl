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
         create_observable_counter/5,
         create_histogram/4,
         create_observable_gauge/5,
         create_updown_counter/4,
         create_observable_updowncounter/5]).

-callback create_counter(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
-callback create_observable_counter(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
-callback create_histogram(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
-callback create_observable_gauge(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
-callback create_updown_counter(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().
-callback create_observable_updowncounter(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: otel_meter:opts().

-type callback() :: fun().
-type opts() :: #{description => otel_instrument:description(),
                  unit => otel_instrument:unit()}.

-type t() :: {module(), term()}.

-export_type([t/0,
              opts/0,
              callback/0]).

-spec create_counter(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_counter(Meter={Module, _}, Name, ValueType, Opts) ->
    Module:create_counter(Meter, Name, ValueType, Opts).

-spec create_observable_counter(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_observable_counter(Meter={Module, _}, Name, Callback, ValueType, Opts) ->
    Module:create_observable_counter(Meter, Name, Callback, ValueType, Opts).

-spec create_histogram(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_histogram(Meter={Module, _}, Name, ValueType, Opts) ->
    Module:create_histogram(Meter, Name, ValueType, Opts).

-spec create_observable_gauge(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_observable_gauge(Meter={Module, _}, Name, Callback, ValueType, Opts) ->
    Module:create_observable_gauge(Meter, Name, Callback, ValueType, Opts).

-spec create_updown_counter(Meter, Name, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_updown_counter(Meter={Module, _}, Name, ValueType, Opts) ->
    Module:create_updown_counter(Meter, Name, ValueType, Opts).

-spec create_observable_updowncounter(Meter, Name, Callback, ValueType, Opts) -> otel_instrument:t() when
      Meter :: t(),
      Name :: otel_instrument:name(),
      Callback :: callback(),
      ValueType :: otel_instrument:value_type(),
      Opts :: opts().
create_observable_updowncounter(Meter={Module, _}, Name, Callback, ValueType, Opts) ->
    Module:create_observable_updowncounter(Meter, Name, Callback, ValueType, Opts).
