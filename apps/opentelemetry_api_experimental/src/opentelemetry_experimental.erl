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
-module(opentelemetry_experimental).

-export([set_meter/2,
         set_default_meter/1,
         set_default_meter/2,
         get_meter/0,
         get_meter/1]).

-include("otel_meter.hrl").

-export_type([meter/0]).

-type meter() :: {module(), term()}.

-define(METER_KEY(MeterProvider, Scope), {?MODULE, MeterProvider, meter, Scope}).
-define(DEFAULT_METER_KEY(MeterProvider), ?METER_KEY(MeterProvider, '$__default_meter')).

-spec set_default_meter(meter()) -> boolean().
set_default_meter(Meter) ->
    set_default_meter(?GLOBAL_METER_PROVIDER_NAME, Meter).

-spec set_default_meter(atom(), meter()) -> boolean().
set_default_meter(MeterProvider, Meter) ->
    opentelemetry:verify_and_set_term(Meter, ?DEFAULT_METER_KEY(MeterProvider), otel_meter).

-spec get_meter() -> meter().
get_meter() ->
    get_meter_(?GLOBAL_METER_PROVIDER_NAME).

-spec get_meter_(atom()) -> meter().
get_meter_(MeterProvider) ->
    persistent_term:get(?DEFAULT_METER_KEY(MeterProvider), {otel_meter_noop, []}).

-spec get_meter(InstrumentationScope) -> Meter when
      InstrumentationScope :: '$__default_meter' | opentelemetry:instrumentation_scope(),
      Meter:: meter().
get_meter('$__default_meter') ->
    get_meter();
get_meter(InstrumentationScope) ->
    get_meter(?GLOBAL_METER_PROVIDER_NAME, InstrumentationScope).

-spec get_meter(MeterProvider, InstrumentationScope) -> Meter when
      MeterProvider :: atom() | pid(),
      InstrumentationScope :: opentelemetry:instrumentation_scope(),
      Meter:: meter().
get_meter(MeterProvider, InstrumentationScope) ->
    %% check cache and then use provider to get the meter if it isn't cached yet
    case persistent_term:get(?METER_KEY(MeterProvider, InstrumentationScope), undefined) of
        undefined ->
            Meter = otel_meter_provider:get_meter(MeterProvider, InstrumentationScope),

            %% cache the meter
            _ = set_meter(InstrumentationScope, Meter),

            Meter;
        Meter ->
            Meter
    end.

-spec set_meter(InstrumentationScope, Meter) -> boolean() when
      InstrumentationScope :: opentelemetry:instrumentation_scope(),
      Meter:: meter().
set_meter(InstrumentationScope, Meter) ->
    set_meter(?GLOBAL_METER_PROVIDER_NAME, InstrumentationScope, Meter).

-spec set_meter(MeterProvider, InstrumentationScope, Meter) -> boolean() when
      MeterProvider :: atom(),
      InstrumentationScope :: opentelemetry:instrumentation_scope(),
      Meter:: meter().
set_meter(MeterProvider, InstrumentationScope, Meter) ->
    opentelemetry:verify_and_set_term(Meter, ?METER_KEY(MeterProvider, InstrumentationScope), otel_meter).
