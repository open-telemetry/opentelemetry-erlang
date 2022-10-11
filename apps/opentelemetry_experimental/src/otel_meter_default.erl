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

-export([instrument/5,
         instrument/7,
         register_callback/4,
         scope/1]).

-export([record/3]).

-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_metrics.hrl").

instrument(Meter, Name, Kind, ValueType, Opts) ->
    otel_instrument:new(?MODULE, Meter, Kind, Name, maps:get(description, Opts, undefined),
                        maps:get(unit, Opts, undefined), ValueType).

instrument(Meter, Name, Kind, ValueType, Callback, CallbackArgs, Opts) ->
    Instrument=#instrument{meter={_, #meter{provider=Provider}}} =
        otel_instrument:new(?MODULE, Meter, Kind, Name, maps:get(description, Opts, undefined),
                            maps:get(unit, Opts, undefined), ValueType, Callback, CallbackArgs),
    _ = otel_meter_server:add_instrument(Provider, Instrument),
    Instrument.

register_callback({_, #meter{provider=Provider}}, Instruments, Callback, CallbackArgs) ->
    otel_meter_server:register_callback(Provider, Instruments, Callback, CallbackArgs);
register_callback(_, _, _, _) ->
    ok.

scope({_, #meter{instrumentation_scope=Scope}}) ->
    Scope.

%%

record(Instrument=#instrument{meter={_, #meter{provider=Provider}}}, Number, Attributes) ->
    otel_meter_server:record(Provider, Instrument, Number, Attributes).

