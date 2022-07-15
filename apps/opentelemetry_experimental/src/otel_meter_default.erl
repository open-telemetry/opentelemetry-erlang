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
         instrument/6]).

%% also act as default version of instruments
-export([add/3,
         record/3]).

instrument(Meter, Name, Kind, ValueType, Opts) ->
    Instrument = otel_instrument:new(?MODULE, Meter, Kind, Name, maps:get(description, Opts, undefined),
                                     maps:get(unit, Opts, undefined), ValueType),
    otel_meter_server:add_instrument(Instrument),
    Instrument.

instrument(Meter, Name, Kind, ValueType, Callback, Opts) ->
    Instrument = otel_instrument:new(?MODULE, Meter, Kind, Name, maps:get(description, Opts, undefined),
                                     maps:get(unit, Opts, undefined), ValueType, Callback),
    otel_meter_server:add_instrument(Instrument),
    Instrument.

%%

%% handles both default counter and default updown counter
add(Instrument, Number, Attributes) ->
    otel_meter_server:record(Instrument, Number, Attributes).

record(Instrument, Number, Attributes) ->
    otel_meter_server:record(Instrument, Number, Attributes).

