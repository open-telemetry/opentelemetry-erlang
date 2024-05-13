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
-module(otel_meter_noop).

-behaviour(otel_meter).

-export([register_callback/4,
         create_instrument/4,
         create_instrument/6]).

%% also act as noop version of instruments
-export([record/4,
         record/3]).

%%

record(_Meter, _Name, _Number, _Attributes) ->
    ok.

record(_Instrument, _Number, _Attributes) ->
    ok.

%%

register_callback(_Meter, _Instruments, _Callback, _CallbackArgs) ->
    ok.

create_instrument(Meter, Name, Kind, Opts) ->
    otel_instrument:new(?MODULE, Meter, Kind, Name, Opts).

create_instrument(Meter, Name, Kind, Callback, CallbackArgs, Opts) ->
    otel_instrument:new(?MODULE, Meter, Kind, Name, Callback, CallbackArgs, Opts).
