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
%% @doc Asynchronous Gauge is an asynchronous Instrument which reports
%% non-additive value(s) (e.g. the room temperature - it makes no sense
%% to report the temperature value from multiple rooms and sum them up)
%% when the instrument is being observed.

%% Note: if the values are additive (e.g. the process heap size - it makes
%% sense to report the heap size from multiple processes and sum them up,
%% so we get the total heap usage), use Asynchronous Counter or
%% Asynchronous UpDownCounter.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_observable_gauge).

-export([create/3, create/5]).

create(Meter, Name, Opts) ->
    otel_meter:create_observable_gauge(Meter, Name, Opts).

create(Meter, Name, Callback, CallbackArgs, Opts) ->
    otel_meter:create_observable_gauge(Meter, Name, Callback, CallbackArgs, Opts).
