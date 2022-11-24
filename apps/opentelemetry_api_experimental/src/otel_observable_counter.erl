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
%% @doc Asynchronous Counter is an asynchronous Instrument which reports
%% monotonically increasing value(s) when the instrument is being observed.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_observable_counter).

-export([create/6]).

create(Meter, Name, Callback, CallbackArgs, ValueType, Opts) ->
    otel_meter:create_observable_counter(Meter, Name, Callback, CallbackArgs, ValueType, Opts).
