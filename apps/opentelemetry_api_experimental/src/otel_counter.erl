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
%% @doc Counter is a synchronous Instrument which supports non-negative
%% increments.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_counter).

-export([add/3]).

-include("otel_metrics.hrl").

-callback add(otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok.

-spec add(otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok.
add(Instrument=#instrument{module=Module}, Number, Attributes) ->
    Module:add(Instrument, Number, Attributes).
