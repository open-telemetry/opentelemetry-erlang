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
%% @doc UpDownCounter is a synchronous Instrument which supports increments
%% and decrements.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_updown_counter).

-export([add/3]).

-include("otel_metrics.hrl").

-callback add(otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok.

-spec add(otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok.
add(Instrument=#instrument{module=Module,
                           value_type=?VALUE_TYPE_INTEGER}, Number, Attributes)
  when is_integer(Number) ->
    Module:record(Instrument, Number, Attributes);
add(Instrument=#instrument{module=Module,
                           value_type=?VALUE_TYPE_FLOAT}, Number, Attributes)
  when is_float(Number) ->
    Module:record(Instrument, Number, Attributes);
add(_, _, _) ->
    %% TODO: add debug, warning or info log here?
    ok.
