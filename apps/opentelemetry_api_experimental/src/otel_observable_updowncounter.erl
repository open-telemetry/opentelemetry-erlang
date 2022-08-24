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
%% @doc Asynchronous UpDownCounter is an asynchronous Instrument which
%% reports additive value(s) (e.g. the process heap size - it makes
%% sense to report the heap size from multiple processes and sum them up,
%% so we get the total heap usage) when the instrument is being observed.
%%
%% Note: if the value is monotonically increasing, use Asynchronous Counter
%% instead; if the value is non-additive, use Asynchronous Gauge instead.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_observable_updowncounter).

-export([add/3]).

-include("otel_metrics.hrl").
-include_lib("kernel/include/logger.hrl").

-spec add(otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok.
add(Instrument=#instrument{module=Module,
                           value_type=?VALUE_TYPE_INTEGER}, Number, Attributes)
  when is_integer(Number) ->
    Module:sync_record(Instrument, Number, Attributes);
add(Instrument=#instrument{module=Module,
                           value_type=?VALUE_TYPE_FLOAT}, Number, Attributes)
  when is_float(Number) ->
    Module:sync_record(Instrument, Number, Attributes);
add(#instrument{name=Name,
                value_type=?VALUE_TYPE_INTEGER}, Number, _) ->
    ?LOG_DEBUG("ObservableUpDownCounter instrument ~p does not support adding value ~p. "
               "The value must be an integer.", [Name, Number]),
    ok;
add(#instrument{name=Name,
                value_type=?VALUE_TYPE_FLOAT}, Number, _) ->
    ?LOG_DEBUG("ObservableUpDownCounter instrument ~p does not support adding value ~p. "
               "The value must be a float.", [Name, Number]),
    ok.
