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

-export([add/3]).

-include("otel_metrics.hrl").
-include_lib("kernel/include/logger.hrl").

-spec add(otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok.
add(Instrument=#instrument{module=Module,
                           value_type=?VALUE_TYPE_INTEGER}, Number, Attributes)
  when is_integer(Number) andalso Number >= 0 ->
    Module:sync_record(Instrument, Number, Attributes);
add(Instrument=#instrument{module=Module,
                           value_type=?VALUE_TYPE_FLOAT}, Number, Attributes)
  when is_float(Number) andalso Number >= 0 ->
    Module:sync_record(Instrument, Number, Attributes);
add(#instrument{name=Name,
                value_type=?VALUE_TYPE_INTEGER}, Number, _) ->
    ?LOG_DEBUG("Counter instrument ~p does not support adding value ~p. "
               "The value must be a positive integer.", [Name, Number]),
    ok;
add(#instrument{name=Name,
                value_type=?VALUE_TYPE_FLOAT}, Number, _) ->
    ?LOG_DEBUG("Counter instrument ~p does not support adding value ~p. "
               "The value must be a positive float.", [Name, Number]),
    ok.
