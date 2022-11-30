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
-module(otel_aggregation_sum).

-behaviour(otel_aggregation).

-export([init/2,
         aggregate/4,
         checkpoint/6,
         collect/5]).

-include("otel_metrics.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").

-type t() :: #sum_aggregation{}.

-export_type([t/0]).

init(Key, _Options) ->
    #sum_aggregation{key=Key,
                     start_time_unix_nano=erlang:system_time(nanosecond),
                     value=0}.

aggregate(Tab, Key, Value, _Options) ->
    try
        _ = ets:update_counter(Tab, Key, {#sum_aggregation.value, Value}),
        true
    catch
        error:badarg ->
            %% the use of `update_counter' guards against conflicting with another process
            %% doing the update at the same time

            %% the default isn't just given in the first `update_counter' because then
            %% we'd have to call `system_time' for every single measurement taken
            _ = ets:update_counter(Tab, Key, {#sum_aggregation.value, Value},
                                   init(Key, [])),
            true
    end.

-dialyzer({nowarn_function, checkpoint/6}).
checkpoint(Tab, Name, ReaderPid, ?AGGREGATION_TEMPORALITY_DELTA, ?VALUE_TYPE_INTEGER, CollectionStartNano) ->
    MS = [{#sum_aggregation{key='$1',
                            start_time_unix_nano='_',
                            checkpoint='_',
                            value='$2'},
           [{'=:=', {element, 1, '$1'}, {const, Name}},
            {'=:=', {element, 3, '$1'}, {const, ReaderPid}}],
           [{#sum_aggregation{key='$1',
                              start_time_unix_nano={const, CollectionStartNano},
                              checkpoint='$2',
                              value=0}}]}],
    _ = ets:select_replace(Tab, MS),
    ok;
checkpoint(Tab, Name, ReaderPid, ?AGGREGATION_TEMPORALITY_CUMULATIVE, ?VALUE_TYPE_INTEGER, _CollectionStartNano) ->
    MS = [{#sum_aggregation{key='$1',
                            start_time_unix_nano='$2',
                            checkpoint='_',
                            value='$3'},
           [{'=:=', {element, 1, '$1'}, {const, Name}},
            {'=:=', {element, 3, '$1'}, {const, ReaderPid}}],
           [{#sum_aggregation{key='$1',
                              start_time_unix_nano='$2',
                              checkpoint='$3',
                              value='$3'}}]}],
    _ = ets:select_replace(Tab, MS),
    ok.

collect(Tab, Name, ReaderPid, _, CollectionStartTime) ->
    Select = [{'$1',
               [{'=:=', Name, {element, 1, {element, 2, '$1'}}},
                {'=:=', ReaderPid, {element, 3, {element, 2, '$1'}}}],
               ['$1']}],
    AttributesAggregation = ets:select(Tab, Select),
    [datapoint(CollectionStartTime, SumAgg) || SumAgg <- AttributesAggregation].

datapoint(CollectionStartNano, #sum_aggregation{key={_, Attributes, _},
                                                start_time_unix_nano=StartTimeUnixNano,
                                                checkpoint=Value}) ->
    #datapoint{
       attributes=Attributes,
       start_time_unix_nano=StartTimeUnixNano,
       time_unix_nano=CollectionStartNano,
       value=Value,
       exemplars=[],
       flags=0
      }.
