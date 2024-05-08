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
-module(otel_aggregation_last_value).

-behaviour(otel_aggregation).

-export([init/2,
         aggregate/4,
         checkpoint/5,
         collect/5]).

-include("otel_metrics2.hrl").

-type t() :: #last_value_aggregation{}.

-export_type([t/0]).

-include_lib("opentelemetry_api/include/gradualizer.hrl").

init(Key, _Options) ->
    #last_value_aggregation{key=Key,
                            value=undefined}.

aggregate(Tab, Key, Value, Options) ->
    case ets:update_element(Tab, Key, {#last_value_aggregation.value, Value}) of
        true ->
            true;
        false ->
            Metric = init(Key, Options),
            ets:insert(Tab, ?assert_type((?assert_type(Metric, #last_value_aggregation{}))#last_value_aggregation{value=Value}, tuple()))
    end.

-dialyzer({nowarn_function, checkpoint/5}).
checkpoint(Tab, Name, ReaderPid, _, _CollectionStartNano) ->
    MS = [{#last_value_aggregation{key='$1',
                                   checkpoint='_',
                                   value='$2'},
           [{'=:=', {element, 1, '$1'}, {const, Name}},
            {'=:=', {element, 3, '$1'}, {const, ReaderPid}}],
           [{#last_value_aggregation{key='$1',
                                     checkpoint='$2',
                                     value='$2'}}]}],
    _ = ets:select_replace(Tab, MS),

    ok.

collect(Tab, Name, ReaderPid, _, CollectionStartTime) ->
    Select = [{'$1',
               [{'=:=', Name, {element, 1, {element, 2, '$1'}}},
                {'=:=', ReaderPid, {element, 3, {element, 2, '$1'}}}],
               ['$1']}],
    AttributesAggregation = ets:select(Tab, Select),
    [datapoint(CollectionStartTime, LastValueAgg) || LastValueAgg <- AttributesAggregation].

%%

datapoint(CollectionStartNano, #last_value_aggregation{key={_, Attributes, _},
                                                       checkpoint=Checkpoint}) ->
    #datapoint{attributes=Attributes,
               time_unix_nano=CollectionStartNano,
               value=Checkpoint,
               exemplars=[],
               flags=0}.
