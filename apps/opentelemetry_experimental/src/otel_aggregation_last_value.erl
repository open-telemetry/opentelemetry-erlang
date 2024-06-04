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
         aggregate/7,
         collect/4]).

-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_metrics.hrl").

-type t() :: #last_value_aggregation{}.

-export_type([t/0]).

%% ignore eqwalizer errors in functions using a lot of matchspecs
-eqwalizer({nowarn_function, checkpoint/3}).
-eqwalizer({nowarn_function, collect/4}).
-dialyzer({nowarn_function, checkpoint/3}).
-dialyzer({nowarn_function, aggregate/7}).
-dialyzer({nowarn_function, collect/4}).
-dialyzer({nowarn_function, maybe_delete_old_generation/4}).
-dialyzer({nowarn_function, datapoint/4}).

-include_lib("opentelemetry_api/include/gradualizer.hrl").
-include("otel_view.hrl").

init(#stream{name=Name,
             reader=ReaderId,
             aggregation_options=_Options,
             forget=Forget}, Attributes) when erlang:is_reference(ReaderId) ->
    Generation = case Forget of
                     true ->
                         otel_metric_reader:checkpoint_generation(ReaderId);
                     _ ->
                         0
                 end,
    Key = {Name, Attributes, ReaderId, Generation},
    #last_value_aggregation{key=Key,
                            start_time=opentelemetry:timestamp(),
                            value=undefined,
                            %% not needed or used, but makes eqwalizer happy
                            checkpoint=0}.

aggregate(Ctx, Tab, ExemplarsTab, #stream{name=Name,
                                          reader=ReaderId,
                                          forget=Forget,
                                          exemplar_reservoir=ExemplarReservoir}, Value, Attributes, DroppedAttributes) ->
    Generation = case Forget of
                     true ->
                         otel_metric_reader:checkpoint_generation(ReaderId);
                     _ ->
                         0
                 end,
    Key = {Name, Attributes, ReaderId, Generation},
    case ets:update_element(Tab, Key, {#last_value_aggregation.value, Value}) of
        true ->
            otel_metric_exemplar_reservoir:offer(Ctx, ExemplarReservoir, ExemplarsTab, Key, Value, DroppedAttributes),
            true;
        false ->
            false
    end.

checkpoint(Tab, #stream{name=Name,
                        reader=ReaderId,
                        temporality=?TEMPORALITY_DELTA}, Generation) ->
    MS = [{#last_value_aggregation{key={Name, '$1', ReaderId, Generation},
                                   start_time='$3',
                                   checkpoint='_',
                                   value='$2'},
           [],
           [{#last_value_aggregation{key={{Name, '$1', {const, ReaderId}, {const, Generation}}},
                                     start_time='$3',
                                     checkpoint='$2',
                                     value='$2'}}]}],
    _ = ets:select_replace(Tab, MS),

    ok;
checkpoint(Tab, #stream{name=Name,
                        reader=ReaderId}, Generation) ->
    MS = [{#last_value_aggregation{key={Name, '$1', ReaderId, Generation},
                                   start_time='$3',
                                   checkpoint='_',
                                   value='$2'},
           [],
           [{#last_value_aggregation{key={{{const, Name}, '$1', {const, ReaderId}, {const, Generation}}},
                                     start_time='$3',
                                     checkpoint='$2',
                                     value='$2'}}]}],
    _ = ets:select_replace(Tab, MS),

    ok.


collect(Tab, ExemplarsTab, Stream=#stream{name=Name,
                                          reader=ReaderId,
                                          forget=Forget,
                                          exemplar_reservoir=ExemplarReservoir}, Generation0) ->
    CollectionStartTime = opentelemetry:timestamp(),
    Generation = case Forget of
                     true ->
                         Generation0;
                     _ ->
                         0
                 end,

    checkpoint(Tab, Stream, Generation),

    Select = [{#last_value_aggregation{key={Name, '_', ReaderId, Generation},
                                       _='_'}, [], ['$_']}],
    AttributesAggregation = ets:select(Tab, Select),
    Result = #gauge{datapoints=[datapoint(ExemplarReservoir, ExemplarsTab, CollectionStartTime, LastValueAgg) ||
                                   LastValueAgg <- AttributesAggregation]},

    %% would be nice to do this in the reader so its not duplicated in each aggregator
    maybe_delete_old_generation(Tab, Name, ReaderId, Generation),

    Result.

%%

%% 0 means it is either cumulative or the first generation with nothing older to delete
maybe_delete_old_generation(_Tab, _Name, _ReaderId, 0) ->
    ok;
maybe_delete_old_generation(Tab, Name, ReaderId, Generation) ->
    %% delete all older than the Generation instead of just the previous in case a
    %% a crash had happened between incrementing the Generation counter and doing
    %% the delete in a previous collection cycle
    %% eqwalizer:ignore matchspecs mess with the typing
    Select = [{#last_value_aggregation{key={Name, '_', ReaderId, '$1'}, _='_'},
               [{'<', '$1', {const, Generation}}],
               [true]}],
    ets:select_delete(Tab, Select).

datapoint(ExemplarReservoir, ExemplarsTab, CollectionStartTime, #last_value_aggregation{key=Key={_, Attributes, _, _},
                                                                                        start_time=StartTime,
                                                                                        checkpoint=Checkpoint}) ->
    Exemplars = otel_metric_exemplar_reservoir:collect(ExemplarReservoir, ExemplarsTab, Key),
    #datapoint{attributes=binary_to_term(Attributes),
               start_time=StartTime,
               time=CollectionStartTime,
               value=Checkpoint,
               exemplars=Exemplars,
               flags=0}.
