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
         aggregate/7,
         collect/4]).

-include("otel_metrics.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").

-type t() :: #sum_aggregation{}.

-export_type([t/0]).

%% ignore eqwalizer errors in functions using a lot of matchspecs
-eqwalizer({nowarn_function, checkpoint/3}).
-eqwalizer({nowarn_function, aggregate/7}).
-dialyzer({nowarn_function, checkpoint/3}).
-dialyzer({nowarn_function, aggregate/7}).
-dialyzer({nowarn_function, collect/4}).
-dialyzer({nowarn_function, maybe_delete_old_generation/4}).
-dialyzer({nowarn_function, datapoint/7}).

init(#stream{name=Name,
             reader=ReaderId,
             forget=Forget}, Attributes) when is_reference(ReaderId) ->
    Generation = case Forget of
                     true ->
                         otel_metric_reader:checkpoint_generation(ReaderId);
                     _ ->
                         0
                 end,
    StartTime = opentelemetry:timestamp(),
    Key = {Name, Attributes, ReaderId, Generation},
    #sum_aggregation{key=Key,
                     start_time=StartTime,
                     checkpoint=0, %% 0 value is never reported but gets copied to previous_checkpoint
                     %% which is used to add/subtract for conversion of temporality
                     previous_checkpoint=0,
                     int_value=0,
                     float_value=0.0}.

aggregate(Ctx, Tab, ExemplarsTab, #stream{name=Name,
                                          reader=ReaderId,
                                          forget=Forget,
                                          exemplar_reservoir=ExemplarReservoir}, Value, Attributes, DroppedAttributes)
  when is_integer(Value) ->
    Generation = case Forget of
                     true ->
                         otel_metric_reader:checkpoint_generation(ReaderId);
                     _ ->
                         0
                 end,
    Key = {Name, Attributes, ReaderId, Generation},
    try ets:update_counter(Tab, Key, {#sum_aggregation.int_value, Value}) of
        _ ->
            otel_metric_exemplar_reservoir:offer(Ctx, ExemplarReservoir, ExemplarsTab, Key, Value, DroppedAttributes),
            true
    catch
        error:badarg ->
            %% the use of `update_counter' guards against conflicting with another process
            %% doing the update at the same time

            %% the default isn't just given in the first `update_counter' because then
            %% we'd have to call `system_time' for every single measurement taken
            %% _ = ets:update_counter(Tab, Key, {#sum_aggregation.value, Value},
            %%                        init(Key, Options)),
            %% true
            false
    end;
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
    MS = [{#sum_aggregation{key=Key,
                            start_time='$1',
                            checkpoint='$2',
                            previous_checkpoint='$6',
                            int_value='$3',
                            float_value='$4'},
           [],
           [{#sum_aggregation{key={element, 2, '$_'},
                              start_time='$1',
                              checkpoint='$2',
                              previous_checkpoint='$6',
                              int_value='$3',
                              float_value={'+', '$4', {const, Value}}}}]}],
    case ets:select_replace(Tab, MS) of
        1 ->
            otel_metric_exemplar_reservoir:offer(Ctx, ExemplarReservoir, ExemplarsTab, Key, Value, DroppedAttributes),
            true;
        _ ->
            false
    end.

checkpoint(Tab, #stream{name=Name,
                        reader=ReaderId,
                        temporality=?TEMPORALITY_DELTA}, Generation) ->
    MS = [{#sum_aggregation{key={Name, '$1', ReaderId, Generation},
                            start_time='$4',
                            checkpoint='$5',
                            previous_checkpoint='_',
                            int_value='$2',
                            float_value='$3'},
           [{'=:=', '$3', {const, 0.0}}],
           [{#sum_aggregation{key={{Name, '$1', {const, ReaderId}, {const, Generation}}},
                              start_time='$4',
                              checkpoint='$2',
                              previous_checkpoint='$5',
                              int_value=0,
                              float_value=0.0}}]},
          {#sum_aggregation{key={Name, '$1', ReaderId, Generation},
                            start_time='$4',
                            checkpoint='$5',
                            previous_checkpoint='_',
                            int_value='$2',
                            float_value='$3'},
           [],
           [{#sum_aggregation{key={{Name, '$1', {const, ReaderId}, {const, Generation}}},
                              start_time='$4',
                              checkpoint={'+', '$2', '$3'},
                              previous_checkpoint='$5',
                              int_value=0,
                              float_value=0.0}}]}],
    _ = ets:select_replace(Tab, MS),
    ok;
checkpoint(Tab, #stream{name=Name,
                        reader=ReaderId,
                        forget=Forget,
                        temporality=?TEMPORALITY_CUMULATIVE}, Generation0) ->
    Generation = case Forget of
                     true ->
                         Generation0;
                     _ ->
                         0
                 end,
    MS = [{#sum_aggregation{key={Name, '$1', ReaderId, Generation},
                            start_time='$2',
                            checkpoint='$5',
                            previous_checkpoint='$6',
                            int_value='$3',
                            float_value='$4'},
           [{'=:=', '$4', {const, 0.0}}],
           [{#sum_aggregation{key={{Name, '$1', {const, ReaderId}, {const, Generation}}},
                              start_time='$2',
                              checkpoint='$3',
                              previous_checkpoint={'+', '$5', '$6'},
                              int_value=0,
                              float_value=0.0}}]},
          {#sum_aggregation{key={Name, '$1', ReaderId, Generation},
                            start_time='$2',
                            checkpoint='$5',
                            previous_checkpoint='$6',
                            int_value='$3',
                            float_value='$4'},
           [],
           [{#sum_aggregation{key={{Name, '$1', {const, ReaderId}, {const, Generation}}},
                              start_time='$2',
                              checkpoint={'+', '$3', '$4'},
                              previous_checkpoint={'+', '$5', '$6'},
                              int_value=0,
                              float_value=0.0}}]}],
    _ = ets:select_replace(Tab, MS),
    ok.

collect(Tab, ExemplarsTab, Stream=#stream{name=Name,
                                          reader=ReaderId,
                                          instrument=#instrument{temporality=InstrumentTemporality},
                                          temporality=Temporality,
                                          is_monotonic=IsMonotonic,
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

    %% eqwalizer:ignore matchspecs mess with the typing
    Select = [{#sum_aggregation{key={Name, '_', ReaderId, Generation}, _='_'}, [], ['$_']}],
    AttributesAggregation = ets:select(Tab, Select),
    Result = #sum{aggregation_temporality=Temporality,
                  is_monotonic=IsMonotonic,
                  datapoints=[datapoint(Tab, ExemplarReservoir, ExemplarsTab, CollectionStartTime, InstrumentTemporality, Temporality, SumAgg) || SumAgg <- AttributesAggregation]},

    %% would be nice to do this in the reader so its not duplicated in each aggregator
    maybe_delete_old_generation(Tab, Name, ReaderId, Generation),

    Result.

%% 0 means it is either cumulative or the first generation with nothing older to delete
maybe_delete_old_generation(_Tab, _Name, _ReaderId, 0) ->
    ok;
maybe_delete_old_generation(Tab, Name, ReaderId, Generation) ->
    %% delete all older than the Generation instead of just the previous in case a
    %% a crash had happened between incrementing the Generation counter and doing
    %% the delete in a previous collection cycle
    %% eqwalizer:ignore matchspecs mess with the typing
    Select = [{#sum_aggregation{key={Name, '_', ReaderId, '$1'}, _='_'},
               [{'<', '$1', {const, Generation}}],
               [true]}],
    ets:select_delete(Tab, Select).

%% nothing special to do if the instrument temporality and view temporality are the same
datapoint(_Tab, ExemplarReservoir, ExemplarsTab, CollectionStartTime, Temporality, Temporality, #sum_aggregation{key=Key={_, Attributes, _, _},
                                                                                                                 start_time=StartTime,
                                                                                                                 checkpoint=Value}) ->
    Exemplars = otel_metric_exemplar_reservoir:collect(ExemplarReservoir, ExemplarsTab, Key),
    #datapoint{
       attributes=Attributes,
       start_time=StartTime,
       time=CollectionStartTime,
       value=Value,
       exemplars=Exemplars,
       flags=0
      };
%% converting an instrument of delta temporality to cumulative means we need to add the
%% previous value to the current because the actual value is only a delta
datapoint(_Tab, ExemplarReservoir, ExemplarsTab, Time, _, ?TEMPORALITY_CUMULATIVE, #sum_aggregation{key=Key={_Name, Attributes, _ReaderId, _Generation},
                                                                                                    start_time=StartTime,
                                                                                                    previous_checkpoint=PreviousCheckpoint,
                                                                                                    checkpoint=Value}) ->
    Exemplars = otel_metric_exemplar_reservoir:collect(ExemplarReservoir, ExemplarsTab, Key),
    #datapoint{
       attributes=Attributes,
       start_time=StartTime,
       time=Time,
       value=Value + PreviousCheckpoint,
       exemplars=Exemplars,
       flags=0
      };
%% converting an instrument of cumulative temporality to delta means subtracting the
%% value of the previous collection, if one exists.
%% because we use a generation counter to reset delta aggregates the previous value
%% has to be looked up with an ets lookup of the previous generation
datapoint(Tab, ExemplarReservoir, ExemplarsTab, Time, _, ?TEMPORALITY_DELTA, #sum_aggregation{key=Key={Name, Attributes, ReaderId, Generation},
                                                                                              start_time=StartTime,
                                                                                              checkpoint=Value}) ->
    %% converting from cumulative to delta by grabbing the last generation and subtracting it
    %% can't use `previous_checkpoint' because with delta metrics have their generation changed
    %% at each collection
    PreviousCheckpoint =
        otel_metrics_tables:lookup_sum_checkpoint(Tab, Name, Attributes, ReaderId, Generation-1),
    Exemplars = otel_metric_exemplar_reservoir:collect(ExemplarReservoir, ExemplarsTab, Key),
    #datapoint{
       attributes=Attributes,
       start_time=StartTime,
       time=Time,
       value=Value - PreviousCheckpoint,
       exemplars=Exemplars,
       flags=0
      }.

