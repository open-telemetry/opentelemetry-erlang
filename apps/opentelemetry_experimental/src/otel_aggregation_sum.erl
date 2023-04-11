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
         checkpoint/3,
         collect/3]).

-include("otel_metrics.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").

-type t() :: #sum_aggregation{}.

-export_type([t/0]).

init(#view_aggregation{name=Name,
                       reader=ReaderId}, Attributes) ->
    Key = {Name, Attributes, ReaderId},
    #sum_aggregation{key=Key,
                     start_time_unix_nano=erlang:system_time(nanosecond),
                     checkpoint=0, %% 0 value is never reported but gets copied to previous_checkpoint
                                   %% which is used to add/subtract for conversion of temporality
                     int_value=0,
                     float_value=0.0}.

aggregate(Tab, #view_aggregation{name=Name,
                                 reader=ReaderId,
                                 is_monotonic=IsMonotonic}, Value, Attributes)
  when is_integer(Value) andalso
       ((IsMonotonic andalso Value >= 0) orelse not IsMonotonic) ->
    Key = {Name, Attributes, ReaderId},
    try
        _ = ets:update_counter(Tab, Key, {#sum_aggregation.int_value, Value}),
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
aggregate(Tab, #view_aggregation{name=Name,
                                 reader=ReaderId,
                                 is_monotonic=IsMonotonic}, Value, Attributes)
  when (IsMonotonic andalso Value >= 0.0) orelse not IsMonotonic ->
    Key = {Name, Attributes, ReaderId},
    MS = [{#sum_aggregation{key=Key,
                            start_time_unix_nano='$1',
                            last_start_time_unix_nano='$5',
                            checkpoint='$2',
                            int_value='$3',
                            float_value='$4'},
           [],
           [{#sum_aggregation{key={element, 2, '$_'},
                              start_time_unix_nano='$1',
                              last_start_time_unix_nano='$5',
                              checkpoint='$2',
                              int_value='$3',
                              float_value={'+', '$4', {const, Value}}}}]}],
    1 =:= ets:select_replace(Tab, MS);
aggregate(_Tab, #view_aggregation{name=_Name,
                                  is_monotonic=_IsMonotonic}, _Value, _) ->
    false.

-dialyzer({nowarn_function, checkpoint/3}).
checkpoint(Tab, #view_aggregation{name=Name,
                                  reader=ReaderPid,
                                  temporality=?TEMPORALITY_DELTA}, CollectionStartNano) ->
    MS = [{#sum_aggregation{key='$1',
                            start_time_unix_nano='$4',
                            last_start_time_unix_nano='_',
                            checkpoint='$5',
                            previous_checkpoint='_',
                            int_value='$2',
                            float_value='$3'},
           [{'=:=', {element, 1, '$1'}, {const, Name}},
            {'=:=', {element, 3, '$1'}, {const, ReaderPid}},
            {'=:=', '$3', {const, 0.0}}],
           [{#sum_aggregation{key='$1',
                              start_time_unix_nano={const, CollectionStartNano},
                              last_start_time_unix_nano='$4',
                              checkpoint='$2',
                              previous_checkpoint='$5',
                              int_value=0,
                              float_value=0.0}}]},
          {#sum_aggregation{key='$1',
                            start_time_unix_nano='$4',
                            last_start_time_unix_nano='_',
                            checkpoint='$5',
                            previous_checkpoint='_',
                            int_value='$2',
                            float_value='$3'},
           [{'=:=', {element, 1, '$1'}, {const, Name}},
            {'=:=', {element, 3, '$1'}, {const, ReaderPid}}],
           [{#sum_aggregation{key='$1',
                              start_time_unix_nano={const, CollectionStartNano},
                              last_start_time_unix_nano='$4',
                              checkpoint={'+', '$2', '$3'},
                              previous_checkpoint='$5',
                              int_value=0,
                              float_value=0.0}}]}],
    _ = ets:select_replace(Tab, MS),
    ok;
checkpoint(Tab, #view_aggregation{name=Name,
                                  reader=ReaderPid,
                                  temporality=?TEMPORALITY_CUMULATIVE}, _CollectionStartNano) ->
    MS = [{#sum_aggregation{key='$1',
                            start_time_unix_nano='$2',
                            last_start_time_unix_nano='_',
                            checkpoint='$5',
                            previous_checkpoint='_',
                            int_value='$3',
                            float_value='$4'},
           [{'=:=', {element, 1, '$1'}, {const, Name}},
            {'=:=', {element, 3, '$1'}, {const, ReaderPid}},
            {'=:=', '$4', {const, 0.0}}],
           [{#sum_aggregation{key='$1',
                              start_time_unix_nano='$2',
                              last_start_time_unix_nano='$2',
                              checkpoint='$3',
                              previous_checkpoint='$5',
                              int_value=0,
                              float_value=0.0}}]},
          {#sum_aggregation{key='$1',
                            start_time_unix_nano='$2',
                            last_start_time_unix_nano='_',
                            checkpoint='$5',
                            previous_checkpoint='_',
                            int_value='$3',
                            float_value='$4'},
           [{'=:=', {element, 1, '$1'}, {const, Name}},
            {'=:=', {element, 3, '$1'}, {const, ReaderPid}}],
           [{#sum_aggregation{key='$1',
                              start_time_unix_nano='$2',
                              last_start_time_unix_nano='$2',
                              checkpoint={'+', '$3', '$4'},
                              previous_checkpoint='$5',
                              int_value=0,
                              float_value=0.0}}]}],
    _ = ets:select_replace(Tab, MS),
    ok.

collect(Tab, #view_aggregation{name=Name,
                               reader=ReaderId,
                               instrument=#instrument{temporality=InstrumentTemporality},
                               temporality=Temporality,
                               is_monotonic=IsMonotonic}, CollectionStartTime) ->
    Select = [{'$1',
               [{'=:=', Name, {element, 1, {element, 2, '$1'}}},
                {'=:=', ReaderId, {element, 3, {element, 2, '$1'}}}],
               ['$1']}],
    AttributesAggregation = ets:select(Tab, Select),
    #sum{aggregation_temporality=Temporality,
         is_monotonic=IsMonotonic,
         datapoints=[datapoint(CollectionStartTime, InstrumentTemporality, Temporality, SumAgg) || SumAgg <- AttributesAggregation]}.

datapoint(CollectionStartNano, Temporality, Temporality, #sum_aggregation{key={_, Attributes, _},
                                                                          last_start_time_unix_nano=StartTimeUnixNano,
                                                                          checkpoint=Value}) ->
    #datapoint{
       %% eqwalizer:ignore something
       attributes=Attributes,
       %% eqwalizer:ignore something
       start_time_unix_nano=StartTimeUnixNano,
       time_unix_nano=CollectionStartNano,
       %% eqwalizer:ignore something
       value=Value,
       exemplars=[],
       flags=0
      };
datapoint(CollectionStartNano, _, ?TEMPORALITY_CUMULATIVE, #sum_aggregation{key={_, Attributes, _},
                                                                            last_start_time_unix_nano=StartTimeUnixNano,
                                                                            previous_checkpoint=PreviousCheckpoint,
                                                                            checkpoint=Value}) ->
    #datapoint{
       %% eqwalizer:ignore something
       attributes=Attributes,
       %% eqwalizer:ignore something
       start_time_unix_nano=StartTimeUnixNano,
       time_unix_nano=CollectionStartNano,
       value=Value + PreviousCheckpoint,
       exemplars=[],
       flags=0
      };
datapoint(CollectionStartNano, _, ?TEMPORALITY_DELTA, #sum_aggregation{key={_, Attributes, _},
                                                                       last_start_time_unix_nano=StartTimeUnixNano,
                                                                       previous_checkpoint=PreviousCheckpoint,
                                                                       checkpoint=Value}) ->
    #datapoint{
       attributes=Attributes,
       start_time_unix_nano=StartTimeUnixNano,
       time_unix_nano=CollectionStartNano,
       value=Value - PreviousCheckpoint,
       exemplars=[],
       flags=0
      }.
