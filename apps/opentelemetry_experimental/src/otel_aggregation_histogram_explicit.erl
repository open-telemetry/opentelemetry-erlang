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
-module(otel_aggregation_histogram_explicit).

-export([init/2,
         aggregate/3,
         checkpoint/5,
         collect/4]).

-include("otel_metrics.hrl").

-type t() :: #explicit_histogram_aggregation{}.

-export_type([t/0]).

-define(DEFAULT_BOUNDARIES, [0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 1000.0]).

init(Key, Options) ->
    Boundaries = maps:get(boundaries, Options, ?DEFAULT_BOUNDARIES),
    RecordMinMax = maps:get(record_min_max, Options, true),
    #explicit_histogram_aggregation{key=Key,
                                    start_time_unix_nano=erlang:system_time(nanosecond),
                                    boundaries=Boundaries,
                                    bucket_counts=zero_buckets(length(Boundaries)),
                                    record_min_max=RecordMinMax,
                                    min=infinity, %% works because any atom is > any integer
                                    max=neg_infinity, %% requires an explicit check
                                    sum=0
                                   }.

aggregate(Table, Key, Value) ->
    case ets:lookup(Table, Key) of
        [Current] ->
            %% TODO: needs to be changed to work with concurrent updates
            %% at this time there are no concurrent updates, so nothing is lost
            %% a basic compare and swap could fail
            %% consider making each bucket its own record
            ets:insert(Table, aggregate(Value, Current));
        _ ->
            %% since we need the options to initialize a histogram `false' is
            %% returned and `otel_metric_server' will initialize the histogram
            false
    end.

aggregate(MeasurementValue,
          Aggregation=#explicit_histogram_aggregation{record_min_max=true,
                                                      boundaries=Boundaries,
                                                      bucket_counts=Buckets,
                                                      min=Min,
                                                      max=Max,
                                                      sum=Sum}) ->
    Buckets1 = bump_bucket_counts(MeasurementValue, Boundaries, Buckets),
    Aggregation#explicit_histogram_aggregation{bucket_counts=Buckets1,
                                               min=min(Min, MeasurementValue),
                                               max=case Max of
                                                       neg_infinity -> MeasurementValue;
                                                       _ -> max(Max, MeasurementValue)
                                                   end,
                                               sum=Sum+MeasurementValue};
aggregate(MeasurementValue,
          Aggregation=#explicit_histogram_aggregation{boundaries=Boundaries,
                                                      bucket_counts=Buckets,
                                                      sum=Sum}) ->
    Buckets1 = bump_bucket_counts(MeasurementValue, Boundaries, Buckets),
    Aggregation#explicit_histogram_aggregation{bucket_counts=Buckets1,
                                               sum=Sum+MeasurementValue}.

-dialyzer({nowarn_function, checkpoint/5}).
%% TODO: handle delta temporary checkpoints
checkpoint(Tab, Name, _, _, _CollectionStartNano) ->
    MS = [{#explicit_histogram_aggregation{key='$1',
                                           start_time_unix_nano='$2',
                                           boundaries='$3',
                                           record_min_max='$4',
                                           checkpoint='_',
                                           bucket_counts='$5',
                                           min='$6',
                                           max='$7',
                                           sum='$8'
                                          },
           [{'=:=', {element, 1, '$1'}, {const, Name}}],
           [{#explicit_histogram_aggregation{key='$1',
                                             start_time_unix_nano='$2',
                                             boundaries='$3',
                                             record_min_max='$4',
                                             checkpoint={#explicit_histogram_checkpoint{bucket_counts='$5',
                                                                                        min='$6',
                                                                                        max='$7',
                                                                                        sum='$8'}},
                                             bucket_counts='$5',
                                             min='$6',
                                             max='$7',
                                             sum='$8'}}]}],
    _ = ets:select_replace(Tab, MS),

    ok.

collect(Tab, Name, _, CollectionStartTime) ->
    Select = [{'$1',
               [{'==', Name, {element, 1, {element, 2, '$1'}}}],
               ['$1']}],
    AttributesAggregation = ets:select(Tab, Select),
    [datapoint(CollectionStartTime, SumAgg) || SumAgg <- AttributesAggregation].

%%

datapoint(CollectionStartNano, #explicit_histogram_aggregation{
                                  key={_, Attributes},
                                  start_time_unix_nano=StartTimeUnixNano,
                                  boundaries=Boundaries,
                                  checkpoint=#explicit_histogram_checkpoint{bucket_counts=Buckets,
                                                                            min=Min,
                                                                            max=Max,
                                                                            sum=Sum}
                                 }) ->
    #histogram_datapoint{
       attributes=Attributes,
       start_time_unix_nano=StartTimeUnixNano,
       time_unix_nano=CollectionStartNano,
       count=lists:sum(erlang:tuple_to_list(Buckets)),
       sum=Sum,
       bucket_counts=Buckets,
       explicit_bounds=Boundaries,
       exemplars=[],
       flags=0,
       min=Min,
       max=Max
      }.

zero_buckets(Size) ->
    erlang:list_to_tuple(lists:duplicate(Size, 0)).

find_bucket(Boundaries, Value) ->
    find_bucket(Boundaries, Value, 1).

find_bucket([X | _Rest], Value, Pos) when Value =< X ->
    Pos;
find_bucket([_X], _Value, Pos) ->
    Pos;
find_bucket([_X | Rest], Value, Pos) ->
    find_bucket(Rest, Value, Pos+1);
find_bucket(_, _, Pos) ->
    Pos.

bump_bucket_counts(MeasurementValue, Boundaries, Buckets) ->
    Pos = find_bucket(Boundaries, MeasurementValue),
    setelement(Pos, Buckets, element(Pos, Buckets) + 1).
