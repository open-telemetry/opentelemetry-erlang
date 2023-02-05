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

-behaviour(otel_aggregation).

-export([init/2,
         aggregate/4,
         checkpoint/5,
         collect/5]).

-include("otel_metrics.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").

-type t() :: #explicit_histogram_aggregation{}.

-export_type([t/0]).

-define(DEFAULT_BOUNDARIES, [0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 1000.0]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(MIN_DOUBLE, -9223372036854775807.0). %% the proto representation of size `fixed64'

init(#view_aggregation{name=Name,
                       reader=ReaderId,
                       aggregation_options=Options}, Attributes) ->
    Key = {Name, Attributes, ReaderId},
    Boundaries = maps:get(boundaries, Options, ?DEFAULT_BOUNDARIES),
    RecordMinMax = maps:get(record_min_max, Options, true),
    #explicit_histogram_aggregation{key=Key,
                                    start_time_unix_nano=erlang:system_time(nanosecond),
                                    boundaries=Boundaries,
                                    bucket_counts=new_bucket_counts(Boundaries),
                                    checkpoint=undefined,
                                    record_min_max=RecordMinMax,
                                    min=infinity, %% works because any atom is > any integer
                                    max=?MIN_DOUBLE,
                                    sum=0
                                   }.

aggregate(Table, #view_aggregation{name=Name,
                                   reader=ReaderId,
                                   aggregation_options=Options}, Value, Attributes) ->
    Key = {Name, Attributes, ReaderId},
    Boundaries = maps:get(boundaries, Options, ?DEFAULT_BOUNDARIES),
    try ets:lookup_element(Table, Key, #explicit_histogram_aggregation.bucket_counts) of
        BucketCounts0 ->
            BucketCounts = case BucketCounts0 of
                               undefined ->
                                   new_bucket_counts(Boundaries);
                               _ ->
                                   BucketCounts0
                           end,

            BucketIdx = find_bucket(Boundaries, Value),
            counters:add(BucketCounts, BucketIdx, 1),

            %% since we need the Key in the MatchHead for the index to be used we
            %% can't use `ets:fun2ms' as it will shadow `Key' in the `fun' head
            MS = [{{explicit_histogram_aggregation,Key,'_','_','_','_','_','$1','$2','$3'},
                   [{'<','$2',{const,Value}},{'>','$1',{const,Value}}],
                   [{{explicit_histogram_aggregation,
                      {element,2,'$_'},
                      {element,3,'$_'},
                      {element,4,'$_'},
                      {element,5,'$_'},
                      {element,6,'$_'},
                      {const,BucketCounts},
                      {const,Value},
                      {const,Value},
                      {'+','$3',{const,Value}}}}]},
                  {{explicit_histogram_aggregation,Key,'_','_','_','_','_','_','$1','$2'},
                   [{'<','$1',{const,Value}}],
                   [{{explicit_histogram_aggregation,
                      {element,2,'$_'},
                      {element,3,'$_'},
                      {element,4,'$_'},
                      {element,5,'$_'},
                      {element,6,'$_'},
                      {const,BucketCounts},
                      {element,8,'$_'},
                      {const,Value},
                      {'+','$2',{const,Value}}}}]},
                  {{explicit_histogram_aggregation,Key,'_','_','_','_','_','$1','_','$2'},
                   [{'>','$1',{const,Value}}],
                   [{{explicit_histogram_aggregation,
                      {element,2,'$_'},
                      {element,3,'$_'},
                      {element,4,'$_'},
                      {element,5,'$_'},
                      {element,6,'$_'},
                      {const,BucketCounts},
                      {const,Value},
                      {element,9,'$_'},
                      {'+','$2',{const,Value}}}}]},
                  {{explicit_histogram_aggregation,Key,'_','_','_','_','_','_','_','$1'},
                   [],
                   [{{explicit_histogram_aggregation,
                      {element,2,'$_'},
                      {element,3,'$_'},
                      {element,4,'$_'},
                      {element,5,'$_'},
                      {element,6,'$_'},
                      {const,BucketCounts},
                      {element,8,'$_'},
                      {element,9,'$_'},
                      {'+','$1',{const,Value}}}}]}],
            1 =:= ets:select_replace(Table, MS)
    catch
        error:badarg->
            %% since we need the options to initialize a histogram `false' is
            %% returned and `otel_metric_server' will initialize the histogram
            false
    end.

-dialyzer({nowarn_function, checkpoint/5}).
checkpoint(Tab, Name, ReaderPid, ?AGGREGATION_TEMPORALITY_DELTA, CollectionStartNano) ->
    MS = [{#explicit_histogram_aggregation{key='$1',
                                           start_time_unix_nano='_',
                                           boundaries='$2',
                                           record_min_max='$3',
                                           checkpoint='_',
                                           bucket_counts='$5',
                                           min='$6',
                                           max='$7',
                                           sum='$8'
                                          },
           [{'=:=', {element, 1, '$1'}, {const, Name}},
            {'=:=', {element, 3, '$1'}, {const, ReaderPid}}],
           [{#explicit_histogram_aggregation{key='$1',
                                             start_time_unix_nano={const, CollectionStartNano},
                                             boundaries='$2',
                                             record_min_max='$3',
                                             checkpoint={#explicit_histogram_checkpoint{bucket_counts='$5',
                                                                                        min='$6',
                                                                                        max='$7',
                                                                                        sum='$8'}},
                                             bucket_counts={const, undefined},
                                             min=infinity,
                                             max=?MIN_DOUBLE,
                                             sum=0}}]}],
    _ = ets:select_replace(Tab, MS),

    ok;
checkpoint(_Tab, _Name, _ReaderPid, _, _CollectionStartNano) ->
    %% no good way to checkpoint the `counters' without being out of sync with
    %% min/max/sum, so may as well just collect them in `collect', which will
    %% also be out of sync, but best we can do right now
    
    ok.

collect(Tab, Name, ReaderPid, _, CollectionStartTime) ->
    Select = [{'$1',
               [{'==', Name, {element, 1, {element, 2, '$1'}}},
                {'==', ReaderPid, {element, 3, {element, 2, '$1'}}}],
               ['$1']}],
    AttributesAggregation = ets:select(Tab, Select),
    [datapoint(CollectionStartTime, SumAgg) || SumAgg <- AttributesAggregation].

%%

datapoint(CollectionStartNano, #explicit_histogram_aggregation{
                                  key={_, Attributes, _},
                                  boundaries=Boundaries,
                                  start_time_unix_nano=StartTimeUnixNano,
                                  checkpoint=undefined,
                                  bucket_counts=BucketCounts,
                                  min=Min,
                                  max=Max,
                                  sum=Sum
                                 }) ->
    Buckets = get_buckets(BucketCounts, Boundaries),
    #histogram_datapoint{
       attributes=Attributes,
       start_time_unix_nano=StartTimeUnixNano,
       time_unix_nano=CollectionStartNano,
       count=lists:sum(Buckets),
       sum=Sum,
       bucket_counts=Buckets,
       explicit_bounds=Boundaries,
       exemplars=[],
       flags=0,
       min=Min,
       max=Max
      };
datapoint(CollectionStartNano, #explicit_histogram_aggregation{
                                  key={_, Attributes, _},
                                  boundaries=Boundaries,
                                  start_time_unix_nano=StartTimeUnixNano,
                                  checkpoint=#explicit_histogram_checkpoint{bucket_counts=BucketCounts,
                                                                            min=Min,
                                                                            max=Max,
                                                                            sum=Sum}
                                 }) ->
    Buckets = get_buckets(BucketCounts, Boundaries),
    #histogram_datapoint{
       attributes=Attributes,
       start_time_unix_nano=StartTimeUnixNano,
       time_unix_nano=CollectionStartNano,
       count=lists:sum(Buckets),
       sum=Sum,
       bucket_counts=Buckets,
       explicit_bounds=Boundaries,
       exemplars=[],
       flags=0,
       min=Min,
       max=Max
      }.

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

get_buckets(BucketCounts, Boundaries) ->
    lists:foldl(fun(Idx, Acc) ->
                        Acc ++ [counters_get(BucketCounts, Idx)]
                end, [], lists:seq(1, length(Boundaries))).

counters_get(undefined, _) ->
    0;
counters_get(Counter, Idx) ->
    counters:get(Counter, Idx).

new_bucket_counts(Boundaries) ->
    counters:new(length(Boundaries), [write_concurrency]).
