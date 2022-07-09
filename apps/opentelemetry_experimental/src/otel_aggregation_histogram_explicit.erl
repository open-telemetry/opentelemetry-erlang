-module(otel_aggregation_histogram_explicit).

-export([aggregate/3,
         collect/4]).

-include("otel_metrics.hrl").

-type t() :: #explicit_histogram_aggregation{}.

-export_type([t/0]).

-define(DEFAULT_BOUNDARIES, [0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 1000.0]).

aggregate(Table, Key, Value) ->
    case ets:lookup(Table, Key) of
        [ActiveMetric=#active_metric{value=Current}] ->
            ets:insert(Table, ActiveMetric#active_metric{value=aggregate(Value, Current)});
        _ ->
            Boundaries = ?DEFAULT_BOUNDARIES,
            Init = #explicit_histogram_aggregation{boundaries=Boundaries,
                                                   bucket_counts=zero_buckets(length(Boundaries)),
                                                   record_min_max=true,
                                                   min=infinity, %% works because any atom is < any integer
                                                   max=neg_infinity,
                                                   sum=0
                                                  },

            ets:insert(Table, #active_metric{key=Key,
                                             start_time_unix_nano=erlang:system_time(nanosecond),
                                             value=aggregate(Value, Init)})
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

collect(_, _AggregationTemporality, CollectionStartNano, #active_metric{start_time_unix_nano=StartTimeUnixNano,
                                                                        value=#explicit_histogram_aggregation
                                                                        {
                                                                          boundaries=Boundaries,
                                                                          bucket_counts=Buckets,
                                                                          record_min_max=_RecordMinMax,
                                                                          min=Min,
                                                                          max=Max,
                                                                          sum=Sum
                                                                        }}, Attributes) ->
    #histogram_datapoint
        {
       attributes=Attributes,
       start_time_unix_nano=StartTimeUnixNano,
       time_unix_nano=CollectionStartNano,
       count=0,
       sum=Sum,
       bucket_counts=Buckets,
       explicit_bounds=Boundaries,
       exemplars=[],
       flags=0,
       min=Min,
       max=Max
      }.


%%

zero_buckets(Size) ->
    erlang:list_to_tuple(lists:duplicate(Size, 0)).

find_bucket(Boundaries, Value) ->
    find_bucket(Boundaries, Value, 1).

find_bucket([X | _Rest], Value, Pos) when Value =< X ->
    Pos;
find_bucket([_X], Value, Pos) ->
    Pos;
find_bucket([_X | Rest], Value, Pos) ->
    find_bucket(Rest, Value, Pos+1);
find_bucket(_, _, Pos) ->
    Pos.

bump_bucket_counts(MeasurementValue, Boundaries, Buckets) ->
    Pos = find_bucket(Boundaries, MeasurementValue),
    setelement(Pos, Buckets, element(Pos, Buckets) + 1).
