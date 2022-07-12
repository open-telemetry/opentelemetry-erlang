-module(otel_aggregation_histogram_explicit).

-export([init/2,
         aggregate/3,
         collect/5]).

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

collect(_, _AggregationTemporality, CollectionStartNano,
        #explicit_histogram_aggregation{
           start_time_unix_nano=StartTimeUnixNano,
           boundaries=Boundaries,
           bucket_counts=Buckets,
           record_min_max=_RecordMinMax,
           min=Min,
           max=Max,
           sum=Sum
          }, Attributes) ->
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


%%

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
