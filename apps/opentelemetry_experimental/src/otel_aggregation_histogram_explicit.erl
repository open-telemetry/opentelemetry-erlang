-module(otel_aggregation_histogram_explicit).

-export([new/4,
         aggregate/2,
         collect/3]).

-include("otel_metrics.hrl").

-type t() :: #explicit_histogram_aggregation{}.

-export_type([t/0]).

new(_Instrument, Attributes, StartTimeUnixNano, Options) ->
    Boundaries = maps:get(boundaries, Options, {0.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0, 250.0, 500.0, 1000.0}),
    RecordMinMax = maps:get(record_min_max, Options, true),
    #explicit_histogram_aggregation{attributes=Attributes,
                                    start_time_unix_nano=StartTimeUnixNano,
                                    boundaries=Boundaries,
                                    bucket_counts=zero_buckets(tuple_size(Boundaries)),
                                    record_min_max=RecordMinMax,
                                    min=neg_infinity,
                                    max=infinity,
                                    sum=0,
                                    instrument_temporality=?AGGREGATION_TEMPORALITY_DELTA}.

aggregate(_Measurement, Aggregation) ->
    Aggregation.

collect(_AggregationTemporality, _CollectionStartNano, _Aggregation) ->
    #datapoint{}.

%%

zero_buckets(Size) ->
    erlang:list_to_tuple(lists:duplicate(Size, 0)).
