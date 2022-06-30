-module(otel_aggregation_last_value).

-export([new/4,
         aggregate/2,
         collect/3]).

-include("otel_metrics.hrl").

-type t() :: #last_value_aggregation{}.

-export_type([t/0]).

new(_Instrument, Attributes, StartTimeUnixNano, _Options) ->
    #last_value_aggregation{attributes=Attributes,
                            start_time_unix_nano=StartTimeUnixNano}.

aggregate(#measurement{value=MeasurementValue}, Aggregation) ->
    Aggregation#last_value_aggregation{value=MeasurementValue}.

collect(_AggregationTemporality, CollectionStartNano, #last_value_aggregation{attributes=Attributes,
                                                                              value=Value}) ->
    #datapoint{attributes=Attributes,
               start_time_unix_nano=0,
               time_unix_nano=CollectionStartNano,
               value=Value,
               exemplars=[],
               flags=0}.
