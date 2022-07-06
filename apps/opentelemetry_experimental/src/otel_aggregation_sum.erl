-module(otel_aggregation_sum).

-export([new/4,
         aggregate/2,
         collect/4]).

-include("otel_metrics.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").

-type t() :: #sum_aggregation{}.

-export_type([t/0]).

new(Instrument, Attributes, StartTimeUnixNano, _Options) ->
    new_(Attributes,
         otel_instrument:is_monotonic(Instrument),
         otel_aggregation:instrument_temporality(Instrument),
         StartTimeUnixNano).

new_(Attributes, IsMonotonic, Temporality, StartTimeUnixNano) ->
    #sum_aggregation{attributes=Attributes,
                     instrument_is_monotonic=IsMonotonic,
                     instrument_temporality=Temporality,
                     start_time_unix_nano=StartTimeUnixNano,
                     value=case Temporality of
                               ?AGGREGATION_TEMPORALITY_UNSPECIFIED ->
                                   0;
                               ?AGGREGATION_TEMPORALITY_DELTA ->
                                   0;
                               ?AGGREGATION_TEMPORALITY_CUMULATIVE ->
                                   undefined
                           end}.

aggregate(#measurement{value=MeasurementValue}, Aggregation=#sum_aggregation{value=undefined}) ->
    Aggregation#sum_aggregation{value=MeasurementValue};
aggregate(#measurement{value=MeasurementValue}, Aggregation=#sum_aggregation{value=Value}) ->
   Aggregation#sum_aggregation{value=Value + MeasurementValue}.

collect(_AggregationTemporality, CollectionStartNano, #sum_aggregation{start_time_unix_nano=StartTimeUnixNano,
                                                                       value=Value}, Attributes) ->
    #datapoint{
       attributes=Attributes,
       start_time_unix_nano=StartTimeUnixNano,
       time_unix_nano=CollectionStartNano,
       value=Value,
       exemplars=undefined,
       flags=0
      }.
