-module(otel_aggregation_drop).

-export([new/4,
         aggregate/2,
         collect/3]).

-include("otel_metrics.hrl").

-type t() :: #drop_aggregation{}.

-export_type([t/0]).

new(_Instrument, _Attributes, _StartTimeUnixNano, _Options) ->
    #drop_aggregation{}.

aggregate(_, Aggregation) ->
    Aggregation.

collect(_, _, _) ->
    [].
