-module(otel_aggregation_drop).

-export([aggregate/3,
         collect/5]).

-include("otel_metrics.hrl").

-type t() :: #drop_aggregation{}.

-export_type([t/0]).

aggregate(_, _, _) ->
    true.

collect(_, _, _, _) ->
    [].
