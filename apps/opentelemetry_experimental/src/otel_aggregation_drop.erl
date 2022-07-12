-module(otel_aggregation_drop).

-export([init/2,
         aggregate/3,
         collect/5]).

-include("otel_metrics.hrl").

-type t() :: #drop_aggregation{}.

-export_type([t/0]).

init(_, _) ->
    #drop_aggregation{}.

aggregate(_, _, _) ->
    true.

collect(_, _, _, _, _) ->
    [].
