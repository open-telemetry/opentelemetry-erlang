-module(otel_aggregation_drop).

-behaviour(otel_aggregation).

-export([init/2,
         aggregate/7,
         collect/3]).

-include("otel_metrics.hrl").

-type t() :: #drop_aggregation{}.

-export_type([t/0]).

init(_, _) ->
    #drop_aggregation{}.

aggregate(_, _, _, _, _, _, _) ->
    true.

collect(_, _, _) ->
    {}.
