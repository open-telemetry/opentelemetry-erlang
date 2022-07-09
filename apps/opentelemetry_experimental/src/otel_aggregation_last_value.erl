-module(otel_aggregation_last_value).

-export([aggregate/3,
         collect/5]).

-include("otel_metrics.hrl").

-type t() :: #last_value_aggregation{}.

-export_type([t/0]).

aggregate(Tab, Key, Value) ->
    case ets:update_element(Tab, Key, {#active_metric.value, Value}) of
        true ->
            true;
        false ->
            ets:insert(Tab, #active_metric{key=Key,
                                           start_time_unix_nano=erlang:system_time(nanosecond),
                                           value=Value})
    end.

collect(_, _AggregationTemporality, CollectionStartNano, #active_metric{value=Value}, Attributes) ->
    #datapoint{attributes=Attributes,
               time_unix_nano=CollectionStartNano,
               value=Value,
               exemplars=[],
               flags=0}.
