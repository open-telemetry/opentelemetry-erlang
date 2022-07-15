-module(otel_aggregation_last_value).

-export([init/2,
         aggregate/3,
         collect/5]).

-include("otel_metrics.hrl").

-type t() :: #last_value_aggregation{}.

-export_type([t/0]).

init(Key, _Options) ->
    #last_value_aggregation{key=Key,
                            value=0}.

aggregate(Tab, Key, Value) ->
    case ets:update_element(Tab, Key, {#last_value_aggregation.value, Value}) of
        true ->
            true;
        false ->
            Metric = init(Key, []),
            ets:insert(Tab, Metric#last_value_aggregation{value=Value})
    end.

collect(_Tab, _, _, #last_value_aggregation{value=undefined}, _) ->
    undefined;
collect(Tab, _, CollectionStartNano, #last_value_aggregation{key=Key,
                                                             value=Value}, Attributes) ->
    _ = ets:update_element(Tab, Key, {#last_value_aggregation.value, undefined}),
    #datapoint{attributes=Attributes,
               time_unix_nano=CollectionStartNano,
               value=Value,
               exemplars=[],
               flags=0}.
