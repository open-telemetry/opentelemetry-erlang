-module(otel_aggregation_sum).

-export([init/2,
         aggregate/3,
         collect/5]).

-include("otel_metrics.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").

-type t() :: #sum_aggregation{}.

-export_type([t/0]).

init(Key, _Options) ->
    #sum_aggregation{key=Key,
                     start_time_unix_nano=erlang:system_time(nanosecond),
                     value=0}.

aggregate(Tab, Key, Value) ->
    try
        _ = ets:update_counter(Tab, Key, {#sum_aggregation.value, Value}),
        true
    catch
        error:badarg ->
            %% the use of `update_counter' guards against conflicting with another process
            %% doing the update at the same time -- if we ever support that

            %% the default isn't just given in the first `update_counter' because then
            %% we'd have to call `system_time' for every single measurement taken
            _ = ets:update_counter(Tab, Key, {#sum_aggregation.value, Value},
                                   init(Key, [])),
            true
    end.

collect(Tab, ?AGGREGATION_TEMPORALITY_DELTA, CollectionStartNano, ActiveMetric=#sum_aggregation{key=Key}, Attributes) ->
    _ = ets:update_element(Tab, Key, [{#sum_aggregation.start_time_unix_nano, CollectionStartNano},
                                      {#sum_aggregation.value, 0}]),
    datapoint(CollectionStartNano, ActiveMetric, Attributes);
collect(_Tab, _AggregationTemporality, CollectionStartNano, ActiveMetric, Attributes) ->
    datapoint(CollectionStartNano, ActiveMetric, Attributes).

datapoint(CollectionStartNano, #sum_aggregation{start_time_unix_nano=StartTimeUnixNano,
                                                value=Value}, Attributes) ->
    #datapoint{
       attributes=Attributes,
       start_time_unix_nano=StartTimeUnixNano,
       time_unix_nano=CollectionStartNano,
       value=Value,
       exemplars=[],
       flags=0
      }.
