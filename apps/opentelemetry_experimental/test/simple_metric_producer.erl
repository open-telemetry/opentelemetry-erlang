-module(simple_metric_producer).

-export([init/1,
         produce_batch/1]).

-include("otel_metrics.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

init(_) ->
    [].

produce_batch(_) ->
    [#metric{scope=#instrumentation_scope{name = <<"scope-1">>,
                                          version = <<"1.1.1">>,
                                          schema_url=undefined},
             name=external_counter_1,
             description = <<"external counter description">>,
             unit=kb,
             data=#sum{datapoints=[#datapoint{
                                      attributes=#{<<"a">> => <<"b">>},
                                      start_time=opentelemetry:timestamp(),
                                      time=opentelemetry:timestamp(),
                                      value=50,
                                      exemplars=[],
                                      flags=0
                                     }]}
            }].
