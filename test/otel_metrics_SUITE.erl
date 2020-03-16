-module(otel_metrics_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").
-include("meter.hrl").

all() ->
    [noop_metrics, macros].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

noop_metrics(_Config) ->
    Meter = opentelemetry:get_meter(),
    ?assertMatch({ot_meter_noop, _}, Meter),

    ot_meter:new_instruments(Meter, [#{name => <<"noop-measure-1">>,
                                       description => <<"some description">>,
                                       kind => counter,
                                       label_keys => [],
                                       monotonic => true,
                                       absolute => true,
                                       unit => one}]),

    ok.

macros(_Config) ->
    ?new_instruments([#{name => <<"macros-measure-1">>,
                        description => <<"some description">>,
                        kind => counter,
                        label_keys => [],
                        monotonic => true,
                        absolute => true,
                        unit => one}]),
    ok.
