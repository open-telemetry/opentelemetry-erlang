-module(otel_metrics_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").
-include("meter.hrl").

all() ->
    [noop_metrics, macros, non_overridable].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

noop_metrics(_Config) ->
    Meter = opentelemetry:get_meter(),
    ?assertMatch({ot_meter_noop, _}, Meter),

    ?assert(ot_counter:new(Meter, <<"noop-measure-1">>, #{description => <<"some description">>})),

    ok.

macros(_Config) ->
    ?ot_new_instruments([#{name => <<"macros-measure-1">>,
                           description => <<"some description">>,
                           kind => counter,
                           label_keys => [],
                           monotonic => true,
                           absolute => true,
                           unit => one}]),
    ok.

%% checks that opts from the user can't override static attributes of an instrument
non_overridable(_Config) ->
    {_, _, Instrument} = ot_counter:definition(<<"noop-measure-1">>, #{description => <<"some description">>,
                                                                       monotonic => false,
                                                                       synchronous => false}),

    ?assert(maps:get(monotonic, Instrument)),
    ?assert(maps:get(synchronous, Instrument)),

    ok.
