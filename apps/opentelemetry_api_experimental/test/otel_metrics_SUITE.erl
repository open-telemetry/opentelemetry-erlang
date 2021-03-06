-module(otel_metrics_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_meter.hrl").

all() ->
    [noop_metrics, macros, non_overridable].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

noop_metrics(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({otel_meter_noop, _}, Meter),

    ?assert(otel_counter:new(Meter, <<"noop-measure-1">>, #{description => <<"some description">>})),

    ok.

macros(_Config) ->
    ?otel_new_instruments([#{name => <<"macros-measure-1">>,
                           description => <<"some description">>,
                           kind => counter,
                           label_keys => [],
                           monotonic => true,
                           absolute => true,
                           unit => one}]),
    ok.

%% checks that opts from the user can't override static attributes of an instrument
non_overridable(_Config) ->
    {_, _, Instrument} = otel_counter:definition(<<"noop-measure-1">>, #{description => <<"some description">>,
                                                                       monotonic => false,
                                                                       synchronous => false}),

    ?assert(maps:get(monotonic, Instrument)),
    ?assert(maps:get(synchronous, Instrument)),

    ok.
