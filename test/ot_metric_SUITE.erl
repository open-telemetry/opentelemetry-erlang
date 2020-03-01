-module(ot_metric_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_test_utils.hrl").

all() ->
    [observer, counter, mmsc_aggregator].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config.

end_per_suite(_Config) ->
    _ = application:stop(opentelemetry),
    ok.

observer(_Config) ->
    ot_meter_default:new_instruments([], [#{name => myobserver,
                                            kind => observer,
                                            input_type => integer,
                                            label_keys => ["a"]}]),

    ot_meter_default:register_observer(meter, myobserver, fun(R) ->
                                                                  ot_observer:observe(R, 3, #{"a" => "b"}),
                                                                  ok
                                                          end),

    ot_metric_accumulator:collect(),
    Records = ot_metric_integrator:read(),

    ?assertMatch(#{{myobserver, #{"a" => "b"}} := {3, _}}, Records),

    ot_meter_default:register_observer(meter, myobserver, fun(R) ->
                                                                  ot_observer:observe(R, 5, #{}),
                                                                  ok
                                                          end),
    ot_meter_default:register_observer(meter, myobserver, fun(R) ->
                                                                  ot_observer:observe(R, 6, #{}),
                                                                  ok
                                                          end),

    ot_metric_accumulator:collect(),
    ?assertMatch(#{{myobserver, #{"a" => "b"}} := {3, _},
                   {myobserver, #{}} := {6, _}}, ot_metric_integrator:read()),

    ok.

counter(_Config) ->
    ot_meter_default:new_instruments([], [#{name => c1,
                                            kind => counter,
                                            input_type => integer,
                                            label_keys => [key1]}]),

    ot_meter_default:record(meter, c1, #{key1 => value1}, 2),
    ot_meter_default:record(meter, c1, #{key1 => value2}, 8),
    ot_meter_default:record(meter, c1, #{key1 => value1}, 5),

    %% returns once all records are complete
    ot_meter_default:wait(),

    ot_metric_accumulator:collect(),
    Records = ot_metric_integrator:read(),

    %% check mmsc (min, max, sum, count) aggregation values
    ?assertMatch(#{{c1, #{key1 => value1}} := 7,
                   {c1, #{key1 => value2}} := 8}, Records),
    ok.


mmsc_aggregator(_Config) ->
    ot_meter_default:new_instruments([], [#{name => m1,
                                            kind => measure,
                                            input_type => integer,
                                            label_keys => [key1]}]),

    ot_meter_default:record(meter, m1, #{key1 => value1}, 2),
    ot_meter_default:record(meter, m1, #{key1 => value2}, 8),
    ot_meter_default:record(meter, m1, #{key1 => value1}, 5),

    %% returns once all records are complete
    ot_meter_default:wait(),

    ot_metric_accumulator:collect(),
    Records = ot_metric_integrator:read(),

    %% check mmsc (min, max, sum, count) aggregation values
    ?assertMatch(#{{m1, #{key1 => value1}} := {2, 5, 7, 2},
                   {m1, #{key1 => value2}} := {8, 8, 8, 1}}, Records),
    ok.
