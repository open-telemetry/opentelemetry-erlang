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
    ok.


counter(_Config) ->
    ot_meter_default:new_instruments([], [#{name => c1,
                                            kind => counter,
                                            input_type => integer,
                                            label_keys => [key1]}]),

    ot_meter_default:record(meter, c1, #{key1 => value1}, 2),
    ot_meter_default:record(meter, c1, #{key1 => value2}, 8),
    ot_meter_default:record(meter, c1, #{key1 => value1}, 5),

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

    ot_meter_default:wait(),

    ot_metric_accumulator:collect(),
    Records = ot_metric_integrator:read(),

    %% check mmsc (min, max, sum, count) aggregation values
    ?assertMatch(#{{m1, #{key1 => value1}} := {2, 5, 7, 2},
                   {m1, #{key1 => value2}} := {8, 8, 8, 1}}, Records),
    ok.
