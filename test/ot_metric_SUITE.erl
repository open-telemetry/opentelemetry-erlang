-module(ot_metric_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/meter.hrl").
-include("ot_test_utils.hrl").

all() ->
    [{group, without_api},
     {group, with_api}].

groups() ->
    [{without_api, [shuffle], [observer, counter, mmsc]},
     {with_api, [shuffle], [e2e_test]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%% without_api means the tests call the functions in this
%% application directly and do not go through the api
init_per_group(without_api, Config) ->
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config;
init_per_group(with_api, Config) ->
    %% TODO: configure an exporter and test that works
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config.

end_per_group(_, _Config) ->
    _ = application:stop(opentelemetry),
    ok.

e2e_test(_Config) ->
    ?assertMatch({ot_meter_default, _}, ?current_meter),

    ?new_instruments([#{name => mycounter,
                        kind => counter,
                        input_type => integer,
                        label_keys => ["a"]}]),
    ?new_instruments([#{name => myfloat,
                        kind => counter,
                        input_type => float,
                        label_keys => ["a"]}]),
    ?counter_add(mycounter, 4, []),
    ?counter_add(mycounter, 5, []),

    ?counter_add(myfloat, 4.1, []),
    ?counter_add(myfloat, 5.1, []),

    ot_metric_accumulator:collect(),
    Records = ot_metric_integrator:read(),

    ?assertMatch(#{{mycounter, []} := #{value := 9},
                   {myfloat, []} := #{value := 9.2}}, Records),

    ok.

observer(_Config) ->
    ot_meter_default:new_instruments([], [#{name => myobserver,
                                            kind => observer,
                                            input_type => integer,
                                            label_keys => ["a"]}]),

    ot_meter_default:register_observer(meter, myobserver, fun(R) ->
                                                                  ot_observer:observe(R, 3, [{"a", "b"}]),
                                                                  ok
                                                          end),

    ot_metric_accumulator:collect(),
    Records = ot_metric_integrator:read(),

    ?assertMatch(#{{myobserver, [{"a", "b"}]} := #{value := {3, _}}}, Records),

    ot_meter_default:register_observer(meter, myobserver, fun(R) ->
                                                                  ot_observer:observe(R, 5, []),
                                                                  ok
                                                          end),
    ot_meter_default:register_observer(meter, myobserver, fun(R) ->
                                                                  ot_observer:observe(R, 6, []),
                                                                  ok
                                                          end),

    ot_metric_accumulator:collect(),
    ?assertMatch(#{{myobserver, [{"a", "b"}]} := #{value := undefined},
                   {myobserver, []} := #{value := {6, _}}}, ot_metric_integrator:read()),

    ok.

counter(_Config) ->
    ot_meter_default:new_instruments([], [#{name => c1,
                                            kind => counter,
                                            input_type => integer,
                                            label_keys => [key1]}]),

    %% a bad measurement should be ignored
    ?assertEqual(ok, ot_meter_default:record(meter, m1, [{key1, value1}], undefined)),

    ot_meter_default:record(meter, c1, [{key1, value1}], 2),
    ot_meter_default:record(meter, c1, [{key1, value2}], 8),
    ot_meter_default:record(meter, c1, [{key1, value1}], 5),

    ot_metric_accumulator:collect(),
    Records = ot_metric_integrator:read(),

    %% check mmsc (min, max, sum, count) aggregation values
    ?assertMatch(#{{c1, [{key1, value1}]} := #{value := 7},
                   {c1, [{key1, value2}]} := #{value := 8}}, Records),
    ok.


mmsc(_Config) ->
    ot_meter_default:new_instruments([], [#{name => m1,
                                            kind => measure,
                                            input_type => integer,
                                            label_keys => [key1]}]),

    ot_meter_default:record(meter, m1, [{key1, value1}], 2),
    ot_meter_default:record(meter, m1, [{key1, value2}], 8),
    ot_meter_default:record(meter, m1, [{key1, value1}], 5),

    ot_metric_accumulator:collect(),
    Records = ot_metric_integrator:read(),

    %% check mmsc (min, max, sum, count) aggregation values
    ?assertMatch(#{{m1, [{key1, value1}]} := #{value := {2, 5, 7, 2}},
                   {m1, [{key1, value2}]} := #{value := {8, 8, 8, 1}}}, Records),

    ot_meter_default:record(meter, m1, [{key1, value1}], 2),
    ot_meter_default:record(meter, m1, [{key1, value2}], 8),
    ot_meter_default:record(meter, m1, [{key1, value1}], 5),

    ot_metric_accumulator:collect(),
    Records1 = ot_metric_integrator:read(),

    %% check mmsc (min, max, sum, count) aggregation values
    ?assertMatch(#{{m1, [{key1, value1}]} := #{value := {2, 5, 7, 2}},
                   {m1, [{key1, value2}]} := #{value := {8, 8, 8, 1}}}, Records1),

    ok.
