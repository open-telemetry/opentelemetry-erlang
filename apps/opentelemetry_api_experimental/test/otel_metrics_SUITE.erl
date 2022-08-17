-module(otel_metrics_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

all() ->
    [{group, integer}, {group, float}].

groups() ->
    [{integer, [shuffle, parallel], [sync_instruments]},
     {float, [shuffle, parallel], [sync_instruments]}].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Group, Config) ->
    [{value_type, Group} | Config].

end_per_group(_, _) ->
    ok.

sync_instruments(Config) ->
    ValueType = ?config(value_type, Config),

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({otel_meter_noop, _}, Meter),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    Counter = otel_meter:create_counter(Meter, CounterName, ValueType,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),
    ?assertEqual(#{meter => {otel_meter_noop,[]},
                   module => otel_meter_noop,
                   name => CounterName,
                   description => CounterDesc,
                   kind => counter,
                   value_type => ValueType,
                   unit => CounterUnit}, Counter),

    ?assertEqual(ok, otel_counter:add(Counter, value(ValueType), #{})),

    UpDownCounterName = a_updown_counter,
    UpDownCounterDesc = <<"updown counter description">>,
    UpDownCounterUnit = '1',

    UpDownCounter = otel_meter:create_updown_counter(Meter, UpDownCounterName, ValueType,
                                        #{description => UpDownCounterDesc,
                                          unit => UpDownCounterUnit}),
    ?assertEqual(#{meter => {otel_meter_noop,[]},
                   module => otel_meter_noop,
                   name => UpDownCounterName,
                   description => UpDownCounterDesc,
                   kind => updown_counter,
                   value_type => ValueType,
                   unit => UpDownCounterUnit}, UpDownCounter),

    ?assertEqual(ok, otel_counter:add(Counter, 0 - value(ValueType), #{})),

    HistogramName = a_histogram,
    HistogramDesc = <<"histogram description">>,
    HistogramUnit = ms,

    Histogram = otel_meter:create_histogram(Meter, HistogramName, ValueType,
                                        #{description => HistogramDesc,
                                          unit => HistogramUnit}),
    ?assertEqual(#{meter => {otel_meter_noop,[]},
                   module => otel_meter_noop,
                   name => HistogramName,
                   description => HistogramDesc,
                   kind => histogram,
                   value_type => ValueType,
                   unit => HistogramUnit}, Histogram),

    ?assertEqual(ok, otel_histogram:record(Counter, value(ValueType), #{})),

    ok.

%%

value(integer) ->
    3;
value(float) ->
    2.0.
