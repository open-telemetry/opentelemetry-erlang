-module(otel_metrics_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("otel_test_utils.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").
-include("otel_metrics.hrl").

-define(assertReceive(Name, Description, Value, Unit, Attributes),
        (fun() ->
                 receive
                     #metric{name=MetricName,
                             description=MetricDescription,
                             unit=MetricUnit,
                             data=#sum{datapoints=[#datapoint{value=MetricValue,
                                                              attributes=MetricAttributes}]}}
                     when MetricName =:= Name ->
                         ?assertEqual(Description, MetricDescription),
                         ?assertEqual(Unit, MetricUnit),
                         ?assertEqual(Description, MetricDescription),
                         ?assertEqual(Value, MetricValue),
                         ?assertEqual(Attributes, MetricAttributes)
                 after
                     5000 ->
                         ct:fail(metric_receive_timeout)
                 end
         end)()).

all() ->
    [provider_test, view_creation_test, counter_add, add_readers,
     reader_aggregations].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(opentelemetry_experimental),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opentelemetry_experimental),
    ok.

provider_test(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,
    ValueType = integer,

    Counter = otel_meter:create_counter(Meter, CounterName, ValueType,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             value_type = ValueType,
                             unit = CounterUnit}, Counter),

    ?assertEqual(ok, otel_meter_server:add_metric_reader(otel_metric_reader, #{exporter => {otel_metric_exporter_pid, self()}})),

    otel_meter_server:add_view(#{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum}),
    otel_meter_server:add_view(view_a, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum}),
    otel_meter_server:add_view(view_b, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum}),

    %% use the default Reader aggregation
    otel_meter_server:add_view(view_c, #{instrument_name => a_counter}, #{}),


    ?assertEqual(ok, otel_counter:add(Counter, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 4, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 5, #{<<"c">> => <<"b">>})),

    ?assertReceive(a_counter, <<"counter description">>, 11, kb, #{<<"c">> => <<"b">>}),
    ?assertReceive(view_c, <<"counter description">>, 11, kb, #{<<"c">> => <<"b">>}),

    ok.

view_creation_test(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,
    ValueType = integer,

    Counter = otel_meter:create_counter(Meter, CounterName, ValueType,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             value_type = ValueType,
                             unit = CounterUnit}, Counter),


    ?assert(otel_meter_server:add_view(view_a, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum})),

    View = otel_view:new(#{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum}),
    %% view name becomes the instrument name
    ?assertEqual(a_counter, View#view.name),
    Attributes = #{},
    Matches = otel_view:match_instrument_to_views(Counter, Attributes, [View], [otel_aggregation:default_mapping()]),
    ?assertMatch([_], Matches),

    %% views require a unique name
    ?assert(otel_meter_server:add_view(view_b, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum})),
    %% ?assertNot(otel_meter_server:add_view(view_b, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum})),

    %% only one view that matches all instruments can be allowed
    ?assert(otel_meter_server:add_view(view_c, #{}, #{aggregation => otel_aggregation_sum})),
    %% ?assertNot(otel_meter_server:add_view(view_d, #{}, #{aggregation => otel_aggregation_sum})),

    ?assert(otel_meter_server:add_view(#{instrument_name => b_counter}, #{aggregation => otel_aggregation_sum})),
    ?assert(otel_meter_server:add_view(#{instrument_name => c_counter}, #{aggregation => otel_aggregation_sum})),

    ok.

counter_add(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,
    ValueType = integer,

    Counter = otel_meter:create_counter(Meter, CounterName, ValueType,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),

    ?assertMatch(ok, otel_counter:add(Counter, 3, #{})),
    ok.

add_readers(_Config) ->
    ?assertEqual(ok, otel_meter_server:add_metric_reader(otel_metric_reader, #{})),
    ?assertEqual(ok, otel_meter_server:add_metric_reader(otel_metric_reader,
                                                         #{default_aggregation_mapping =>
                                                               #{?KIND_COUNTER => otel_aggregation_histogram_explicit,
                                                                 ?KIND_OBSERVABLE_GAUGE => otel_aggregation_histogram_explicit}})),

    ok.

reader_aggregations(_Config) ->
    %% ?assertEqual(ok, otel_meter_server:add_metric_reader(otel_metric_reader,
    %%                                                      #{default_aggregation_mapping =>
    %%                                                            #{?KIND_COUNTER => otel_aggregation_histogram_explicit,
    %%                                                              ?KIND_OBSERVABLE_GAUGE => otel_aggregation_histogram_explicit}})),

    ok.
