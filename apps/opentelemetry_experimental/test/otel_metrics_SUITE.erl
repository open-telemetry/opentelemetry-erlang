-module(otel_metrics_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("otel_test_utils.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").
-include("otel_metrics.hrl").

-define(assertReceive(Name, Description, Unit, Datapoints),
        (fun() ->
                 receive
                     {otel_metric, #metric{name=MetricName,
                                           description=MetricDescription,
                                           unit=MetricUnit,
                                           data=#sum{datapoints=MetricDatapoints}}}
                       when MetricName =:= Name ->
                         ?assertEqual(Description, MetricDescription),
                         ?assertEqual(Unit, MetricUnit),
                         ?assertEqual(Description, MetricDescription),

                         SortedDatapoints =
                             lists:sort([{MetricValue, MetricAttributes} ||
                                            #datapoint{value=MetricValue,
                                                       attributes=MetricAttributes} <- MetricDatapoints]),
                         ?assertMatch([], lists:sort(Datapoints) -- SortedDatapoints, SortedDatapoints)
                 after
                     5000 ->
                         ct:fail({metric_receive_timeout, ?LINE})
                 end
         end)()).

-define(assertNotReceive(Name, Description, Unit),
        (fun() ->
                 receive
                     M=#metric{name=MetricName,
                               description=MetricDescription,
                               unit=MetricUnit}
                       when MetricName =:= Name,
                            MetricDescription =:= Description,
                            MetricUnit =:= Unit ->
                         ct:fail({metric_received, M})
                 after
                     0 ->
                         ok
                 end
         end)()).

all() ->
    [default_view, provider_test, view_creation_test, counter_add, add_readers,
     explicit_histograms, reader_aggregations, cumulative_counter].

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

default_view(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = z_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,
    ValueType = integer,

    Counter = otel_meter:counter(Meter, CounterName, ValueType,
                                 #{description => CounterDesc,
                                   unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             value_type = ValueType,
                             unit = CounterUnit}, Counter),

    ?assertEqual(ok, otel_meter_server:add_metric_reader(?DEFAULT_METER_PROVIDER, otel_metric_reader, #{exporter => {otel_metric_exporter_pid, self()}})),

    ?assertEqual(ok, otel_counter:add(Counter, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 4, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 5, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(?DEFAULT_METER_PROVIDER),

    ?assertReceive(z_counter, <<"counter description">>, kb, [{11, #{<<"c">> => <<"b">>}}]),

    ok.

provider_test(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,
    ValueType = integer,

    Counter = otel_meter:counter(Meter, CounterName, ValueType,
                                 #{description => CounterDesc,
                                   unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             value_type = ValueType,
                             unit = CounterUnit}, Counter),

    ?assertEqual(ok, otel_meter_server:add_metric_reader(?DEFAULT_METER_PROVIDER, otel_metric_reader, #{exporter => {otel_metric_exporter_pid, self()}})),

    otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum}),
    otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, view_a, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum}),
    otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, view_b, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum}),

    %% use the default Reader aggregation
    otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, view_c, #{instrument_name => a_counter}, #{}),

    ?assertEqual(ok, otel_counter:add(Counter, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 4, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 5, #{<<"c">> => <<"b">>})),

    %% this measurement will be ignored because counter is of type integer
    ?assertEqual(ok, otel_counter:add(Counter, 5.0, #{<<"c">> => <<"b">>})),

    %% ignored because only positive measurements are allowed for counters
    ?assertEqual(ok, otel_counter:add(Counter, -10, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(?DEFAULT_METER_PROVIDER),

    ?assertReceive(a_counter, <<"counter description">>, kb, [{11, #{<<"c">> => <<"b">>}}]),
    ?assertReceive(view_c, <<"counter description">>, kb, [{11, #{<<"c">> => <<"b">>}}]),

    %% sum agg is default delta temporality so counter will reset
    ?assertEqual(ok, otel_counter:add(Counter, 7, #{<<"c">> => <<"b">>})),
    otel_meter_server:force_flush(?DEFAULT_METER_PROVIDER),
    ?assertReceive(a_counter, <<"counter description">>, kb, [{7, #{<<"c">> => <<"b">>}},
                                                              {0, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>}}]),

    ok.

view_creation_test(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,
    ValueType = integer,

    Counter = otel_meter:counter(Meter, CounterName, ValueType,
                                 #{description => CounterDesc,
                                   unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             value_type = ValueType,
                             unit = CounterUnit}, Counter),


    ?assert(otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, view_a, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum})),

    View = otel_view:new(#{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum}),
    %% view name becomes the instrument name
    ?assertEqual(a_counter, View#view.name),

    Matches = otel_view:match_instrument_to_views(Counter, [View]),
    ?assertMatch([_], Matches),

    %% views require a unique name
    ?assert(otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, view_b, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum})),
    %% ?assertNot(otel_meter_server:add_view(view_b, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum})),

    %% only one view that matches all instruments can be allowed
    ?assert(otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, view_c, #{}, #{aggregation => otel_aggregation_sum})),
    %% ?assertNot(otel_meter_server:add_view(view_d, #{}, #{aggregation => otel_aggregation_sum})),

    ?assert(otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, #{instrument_name => b_counter}, #{aggregation => otel_aggregation_sum})),
    ?assert(otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, #{instrument_name => c_counter}, #{aggregation => otel_aggregation_sum})),

    ok.

counter_add(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,
    ValueType = integer,

    Counter = otel_meter:counter(Meter, CounterName, ValueType,
                                 #{description => CounterDesc,
                                   unit => CounterUnit}),

    ?assertMatch(ok, otel_counter:add(Counter, 3, #{})),
    ok.

add_readers(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),

    CounterDesc = <<"counter description">>,
    CounterUnit = kb,
    ValueType = integer,

    CounterA = otel_meter:counter(Meter, a_counter, ValueType,
                                  #{description => CounterDesc,
                                    unit => CounterUnit}),
    CounterB = otel_meter:counter(Meter, b_counter, ValueType,
                                  #{description => CounterDesc,
                                    unit => CounterUnit}),

    ?assertEqual(ok, otel_meter_server:add_metric_reader(?DEFAULT_METER_PROVIDER,
                                                         otel_metric_reader,
                                                         #{exporter => {otel_metric_exporter_pid, self()}})),
    ?assertEqual(ok, otel_meter_server:add_metric_reader(?DEFAULT_METER_PROVIDER,
                                                         otel_metric_reader,
                                                         #{exporter => {otel_metric_exporter_pid, self()},
                                                           default_aggregation_mapping =>
                                                               #{?KIND_COUNTER => otel_aggregation_drop}})),

    otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum}),
    otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, #{instrument_name => b_counter}, #{}),

    ?assertEqual(ok, otel_counter:add(CounterA, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(CounterA, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(CounterA, 4, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(CounterA, 5, #{<<"c">> => <<"b">>})),

    ?assertEqual(ok, otel_counter:add(CounterB, 2, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(?DEFAULT_METER_PROVIDER),

    %% 2nd reader has counter set to drop so only 1 of b_counter is expected bc it does
    %% not set an aggregation in the view definition
    ?assertReceive(a_counter, <<"counter description">>, kb, [{11, #{<<"c">> => <<"b">>}}]),
    ?assertReceive(a_counter, <<"counter description">>, kb, [{11, #{<<"c">> => <<"b">>}}]),
    ?assertReceive(b_counter, <<"counter description">>, kb, [{2, #{<<"c">> => <<"b">>}}]),
    ?assertNotReceive(b_counter, <<"counter description">>, kb),

    ok.

explicit_histograms(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    HistogramName = a_histogram,
    HistogramDesc = <<"histogram description">>,
    HistogramUnit = ms,
    ValueType = integer,

    Histogram = otel_meter:histogram(Meter, HistogramName, ValueType,
                                     #{description => HistogramDesc,
                                       unit => HistogramUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = HistogramName,
                             description = HistogramDesc,
                             kind = histogram,
                             value_type = ValueType,
                             unit = HistogramUnit}, Histogram),

    ?assertEqual(ok, otel_meter_server:add_metric_reader(?DEFAULT_METER_PROVIDER, otel_metric_reader, #{exporter => {otel_metric_exporter_pid, self()}})),

    otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, #{instrument_name => a_histogram}, #{}),


    ?assertEqual(ok, otel_histogram:record(Histogram, 20, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Histogram, 30, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_histogram:record(Histogram, 44, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Histogram, 100, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(?DEFAULT_METER_PROVIDER),

    receive
        {otel_metric, #metric{name=a_histogram,
                              data=#histogram{datapoints=Datapoints}}} ->
            AttributeBuckets =
                [{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                              attributes=Attributes,
                                                                              min=Min,
                                                                              max=Max,
                                                                              sum=Sum}  <- Datapoints],
            ?assertEqual([], [{#{<<"c">> => <<"b">>}, {0,0,0,1,1,0,1,0,0,0}, 20, 100, 164},
                              {#{<<"a">> => <<"b">>, <<"d">> => <<"e">>}, {0,0,0,0,1,0,0,0,0,0}, 30, 30, 30}]
                         -- AttributeBuckets, AttributeBuckets)
    after
        5000 ->
            ct:fail(histogram_receive_timeout)
    end,

    ok.

reader_aggregations(_Config) ->
    %% ?assertEqual(ok, otel_meter_server:add_metric_reader(otel_metric_reader,
    %%                                                      #{default_aggregation_mapping =>
    %%                                                            #{?KIND_COUNTER => otel_aggregation_histogram_explicit,
    %%                                                              ?KIND_OBSERVABLE_GAUGE => otel_aggregation_histogram_explicit}})),

    ok.

cumulative_counter(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,
    ValueType = integer,

    Counter = otel_meter:counter(Meter, CounterName, ValueType,
                                 #{description => CounterDesc,
                                   unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             value_type = ValueType,
                             unit = CounterUnit}, Counter),

    CumulativeCounterTemporality = #{?KIND_COUNTER =>?AGGREGATION_TEMPORALITY_CUMULATIVE},
    ?assertEqual(ok, otel_meter_server:add_metric_reader(?DEFAULT_METER_PROVIDER,
                                                         otel_metric_reader,
                                                         #{exporter => {otel_metric_exporter_pid, self()},
                                                           default_temporality_mapping =>
                                                               CumulativeCounterTemporality})),

    otel_meter_server:add_view(?DEFAULT_METER_PROVIDER, #{instrument_name => a_counter}, #{aggregation => otel_aggregation_sum}),

    ?assertEqual(ok, otel_counter:add(Counter, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 4, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(?DEFAULT_METER_PROVIDER),

    ?assertReceive(a_counter, <<"counter description">>, kb, [{6, #{<<"c">> => <<"b">>}}]),

    ?assertEqual(ok, otel_counter:add(Counter, 5, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Counter, 7, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(?DEFAULT_METER_PROVIDER),

    ?assertReceive(a_counter, <<"counter description">>, kb, [{18, #{<<"c">> => <<"b">>}}]),

    ok.
