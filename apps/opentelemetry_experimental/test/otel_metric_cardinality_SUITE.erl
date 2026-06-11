%% Common Test integration suite for the metrics cardinality limit + overflow.
%%
%% Exercises the end-to-end behaviour through a real meter + reader, mirroring
%% the intent of opentelemetry-go's aggregate overflow tests and
%% opentelemetry-java's *SynchronousMetricStorage overflow tests:
%%
%%   * a reader-level `cardinality_limit` option caps distinct series per stream
%%   * once the limit is reached, further distinct attribute sets are folded
%%     into a single overflow series tagged {<<"otel.metric.overflow">>, true}
%%   * the overflow series aggregates the spillover (sum / histogram count)
%%   * an already-seen series keeps updating even after overflow has started
%%   * `cardinality_limit => 0` disables the limit (Go semantics)
%%   * the default limit (2000) does not overflow for a handful of series
%%
%% Reader config knob under test (to be implemented):
%%   #{module => otel_metric_reader,
%%     config => #{exporter => {otel_metric_exporter_pid, self()},
%%                 cardinality_limit => N}}    %% N =< 0 disables; default 2000
-module(otel_metric_cardinality_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_metrics.hrl").

-define(OVERFLOW, #{<<"otel.metric.overflow">> => true}).

all() ->
    [counter_overflow,
     existing_series_keeps_updating_after_overflow,
     zero_limit_is_unlimited,
     default_limit_does_not_overflow,
     histogram_overflow].

init_per_suite(Config) ->
    application:load(opentelemetry_experimental),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    application:load(opentelemetry_experimental),
    CardinalityOpts =
        case TestCase of
            zero_limit_is_unlimited        -> #{cardinality_limit => 0};
            default_limit_does_not_overflow -> #{};            %% rely on default 2000
            _                              -> #{cardinality_limit => 3}
        end,
    ReaderConfig = maps:merge(#{exporter => {otel_metric_exporter_pid, self()},
                                default_temporality_mapping => default_temporality_mapping()},
                              CardinalityOpts),
    ok = application:set_env(opentelemetry_experimental, readers,
                             [#{module => otel_metric_reader,
                                config => ReaderConfig}]),
    {ok, _} = application:ensure_all_started(opentelemetry_experimental),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = application:stop(opentelemetry_experimental),
    _ = application:unload(opentelemetry_experimental),
    ok.

%% limit => 3 means 2 real series are allowed; the 3rd+ distinct sets overflow
%% and their values are summed into the single overflow series.
counter_overflow(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    Name = overflow_counter,
    Desc = <<"counter description">>,
    Unit = kb,
    _ = otel_counter:create(Meter, Name, #{description => Desc, unit => Unit}),
    Ctx = otel_ctx:new(),

    ok = otel_counter:add(Ctx, Meter, Name, 1, #{<<"k">> => <<"s1">>}),
    ok = otel_counter:add(Ctx, Meter, Name, 2, #{<<"k">> => <<"s2">>}),
    %% s3 and s4 exceed the 2-real-series budget -> overflow (4 + 8 = 12)
    ok = otel_counter:add(Ctx, Meter, Name, 4, #{<<"k">> => <<"s3">>}),
    ok = otel_counter:add(Ctx, Meter, Name, 8, #{<<"k">> => <<"s4">>}),

    otel_meter_server:force_flush(),

    Datapoints = recv_sum(Name),
    ?assertEqual(lists:sort([{1, #{<<"k">> => <<"s1">>}},
                             {2, #{<<"k">> => <<"s2">>}},
                             {12, ?OVERFLOW}]),
                 lists:sort(Datapoints)).

%% A series that was admitted before the limit was hit must keep updating even
%% after overflow has begun; only *new* distinct sets are redirected.
existing_series_keeps_updating_after_overflow(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    Name = existing_after_overflow_counter,
    Desc = <<"counter description">>,
    Unit = kb,
    _ = otel_counter:create(Meter, Name, #{description => Desc, unit => Unit}),
    Ctx = otel_ctx:new(),

    ok = otel_counter:add(Ctx, Meter, Name, 1, #{<<"k">> => <<"s1">>}),
    ok = otel_counter:add(Ctx, Meter, Name, 2, #{<<"k">> => <<"s2">>}),
    ok = otel_counter:add(Ctx, Meter, Name, 4, #{<<"k">> => <<"s3">>}),  %% overflow
    ok = otel_counter:add(Ctx, Meter, Name, 10, #{<<"k">> => <<"s1">>}), %% existing -> s1 = 11
    ok = otel_counter:add(Ctx, Meter, Name, 8, #{<<"k">> => <<"s4">>}),  %% overflow -> 4 + 8 = 12

    otel_meter_server:force_flush(),

    Datapoints = recv_sum(Name),
    ?assertEqual(lists:sort([{11, #{<<"k">> => <<"s1">>}},
                             {2, #{<<"k">> => <<"s2">>}},
                             {12, ?OVERFLOW}]),
                 lists:sort(Datapoints)).

%% cardinality_limit => 0 disables the cap: every distinct set gets its own
%% series and no overflow series is produced.
zero_limit_is_unlimited(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    Name = unlimited_counter,
    Desc = <<"counter description">>,
    Unit = kb,
    _ = otel_counter:create(Meter, Name, #{description => Desc, unit => Unit}),
    Ctx = otel_ctx:new(),

    Sets = [{N, #{<<"k">> => integer_to_binary(N)}} || N <- lists:seq(1, 6)],
    [ok = otel_counter:add(Ctx, Meter, Name, N, Attrs) || {N, Attrs} <- Sets],

    otel_meter_server:force_flush(),

    Datapoints = recv_sum(Name),
    ?assertEqual(lists:sort(Sets), lists:sort(Datapoints)),
    ?assertNot(has_overflow(Datapoints)).

%% With no cardinality_limit configured the default (2000) applies, so a
%% handful of series must all be recorded individually with no overflow.
default_limit_does_not_overflow(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    Name = default_limit_counter,
    Desc = <<"counter description">>,
    Unit = kb,
    _ = otel_counter:create(Meter, Name, #{description => Desc, unit => Unit}),
    Ctx = otel_ctx:new(),

    Sets = [{N, #{<<"k">> => integer_to_binary(N)}} || N <- lists:seq(1, 5)],
    [ok = otel_counter:add(Ctx, Meter, Name, N, Attrs) || {N, Attrs} <- Sets],

    otel_meter_server:force_flush(),

    Datapoints = recv_sum(Name),
    ?assertEqual(lists:sort(Sets), lists:sort(Datapoints)),
    ?assertNot(has_overflow(Datapoints)).

%% Histograms overflow the same way: spillover measurements land in a single
%% overflow histogram series whose count equals the number of overflowed records.
histogram_overflow(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    Name = overflow_histogram,
    Desc = <<"histogram description">>,
    Unit = ms,
    _ = otel_meter:create_histogram(Meter, Name, #{description => Desc, unit => Unit}),
    Ctx = otel_ctx:new(),

    ok = otel_histogram:record(Ctx, Meter, Name, 1, #{<<"k">> => <<"s1">>}),
    ok = otel_histogram:record(Ctx, Meter, Name, 2, #{<<"k">> => <<"s2">>}),
    %% two overflowing records -> overflow series count = 2
    ok = otel_histogram:record(Ctx, Meter, Name, 5, #{<<"k">> => <<"s3">>}),
    ok = otel_histogram:record(Ctx, Meter, Name, 7, #{<<"k">> => <<"s4">>}),

    otel_meter_server:force_flush(),

    Counts = recv_histogram_counts(Name),
    %% overflow series present with a combined count of 2
    ?assertEqual(2, proplists:get_value(?OVERFLOW, Counts)),
    %% and the two real series are still there individually
    ?assertEqual(1, proplists:get_value(#{<<"k">> => <<"s1">>}, Counts)),
    ?assertEqual(1, proplists:get_value(#{<<"k">> => <<"s2">>}, Counts)).

%% --- helpers ----------------------------------------------------------------

%% Receive a sum metric for Name and return [{Value, Attributes}].
recv_sum(Name) ->
    receive
        {otel_metric, #metric{name = MetricName, data = #sum{datapoints = DPs}}}
          when MetricName =:= Name ->
            [{V, A} || #datapoint{value = V, attributes = A} <- DPs]
    after
        5000 ->
            ct:fail({metric_receive_timeout, Name})
    end.

%% Receive a histogram metric for Name and return [{Attributes, Count}].
recv_histogram_counts(Name) ->
    receive
        {otel_metric, #metric{name = MetricName, data = #histogram{datapoints = DPs}}}
          when MetricName =:= Name ->
            [{A, C} || #histogram_datapoint{attributes = A, count = C} <- DPs]
    after
        5000 ->
            ct:fail({metric_receive_timeout, Name})
    end.

has_overflow(Datapoints) ->
    lists:any(fun({_V, Attrs}) -> Attrs =:= ?OVERFLOW end, Datapoints).

default_temporality_mapping() ->
    #{?KIND_COUNTER => ?TEMPORALITY_DELTA,
      ?KIND_OBSERVABLE_COUNTER => ?TEMPORALITY_CUMULATIVE,
      ?KIND_UPDOWN_COUNTER => ?TEMPORALITY_DELTA,
      ?KIND_OBSERVABLE_UPDOWNCOUNTER => ?TEMPORALITY_CUMULATIVE,
      ?KIND_HISTOGRAM => ?TEMPORALITY_DELTA,
      ?KIND_OBSERVABLE_GAUGE => ?TEMPORALITY_CUMULATIVE}.
