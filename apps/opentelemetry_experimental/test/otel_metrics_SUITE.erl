-module(otel_metrics_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("otel_test_utils.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").
-include("otel_view.hrl").
-include("otel_metrics.hrl").
-include("otel_metric_exemplar.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-define(assertSumReceive(Name, Description, Unit, Datapoints),
        (fun() ->
                 receive
                     {otel_metric, #metric{name=MetricName,
                                           description=MetricDescription,
                                           unit=MetricUnit,
                                           data=#sum{datapoints=MetricDatapoints}}}
                       when MetricName =:= Name, MetricUnit =:= Unit ->
                         ?assertEqual(Description, MetricDescription),
                         ?assertEqual(Unit, MetricUnit),
                         ?assertEqual(Description, MetricDescription),

                         SortedDatapoints =
                             lists:sort([{MetricValue, MetricAttributes} ||
                                            #datapoint{value=MetricValue,
                                                       attributes=MetricAttributes,
                                                       start_time=StartTime,
                                                       time=Time} <- MetricDatapoints, StartTime =< Time]),
                         ?assert(is_subset(Datapoints, SortedDatapoints), SortedDatapoints)
                 after
                     5000 ->
                         ct:fail({metric_receive_timeout, ?LINE})
                 end
         end)()).

-define(assertSumExemplarReceive(Name, Description, Unit, Datapoints),
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
                             lists:sort([{MetricValue, MetricAttributes, [{ExemplarValue, FilteredAttributes}]} ||
                                            #datapoint{value=MetricValue,
                                                       attributes=MetricAttributes,
                                                       start_time=StartTime,
                                                       time=Time,
                                                       exemplars=[#exemplar{value=ExemplarValue,
                                                                           filtered_attributes=FilteredAttributes}]} <- MetricDatapoints, StartTime =< Time]),

                         ?assert(is_subset(Datapoints, SortedDatapoints), SortedDatapoints)
                 after
                     5000 ->
                         ct:fail({metric_receive_timeout, ?LINE})
                 end
         end)()).

-define(assertLastValueReceive(Name, Description, Unit, Datapoints),
        (fun() ->
                 receive
                     {otel_metric, #metric{name=MetricName,
                                           description=MetricDescription,
                                           unit=MetricUnit,
                                           data=#gauge{datapoints=MetricDatapoints}}}
                       when MetricName =:= Name ->
                         ?assertEqual(Description, MetricDescription),
                         ?assertEqual(Unit, MetricUnit),
                         ?assertEqual(Description, MetricDescription),

                         SortedDatapoints =
                             lists:sort([{MetricValue, MetricAttributes} ||
                                            #datapoint{value=MetricValue,
                                                       attributes=MetricAttributes,
                                                       start_time=StartTime,
                                                       time=Time} <- MetricDatapoints, StartTime =< Time]),
                         ?assert(is_subset(Datapoints, SortedDatapoints), SortedDatapoints)
                 after
                     5000 ->
                         ct:fail({metric_receive_timeout, ?LINE})
                 end
         end)()).

-define(assertNotReceive(Name, Description, Unit),
        (fun() ->
                 receive
                     {otel_metric, M=#metric{name=MetricName,
                                             description=MetricDescription,
                                             unit=MetricUnit}}
                       when MetricName =:= Name,
                            MetricDescription =:= Description,
                            MetricUnit =:= Unit ->
                         ct:fail({metric_received, M})
                 after
                     50 ->
                         ok
                 end
         end)()).

all() ->
    [default_view, instrument_aliases, concurrent_instrument_alias_registration,
     same_name_different_unit, case_insensitive_name_conflict,
     view_resolves_stream_conflict, late_named_view_replaces_default,
     late_incompatible_aggregation_gets_new_stream_id,
     late_incompatible_aggregation_options_get_new_stream_id,
     same_name_views_remain_independent, same_identity_views_log_conflict,
     provider_test, view_creation_test, wildcard_view, counter_add, multiple_readers,
     explicit_histograms, delta_explicit_histograms, delta_counter, cumulative_counter,
     kill_reader, kill_server, observable_counter, duplicate_observable_counter_callbacks,
     observable_updown_counter, observable_gauge,
     multi_instrument_callback, multi_instrument_callback_macros,
     using_macros, float_counter, float_updown_counter, float_histogram,
     sync_filtered_attributes, async_filtered_attributes, delta_observable_counter,
     bad_observable_return, default_resource, histogram_aggregation_options, advisory_params,
     sync_delta_histogram, async_cumulative_page_faults, async_delta_page_faults,
     async_attribute_removal, sync_cumulative_histogram, simple_fixed_exemplars,
     float_simple_fixed_exemplars, explicit_histogram_exemplars, trace_based_exemplars,
     observable_exemplars, simple_producer
    ].

init_per_suite(Config) ->
    application:load(opentelemetry_experimental),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(provider_test, Config) ->
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers,
                             [#{module => otel_metric_reader,
                                config => #{exporter => {otel_metric_exporter_pid, self()},
                                            default_temporality_mapping => default_temporality_mapping()}}]),
    ok = application:set_env(opentelemetry_experimental, views,
                             [#{selector => #{instrument_name => a_counter},
                                aggregation_module => otel_aggregation_sum},
                              #{name => view_a,
                                selector => #{instrument_name => a_counter},
                                aggregation_module => otel_aggregation_sum},
                              #{name => view_b,
                                selector => #{instrument_name => a_counter},
                                aggregation_module => otel_aggregation_sum},
                              #{name => view_c,
                                selector => #{instrument_name => a_counter}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(multiple_readers, Config) ->
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers,
                             [#{module => otel_metric_reader,
                                config => #{exporter => {otel_metric_exporter_pid, self()},
                                            default_temporality_mapping => default_temporality_mapping()}},
                              #{module => otel_metric_reader,
                                config => #{exporter => {otel_metric_exporter_pid, self()},
                                            default_temporality_mapping => default_temporality_mapping(),
                                            default_aggregation_mapping =>
                                                #{?KIND_COUNTER => otel_aggregation_drop}}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(delta_counter, Config) ->
    application:load(opentelemetry_experimental),

    %% delta is the default for a counter with sum aggregation
    %% so no need to set any temporality mapping in the reader
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()}}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(cumulative_counter, Config) ->
    CumulativeCounterTemporality = maps:put(?KIND_COUNTER, ?TEMPORALITY_CUMULATIVE, default_temporality_mapping()),
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping =>
                                                                                     CumulativeCounterTemporality}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(delta_explicit_histograms, Config) ->
    DeltaHistogramTemporality = maps:put(?KIND_HISTOGRAM, ?TEMPORALITY_DELTA, default_temporality_mapping()),
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping =>
                                                                                     DeltaHistogramTemporality}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(sync_delta_histogram, Config) ->
    DeltaHistogramTemporality = maps:put(?KIND_HISTOGRAM, ?TEMPORALITY_DELTA, default_temporality_mapping()),
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping =>
                                                                                     DeltaHistogramTemporality}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(sync_cumulative_histogram, Config) ->
    DeltaHistogramTemporality = maps:put(?KIND_HISTOGRAM, ?TEMPORALITY_CUMULATIVE, default_temporality_mapping()),
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping =>
                                                                                     DeltaHistogramTemporality}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(async_cumulative_page_faults, Config) ->
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()}}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(async_delta_page_faults, Config) ->
    DeltaCounterTemporality = maps:put(?KIND_OBSERVABLE_COUNTER, ?TEMPORALITY_DELTA, default_temporality_mapping()),
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping =>
                                                                                     DeltaCounterTemporality}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(delta_observable_counter, Config) ->
    DeltaObservableCounterTemporality = maps:put(?KIND_OBSERVABLE_COUNTER, ?TEMPORALITY_DELTA, default_temporality_mapping()),
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping =>
                                                                                     DeltaObservableCounterTemporality}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(simple_fixed_exemplars, Config) ->
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping => default_temporality_mapping()}}]),

    ok = application:set_env(opentelemetry_experimental, exemplars_enabled, true),
    ok = application:set_env(opentelemetry_experimental, exemplar_filter, always_on),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(float_simple_fixed_exemplars, Config) ->
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping => default_temporality_mapping()}}]),

    ok = application:set_env(opentelemetry_experimental, exemplars_enabled, true),
    ok = application:set_env(opentelemetry_experimental, exemplar_filter, always_on),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(explicit_histogram_exemplars, Config) ->
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping => default_temporality_mapping()}}]),

    ok = application:set_env(opentelemetry_experimental, exemplars_enabled, true),
    ok = application:set_env(opentelemetry_experimental, exemplar_filter, always_on),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(trace_based_exemplars, Config) ->
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping => default_temporality_mapping()}}]),

    ok = application:set_env(opentelemetry_experimental, exemplars_enabled, true),
    ok = application:set_env(opentelemetry_experimental, exemplar_filter, trace_based),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(observable_exemplars, Config) ->
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping => default_temporality_mapping()}}]),

    ok = application:set_env(opentelemetry_experimental, exemplars_enabled, true),
    ok = application:set_env(opentelemetry_experimental, exemplar_filter, always_on),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(simple_producer, Config) ->
    application:load(opentelemetry_experimental),

    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping => default_temporality_mapping()}}]),
    ok = application:set_env(opentelemetry_experimental, metric_producers, [simple_metric_producer]),
    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Config;
init_per_testcase(_, Config) ->
    application:load(opentelemetry_experimental),
    ok = application:set_env(opentelemetry_experimental, readers, [#{module => otel_metric_reader,
                                                                     config => #{exporter => {otel_metric_exporter_pid, self()},
                                                                                 default_temporality_mapping => default_temporality_mapping()}}]),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),


    Config.

end_per_testcase(_, _Config) ->
    ok = otel_instrument:unregister_alias(test_counter_alias),
    ok = otel_instrument:unregister_alias(concurrent_counter_alias),
    ok = application:stop(opentelemetry_experimental),
    application:unload(opentelemetry_experimental),
    ok.

default_resource(_Config) ->
    Resource = eqwalizer:dynamic_cast(otel_meter_provider:resource()),

    ?assertMatch(#{'process.executable.name' := <<"erl">>},
                 otel_attributes:map(eqwalizer:dynamic_cast(otel_resource:attributes(Resource)))),

    ok.

instrument_aliases(_Config) ->
    Counter = ?create_counter(test_alias_counter,
                              #{description => <<"counter registered by alias">>,
                                unit => request}),
    {ok, Counter} = ?register_instrument(test_counter_alias, Counter),
    {ok, Counter} = ?register_instrument(test_counter_alias, Counter),
    OtherCounter = ?create_counter(other_alias_counter, #{}),
    ?assertMatch({error, {alias_conflict, test_counter_alias, _, _}},
                 ?register_instrument(test_counter_alias, OtherCounter)),

    ?assertEqual(ok, ?counter_add(test_counter_alias, 1, #{})),
    Parent = self(),
    spawn(fun() ->
                  Result = ?counter_add(test_counter_alias, 2, #{}),
                  Parent ! {alias_record_result, Result}
          end),
    receive
        {alias_record_result, Result} -> ?assertEqual(ok, Result)
    after 5000 ->
        ct:fail(alias_record_timeout)
    end,
    ?assertEqual(ok, ?counter_add(Counter, 3, #{})),
    ?assertEqual(false, ?histogram_record(test_counter_alias, 4, #{})),
    ?assertEqual(false, ?counter_add(unregistered_counter_alias, 1, #{})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(test_alias_counter,
                      <<"counter registered by alias">>,
                      request,
                      [{6, #{}}]),
    ok.

concurrent_instrument_alias_registration(_Config) ->
    Alias = concurrent_counter_alias,
    Meter = opentelemetry_experimental:get_meter(),
    Instruments = [otel_instrument:new(otel_meter_noop,
                                       Meter,
                                       ?KIND_COUNTER,
                                       concurrent_alias_counter,
                                       #{description => integer_to_binary(N)})
                   || N <- lists:seq(1, 200)],
    Parent = self(),
    Workers = [spawn(fun() ->
                             Parent ! {alias_worker_ready, self()},
                             receive
                                 register_alias ->
                                     Result = otel_instrument:register_alias(Alias, Instrument),
                                     Parent ! {alias_registration_result, self(), Result}
                             end
                     end)
               || Instrument <- Instruments],
    [receive
         {alias_worker_ready, Worker} -> ok
     after 5000 ->
         ct:fail({alias_worker_ready_timeout, Worker})
     end || Worker <- Workers],
    [Worker ! register_alias || Worker <- Workers],
    Results = [receive
                   {alias_registration_result, Worker, Result} -> Result
               after 5000 ->
                   ct:fail({alias_bind_timeout, Worker})
               end || Worker <- Workers],
    ?assertEqual(1, length([ok || {ok, _} <- Results])),
    ?assertEqual(length(Workers) - 1,
                 length([conflict || {error, {alias_conflict, ConflictAlias, _, _}} <- Results,
                                     ConflictAlias =:= Alias])),
    ok.

same_name_different_unit(_Config) ->
    with_log_capture(
      fun() ->
              Meter = opentelemetry_experimental:get_meter(),
              Name = test_duration,
              Description = <<"duration in the selected unit">>,
              Milliseconds = otel_meter:create_counter(Meter, Name,
                                                       #{description => Description,
                                                         unit => ms}),
              Seconds = otel_meter:create_counter(Meter, Name,
                                                  #{description => Description,
                                                    unit => s}),
              MillisecondsAgain = otel_meter:create_counter(Meter, Name,
                                                            #{description => Description,
                                                              unit => ms}),

              ?assertNotEqual(otel_instrument:id(Milliseconds), otel_instrument:id(Seconds)),
              ?assertEqual(otel_instrument:id(Milliseconds), otel_instrument:id(MillisecondsAgain)),
              assert_stream_conflict_log(),
              _ = otel_meter:create_counter(Meter, unrelated_counter, #{}),
              refute_stream_conflict_log(),
              ?assertEqual(ok, ?counter_add(Milliseconds, 1000, #{})),
              ?assertEqual(ok, ?counter_add(Seconds, 1, #{})),

              otel_meter_server:force_flush(),
              ?assertSumReceive(Name, Description, ms, [{1000, #{}}]),
              ?assertSumReceive(Name, Description, s, [{1, #{}}]),
              ok
      end).

case_insensitive_name_conflict(_Config) ->
    with_log_capture(
      fun() ->
              Meter = opentelemetry_experimental:get_meter(),
              ViewDescription = <<"case-insensitive view match">>,
              First = otel_meter:create_counter(Meter, requestCount, #{}),
              Duplicate = otel_meter:create_counter(Meter, 'RequestCount', #{}),

              ?assertEqual(otel_instrument:id(First), otel_instrument:id(Duplicate)),
              ?assertMatch(#instrument{name=requestCount}, Duplicate),
              assert_name_conflict_log('RequestCount', requestCount),

              ?assert(otel_meter_server:add_view(
                        #{instrument_name => 'RequestCount'},
                        #{description => ViewDescription})),
              ?assertMatch(#stream{name=requestCount,
                                   description=ViewDescription},
                           only_instrument_stream(First)),
              ?assertEqual(ok, ?counter_add(Duplicate, 1, #{})),

              otel_meter_server:force_flush(),
              ?assertSumReceive(requestCount, ViewDescription, undefined, [{1, #{}}])
      end).

view_resolves_stream_conflict(_Config) ->
    with_log_capture(
      fun() ->
              Meter = opentelemetry_experimental:get_meter(),
              Name = view_resolved_duration,
              Description = <<"duration in the selected unit">>,
              ?assert(otel_meter_server:add_view(view_resolved_duration_ms,
                                                 #{instrument_name => Name,
                                                   instrument_unit => ms},
                                                 #{})),
              _ = otel_meter:create_counter(Meter, Name,
                                            #{description => Description,
                                              unit => ms}),
              _ = otel_meter:create_counter(Meter, Name,
                                            #{description => Description,
                                              unit => s}),
              refute_stream_conflict_log()
      end).

late_named_view_replaces_default(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    InstrumentName = late_view_counter,
    ViewName = renamed_late_view_counter,
    Description = <<"late view counter">>,
    Unit = request,
    Counter=#instrument{id=InstrumentId,
                        meter={_, #meter{streams_tab=StreamsTab}}} =
        otel_meter:create_counter(Meter, InstrumentName,
                                  #{description => Description,
                                    unit => Unit}),

    ?assertEqual(ok, ?counter_add(Counter, 1, #{})),
    DefaultStream=#stream{id=DefaultStreamId} = only_instrument_stream(Counter),
    ?assertEqual(set, ets:info(StreamsTab, type)),
    ?assertEqual([{InstrumentId, [DefaultStream]}],
                 ets:lookup(StreamsTab, InstrumentId)),

    ?assert(otel_meter_server:add_view(ViewName,
                                       #{instrument_name => InstrumentName},
                                       #{})),
    RenamedStream=#stream{id=RenamedStreamId} = only_instrument_stream(Counter),
    ?assertEqual([{InstrumentId, [RenamedStream]}],
                 ets:lookup(StreamsTab, InstrumentId)),
    ?assertEqual(DefaultStreamId, RenamedStreamId),
    ?assertEqual(ok, ?counter_add(Counter, 1, #{})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(ViewName, Description, Unit, [{2, #{}}]),
    ?assertNotReceive(InstrumentName, Description, Unit),
    ok.

late_incompatible_aggregation_gets_new_stream_id(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    Name = late_last_value_counter,
    Description = <<"late last-value counter">>,
    Unit = request,
    Counter = otel_meter:create_counter(Meter, Name,
                                        #{description => Description,
                                          unit => Unit}),

    ?assertEqual(ok, ?counter_add(Counter, 1, #{})),
    #stream{id=SumStreamId,
            aggregation_module=otel_aggregation_sum} = only_instrument_stream(Counter),

    ?assert(otel_meter_server:add_view(
              #{instrument_name => Name},
              #{aggregation_module => otel_aggregation_last_value})),
    #stream{id=LastValueStreamId,
            aggregation_module=otel_aggregation_last_value} = only_instrument_stream(Counter),
    ?assertNotEqual(SumStreamId, LastValueStreamId),

    ?assertEqual(ok, ?counter_add(Counter, 2, #{})),
    otel_meter_server:force_flush(),

    ?assertLastValueReceive(Name, Description, Unit, [{2, #{}}]),
    ok.

late_incompatible_aggregation_options_get_new_stream_id(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    Name = late_histogram_boundaries,
    Description = <<"late histogram boundaries">>,
    Unit = ms,
    Histogram = otel_meter:create_histogram(Meter, Name,
                                            #{description => Description,
                                              unit => Unit}),

    ?assertEqual(ok, ?histogram_record(Histogram, 5, #{})),
    #stream{id=DefaultStreamId,
            aggregation_module=otel_aggregation_histogram_explicit,
            aggregation_options=DefaultOptions} = only_instrument_stream(Histogram),

    ?assert(otel_meter_server:add_view(
              #{instrument_name => Name},
              #{aggregation_module => otel_aggregation_histogram_explicit,
                aggregation_options => #{explicit_bucket_boundaries => [10]}})),
    #stream{id=CustomStreamId,
            aggregation_module=otel_aggregation_histogram_explicit,
            aggregation_options=CustomOptions} = only_instrument_stream(Histogram),
    ?assertNotEqual(DefaultOptions, CustomOptions),
    ?assertNotEqual(DefaultStreamId, CustomStreamId),

    ?assertEqual(ok, ?histogram_record(Histogram, 20, #{})),
    otel_meter_server:force_flush(),

    receive
        {otel_metric,
         #metric{name=Name,
                 description=Description,
                 unit=Unit,
                 data=#histogram{datapoints=[#histogram_datapoint{
                                               bucket_counts=[0, 1],
                                               min=20,
                                               max=20,
                                               sum=20}]}}} ->
            ok
    after
        5000 ->
            ct:fail({metric_receive_timeout, ?LINE})
    end,
    ok.

same_name_views_remain_independent(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    Name = independent_view_counter,
    Unit = request,
    AllDescription = <<"all attributes">>,
    FilteredDescription = <<"filtered attributes">>,

    ?assert(otel_meter_server:add_view(
              #{instrument_name => Name},
              #{description => AllDescription})),
    ?assert(otel_meter_server:add_view(
              #{instrument_name => Name},
              #{description => FilteredDescription,
                attribute_keys => [kept]})),

    Counter = otel_meter:create_counter(Meter, Name, #{unit => Unit}),
    ?assertEqual(ok, ?counter_add(Counter, 1, #{kept => true, dropped => true})),

    otel_meter_server:force_flush(),

    Metrics = [receive
                   {otel_metric, #metric{name=Name,
                                         description=MetricDescription,
                                         unit=Unit,
                                         data=#sum{datapoints=Datapoints}}} ->
                       {MetricDescription,
                        [{Value, Attributes} || #datapoint{value=Value,
                                                          attributes=Attributes} <- Datapoints]}
               after
                   5000 ->
                       ct:fail({metric_receive_timeout, ?LINE})
               end || _ <- lists:seq(1, 2)],
    ?assertEqual(
       lists:sort([{AllDescription, [{1, #{kept => true, dropped => true}}]},
                   {FilteredDescription, [{1, #{kept => true}}]}]),
       lists:sort(Metrics)),
    ok.

same_identity_views_log_conflict(_Config) ->
    with_log_capture(
      fun() ->
              Meter = opentelemetry_experimental:get_meter(),
              Name = same_identity_view_counter,
              ?assert(otel_meter_server:add_view(
                        #{instrument_name => Name},
                        #{})),
              ?assert(otel_meter_server:add_view(
                        #{instrument_name => Name},
                        #{attribute_keys => [kept]})),

              _ = otel_meter:create_counter(Meter, Name, #{}),
              assert_stream_conflict_log()
      end).

with_log_capture(Fun) ->
    HandlerId = metric_stream_conflict_test,
    ok = logger:add_handler(HandlerId, ?MODULE,
                            #{level => warning,
                              config => #{test_pid => self()}}),
    try
        Fun()
    after
        ok = logger:remove_handler(HandlerId)
    end.

assert_stream_conflict_log() ->
    receive
        {captured_log,
         #{level := warning,
           msg := {"Conflicting metric streams remain after applying Views; both streams remain active. "
                   "Configure a View to rename one stream or align its identifying fields. "
                   "name=~p scope=~p stream_definitions=~p", _}}} ->
            ok
    after
        5000 ->
            ct:fail(stream_conflict_log_timeout)
    end.

assert_name_conflict_log(Name, ExistingName) ->
    receive
        {captured_log,
         #{level := warning,
           msg := {"Instrument name ~p conflicts case-insensitively with previously registered name ~p; "
                   "returning the first-created Instrument.",
                   [Name, ExistingName]}}} ->
            ok
    after
        5000 ->
            ct:fail(name_conflict_log_timeout)
    end.

refute_stream_conflict_log() ->
    receive
        {captured_log,
         #{level := warning,
           msg := {"Conflicting metric streams remain after applying Views; both streams remain active. "
                   "Configure a View to rename one stream or align its identifying fields. "
                   "name=~p scope=~p stream_definitions=~p", _}} = LogEvent} ->
            ct:fail({unexpected_stream_conflict_log, LogEvent})
    after
        100 ->
            ok
    end.

adding_handler(Config) ->
    {ok, Config}.

removing_handler(_Config) ->
    ok.

log(LogEvent, #{config := #{test_pid := TestPid}}) ->
    TestPid ! {captured_log, LogEvent},
    ok.

default_temporality_mapping() ->
    #{
        ?KIND_COUNTER => ?TEMPORALITY_DELTA,
        ?KIND_OBSERVABLE_COUNTER => ?TEMPORALITY_CUMULATIVE,
        ?KIND_UPDOWN_COUNTER => ?TEMPORALITY_DELTA,
        ?KIND_OBSERVABLE_UPDOWNCOUNTER => ?TEMPORALITY_CUMULATIVE,
        ?KIND_HISTOGRAM => ?TEMPORALITY_DELTA,
        ?KIND_OBSERVABLE_GAUGE => ?TEMPORALITY_CUMULATIVE
    }.

using_macros(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = m_counter,
    CounterDesc = <<"macro made counter description">>,
    CounterUnit = kb,

    Counter = ?create_counter(CounterName, #{description => CounterDesc,
                                             unit => CounterUnit}),

    Ctx = otel_ctx:new(),

    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, ?counter_add(Counter, 5, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(m_counter, <<"macro made counter description">>, kb,
                      [{12, #{<<"c">> => <<"b">>}}]),

    ok.

float_counter(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = f_counter,
    CounterDesc = <<"macro made counter description">>,
    CounterUnit = kb,

    Counter = ?create_counter(CounterName, #{description => CounterDesc,
                                             unit => CounterUnit}),

    Ctx = otel_ctx:new(),

    ?assertEqual(ok, ?counter_add(Counter, 10.3, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, ?counter_add(Counter, 5.5, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, ?counter_add(Counter, 5, #{<<"c">> => <<"b">>})),

    %% without attributes
    ?assertEqual(ok, ?counter_add(Counter, 1.2, #{})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2.1, #{})),

    %% negative values are discarded
    ?assertEqual(ok, ?counter_add(Counter, -2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, -2, #{})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(f_counter, <<"macro made counter description">>, kb,
                      [{3.3, #{}}, {20.8, #{<<"c">> => <<"b">>}}]),

    ok.

float_updown_counter(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = f_counter,
    CounterDesc = <<"macro made updown counter description">>,
    CounterUnit = kb,

    Counter = ?create_updown_counter(CounterName, #{description => CounterDesc,
                                                    unit => CounterUnit}),

    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_updown_counter:add(Ctx, Counter, 10.5, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, ?updown_counter_add(Counter, -5.5, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, ?updown_counter_add(Counter, 5, #{<<"c">> => <<"b">>})),

    %% without attributes
    ?assertEqual(ok, ?updown_counter_add(Counter, 1.2, #{})),
    ?assertEqual(ok, otel_updown_counter:add(Ctx, Counter, 2.1, #{})),


    otel_meter_server:force_flush(),

    ?assertSumReceive(f_counter, <<"macro made updown counter description">>, kb,
                      [{3.3, #{}}, {10.0, #{<<"c">> => <<"b">>}}]),

    ok.

float_histogram(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = f_histogram,
    CounterDesc = <<"macro made histogram description">>,
    CounterUnit = kb,

    Counter = ?create_histogram(CounterName, #{description => CounterDesc,
                                               unit => CounterUnit}),
    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_histogram:record(Ctx, Counter, 10.3, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Counter, 10.3, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, ?histogram_record(Counter, 5.5, #{<<"c">> => <<"b">>})),

    %% without attributes
    ?assertEqual(ok, ?histogram_record(Counter, 1.2, #{})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Counter, 2.1, #{})),

    %% negative values are discarded
    ?assertEqual(ok, ?histogram_record(Counter, -2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Counter, -2, #{})),

    %% float type accepts integers
    ?assertEqual(ok, ?histogram_record(Counter, 5, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=f_histogram,
                              data=#histogram{datapoints=Datapoints}}} ->
            AttributeBuckets =
                [{Attributes, Buckets, Min, Max, Sum}
                 || #histogram_datapoint{bucket_counts=Buckets,
                                         attributes=Attributes,
                                         min=Min,
                                         max=Max,
                                         sum=Sum}  <- Datapoints],
            ?assertEqual([], [
                              {#{<<"c">> => <<"b">>}, [0,1,1,2,0,0,0,0,0,0,0,0,0,0,0,0], 5, 10.3, 31.1},
                              {#{}, [0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 1.2, 2.1, 3.3}]
                         -- AttributeBuckets, AttributeBuckets)
    after
        5000 ->
            ct:fail(histogram_receive_timeout)
    end,


    ok.

default_view(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = z_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    Counter = otel_meter:create_counter(Meter, CounterName,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 4, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(z_counter, <<"counter description">>, kb, [{11, #{<<"c">> => <<"b">>}}]),

    ok.

provider_test(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    Counter = otel_meter:create_counter(Meter, CounterName,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 4, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5, #{<<"c">> => <<"b">>})),

    %% converts counter to a float value
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5.0, #{<<"c">> => <<"b">>})),

    %% ignored because only positive measurements are allowed for counters
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, -10, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(a_counter, <<"counter description">>, kb, [{16.0, #{<<"c">> => <<"b">>}}]),
    ?assertSumReceive(view_a, <<"counter description">>, kb, [{16.0, #{<<"c">> => <<"b">>}}]),
    ?assertSumReceive(view_b, <<"counter description">>, kb, [{16.0, #{<<"c">> => <<"b">>}}]),
    ?assertSumReceive(view_c, <<"counter description">>, kb, [{16.0, #{<<"c">> => <<"b">>}}]),

    %% sum agg is default delta temporality so counter will reset
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 7, #{<<"c">> => <<"b">>})),
    otel_meter_server:force_flush(),
    ?assertSumReceive(a_counter, <<"counter description">>, kb, [{7, #{<<"c">> => <<"b">>}}]),

    ok.

view_creation_test(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    Counter = otel_counter:create(Meter, CounterName,
                                  #{description => CounterDesc,
                                    unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    ?assert(otel_meter_server:add_view(view_a, #{instrument_name => a_counter}, #{aggregation_module => otel_aggregation_sum})),

    {ok, View} = otel_view:new(#{instrument_name => a_counter}, #{aggregation_module => otel_aggregation_sum}),
    ?assertEqual(undefined, View#view.name),
    ?assertEqual(a_counter, View#view.instrument_name),

    Matches = otel_view:match_instrument_to_views(Counter, [View], false, always_off),
    ?assertMatch([{#view{}, #stream{name=CounterName}}], Matches),

    {ok, CaseInsensitiveView} = otel_view:new(
                                  #{instrument_name => 'A_Counter'},
                                  #{aggregation_module => otel_aggregation_sum}),
    ?assertMatch([{#view{}, #stream{name=CounterName}}],
                 otel_view:match_instrument_to_views(
                   Counter, [CaseInsensitiveView], false, always_off)),

    {ok, ViewUnitMatch} = otel_view:new(#{instrument_name => CounterName, instrument_unit => CounterUnit}, #{aggregation_module => otel_aggregation_sum}),
    ?assertMatch([{#view{}, _}], otel_view:match_instrument_to_views(Counter, [ViewUnitMatch], false, always_off)),

    {ok, ViewUnitNotMatch} = otel_view:new(#{instrument_name => CounterName, instrument_unit => not_matching}, #{aggregation_module => otel_aggregation_sum}),
    ?assertMatch([{undefined, _}], otel_view:match_instrument_to_views(Counter, [ViewUnitNotMatch], false, always_off)),

    %% views require a unique name
    ?assert(otel_meter_server:add_view(view_b, #{instrument_name => a_counter}, #{aggregation_module => otel_aggregation_sum})),
    %% ?assertNot(otel_meter_server:add_view(view_b, #{instrument_name => a_counter}, #{aggregation_module => otel_aggregation_sum})),

    %% only one view that matches all instruments can be allowed
    ?assert(otel_meter_server:add_view(view_c, #{}, #{aggregation_module => otel_aggregation_sum})),
    %% ?assertNot(otel_meter_server:add_view(view_d, #{}, #{aggregation_module => otel_aggregation_sum})),

    ?assert(otel_meter_server:add_view(#{instrument_name => b_counter}, #{aggregation_module => otel_aggregation_sum})),
    ?assert(otel_meter_server:add_view(#{instrument_name => c_counter}, #{aggregation_module => otel_aggregation_sum})),

    ok.

wildcard_view(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),

    ViewCriteria = #{instrument_name => '*'},
    ViewConfig = #{aggregation_module => otel_aggregation_drop},

    ?assert(otel_meter_server:add_view(ViewCriteria, ViewConfig)),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    Counter = otel_counter:create(Meter, CounterName,
                                  #{description => CounterDesc,
                                    unit => CounterUnit}),

    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 1, #{})),

    otel_meter_server:force_flush(),

    ?assertNotReceive(CounterName, CounterDesc, CounterUnit),

    {ok, View} = otel_view:new(ViewCriteria, ViewConfig),
    ?assertMatch([{#view{}, _}], otel_view:match_instrument_to_views(Counter, [View], false, always_off)),

    %% not possible to create wildcard views with a name
    {error, named_wildcard_view} = otel_view:new(view_name, ViewCriteria, ViewConfig),

    ok.

counter_add(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    Counter = otel_counter:create(Meter, CounterName,
                                  #{description => CounterDesc,
                                    unit => CounterUnit}),

    Ctx = otel_ctx:new(),

    ?assertMatch(ok, otel_counter:add(Ctx, Counter, 3, #{})),
    ok.

multiple_readers(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),

    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    CounterAName = a_counter,
    CounterA = otel_meter:create_counter(Meter, CounterAName,
                                         #{description => CounterDesc,
                                           unit => CounterUnit}),
    CounterBName = b_counter,
    CounterB = otel_meter:create_counter(Meter, CounterBName,
                                         #{description => CounterDesc,
                                           unit => CounterUnit}),

    Ctx = otel_ctx:new(),

    otel_meter_server:add_view(#{instrument_name => a_counter}, #{aggregation_module => otel_aggregation_sum}),
    otel_meter_server:add_view(#{instrument_name => b_counter}, #{}),

    ?assertEqual(ok, otel_counter:add(Ctx, CounterA, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, CounterA, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, CounterA, 4, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, CounterA, 5, #{<<"c">> => <<"b">>})),

    ?assertEqual(ok, otel_counter:add(Ctx, CounterB, 2, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    %% 2nd reader has counter set to drop so only 1 of b_counter is expected bc it does
    %% not set an aggregation in the view definition
    ?assertSumReceive(a_counter, <<"counter description">>, kb, [{11, #{<<"c">> => <<"b">>}}]),
    ?assertSumReceive(a_counter, <<"counter description">>, kb, [{11, #{<<"c">> => <<"b">>}}]),
    ?assertSumReceive(b_counter, <<"counter description">>, kb, [{2, #{<<"c">> => <<"b">>}}]),
    ?assertNotReceive(b_counter, <<"counter description">>, kb),

    ok.

explicit_histograms(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    HistogramName = a_histogram,
    HistogramDesc = <<"histogram description">>,
    HistogramUnit = ms,

    Histogram = otel_meter:create_histogram(Meter, HistogramName,
                                            #{description => HistogramDesc,
                                              unit => HistogramUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = HistogramName,
                             description = HistogramDesc,
                             kind = histogram,
                             unit = HistogramUnit}, Histogram),

    Ctx = otel_ctx:new(),

    otel_meter_server:add_view(#{instrument_name => a_histogram}, #{}),

    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 20, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 30, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 44, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 100, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 20000, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=a_histogram,
                              data=#histogram{datapoints=Datapoints}}} ->
            AttributeBuckets =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum}  <- Datapoints]),
            ?assertEqual([], [{#{<<"c">> => <<"b">>}, [0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,1], 20, 20000, 20164},
                              {#{<<"a">> => <<"b">>, <<"d">> => <<"e">>}, [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0], 30, 30, 30}]
                         -- AttributeBuckets, AttributeBuckets)
    after
        5000 ->
            ct:fail(histogram_receive_timeout)
    end,

    ok.

delta_explicit_histograms(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    HistogramName = a_histogram,
    HistogramDesc = <<"histogram description">>,
    HistogramUnit = ms,

    Ctx = otel_ctx:new(),

    Histogram = otel_meter:create_histogram(Meter, HistogramName,
                                            #{description => HistogramDesc,
                                              unit => HistogramUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = HistogramName,
                             description = HistogramDesc,
                             kind = histogram,
                             unit = HistogramUnit}, Histogram),

    otel_meter_server:add_view(#{instrument_name => a_histogram}, #{}),


    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 20, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 30, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 44, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 100, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=a_histogram,
                              data=#histogram{datapoints=Datapoints}}} ->
            AttributeBuckets =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum}  <- Datapoints]),
            ?assertEqual([], [{#{<<"c">> => <<"b">>}, [0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0], 20, 100, 164},
                              {#{<<"a">> => <<"b">>, <<"d">> => <<"e">>}, [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0], 30, 30, 30}]
                         -- AttributeBuckets, AttributeBuckets)
    after
        5000 ->
            ct:fail(histogram_receive_timeout)
    end,

    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 88, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=a_histogram,
                              data=#histogram{datapoints=Datapoints1}}} ->
            AttributeBuckets1 =
                [{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                              attributes=Attributes,
                                                                              min=Min,
                                                                              max=Max,
                                                                              sum=Sum}  <- Datapoints1],
            ?assertEqual([], [{#{<<"c">> => <<"b">>}, [0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0], 88, 88, 88}]                                                      -- AttributeBuckets1, AttributeBuckets1)
    after
        5000 ->
            ct:fail(histogram_receive_timeout)
    end,

    ok.

delta_counter(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    Counter = otel_counter:create(Meter, CounterName,
                                  #{description => CounterDesc,
                                    unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    otel_meter_server:add_view(#{instrument_name => a_counter}, #{}),

    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 4, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(a_counter, <<"counter description">>, kb, [{6, #{<<"c">> => <<"b">>}}]),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5, #{<<"c">> => <<"b">>})),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 8, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(a_counter, <<"counter description">>, kb, [{13, #{<<"c">> => <<"b">>}}]),

    ok.

cumulative_counter(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    Counter = otel_counter:create(Meter, CounterName,
                                  #{description => CounterDesc,
                                    unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    Ctx = otel_ctx:new(),

    otel_meter_server:add_view(#{instrument_name => a_counter},
                               #{aggregation_module => otel_aggregation_sum}),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 4, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(a_counter, <<"counter description">>, kb, [{6, #{<<"c">> => <<"b">>}}]),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5, #{<<"c">> => <<"b">>})),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 7, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(a_counter, <<"counter description">>, kb, [{18, #{<<"c">> => <<"b">>}}]),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(a_counter, <<"counter description">>, kb, [{23, #{<<"c">> => <<"b">>}}]),

    ok.

kill_reader(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = z_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    Counter = otel_meter:create_counter(Meter, CounterName,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(z_counter, <<"counter description">>, kb, [{3, #{<<"c">> => <<"b">>}}]),

    %% counter is delta, so is reset to 0. take a measurement here before
    %% killer the reader to test that it isn't lost when the reader restarts
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 4, #{<<"c">> => <<"b">>})),

    [{_, ProviderSupPid, _, _}] = supervisor:which_children(otel_meter_provider_sup),
    {_, ReaderSup, _, _} = lists:keyfind(otel_metric_reader_sup, 1, supervisor:which_children(ProviderSupPid)),

    [ReaderPid] = [Pid || {_, Pid, _, _} <- supervisor:which_children(ReaderSup), is_pid(Pid)],
    erlang:exit(ReaderPid, kill),

    %% loop until a new reader has started
    ?UNTIL([Pid || {_, Pid, _, _} <- supervisor:which_children(ReaderSup),
                   Pid =/= ReaderPid] =/= []),

    %% This will create an ignored duplicate Counter since the Instruments table
    %% is owned by the `otel_metrics_server' process
    Counter = otel_meter:create_counter(Meter, CounterName,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 4, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    %% 13 because we don't lose previous metrics for a reader after it crashes
    ?assertSumReceive(z_counter, <<"counter description">>, kb, [{13, #{<<"c">> => <<"b">>}}]),

    ok.

kill_server(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    ACounterName = a_counter,
    CounterName = z_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    ACounter = otel_meter:create_counter(Meter, ACounterName,
                                         #{description => CounterDesc,
                                           unit => CounterUnit}),
    Counter = otel_meter:create_counter(Meter, CounterName,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_counter:add(Ctx, ACounter, 2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, #{<<"a">> => <<"b">>, <<"d">> => <<"e">>})),

    CurrentPid = erlang:whereis(?GLOBAL_METER_PROVIDER_REG_NAME),
    ProviderPid = eqwalizer:dynamic_cast(erlang:whereis(?GLOBAL_METER_PROVIDER_REG_NAME)),
    erlang:exit(ProviderPid, kill),

    %% wait until process has died and born again
    ?UNTIL(erlang:whereis(?GLOBAL_METER_PROVIDER_REG_NAME) =/= CurrentPid),
    ?UNTIL(erlang:whereis(?GLOBAL_METER_PROVIDER_REG_NAME) =/= undefined),

    %% TODO: Agh! need to supervise ETS tables so meter servers can crash and not then
    %% lose all existing Instrument/View matches
    _RestartedACounter = otel_meter:create_counter(Meter, ACounterName,
                                                   #{description => CounterDesc,
                                                     unit => CounterUnit}),
    RestartedCounter = otel_meter:create_counter(Meter, CounterName,
                                                 #{description => CounterDesc,
                                                   unit => CounterUnit}),

    ?assertEqual(ok, otel_counter:add(Ctx, RestartedCounter, 4, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_counter:add(Ctx, RestartedCounter, 5, #{<<"c">> => <<"b">>})),

    otel_meter_server:force_flush(),

    %% at this time a crashed meter server will mean losing the recorded metrics up to that point
    ?assertSumReceive(a_counter, <<"counter description">>, kb, []),
    ?assertSumReceive(z_counter, <<"counter description">>, kb, [{9, #{<<"c">> => <<"b">>}}]),

    ok.

observable_counter(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_observable_counter,
    CounterDesc = <<"observable counter description">>,
    CounterUnit = kb,

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName},
                                       #{aggregation_module => otel_aggregation_sum})),

    Counter = otel_meter:create_observable_counter(Meter, CounterName,
                                                   fun(_Args) ->
                                                           MeasurementAttributes1 = #{<<"a">> => <<"b">>},
                                                           MeasurementAttributes2 = #{<<"c">> => <<"d">>},
                                                           [{4, MeasurementAttributes1},
                                                            {5, MeasurementAttributes2}]
                                                   end,
                                                   [],
                                                   #{description => CounterDesc,
                                                     unit => CounterUnit}),

    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = observable_counter,
                             unit = CounterUnit,
                             callback=_}, Counter),

    otel_meter_server:force_flush(),

    ?assertSumReceive(CounterName, <<"observable counter description">>, kb, [{4, #{<<"a">> => <<"b">>}},
                                                                              {5, #{<<"c">> => <<"d">>}}]),

    otel_meter_server:force_flush(),

    ?assertSumReceive(CounterName, <<"observable counter description">>, kb, [{4, #{<<"a">> => <<"b">>}},
                                                                              {5, #{<<"c">> => <<"d">>}}]),

    ok.

duplicate_observable_counter_callbacks(_Config) ->
    Meter = opentelemetry_experimental:get_meter(),
    Name = duplicate_observable_counter,
    Description = <<"duplicate observable counter description">>,
    Unit = request,

    First = otel_meter:create_observable_counter(
              Meter,
              Name,
              fun(_) -> [{1, #{source => first}}] end,
              [],
              #{description => Description, unit => Unit}),
    Second = otel_meter:create_observable_counter(
               Meter,
               Name,
               fun(_) -> [{2, #{source => second}}] end,
               [],
               #{description => Description, unit => Unit}),

    ?assertEqual(otel_instrument:id(First), otel_instrument:id(Second)),

    otel_meter_server:force_flush(),

    ?assertSumReceive(Name, Description, Unit,
                      [{1, #{source => first}},
                       {2, #{source => second}}]),

    ok.

observable_updown_counter(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_observable_updown_counter,
    CounterDesc = <<"observable updown counter description">>,
    CounterUnit = kb,

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName}, #{aggregation_module => otel_aggregation_sum})),

    Counter = otel_meter:create_observable_updowncounter(Meter, CounterName,
                                                         fun(_) ->
                                                                 MeasurementAttributes = #{<<"a">> => <<"b">>},
                                                                 [{5, MeasurementAttributes}]
                                                         end,
                                                         [],
                                                         #{description => CounterDesc,
                                                           unit => CounterUnit}),

    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = observable_updowncounter,
                             unit = CounterUnit,
                             callback=_}, Counter),

    otel_meter_server:force_flush(),

    ?assertSumReceive(CounterName, <<"observable updown counter description">>, kb, [{5, #{<<"a">> => <<"b">>}}]),

    ok.

observable_gauge(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_observable_gauge,
    CounterDesc = <<"observable gauge description">>,
    CounterUnit = kb,

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName}, #{aggregation_module => otel_aggregation_last_value})),

    Counter = otel_meter:create_observable_gauge(Meter, CounterName,
                                                 fun(_) ->
                                                         [{5, #{<<"a">> => <<"b">>}}]
                                                 end,
                                                 [],
                                                 #{description => CounterDesc,
                                                   unit => CounterUnit}),

    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = observable_gauge,
                             unit = CounterUnit,
                             callback=_}, Counter),

    otel_meter_server:force_flush(),

    ?assertLastValueReceive(CounterName, CounterDesc, CounterUnit, [{5, #{<<"a">> => <<"b">>}}]),

    ok.

multi_instrument_callback(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_observable_counter,
    CounterDesc = <<"observable counter description">>,

    GaugeName = a_observable_gauge,
    GaugeDesc = <<"observable gauge description">>,

    Unit = kb,

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName}, #{aggregation_module => otel_aggregation_sum})),

    Counter = otel_observable_counter:create(Meter, CounterName, #{description => CounterDesc, unit => Unit}),
    Gauge = otel_observable_gauge:create(Meter, GaugeName, #{description => GaugeDesc, unit => Unit}),

    otel_meter:register_callback(Meter, [Counter, Gauge],
                                 fun(_) ->
                                         [{Counter, [{4, #{<<"a">> => <<"b">>}}]},
                                          {Gauge, [{5, #{<<"a">> => <<"b">>}}]}]
                                 end, []),

    otel_meter_server:force_flush(),

    ?assertSumReceive(CounterName, CounterDesc, Unit, [{4, #{<<"a">> => <<"b">>}}]),
    ?assertLastValueReceive(GaugeName, GaugeDesc, Unit, [{5, #{<<"a">> => <<"b">>}}]),

    ok.

multi_instrument_callback_macros(_Config) ->
    CounterName = a_observable_counter,
    CounterDesc = <<"observable counter description">>,

    GaugeName = a_observable_gauge,
    GaugeDesc = <<"observable gauge description">>,

    Unit = kb,

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName}, #{aggregation_module => otel_aggregation_sum})),

    Counter = ?create_observable_counter(CounterName, #{description => CounterDesc, unit => Unit}),
    Gauge = ?create_observable_gauge(GaugeName, #{description => GaugeDesc, unit => Unit}),

    ?register_callback([Counter, Gauge],
                       fun(_) ->
                               [{Counter, [{4, #{<<"a">> => <<"b">>}}]},
                                {Gauge, [{5, #{<<"a">> => <<"b">>}}]}]
                       end, []),

    otel_meter_server:force_flush(),

    ?assertSumReceive(CounterName, CounterDesc, Unit, [{4, #{<<"a">> => <<"b">>}}]),
    ?assertLastValueReceive(GaugeName, GaugeDesc, Unit, [{5, #{<<"a">> => <<"b">>}}]),

    ok.

sync_filtered_attributes(_Config) ->
    Meter = opentelemetry_experimental:get_meter(
              opentelemetry:get_application_scope(?MODULE)),

    CounterName = a_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    Counter = otel_counter:create(Meter, CounterName,
                                  #{description => CounterDesc,
                                    unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {otel_meter_default, _},
                             module = otel_meter_default,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    Ctx = otel_ctx:new(),

    ?assert(otel_meter_server:add_view(view_a, #{instrument_name => CounterName},
                                       #{aggregation_module => otel_aggregation_sum,
                                         attribute_keys => [a, b]})),

    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2, #{a => 1, b => 2, c => 3})),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5, #{a => 1, b => 2})),
    ?assertEqual(ok, ?counter_add(Counter, 5, #{a => 1, b => 2, c => 3})),

    otel_meter_server:force_flush(),

    ?assertSumReceive(view_a, CounterDesc, kb,
                      [{12, #{a => 1, b => 2}}]),
    ?assertNotReceive(CounterName, CounterDesc, kb),

    ok.

async_filtered_attributes(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_observable_counter,
    CounterDesc = <<"observable counter description">>,
    CounterUnit = kb,

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName},
                                       #{aggregation_module => otel_aggregation_sum,
                                         attribute_keys => [a]})),

    Counter = otel_meter:create_observable_counter(Meter, CounterName,
                                                   fun(_Args) ->
                                                           MeasurementAttributes = #{a => b,
                                                                                     c => d},
                                                           [{4, MeasurementAttributes}]
                                                   end,
                                                   [],
                                                   #{description => CounterDesc,
                                                     unit => CounterUnit}),

    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = observable_counter,
                             unit = CounterUnit,
                             callback=_}, Counter),

    otel_meter_server:force_flush(),

    ?assertSumReceive(CounterName, <<"observable counter description">>, kb, [{4, #{a => b}}]),

    ok.

delta_observable_counter(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_observable_counter,
    CounterDesc = <<"observable counter description">>,
    CounterUnit = kb,

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName}, #{aggregation_module => otel_aggregation_sum})),

    Counter = otel_meter:create_observable_counter(Meter, CounterName,
                                                   fun(_Args) ->
                                                           MeasurementAttributes = #{<<"a">> => <<"b">>},
                                                           [{4, MeasurementAttributes}]
                                                   end,
                                                   [],
                                                   #{description => CounterDesc,
                                                     unit => CounterUnit}),

    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = observable_counter,
                             unit = CounterUnit,
                             callback=_}, Counter),

    otel_meter_server:force_flush(),

    ?assertSumReceive(CounterName, <<"observable counter description">>, kb, [{4, #{<<"a">> => <<"b">>}}]),

    otel_meter_server:force_flush(),

    ?assertSumReceive(CounterName, <<"observable counter description">>, kb, [{0, #{<<"a">> => <<"b">>}}]),

    ok.

bad_observable_return(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_observable_counter,
    CounterName2 = b_observable_counter,
    CounterDesc = <<"observable counter description">>,
    CounterDesc2 = <<"observable counter 2 description">>,
    CounterUnit = kb,

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName}, #{})),
    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName2}, #{})),

    _Counter = otel_meter:create_observable_counter(Meter, CounterName,
                                                    eqwalizer:dynamic_cast(fun(_Args) ->
                                                                                     not_a_list
                                                                             end),
                                                    [],
                                                    #{description => CounterDesc,
                                                      unit => CounterUnit}),

    _Counter2 = otel_meter:create_observable_counter(Meter, CounterName2,
                                                     eqwalizer:dynamic_cast(fun(_Args) ->
                                                                                      [{not_a_number, #{}},
                                                                                       {7, not_a_map},
                                                                                       {8, #{}}]
                                                                              end),
                                                     [],
                                                     #{description => CounterDesc2,
                                                       unit => CounterUnit}),

    otel_meter_server:force_flush(),

    ?assertSumReceive(CounterName2, <<"observable counter 2 description">>, kb, [{8, #{}}]),

    otel_meter_server:force_flush(),

    ?assertSumReceive(CounterName2, <<"observable counter 2 description">>, kb, [{8, #{}}]),

    ok.

advisory_params(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    % explicit_bucket_boundaries allowed only for histograms
    Counter = otel_counter:create(Meter, invalid_1,
                                  #{advisory_params => #{explicit_bucket_boundaries => [1, 2, 3]}}),
    ?assertEqual(Counter#instrument.advisory_params, #{}),

    % advisory parameters different from explicit_bucket_boundaries are not allowed
    Counter1 = otel_counter:create(Meter, invalid_2,
                                   eqwalizer:dynamic_cast(#{advisory_params => #{invalid => invalid}})),
    ?assertEqual(Counter1#instrument.advisory_params, #{}),

    % explicit_bucket_boundaries should be an ordered list of numbers
    Histo1 = otel_histogram:create(
               Meter, invalid_3,
               eqwalizer:dynamic_cast(#{advisory_params => #{explicit_bucket_boundaries => invalid}})),
    ?assertEqual(Histo1#instrument.advisory_params, #{}),

    Histo2 = otel_histogram:create(Meter, invalid_4,
                                  #{advisory_params => #{explicit_bucket_boundaries => [2,1,4]}}),
    ?assertEqual(Histo2#instrument.advisory_params, #{}),

    % when valid use the explicit_bucket_boundaries from advisory_params if not set in a view
    Histogram = otel_histogram:create(Meter, a_histogram,
                                  #{advisory_params => #{explicit_bucket_boundaries => [10, 20, 30]}}),
    ?assertEqual(Histogram#instrument.advisory_params, #{explicit_bucket_boundaries => [10, 20, 30]}),

    %% an empty boundaries list can be used to get a single bucket histogram `(-Inf, +Inf)'
    BHistogram = otel_histogram:create(Meter, b_histogram,
                                       #{advisory_params => #{explicit_bucket_boundaries => []}}),
    ?assertEqual(BHistogram#instrument.advisory_params, #{explicit_bucket_boundaries => []}),

    Ctx = otel_ctx:new(),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 15, #{<<"a">> => <<"1">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 50, #{<<"a">> => <<"1">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 26, #{<<"a">> => <<"2">>})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=a_histogram,
                              data=#histogram{datapoints=Datapoints}}} ->
            AttributeBuckets =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- Datapoints]),
            ?assertEqual([], [{#{<<"a">> => <<"1">>}, [0,1,0,1], 15, 50, 65},
                              {#{<<"a">> => <<"2">>}, [0,0,1,0], 26, 26, 26}]
                         -- AttributeBuckets, AttributeBuckets)
    after
        5000 ->
            ct:fail(histogram_receive_timeout)
    end,

    % explicit_bucket_boundaries from view have precedence
    ?assert(otel_meter_server:add_view(view, #{instrument_name => b_histogram}, #{
        aggregation_module => otel_aggregation_histogram_explicit,
        aggregation_options => #{explicit_bucket_boundaries => [10, 100]}})),

    HistogramBName = b_histogram,
    HistogramB = otel_histogram:create(Meter, HistogramBName,
                                  #{advisory_params => #{explicit_bucket_boundaries => [10, 20, 30]}}),
    %% Identical instruments return the canonical, first-created handle.
    ?assertEqual(HistogramB#instrument.advisory_params, #{explicit_bucket_boundaries => []}),

    ?assertEqual(ok, otel_histogram:record(Ctx, HistogramB, 15, #{<<"a">> => <<"1">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HistogramB, 50, #{<<"a">> => <<"1">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HistogramB, 26, #{<<"a">> => <<"2">>})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=view,
                              data=#histogram{datapoints=DatapointsB}}} ->
            AttributeBucketsB =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- DatapointsB]),
            ?assertEqual([], [{#{<<"a">> => <<"1">>}, [0,2,0], 15, 50, 65},
                              {#{<<"a">> => <<"2">>}, [0,1,0], 26, 26, 26}]
                         -- AttributeBucketsB, AttributeBucketsB)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end.

histogram_aggregation_options(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    ?assert(otel_meter_server:add_view(view, #{instrument_name => histogram}, #{
        aggregation_module => otel_aggregation_histogram_explicit,
        aggregation_options => #{explicit_bucket_boundaries => [10, 100]}})),

    HistogramName = histogram,
    Histogram = otel_histogram:create(Meter, HistogramName, #{}),

    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 15, #{<<"a">> => <<"1">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 50, #{<<"a">> => <<"1">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Histogram, 26, #{<<"a">> => <<"2">>})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=view,
                              data=#histogram{datapoints=DatapointsB}}} ->
            AttributeBucketsB =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- DatapointsB]),
            ?assertEqual([], [{#{<<"a">> => <<"1">>}, [0,2,0], 15, 50, 65},
                              {#{<<"a">> => <<"2">>}, [0,1,0], 26, 26, 26}]
                         -- AttributeBucketsB, AttributeBucketsB)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end.

sync_delta_histogram(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),
    ?assert(otel_meter_server:add_view(http_req_view, #{instrument_name => http_requests}, #{
                                                         aggregation_module => otel_aggregation_histogram_explicit,
                                                         aggregation_options => #{explicit_bucket_boundaries => []}})),

    HttpReqHistogram = otel_histogram:create(Meter, http_requests, #{}),

    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 50, #{verb => <<"GET">>,
                                                                                   status => 200})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 100, #{verb => <<"GET">>,
                                                                                    status => 200})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 1, #{verb => <<"GET">>,
                                                                                  status => 500})),


    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=http_req_view,
                              data=#histogram{datapoints=Datapoints}}} ->
            AttributeBuckets =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- Datapoints]),
            ?assertEqual([], [{#{status => 200,verb => <<"GET">>},[2],50,100,150},
                              {#{status => 500,verb => <<"GET">>},[1],1,1,1}]
                         -- AttributeBuckets, AttributeBuckets)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end,

    otel_meter_server:force_flush(),

    %% TODO: check for nothing

    receive
        {otel_metric, #metric{name=http_req_view,
                              data=#histogram{datapoints=[]}}} ->
            ok
    end,

    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 5, #{verb => <<"GET">>,
                                                                                  status => 500})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 2, #{verb => <<"GET">>,
                                                                                  status => 500})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=http_req_view,
                              data=#histogram{datapoints=Datapoints1}}} ->
            AttributeBuckets1 =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- Datapoints1]),
            ?assertEqual([], [{#{status => 500,verb => <<"GET">>},[2],2,5,7}]
                         -- AttributeBuckets1, AttributeBuckets1)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end,

    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 100, #{verb => <<"GET">>,
                                                                                    status => 200})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=http_req_view,
                              data=#histogram{datapoints=Datapoints2}}} ->
            AttributeBuckets2 =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- Datapoints2]),
            ?assertEqual([], [{#{status => 200,verb => <<"GET">>},[1],100,100,100}]
                         -- AttributeBuckets2, AttributeBuckets2)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end,

    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 200, #{verb => <<"GET">>,
                                                                                    status => 200})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 30, #{verb => <<"GET">>,
                                                                                   status => 200})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 50, #{verb => <<"GET">>,
                                                                                   status => 200})),
    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=http_req_view,
                              data=#histogram{datapoints=Datapoints3}}} ->
            AttributeBuckets3 =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- Datapoints3]),
            ?assertEqual([], [{#{status => 200,verb => <<"GET">>},[3],30,200,280}]
                         -- AttributeBuckets3, AttributeBuckets3)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end,
    ok.

sync_cumulative_histogram(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    ?assert(otel_meter_server:add_view(http_req_view, #{instrument_name => http_requests}, #{
                                                         aggregation_module => otel_aggregation_histogram_explicit,
                                                         aggregation_options => #{explicit_bucket_boundaries => []}})),

    Ctx = otel_ctx:new(),
    HttpReqHistogramName = http_requests,
    HttpReqHistogram = otel_histogram:create(Meter, HttpReqHistogramName, #{}),

    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 50, #{verb => <<"GET">>,
                                                                                   status => 200})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 100, #{verb => <<"GET">>,
                                                                                    status => 200})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 1, #{verb => <<"GET">>,
                                                                                  status => 500})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=http_req_view,
                              data=#histogram{datapoints=Datapoints}}} ->
            AttributeBuckets =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- Datapoints]),
            ?assertEqual([], [{#{status => 200,verb => <<"GET">>},[2],50,100,150},
                              {#{status => 500,verb => <<"GET">>},[1],1,1,1}]
                         -- AttributeBuckets, AttributeBuckets)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end,

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=http_req_view,
                              data=#histogram{datapoints=Datapoints0}}} ->
            AttributeBuckets0 =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- Datapoints0]),
            ?assertEqual([], [{#{status => 200,verb => <<"GET">>},[2],50,100,150},
                              {#{status => 500,verb => <<"GET">>},[1],1,1,1}]
                         -- AttributeBuckets0, AttributeBuckets0)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end,

    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 5, #{verb => <<"GET">>,
                                                                                  status => 500})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 2, #{verb => <<"GET">>,
                                                                                  status => 500})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=http_req_view,
                              data=#histogram{datapoints=Datapoints1}}} ->
            AttributeBuckets1 =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- Datapoints1]),
            ?assertEqual([], [{#{status => 200,verb => <<"GET">>},[2],50,100,150},
                              {#{status => 500,verb => <<"GET">>},[3],1,5,8}]
                         -- AttributeBuckets1, AttributeBuckets1)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end,

    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 100, #{verb => <<"GET">>,
                                                                                    status => 200})),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=http_req_view,
                              data=#histogram{datapoints=Datapoints2}}} ->
            AttributeBuckets2 =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- Datapoints2]),
            ?assertEqual([], [{#{status => 200,verb => <<"GET">>},[3],50,100,250},
                              {#{status => 500,verb => <<"GET">>},[3],1,5,8}]
                         -- AttributeBuckets2, AttributeBuckets2)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end,

    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 100, #{verb => <<"GET">>,
                                                                                    status => 200})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 30, #{verb => <<"GET">>,
                                                                                   status => 200})),
    ?assertEqual(ok, otel_histogram:record(Ctx, HttpReqHistogram, 50, #{verb => <<"GET">>,
                                                                                   status => 200})),
    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=http_req_view,
                              data=#histogram{datapoints=Datapoints3}}} ->
            AttributeBuckets3 =
                lists:sort([{Attributes, Buckets, Min, Max, Sum} || #histogram_datapoint{bucket_counts=Buckets,
                                                                                         attributes=Attributes,
                                                                                         min=Min,
                                                                                         max=Max,
                                                                                         sum=Sum} <- Datapoints3]),
            ?assertEqual([], [{#{status => 200,verb => <<"GET">>},[6],30,100,430},
                              {#{status => 500,verb => <<"GET">>},[3],1,5,8}]
                         -- AttributeBuckets3, AttributeBuckets3)
    after
        1000 ->
            ct:fail(histogram_receive_timeout)
    end,
    ok.

async_cumulative_page_faults(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = page_faults,
    CounterDesc = <<"number of page faults">>,
    CounterUnit = '1',

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName},
                                       #{aggregation_module => otel_aggregation_sum})),

    %% use an atomic to change the returned value of the observable callback on each call
    IntervalCounter = atomics:new(1, []),
    Pid1001 = #{pid => 1001},
    Pid1002 = #{pid => 1002},
    Pid1003 = #{pid => 1003},

    %% tuple of the measurements to return from the observable callback for each time interval
    %% and the corresponding expected metrics to get from the exporter.
    MeasurementsAndExpected = {{[{50, Pid1001}, {30, Pid1002}],
                                [{50, Pid1001}, {30, Pid1002}]},
                               {[{53, Pid1001}, {38, Pid1002}],
                                [{53, Pid1001}, {38, Pid1002}]},
                               {[{56, Pid1001}, {42, Pid1002}],
                                [{56, Pid1001}, {42, Pid1002}]},
                               {[{60, Pid1001}, {47, Pid1002}],
                                [{60, Pid1001}, {47, Pid1002}]},
                               {[{53, Pid1002}, {5, Pid1003}],
                                [{53, Pid1002}, {5, Pid1003}]},
                               {[{10, Pid1001}, {57, Pid1002}, {8, Pid1003}],
                                [{10, Pid1001}, {57, Pid1002}, {8, Pid1003}]}},

    Counter = otel_meter:create_observable_counter(Meter, CounterName,
                                                   fun(_Args) ->
                                                           Interval = atomics:add_get(IntervalCounter, 1, 1),
                                                           element(1, element(Interval, MeasurementsAndExpected))
                                                   end,
                                                   [],
                                                   #{description => CounterDesc,
                                                     unit => CounterUnit}),

    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = observable_counter,
                             unit = CounterUnit,
                             callback=_}, Counter),

    lists:foreach(fun({_, Expected}) ->
                        otel_meter_server:force_flush(),

                        %% verify the delta metrics
                        check_observer_results(CounterName, Expected)
                end, tuple_to_list(MeasurementsAndExpected)),

    ok.

async_delta_page_faults(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = page_faults,
    CounterDesc = <<"number of page faults">>,
    CounterUnit = '1',

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName},
                                       #{aggregation_module => otel_aggregation_sum})),

    %% use an atomic to change the returned value of the observable callback on each call
    IntervalCounter = atomics:new(1, []),
    Pid1001 = #{pid => 1001},
    Pid1002 = #{pid => 1002},
    Pid1003 = #{pid => 1003},

    %% tuple of the measurements to return from the observable callback for each time interval
    %% and the corresponding expected metrics to get from the exporter.
    MeasurementsAndExpected = {{[{50, Pid1001}, {30, Pid1002}],
                                [{50, Pid1001}, {30, Pid1002}]},
                               {[{53, Pid1001}, {38, Pid1002}],
                                [{3, Pid1001}, {8, Pid1002}]},
                               {[{56, Pid1001}, {42, Pid1002}],
                                [{3, Pid1001}, {4, Pid1002}]},
                               {[{60, Pid1001}, {47, Pid1002}],
                                [{4, Pid1001}, {5, Pid1002}]},
                               {[{53, Pid1002}, {5, Pid1003}],
                                [{6, Pid1002}, {5, Pid1003}]},
                               {[{10, Pid1001}, {57, Pid1002}, {8, Pid1003}],
                                [{10, Pid1001}, {4, Pid1002}, {3, Pid1003}]}},

    Counter = otel_meter:create_observable_counter(Meter, CounterName,
                                                   fun(_Args) ->
                                                           Interval = atomics:add_get(IntervalCounter, 1, 1),
                                                           element(1, element(Interval, MeasurementsAndExpected))
                                                   end,
                                                   [],
                                                   #{description => CounterDesc,
                                                     unit => CounterUnit}),

    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = observable_counter,
                             unit = CounterUnit,
                             callback=_}, Counter),

    lists:foldl(fun({_, Expected}, {LastPid1001StartTime, LastPid1002StartTime}) ->
                        otel_meter_server:force_flush(),

                        %% verify the delta metrics
                        Results = check_observer_results(CounterName, Expected),

                        %% check that the start times change on each collection
                        Pid1001StartTime1 =
                            case lists:keyfind(#{pid => 1001}, 2, Results) of
                                false ->
                                    false;
                                {_, _, Pid1001StartTime, _} ->
                                    ?assertNotEqual(Pid1001StartTime, LastPid1001StartTime),
                                    Pid1001StartTime
                            end,

                        Pid1002StartTime1 =
                            case lists:keyfind(#{pid => 1001}, 2, Results) of
                                false ->
                                    false;
                                {_, _, Pid1002StartTime, _} ->
                                    ?assertNotEqual(Pid1002StartTime, LastPid1002StartTime),
                                    Pid1002StartTime
                            end,

                        {Pid1001StartTime1, Pid1002StartTime1}
                end, {0, 0}, tuple_to_list(MeasurementsAndExpected)),

    ok.

async_attribute_removal(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = page_faults,
    CounterDesc = <<"number of page faults">>,
    CounterUnit = '1',

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName},
                                       #{aggregation_module => otel_aggregation_sum,
                                         attribute_keys => []})),

    %% use an atomic to change the returned value of the observable callback on each call
    IntervalCounter = atomics:new(1, []),
    Pid1001 = #{pid => 1001},
    Pid1002 = #{pid => 1002},
    Pid1003 = #{pid => 1003},

    %% tuple of the measurements to return from the observable callback for each time interval
    %% and the corresponding expected metrics to get from the exporter.
    MeasurementsAndExpected = {{[{50, Pid1001}, {30, Pid1002}],
                                [{80, #{}}]},
                               {[{53, Pid1001}, {38, Pid1002}],
                                [{91, #{}}]},
                               {[{56, Pid1001}, {42, Pid1002}],
                                [{98, #{}}]},
                               {[{60, Pid1001}, {47, Pid1002}],
                                [{107, #{}}]},
                               {[{53, Pid1002}, {5, Pid1003}],
                                [{58, #{}}]},
                               {[{10, Pid1001}, {57, Pid1002}, {8, Pid1003}],
                                [{75, #{}}]}},

    Counter = otel_meter:create_observable_counter(Meter, CounterName,
                                                   fun(_Args) ->
                                                           Interval = atomics:add_get(IntervalCounter, 1, 1),
                                                           element(1, element(Interval, MeasurementsAndExpected))
                                                   end,
                                                   [],
                                                   #{description => CounterDesc,
                                                     unit => CounterUnit}),

    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = observable_counter,
                             unit = CounterUnit,
                             callback=_}, Counter),

    lists:foreach(fun({_, Expected}) ->
                          otel_meter_server:force_flush(),
                          check_observer_results(CounterName, Expected)
                  end, tuple_to_list(MeasurementsAndExpected)),

    ok.


simple_fixed_exemplars(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = test_exemplar_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    CBAttributes = #{<<"c">> => <<"b">>},
    CBFGAttributes = #{<<"c">> => <<"b">>, <<"f">> => <<"g">>},
    FGAttributes = #{<<"f">> => <<"g">>},
    ABDEAttributes = #{<<"a">> => <<"b">>, <<"d">> => <<"e">>},

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName},
                                       #{aggregation_module => otel_aggregation_sum,
                                         attribute_keys => [<<"c">>]})),

    Counter = otel_meter:create_counter(Meter, CounterName,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    Ctx = otel_ctx:get_current(),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2, CBFGAttributes)),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, ABDEAttributes)),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 4, CBFGAttributes)),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5, CBFGAttributes)),

    ExemplarsTab = exemplars_otel_meter_provider_global,

    [Reader] = otel_meter_server:get_readers(),
    ReaderId = element(2, Reader),
    StreamId = instrument_stream_id(Counter, ReaderId),

    %% default number of exemplars in SimpleFixedReservoir is the number of schedulers
    MaxExemplars = erlang:system_info(schedulers_online),

    %% measurements recorded before the first collection so generation is `0'
    Generation0 = 0,
    ExemplarReservoir = otel_metric_exemplar_reservoir:new(otel_metric_exemplar_reservoir_simple, #{}, fun otel_metric_exemplar_filter:always_on/6),
    %% exemplars for CBAttributes should be the min of 3
    %% (the number of measurements we made above) and MaxExemplars
    CBAttributesBinary = term_to_binary(CBAttributes),
    [[Count]] = ets:match(ExemplarsTab, {{StreamId, CBAttributesBinary, ReaderId, Generation0}, '$1'}),
    ?assertEqual(3, Count),
    Matches = otel_metric_exemplar_reservoir:collect(ExemplarReservoir, ExemplarsTab, {StreamId, CBAttributesBinary, ReaderId, Generation0}),
    %% collection deletes the objects
    ?assertEqual([], ets:match(ExemplarsTab, {{StreamId, CBAttributesBinary, ReaderId, Generation0}, '$1'})),
    ?assertEqual([], ets:match(ExemplarsTab, {{{StreamId, CBAttributesBinary, ReaderId, Generation0}, '_'}, '$1'})),
    ?assertEqual(min(3, MaxExemplars), length(Matches)),

    %% bump generation
    otel_meter_server:force_flush(),

    %% now do more than `MaxExemplars' measurements to check it still only keeps
    %% total of `MaxExemplars'
    TotalMeasurements = MaxExemplars + 20,
    lists:foreach(fun(N) ->
                          ?assertEqual(ok, otel_counter:add(Ctx, Counter, N, CBFGAttributes))
                  end, lists:seq(1, TotalMeasurements)),

    %% total number of exemplars for `CBAttributes' should be `MaxExemplars'
    %% (the number of measurements we made above) and `MaxExemplars'
    Generation1 = 1,

    [[Count1]] = ets:match(ExemplarsTab, {{StreamId, CBAttributesBinary, ReaderId, Generation1}, '$1'}),
    Matches1 = otel_metric_exemplar_reservoir:collect(ExemplarReservoir, ExemplarsTab, {StreamId, CBAttributesBinary, ReaderId, Generation1}),

    %% check that attributes on exemplars are FGAttributes
    ?assertMatch(FGAttributes, (hd(Matches1))#exemplar.filtered_attributes),

    %% collection deletes the objects
    ?assertEqual([], ets:match(ExemplarsTab, {{StreamId, CBAttributesBinary, ReaderId, Generation0}, '$1'})),
    ?assertEqual([], ets:match(ExemplarsTab, {{{StreamId, CBAttributesBinary, ReaderId, Generation0}, '_'}, '$1'})),

    ?assertEqual(TotalMeasurements, Count1),
    ?assertEqual(MaxExemplars, length(Matches1)),

    otel_meter_server:force_flush(),

    ?assertSumReceive(test_exemplar_counter, <<"counter description">>, kb, [{11, #{<<"c">> => <<"b">>} }]),

    ok.

float_simple_fixed_exemplars(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = test_exemplar_float_counter,
    CounterDesc = <<"float counter description">>,
    CounterUnit = kb,

    CBAttributes = #{<<"c">> => <<"b">>},
    ABDEAttributes = #{<<"a">> => <<"b">>, <<"d">> => <<"e">>},

    Counter = otel_meter:create_counter(Meter, CounterName,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    Ctx = otel_ctx:get_current(),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2.2, CBAttributes)),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3.3, ABDEAttributes)),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 4.1, CBAttributes)),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5.8, CBAttributes)),

    ExemplarsTab = exemplars_otel_meter_provider_global,

    [Reader] = otel_meter_server:get_readers(),
    ReaderId = element(2, Reader),
    StreamId = instrument_stream_id(Counter, ReaderId),

    %% default number of exemplars in SimpleFixedReservoir is the number of schedulers
    MaxExemplars = erlang:system_info(schedulers_online),

    [Reader] = otel_meter_server:get_readers(),
    ReaderId = element(2, Reader),

    %% measurements recorded before the first collection so generation is `0'
    Generation0 = 0,
    ExemplarReservoir = otel_metric_exemplar_reservoir:new(otel_metric_exemplar_reservoir_simple, #{}, fun otel_metric_exemplar_filter:always_on/6),
    %% exemplars for CBAttributes should be the min of 3
    %% (the number of measurements we made above) and MaxExemplars
    CBAttributesBinary = term_to_binary(CBAttributes),
    [[Count]] = ets:match(ExemplarsTab, {{StreamId, CBAttributesBinary, ReaderId, Generation0}, '$1'}),
    ?assertEqual(3, Count),
    Matches = otel_metric_exemplar_reservoir:collect(ExemplarReservoir, ExemplarsTab, {StreamId, CBAttributesBinary, ReaderId, Generation0}),
    %% collection deletes the objects
    ?assertEqual([], ets:match(ExemplarsTab, {{StreamId, CBAttributesBinary, ReaderId, Generation0}, '$1'})),
    ?assertEqual([], ets:match(ExemplarsTab, {{{StreamId, CBAttributesBinary, ReaderId, Generation0}, '_'}, '$1'})),
    ?assertEqual(min(3, MaxExemplars), length(Matches)),

    otel_meter_server:force_flush(),
    ?assertSumReceive(CounterName, CounterDesc, CounterUnit, [{12.1, CBAttributes}]),

    %% now do more than `MaxExemplars' measurements to check it still only keeps
    %% total of `MaxExemplars'
    TotalMeasurements = MaxExemplars + 20,
    lists:foreach(fun(N) ->
                          ?assertEqual(ok, otel_counter:add(Ctx, Counter, N, CBAttributes))
                  end, lists:seq(1, TotalMeasurements)),

    %% total number of exemplars for `CBAttributes' should be `MaxExemplars'
    %% (the number of measurements we made above) and `MaxExemplars'
    Generation1 = 1,
    [[Count1]] = ets:match(ExemplarsTab, {{StreamId, CBAttributesBinary, ReaderId, Generation1}, '$1'}),
    Matches1 = otel_metric_exemplar_reservoir:collect(ExemplarReservoir, ExemplarsTab, {StreamId, CBAttributesBinary, ReaderId, Generation1}),

    %% collection deletes the objects
    ?assertEqual([], ets:match(ExemplarsTab, {{StreamId, CBAttributesBinary, ReaderId, Generation0}, '$1'})),
    ?assertEqual([], ets:match(ExemplarsTab, {{{StreamId, CBAttributesBinary, ReaderId, Generation0}, '_'}, '$1'})),

    ?assertEqual(TotalMeasurements, Count1),
    ?assertEqual(MaxExemplars, length(Matches1)),

    ok.

explicit_histogram_exemplars(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = f_histogram,
    CounterDesc = <<"macro made histogram description">>,
    CounterUnit = kb,
    CBAttributes = #{<<"c">> => <<"b">>},

    Counter = ?create_histogram(CounterName, #{description => CounterDesc,
                                               unit => CounterUnit}),
    Ctx = otel_ctx:new(),

    ?assertEqual(ok, otel_histogram:record(Ctx, Counter, 10.3, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Counter, 10.3, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, ?histogram_record(Counter, 5.5, #{<<"c">> => <<"b">>})),

    %% without attributes
    ?assertEqual(ok, ?histogram_record(Counter, 1.2, #{})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Counter, 2.1, #{})),

    %% negative values are discarded
    ?assertEqual(ok, ?histogram_record(Counter, -2, #{<<"c">> => <<"b">>})),
    ?assertEqual(ok, otel_histogram:record(Ctx, Counter, -2, #{})),

    %% float type accepts integers
    ?assertEqual(ok, ?histogram_record(Counter, 5, #{<<"c">> => <<"b">>})),

    ExemplarsTab = exemplars_otel_meter_provider_global,

    [Reader] = otel_meter_server:get_readers(),
    ReaderId = element(2, Reader),
    StreamId = instrument_stream_id(Counter, ReaderId),

    %% measurements recorded before the first collection so generation is `0'
    Generation0 = 0,
    ExemplarReservoir = otel_metric_exemplar_reservoir:new(otel_metric_exemplar_reservoir_aligned_histogram, #{}, fun otel_metric_exemplar_filter:always_on/6),

    CBAttributesBinary = term_to_binary(CBAttributes),
    Matches = otel_metric_exemplar_reservoir:collect(ExemplarReservoir, ExemplarsTab, {StreamId, CBAttributesBinary, ReaderId, Generation0}),
    ?assertEqual([], ets:match(ExemplarsTab, {{{StreamId, CBAttributesBinary, ReaderId, Generation0}, '_'}, '$1'})),
    ?assertMatch([{exemplar,5, _, _ , _, _},
                  {exemplar,5.5, _, _ , _, _},
                  {exemplar,10.3, _, _ , _, _}], lists:sort(Matches)),

    otel_meter_server:force_flush(),

    receive
        {otel_metric, #metric{name=f_histogram,
                              data=#histogram{datapoints=Datapoints}}} ->
            AttributeBuckets =
                [{Attributes, Buckets, Min, Max, Sum}
                 || #histogram_datapoint{bucket_counts=Buckets,
                                         attributes=Attributes,
                                         min=Min,
                                         max=Max,
                                         sum=Sum}  <- Datapoints],
            ?assertEqual([], [
                              {#{<<"c">> => <<"b">>}, [0,1,1,2,0,0,0,0,0,0,0,0,0,0,0,0], 5, 10.3, 31.1},
                              {#{}, [0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 1.2, 2.1, 3.3}]
                         -- AttributeBuckets, AttributeBuckets)
    after
        5000 ->
            ct:fail(histogram_receive_timeout)
    end,

    ok.

%% verify the trace_based filter only creates exemplars for measurements taken
%% when there is an active sampled span
trace_based_exemplars(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = test_exemplar_counter,
    CounterDesc = <<"counter description">>,
    CounterUnit = kb,

    CBAttributes = #{<<"c">> => <<"b">>},
    ABDEAttributes = #{<<"a">> => <<"b">>, <<"d">> => <<"e">>},

    Counter = otel_meter:create_counter(Meter, CounterName,
                                        #{description => CounterDesc,
                                          unit => CounterUnit}),
    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = counter,
                             unit = CounterUnit}, Counter),

    Ctx = otel_ctx:get_current(),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 2, CBAttributes)),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 3, ABDEAttributes)),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 4, CBAttributes)),
    ?assertEqual(ok, otel_counter:add(Ctx, Counter, 5, CBAttributes)),

    ExemplarsTab = exemplars_otel_meter_provider_global,

    [Reader] = otel_meter_server:get_readers(),
    ReaderId = element(2, Reader),
    StreamId = instrument_stream_id(Counter, ReaderId),

    %% measurements recorded before the first collection so generation is `0'
    Generation0 = 0,

    %% doesn't matter what the last 2 args are we are just creating a fake reservoir
    %% structure so we can easily call collect on it
    ExemplarReservoir = otel_metric_exemplar_reservoir:new(otel_metric_exemplar_reservoir_simple, #{}, fun otel_metric_exemplar_filter:trace_based/6),

    %% no active span so no sampled exemplars
    ?assertEqual([], ets:match(ExemplarsTab, {{StreamId, CBAttributes, ReaderId, Generation0}, '$1'})),

    %% bump generation
    otel_meter_server:force_flush(),

    ?with_span(<<"span-1">>, #{}, fun(_) ->
                                          ?assert(otel_span:is_recording(otel_tracer:current_span_ctx())),
                                          ?assertEqual(ok, ?counter_add(Counter, 3, CBAttributes)),
                                          ?assertEqual(ok, ?counter_add(Counter, 5, CBAttributes)),
                                          ?assertEqual(ok, ?counter_add(Counter, 2, CBAttributes))
                                  end),

    Generation1 = 1,
    CBAttributesBinary = term_to_binary(CBAttributes),
    [[_Count1]] = ets:match(ExemplarsTab, {{StreamId, CBAttributesBinary, ReaderId, Generation1}, '$1'}),
    Matches1 = otel_metric_exemplar_reservoir:collect(ExemplarReservoir, ExemplarsTab, {StreamId, CBAttributesBinary, ReaderId, Generation1}),

    %% collection deletes the objects
    ?assertEqual([], ets:match(ExemplarsTab, {{StreamId, CBAttributesBinary, ReaderId, Generation0}, '$1'})),
    ?assertEqual([], ets:match(ExemplarsTab, {{{StreamId, CBAttributesBinary, ReaderId, Generation0}, '_'}, '$1'})),

    %% default number of exemplars in SimpleFixedReservoir is the number of schedulers
    MaxExemplars = erlang:system_info(schedulers_online),

    %% since there was an active span the measurements were sampled
    ?assertEqual(min(3, MaxExemplars), length(Matches1)),

    otel_meter_server:force_flush(),

    ?assertSumReceive(test_exemplar_counter, <<"counter description">>, kb, [{11, CBAttributes}]),

    ok.

observable_exemplars(_Config) ->
    DefaultMeter = otel_meter_default,

    Meter = opentelemetry_experimental:get_meter(),
    ?assertMatch({DefaultMeter, _}, Meter),

    CounterName = a_observable_counter,
    CounterDesc = <<"observable counter description">>,
    CounterUnit = kb,

    ?assert(otel_meter_server:add_view(#{instrument_name => CounterName},
                                       #{aggregation_module => otel_aggregation_sum,
                                         attribute_keys => [<<"a">>]})),

    Counter = otel_meter:create_observable_counter(Meter, CounterName,
                                                   fun(_Args) ->
                                                           MeasurementAttributes1 = #{<<"a">> => <<"b">>, <<"f">> => <<"g">>},
                                                           MeasurementAttributes2 = #{<<"c">> => <<"d">>},
                                                           [{4, MeasurementAttributes1},
                                                            {5, MeasurementAttributes2}]
                                                   end,
                                                   [],
                                                   #{description => CounterDesc,
                                                     unit => CounterUnit}),

    ?assertMatch(#instrument{meter = {DefaultMeter,_},
                             module = DefaultMeter,
                             name = CounterName,
                             description = CounterDesc,
                             kind = observable_counter,
                             unit = CounterUnit,
                             callback=_}, Counter),

    otel_meter_server:force_flush(),

    ?assertSumExemplarReceive(CounterName, <<"observable counter description">>, kb, [{4, #{<<"a">> => <<"b">>}, 
                                                                                       [{4, #{<<"f">> => <<"g">>}}]},
                                                                                      {5, #{}, [{5, #{<<"c">> => <<"d">>}}]}]),

    otel_meter_server:force_flush(),

    ?assertSumExemplarReceive(CounterName, <<"observable counter description">>, kb, [{4, #{<<"a">> => <<"b">>}, [{4, #{<<"f">> => <<"g">>}}]},
                                                                                       {5, #{}, [{5, #{<<"c">> => <<"d">>}}]}]),

    ok.

simple_producer(_Config) ->
    otel_meter_server:force_flush(),

    ?assertSumReceive(external_counter_1, <<"external counter description">>, kb,
                      [{50, #{<<"a">> => <<"b">>}}]),
    ok.

%%

instrument_stream_id(Instrument, ReaderId) ->
    [StreamId] = [Id || #stream{id=Id, reader=IdReader} <-
                            otel_metrics_tables:match_streams(
                              streams_otel_meter_provider_global,
                              Instrument),
                       IdReader =:= ReaderId],
    StreamId.

only_instrument_stream(#instrument{meter={_, #meter{streams_tab=StreamsTab}}}=Instrument) ->
    [Stream] = otel_metrics_tables:match_streams(StreamsTab, Instrument),
    Stream.

check_observer_results(MetricName, Expected) ->
    receive
        {otel_metric, #metric{name=Name,
                              data=#sum{datapoints=MetricDatapoints}}}
          when MetricName =:= Name ->
            Datapoints =
                [{MetricValue, MetricAttributes, StartTime, Time} ||
                    #datapoint{value=MetricValue,
                               attributes=MetricAttributes,
                               start_time=StartTime,
                               time=Time
                              } <- MetricDatapoints, StartTime =< Time
                ],

            DatapointsWithoutTime = [{V, A} || {V, A, _, _} <- Datapoints],
            ?assert(is_subset(Expected, DatapointsWithoutTime), {Expected, MetricDatapoints}),
            Datapoints
    after
        5000 ->
            ct:fail({metric_receive_timeout, ?LINE})
    end.

is_subset(List1, List2) ->
    sets:is_subset(sets:from_list(List1), sets:from_list(List2)).
