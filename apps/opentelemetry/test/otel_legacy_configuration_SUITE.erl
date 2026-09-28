-module(otel_legacy_configuration_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").
-include("otel_tracer.hrl").

all() ->
    [defaults,
     application_environment,
     os_environment_precedence,
     application_tracer_compatibility_alias,
     custom_components,
     repeated_resolution,
     application_startup].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Config.

end_per_suite(_Config) ->
    application:unload(opentelemetry),
    ok.

init_per_testcase(_TestCase, Config) ->
    SavedOSEnv = otel_configuration_test_utils:save_environment(),

    SavedAppEnv = application:get_all_env(opentelemetry),
    [application:unset_env(opentelemetry, Key) || {Key, _} <- SavedAppEnv],

    [{saved_os_env, SavedOSEnv}, {saved_app_env, SavedAppEnv} | Config].

end_per_testcase(_TestCase, Config) ->
    _ = application:stop(opentelemetry),

    ok = otel_configuration_test_utils:restore_environment(
           proplists:get_value(saved_os_env, Config)),

    [application:unset_env(opentelemetry, Key)
     || {Key, _} <- application:get_all_env(opentelemetry)],
    [application:set_env(opentelemetry, Key, Value)
     || {Key, Value} <- proplists:get_value(saved_app_env, Config)],
    ok.

defaults(_Config) ->
    BatchProcessor =
        {otel_batch_processor,
         #{scheduled_delay_ms => 5000,
           exporting_timeout_ms => 30000,
           max_queue_size => 2048,
           exporter => {opentelemetry_exporter, #{}}}},
    Expected =
        #{sdk_disabled => false,
          log_level => info,
          register_loaded_applications => undefined,
          create_application_tracers => true,
          id_generator => otel_id_generator,
          deny_list => [],
          resource_detectors => [otel_resource_env_var],
          resource_detector_timeout => 5000,
          bsp_scheduled_delay_ms => undefined,
          bsp_exporting_timeout_ms => undefined,
          bsp_max_queue_size => undefined,
          ssp_exporting_timeout_ms => undefined,
          text_map_propagators => [trace_context, baggage],
          traces_exporter => {opentelemetry_exporter, #{}},
          metrics_exporter => {opentelemetry_exporter, #{}},
          views => [],
          readers => [],
          exemplars_enabled => false,
          exemplar_filter => trace_based,
          metric_producers => [],
          processors => [BatchProcessor],
          sampler => {parent_based, #{root => always_on}},
          sweeper => #{interval => timer:minutes(10),
                       strategy => drop,
                       span_ttl => timer:minutes(30),
                       storage_size => infinity},
          attribute_count_limit => 128,
          attribute_value_length_limit => infinity,
          event_count_limit => 128,
          link_count_limit => 128,
          attribute_per_event_limit => 128,
          attribute_per_link_limit => 128},

    ?assertEqual(Expected, otel_configuration:merge_with_os([])).

application_environment(_Config) ->
    AppEnv =
        [{sdk_disabled, true},
         {log_level, debug},
         {id_generator, custom_id_generator},
         {deny_list, [opentelemetry]},
         {resource_detectors, [otel_resource_detector_test]},
         {resource_detector_timeout, 1234},
         {text_map_propagators, [b3, baggage]},
         {sampler, {parent_based, #{root => always_off}}},
         {processors, [{custom_span_processor, #{custom => value}}]},
         {sweeper, #{interval => 10,
                     strategy => keep,
                     span_ttl => 20,
                     storage_size => 30}},
         {attribute_count_limit, 64}],

    Config = otel_configuration:merge_with_os(AppEnv),

    ?assertMatch(#{sdk_disabled := true,
                   log_level := debug,
                   id_generator := custom_id_generator,
                   deny_list := [opentelemetry],
                   resource_detectors := [otel_resource_detector_test],
                   resource_detector_timeout := 1234,
                   text_map_propagators := [b3, baggage],
                   sampler := {parent_based, #{root := always_off}},
                   processors := [{custom_span_processor, #{custom := value}}],
                   sweeper := #{interval := 10,
                                strategy := keep,
                                span_ttl := 20,
                                storage_size := 30},
                   attribute_count_limit := 64},
                 Config).

os_environment_precedence(_Config) ->
    OSEnv =
        [{"OTEL_LOG_LEVEL", "error"},
         {"OTEL_PROPAGATORS", "b3"},
         {"OTEL_TRACES_SAMPLER", "always_off"},
         {"OTEL_BSP_SCHEDULE_DELAY_MILLIS", "42"},
         {"OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT", "17"},
         {"OTEL_CREATE_APPLICATION_TRACERS", "false"}],
    [os:putenv(Name, Value) || {Name, Value} <- OSEnv],

    AppEnv =
        [{log_level, debug},
         {text_map_propagators, [baggage]},
         {sampler, always_on},
         {processors, [{otel_batch_processor, #{scheduled_delay_ms => 999}}]},
         {attribute_count_limit, 64},
         {create_application_tracers, true}],

    ?assertMatch(#{log_level := error,
                   text_map_propagators := [b3],
                   sampler := always_off,
                   processors :=
                       [{otel_batch_processor, #{scheduled_delay_ms := 999}}],
                   attribute_count_limit := 17,
                   create_application_tracers := false},
                 otel_configuration:merge_with_os(AppEnv)).

application_tracer_compatibility_alias(_Config) ->
    ?assertMatch(#{create_application_tracers := false},
                 otel_configuration:merge_with_os(
                   [{register_loaded_applications, false}])),
    ?assertMatch(#{create_application_tracers := true},
                 otel_configuration:merge_with_os(
                   [{register_loaded_applications, false},
                    {create_application_tracers, true}])).

custom_components(_Config) ->
    CustomSampler = {static_sampler, #{decision => drop}},
    CustomProcessor = {custom_span_processor, #{custom => value}},
    CustomPropagator = custom_text_map_propagator,

    ?assertMatch(#{id_generator := custom_id_generator,
                   sampler := CustomSampler,
                   processors := [CustomProcessor],
                   text_map_propagators := [CustomPropagator]},
                 otel_configuration:merge_with_os(
                   [{id_generator, custom_id_generator},
                    {sampler, CustomSampler},
                    {processors, [CustomProcessor]},
                    {text_map_propagators, [CustomPropagator]}])).

repeated_resolution(_Config) ->
    AppEnv =
        [{processors, [{otel_batch_processor,
                        #{scheduled_delay_ms => 25,
                          exporter => {custom_exporter, #{endpoint => local}}}}]},
         {sampler, {trace_id_ratio_based, 0.25}},
         {resource_detector_timeout, 250}],

    First = otel_configuration:merge_with_os(AppEnv),
    Second = otel_configuration:merge_with_os(AppEnv),

    ?assertEqual(First, Second).

application_startup(_Config) ->
    application:set_env(
      opentelemetry,
      propagator,
      #{composite => [trace_context, baggage]}),
    application:set_env(
      opentelemetry,
      tracer_provider,
      #{processors =>
            [{otel_batch_processor, #{exporter => {otlp_http, #{}}}}],
        sampler => {parent_based, #{root => always_on}}}),
    {ok, _} = application:ensure_all_started(opentelemetry),

    ?assert(is_pid(whereis(otel_tracer_provider_global))),
    ?assert(is_pid(whereis(otel_span_processor_sup_global))),
    ?assertEqual({otel_propagator_text_map_composite,
                  [otel_propagator_trace_context, otel_propagator_baggage]},
                 opentelemetry:get_text_map_injector()),

    {otel_tracer_default,
     #tracer{sampler=Sampler,
             id_generator=otel_id_generator}} = opentelemetry:get_tracer(),
    ?assertEqual(
       <<"ParentBased{root:AlwaysOnSampler,remoteParentSampled:AlwaysOnSampler,"
         "remoteParentNotSampled:AlwaysOffSampler,localParentSampled:AlwaysOnSampler,"
         "localParentNotSampled:AlwaysOffSampler}">>,
       otel_sampler:description(Sampler)).
