-module(otel_configuration_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("otel_span.hrl").

-define(assertIsSubset(X, Y),
        lists:foreach(fun({K, V}) ->
                              V1 = proplists:get_value(K, Y, undefined),
                              %% include the key in the assert so it is there
                              %% in the error message if it fails
                              ?assertEqual({K, V}, {K, V1})
                      end, X)).

all() ->
    [empty_os_environment, sampler, sampler_parent_based, sampler_parent_based_zero,
     sampler_trace_id, sampler_trace_id_default, sampler_parent_based_one,
     log_level, propagators, propagators_b3, propagators_b3multi, otlp_exporter,
     jaeger_exporter, zipkin_exporter, none_exporter, app_env_exporter,
     otlp_metrics_exporter, none_metrics_exporter, span_limits, bad_span_limits,
     bad_app_config, deny_list, resource_detectors, span_processors].

init_per_testcase(empty_os_environment, Config) ->
    Vars = [],
    [{os_vars, Vars} | Config];
init_per_testcase(log_level, Config) ->
    Vars = [{"OTEL_LOG_LEVEL", "error"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(propagators, Config) ->
    Vars = [{"OTEL_PROPAGATORS", "baggage,afakeone"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(propagators_b3, Config) ->
    Vars = [{"OTEL_PROPAGATORS", "b3"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(propagators_b3multi, Config) ->
    Vars = [{"OTEL_PROPAGATORS", "b3multi"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(sampler, Config) ->
    Vars = [{"OTEL_TRACES_SAMPLER", "parentbased_always_off"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(sampler_trace_id, Config) ->
    Vars = [{"OTEL_TRACES_SAMPLER", "traceidratio"},
            {"OTEL_TRACES_SAMPLER_ARG", "0.5"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(sampler_trace_id_default, Config) ->
    Vars = [{"OTEL_TRACES_SAMPLER", "traceidratio"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(sampler_parent_based, Config) ->
    Vars = [{"OTEL_TRACES_SAMPLER", "parentbased_traceidratio"},
            {"OTEL_TRACES_SAMPLER_ARG", "0.5"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(sampler_parent_based_one, Config) ->
    Vars = [{"OTEL_TRACES_SAMPLER", "parentbased_traceidratio"},
            {"OTEL_TRACES_SAMPLER_ARG", "1"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(sampler_parent_based_zero, Config) ->
    Vars = [{"OTEL_TRACES_SAMPLER", "parentbased_traceidratio"},
            {"OTEL_TRACES_SAMPLER_ARG", "0"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(otlp_exporter, Config) ->
    Vars = [{"OTEL_TRACES_EXPORTER", "otlp"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(zipkin_exporter, Config) ->
    Vars = [{"OTEL_TRACES_EXPORTER", "zipkin"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(jaeger_exporter, Config) ->
    Vars = [{"OTEL_TRACES_EXPORTER", "jaeger"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(none_exporter, Config) ->
    Vars = [{"OTEL_TRACES_EXPORTER", "none"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];

init_per_testcase(otlp_metrics_exporter, Config) ->
    Vars = [{"OTEL_METRICS_EXPORTER", "otlp"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(none_metrics_exporter, Config) ->
    Vars = [{"OTEL_METRICS_EXPORTER", "none"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(deny_list, Config) ->
    Vars = [{"OTEL_DENY_LIST", "opentelemetry_exporter,opentelemetry,nonexisting_atom"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(resource_detectors, Config) ->
    Vars = [{"OTEL_RESOURCE_DETECTORS", "opentelemetry_exporter,opentelemetry,nonexisting_atom"}],

    setup_env(Vars),

    [{os_vars, Vars} | Config];
init_per_testcase(span_limits, Config) ->
    Vars = [{"OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT", "111"},
            {"OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT", "009"},
            {"OTEL_SPAN_EVENT_COUNT_LIMIT", "200"},
            {"OTEL_SPAN_LINK_COUNT_LIMIT", "1101"},
            {"OTEL_EVENT_ATTRIBUTE_COUNT_LIMIT", "400"},
            {"OTEL_LINK_ATTRIBUTE_COUNT_LIMIT", "500"}],

    setup_env(Vars),

    ExpectedOpts = #{attribute_limits => #{attribute_count_limit => 111,
                                           attribute_value_length_limit => 9},
                     event_count_limit => 200,
                     link_count_limit => 1101,
                     attribute_per_event_limit => 400,
                     attribute_per_link_limit => 500},
    ExpectedRecord = #span_limits{attribute_count_limit=111,
                                  attribute_value_length_limit=9,
                                  event_count_limit=200,
                                  link_count_limit=1101,
                                  attribute_per_event_limit=400,
                                  attribute_per_link_limit=500},

    [{expected_opts, ExpectedOpts}, {expected_record, ExpectedRecord}, {os_vars, Vars} | Config];
init_per_testcase(bad_span_limits, Config) ->
    Vars = [{"OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT", "aaa"},
            {"OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT", "bbb"},
            {"OTEL_SPAN_EVENT_COUNT_LIMIT", "1d4"},
            {"OTEL_SPAN_LINK_COUNT_LIMIT", "eee"},
            {"OTEL_EVENT_ATTRIBUTE_COUNT_LIMIT", "$L%"},
            {"OTEL_LINK_ATTRIBUTE_COUNT_LIMIT", "gibberish"}],

    setup_env(Vars),

    ExpectedOpts = #{},
    ExpectedRecord = #span_limits{attribute_count_limit=128,
                                  attribute_value_length_limit=infinity,
                                  event_count_limit=128,
                                  link_count_limit=128,
                                  attribute_per_event_limit=128,
                                  attribute_per_link_limit=128},

    [{expected_opts, ExpectedOpts}, {expected_record, ExpectedRecord}, {os_vars, Vars} | Config];
init_per_testcase(bad_app_config, Config) ->
    [{os_vars, []} | Config];
init_per_testcase(app_env_exporter, Config) ->

    [{os_vars, []} | Config];
init_per_testcase(span_processors, Config) ->
    [{os_vars, []} | Config].

end_per_testcase(_, Config) ->
    Vars = ?config(os_vars, Config),

    [os:unsetenv(Name) || {Name, _} <- Vars],

    ok.

empty_os_environment(_Config) ->
    ?assertMatch(#{create_application_tracers := undefined},
                 otel_configuration:merge_with_os([])),

    ?assertMatch(#{log_level := error},
                 otel_configuration:merge_with_os([{log_level, error}])),

    ok.

sampler(_Config) ->
    ?assertMatch({parent_based, #{root := always_off}},
                 maps:get(sampler, otel_configuration:merge_with_os([]))),

    ok.

sampler_parent_based(_Config) ->
    ?assertMatch({parent_based, #{root := {trace_id_ratio_based, 0.5}}},
                 maps:get(sampler, otel_configuration:merge_with_os([]))),

    ok.

sampler_trace_id(_Config) ->
    ?assertMatch({trace_id_ratio_based, 0.5},
                 maps:get(sampler, otel_configuration:merge_with_os([]))),

    ok.

sampler_trace_id_default(_Config) ->
    ?assertMatch({trace_id_ratio_based, 1.0},
                 maps:get(sampler, otel_configuration:merge_with_os([]))),

    ok.

sampler_parent_based_one(_Config) ->
    ?assertMatch({parent_based, #{root := {trace_id_ratio_based, 1.0}}},
                 maps:get(sampler, otel_configuration:merge_with_os([]))),

    ok.

sampler_parent_based_zero(_Config) ->
    ?assertMatch({parent_based, #{root := {trace_id_ratio_based, +0.0}}},
                 maps:get(sampler, otel_configuration:merge_with_os([]))),

    ok.

log_level(_Config) ->
    ?assertMatch(#{log_level := error}, otel_configuration:merge_with_os([])),
    ?assertMatch(#{log_level := error}, otel_configuration:merge_with_os([{log_level, info}])),

    ok.

propagators(_Config) ->
    ?assertMatch(#{log_level := error,
                   propagator := #{composite := [baggage]}},
                 otel_configuration:merge_with_os([{log_level, error}])),

    ok.

propagators_b3(_Config) ->
    ?assertMatch(#{log_level := error,
                   propagator := #{composite := [b3]}},
                 otel_configuration:merge_with_os([{log_level, error}])),

    ok.

propagators_b3multi(_Config) ->
    ?assertMatch(#{log_level := error,
                   propagator := #{composite := [b3multi]}},
                 otel_configuration:merge_with_os([{log_level, error}])),

    ok.

otlp_exporter(_Config) ->
    ?assertMatch({opentelemetry_exporter, #{}},
                 maps:get(traces_exporter, otel_configuration:merge_with_os([]))),

    ok.

jaeger_exporter(_Config) ->
    ?assertMatch(none,
                 maps:get(traces_exporter, otel_configuration:merge_with_os([]))),
    ok.

zipkin_exporter(_Config) ->
    ?assertMatch(none,
                 maps:get(traces_exporter, otel_configuration:merge_with_os([]))),
    ok.

none_exporter(_Config) ->
    ?assertMatch(none,
                 maps:get(traces_exporter, otel_configuration:merge_with_os([]))),

    ?assertMatch(#{tracer_provider := #{processors := [{simple, #{exporter := none}}]}},
                 otel_configuration:merge_with_os([{span_processor, simple}])),

    ?assertMatch(#{tracer_provider := #{processors := [{batch, #{exporter := none}}]}},
                 otel_configuration:merge_with_os([{span_processor, batch}])),

    ok.

app_env_exporter(_Config) ->
    ?assertMatch({someother_exporter, #{}},
                 maps:get(traces_exporter,
                          otel_configuration:merge_with_os([{traces_exporter, {someother_exporter, #{}}}]))),

    ?assertMatch({opentelemetry_exporter, #{}},
                 maps:get(traces_exporter,
                          otel_configuration:merge_with_os([{traces_exporter, otlp}]))),

    ?assertMatch(none,
                 maps:get(traces_exporter,
                          otel_configuration:merge_with_os([{traces_exporter, jaeger}]))),

    ?assertMatch(none,
                 maps:get(traces_exporter,
                          otel_configuration:merge_with_os([{traces_exporter, zipkin}]))),

    ok.

otlp_metrics_exporter(_Config) ->
    ?assertMatch({opentelemetry_exporter, #{}},
                 maps:get(metrics_exporter, otel_configuration:merge_with_os([]))),

    ok.

none_metrics_exporter(_Config) ->
    ?assertMatch(none,
                 maps:get(metrics_exporter, otel_configuration:merge_with_os([]))),

    ok.

deny_list(_Config) ->
    %% nonexisting_atom in the OTEL_DENY_LIST os var is dropped bc it isn't an existing atom
    ?assertMatch(#{deny_list := [opentelemetry_exporter,opentelemetry]},
                 otel_configuration:merge_with_os([])),

    ok.

resource_detectors(_Config) ->
    %% nonexisting_atom in the OTEL_RESOURCE_DETECTORS os var is dropped bc it isn't an existing atom
    ?assertMatch(#{resource_detectors := [opentelemetry_exporter,opentelemetry]},
                 otel_configuration:merge_with_os([])),

    ok.

span_limits(Config) ->
    compare_span_limits(Config).

bad_span_limits(Config) ->
    compare_span_limits(Config).

bad_app_config(_Config) ->
    ?assertNot(maps:is_key(attribute_value_length_limit, otel_configuration:merge_with_os([{attribute_value_length_limit, "aaa"}]))),

    ok.

compare_span_limits(Config) ->
    ExpectedRecord = ?config(expected_record, Config),
    ExpectedOpts = maps:to_list(?config(expected_opts, Config)),
    Opts = maps:to_list(otel_configuration:merge_with_os([])),

    ?assertIsSubset(ExpectedOpts, Opts),

    otel_span_limits:set(maps:from_list(Opts)),

    SpanLimits = otel_span_limits:get(),

    %% verifies the defaults because the base record has the defaults set as well
    ?assertEqual(ExpectedRecord, SpanLimits),

    ok.

span_processors(_Config) ->
    ?assertMatch(#{tracer_provider := #{processors := [{simple, #{}}],
                                        limits := #{attribute_value_length_limit := 10,
                                                    event_attribute_count_limit := 400,
                                                    event_count_limit := 20}}},
                 otel_configuration:merge_with_os([{span_processor, simple},
                                                   {attribute_value_length_limit, 10},
                                                   {attribute_per_event_limit, 400},
                                                   {event_count_limit, 20}])),

    ?assertMatch(#{tracer_provider :=
                       #{processors := [{batch, #{export_timeout := 2,
                                                  max_queue_size := 1,
                                                  schedule_delay := 15000}}]}},
                 otel_configuration:merge_with_os([{span_processor, batch},
                                                   {bsp_scheduled_delay_ms, 15000},
                                                   {bsp_exporting_timeout_ms, 2},
                                                   {bsp_max_queue_size, 1}])),

    ?assertMatch(#{tracer_provider := #{processors := [{batch, #{exporter := none}}]}},
                 otel_configuration:merge_with_os([{span_processor, batch},
                                                   {traces_exporter, none}])),

    ?assertMatch(#{tracer_provider := #{processors := [{batch, #{}}]}},
                 otel_configuration:merge_with_os([{span_processor, batch}])),

    ?assertMatch(#{tracer_provider :=
                       #{processors := [{batch, #{exporter := {opentelemetry_exporter,
                                                               #{endpoints := ["https://example.com"]}},
                                                  export_timeout := 2,
                                                  max_queue_size := 1,
                                                  schedule_delay := 15000}}]}},
                 otel_configuration:merge_with_os([{processors, [{otel_batch_processor, #{exporter => {opentelemetry_exporter, #{endpoints => ["https://example.com"]}},
                                                                                          schedule_delay => 15000,
                                                                                          max_queue_size => 4,
                                                                                          export_timeout => 3}}]},
                                                   {bsp_exporting_timeout_ms, 2},
                                                   {bsp_max_queue_size, 1}])),

    ?assertMatch(#{tracer_provider := #{processors := [{simple, #{export_timeout := 2}}]}},
                 otel_configuration:merge_with_os([{span_processor, simple},
                                                   {ssp_exporting_timeout_ms, 2}])),

    ok.

%%

setup_env(Vars) ->
    [os:putenv(Name, Value) || {Name, Value} <- Vars].
