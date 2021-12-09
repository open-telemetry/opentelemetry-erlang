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
     jaeger_exporter, zipkin_exporter, none_exporter, span_limits, bad_span_limits,
     bad_app_config, app_env_exporter].

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
init_per_testcase(span_limits, Config) ->
    Vars = [{"OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT", "111"},
            {"OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT", "009"},
            {"OTEL_SPAN_EVENT_COUNT_LIMIT", "200"},
            {"OTEL_SPAN_LINK_COUNT_LIMIT", "1101"},
            {"OTEL_EVENT_ATTRIBUTE_COUNT_LIMIT", "400"},
            {"OTEL_LINK_ATTRIBUTE_COUNT_LIMIT", "500"}],

    setup_env(Vars),

    ExpectedOpts = #{attribute_count_limit => 111,
                     attribute_value_length_limit => 9,
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

    [{os_vars, []} | Config].

end_per_testcase(_, Config) ->
    Vars = ?config(os_vars, Config),

    [os:unsetenv(Name) || {Name, _} <- Vars],

    ok.

empty_os_environment(_Config) ->
    ?assertMatch(#{log_level := info,
                   text_map_propagators := [trace_context, baggage],
                   sampler := {parent_based, #{root := always_on}}},
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
    ?assertMatch({parent_based, #{root := {trace_id_ratio_based, 0.0}}},
                 maps:get(sampler, otel_configuration:merge_with_os([]))),

    ok.

log_level(_Config) ->
    ?assertMatch(#{log_level := error}, otel_configuration:merge_with_os([])),
    ?assertMatch(#{log_level := error}, otel_configuration:merge_with_os([{log_level, info}])),

    ok.

propagators(_Config) ->
    ?assertMatch(#{log_level := error,
                   text_map_propagators := [baggage]},
                 otel_configuration:merge_with_os([{log_level, error}])),

    ok.

propagators_b3(_Config) ->
    ?assertMatch(#{log_level := error,
                   text_map_propagators := [b3]},
                 otel_configuration:merge_with_os([{log_level, error}])),

    ok.

propagators_b3multi(_Config) ->
    ?assertMatch(#{log_level := error,
                   text_map_propagators := [b3multi]},
                 otel_configuration:merge_with_os([{log_level, error}])),

    ok.

otlp_exporter(_Config) ->
    ?assertMatch({opentelemetry_exporter, #{}},
                 maps:get(traces_exporter, otel_configuration:merge_with_os([]))),

    ok.

jaeger_exporter(_Config) ->
    ?assertMatch(undefined,
                 maps:get(traces_exporter, otel_configuration:merge_with_os([]))),
    ok.

zipkin_exporter(_Config) ->
    ?assertMatch(undefined,
                 maps:get(traces_exporter, otel_configuration:merge_with_os([]))),
    ok.

none_exporter(_Config) ->
    ?assertMatch(undefined,
                 maps:get(traces_exporter, otel_configuration:merge_with_os([]))),
    ok.

app_env_exporter(_Config) ->
    ?assertMatch({someother_exporter, #{}},
                 maps:get(traces_exporter,
                          otel_configuration:merge_with_os([{traces_exporter, {someother_exporter, #{}}}]))),

    ok.

span_limits(Config) ->
    compare_span_limits(Config).

bad_span_limits(Config) ->
    compare_span_limits(Config).

bad_app_config(_Config) ->
    ?assertMatch(#{attribute_value_length_limit := infinity},
                 otel_configuration:merge_with_os([{attribute_value_length_limit, "aaa"}])),

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

%%

setup_env(Vars) ->
    [os:putenv(Name, Value) || {Name, Value} <- Vars].
