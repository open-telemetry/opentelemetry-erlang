-module(otel_configuration_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

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
     log_level, propagators, otlp_exporter, jaeger_exporter, zipkin_exporter, none_exporter].

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

    [{os_vars, Vars} | Config].

end_per_testcase(_, Config) ->
    Vars = ?config(os_vars, Config),

    [os:unsetenv(Name) || {Name, _} <- Vars],

    ok.

empty_os_environment(_Config) ->
    ?assertIsSubset([{log_level,info},
                     {propagators,[fun otel_tracer_default:w3c_propagators/0,
                                     fun otel_baggage:get_text_map_propagators/0]},
                     {sampler,{parent_based,#{root => always_on}}}],
                    otel_configuration:merge_with_os([])),

    ?assertIsSubset([{log_level, error}], otel_configuration:merge_with_os([{log_level, error}])),

    ok.

sampler(_Config) ->
    ?assertMatch({sampler, {parent_based, #{root := always_off}}},
                 lists:keyfind(sampler, 1, otel_configuration:merge_with_os([]))),

    ok.

sampler_parent_based(_Config) ->
    ?assertMatch({sampler, {parent_based, #{root := {trace_id_ratio_based, 0.5}}}},
                 lists:keyfind(sampler, 1, otel_configuration:merge_with_os([]))),

    ok.

sampler_trace_id(_Config) ->
    ?assertMatch({sampler, {trace_id_ratio_based, 0.5}},
                 lists:keyfind(sampler, 1, otel_configuration:merge_with_os([]))),

    ok.

sampler_trace_id_default(_Config) ->
    ?assertMatch({sampler, {trace_id_ratio_based, 1.0}},
                 lists:keyfind(sampler, 1, otel_configuration:merge_with_os([]))),

    ok.

sampler_parent_based_one(_Config) ->
    ?assertMatch({sampler, {parent_based, #{root := {trace_id_ratio_based, 1.0}}}},
                 lists:keyfind(sampler, 1, otel_configuration:merge_with_os([]))),

    ok.

sampler_parent_based_zero(_Config) ->
    ?assertMatch({sampler, {parent_based, #{root := {trace_id_ratio_based, 0.0}}}},
                 lists:keyfind(sampler, 1, otel_configuration:merge_with_os([]))),

    ok.

log_level(_Config) ->
    %% TODO: can make this a better error message when it fails with a custom assert macro
    ?assertIsSubset([{log_level, error}], otel_configuration:merge_with_os([])),

    ?assertIsSubset([{log_level, error}], otel_configuration:merge_with_os([{log_level, info}])),

    ok.

propagators(_Config) ->
    %% TODO: can make this a better error message when it fails with a custom assert macro
    ?assertIsSubset([{log_level, error},
                     {propagators, [fun otel_baggage:get_text_map_propagators/0]}],
                    otel_configuration:merge_with_os([{log_level, error}])),

    ok.

otlp_exporter(_Config) ->
    ?assertMatch({traces_exporter, {opentelemetry_exporter, #{}}},
                 lists:keyfind(traces_exporter, 1, otel_configuration:merge_with_os([]))),

    ok.

jaeger_exporter(_Config) ->
    ?assertMatch({traces_exporter, undefined},
                 lists:keyfind(traces_exporter, 1, otel_configuration:merge_with_os([]))),
    ok.

zipkin_exporter(_Config) ->
    ?assertMatch({traces_exporter, undefined},
                 lists:keyfind(traces_exporter, 1, otel_configuration:merge_with_os([]))),
    ok.

none_exporter(_Config) ->
    ?assertMatch({traces_exporter, undefined},
                 lists:keyfind(traces_exporter, 1, otel_configuration:merge_with_os([]))),
    ok.

%%

setup_env(Vars) ->
    [os:putenv(Name, Value) || {Name, Value} <- Vars].
