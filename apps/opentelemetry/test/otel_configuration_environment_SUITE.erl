-module(otel_configuration_environment_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").
-include("otel_tracer.hrl").

all() ->
    [zero_config_starts_sdk,
     environment_configures_sdk,
     environment_exports_spans,
     otlp_signal_precedence,
     environment_can_disable_sdk,
     environment_can_disable_export_and_propagation,
     invalid_and_empty_values_use_defaults,
     explicit_application_configuration_is_authoritative,
     invalid_explicit_configuration_does_not_fall_back].

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(opentelemetry_exporter),
    [{started_apps, Started} | Config].

end_per_suite(Config) ->
    _ = application:unload(opentelemetry),
    [application:stop(App) || App <- lists:reverse(proplists:get_value(started_apps, Config))],
    ok.

init_per_testcase(_Case, Config) ->
    _ = application:stop(opentelemetry),
    _ = application:load(opentelemetry),
    Saved = otel_environment(),
    [os:unsetenv(Name) || {Name, _} <- Saved],
    AppEnv = application:get_all_env(opentelemetry),
    [application:unset_env(opentelemetry, Key) || {Key, _} <- AppEnv],
    [{saved_environment, Saved}, {saved_app_env, AppEnv} | Config].

end_per_testcase(_Case, Config) ->
    _ = application:stop(opentelemetry),
    [os:unsetenv(Name) || {Name, _} <- otel_environment()],
    [os:putenv(Name, Value) || {Name, Value} <- proplists:get_value(saved_environment, Config)],
    [application:unset_env(opentelemetry, Key) || {Key, _} <- application:get_all_env(opentelemetry)],
    [application:set_env(opentelemetry, Key, Value)
     || {Key, Value} <- proplists:get_value(saved_app_env, Config)],
    ok.

otel_environment() ->
    [{Name, Value} || Entry <- os:getenv(), lists:prefix("OTEL_", Entry),
                      [Name, Value] <- [string:split(Entry, "=", leading)]].

zero_config_starts_sdk(_Config) ->
    {ok, Runtime} = otel_configuration_source:resolve([]),
    ?assertEqual(environment, otel_configuration_model:source(otel_configuration_sdk:source(Runtime))),
    ?assertEqual([trace_context, baggage], otel_configuration_sdk:text_map_propagators(Runtime)),
    ?assertMatch(#{sampler := {parent_based, #{root := always_on}},
                   processors := [{otel_batch_processor, #{schedule_delay := 5000,
                                                           export_timeout := 30000,
                                                           max_queue_size := 2048}}]},
                 otel_configuration_sdk:tracer_provider(Runtime)),
    ?assertMatch(#{protocol := http_protobuf,
                   endpoints := [<<"http://localhost:4318/v1/traces">>],
                   configuration_resolved := true}, exporter_options(Runtime)),
    {ok, _} = application:ensure_all_started(opentelemetry),
    ?assert(is_pid(whereis(otel_tracer_provider_global))),
    ?assertMatch([{_, Pid, worker, _}] when is_pid(Pid),
                 supervisor:which_children(otel_span_processor_sup_global)),
    ?assertEqual({otel_propagator_text_map_composite,
                  [otel_propagator_trace_context, otel_propagator_baggage]},
                 opentelemetry:get_text_map_injector()).

environment_configures_sdk(_Config) ->
    set_environment([{"OTEL_EXPORTER_OTLP_ENDPOINT", "http://collector:4318/prefix/"},
                     {"OTEL_SERVICE_NAME", "checkout"},
                     {"OTEL_SERVICE_INSTANCE", "instance-1"},
                     {"OTEL_RESOURCE_ATTRIBUTES", "service.name=lower-priority,region=ca,note=a%3Db"},
                     {"OTEL_TRACES_SAMPLER", "parentbased_traceidratio"},
                     {"OTEL_TRACES_SAMPLER_ARG", "0.25"},
                     {"OTEL_PROPAGATORS", "b3,baggage,b3"},
                     {"OTEL_BSP_SCHEDULE_DELAY_MILLIS", "42"},
                     {"OTEL_BSP_EXPORT_TIMEOUT", "123"},
                     {"OTEL_BSP_MAX_QUEUE_SIZE", "256"},
                     {"OTEL_ATTRIBUTE_COUNT_LIMIT", "64"},
                     {"OTEL_SPAN_EVENT_COUNT_LIMIT", "12"}]),
    {ok, Runtime} = otel_configuration_source:resolve([]),
    ?assertMatch(#{sampler := {parent_based, #{root := {trace_id_ratio_based, 0.25}}},
                   processors := [{otel_batch_processor, #{schedule_delay := 42,
                                                           export_timeout := 123,
                                                           max_queue_size := 256}}]},
                 otel_configuration_sdk:tracer_provider(Runtime)),
    ?assertMatch(#{endpoints := [<<"http://collector:4318/prefix/v1/traces">>]}, exporter_options(Runtime)),
    os:putenv("OTEL_BSP_SCHEDULE_DELAY", "43"),
    {ok, Modern} = otel_configuration_source:resolve([]),
    ?assertMatch(#{processors := [{otel_batch_processor, #{schedule_delay := 43}}]},
                 otel_configuration_sdk:tracer_provider(Modern)),
    ?assertMatch(#{attribute_count_limit := 64, event_count_limit := 12},
                 otel_configuration_sdk:span_limits(Runtime)),
    {ok, _} = application:ensure_all_started(opentelemetry),
    ?assertMatch(#{'service.name' := <<"checkout">>, 'service.instance.id' := <<"instance-1">>,
                   region := <<"ca">>, note := <<"a=b">>},
                 resource_attributes(otel_tracer_provider:resource())),
    ?assertEqual({otel_propagator_text_map_composite,
                  [{otel_propagator_b3, b3single}, otel_propagator_baggage]},
                 opentelemetry:get_text_map_injector()),
    {otel_tracer_default, #tracer{sampler=Sampler}} = opentelemetry:get_tracer(),
    ?assertNotEqual(nomatch, binary:match(otel_sampler:description(Sampler), <<"0.25">>)).

environment_exports_spans(_Config) ->
    set_environment([{"OTEL_EXPORTER_OTLP_ENDPOINT", "http://collector:4318/prefix"},
                     {"OTEL_EXPORTER_OTLP_HEADERS", "test-header=environment"},
                     {"OTEL_BSP_SCHEDULE_DELAY", "60000"},
                     {"OTEL_SERVICE_NAME", "environment-export-test"}]),
    Parent = self(),
    ok = meck:new(httpc, [passthrough]),
    ok = meck:expect(httpc, request,
                     fun(post, {Address, Headers, "application/x-protobuf", Body}, _, _, _) ->
                             Parent ! {export_request, Address, Headers, Body},
                             {ok, {{"1.1", 200, ""}, [], <<>>}}
                     end),
    try
        {ok, _} = application:ensure_all_started(opentelemetry),
        Span = otel_tracer:start_span(opentelemetry:get_tracer(), <<"environment-span">>, #{}),
        otel_span:end_span(Span),
        otel_tracer_provider:force_flush(),
        receive
            {export_request, Address, Headers, Body} ->
                ?assertEqual("http://collector:4318/prefix/v1/traces", Address),
                ?assert(lists:member({"test-header", "environment"}, Headers)),
                ?assert(byte_size(Body) > 0)
        after 5000 -> error(no_environment_export)
        end,
        ?assert(meck:validate(httpc))
    after
        application:stop(opentelemetry),
        meck:unload(httpc)
    end.

otlp_signal_precedence(_Config) ->
    set_environment([{"OTEL_EXPORTER_OTLP_ENDPOINT", "http://general:4318/base"},
                     {"OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", "https://traces:4318/exact"},
                     {"OTEL_EXPORTER_OTLP_PROTOCOL", "grpc"},
                     {"OTEL_EXPORTER_OTLP_TRACES_PROTOCOL", "http/protobuf"},
                     {"OTEL_EXPORTER_OTLP_HEADERS", "generic=discard"},
                     {"OTEL_EXPORTER_OTLP_TRACES_HEADERS", "trace=kept%3Dvalue"},
                     {"OTEL_EXPORTER_OTLP_COMPRESSION", "none"},
                     {"OTEL_EXPORTER_OTLP_TRACES_COMPRESSION", "gzip"}]),
    {ok, Http} = otel_configuration_source:resolve([]),
    ?assertMatch(#{endpoints := [<<"https://traces:4318/exact">>], protocol := http_protobuf,
                   headers := [{<<"trace">>, <<"kept=value">>}], compression := gzip},
                 exporter_options(Http)),
    os:unsetenv("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"),
    %% Invalid signal-specific values are ignored, allowing the general value.
    os:putenv("OTEL_EXPORTER_OTLP_TRACES_PROTOCOL", "unsupported"),
    {ok, Grpc} = otel_configuration_source:resolve([]),
    ?assertMatch(#{endpoints := [<<"http://general:4318/base">>], protocol := grpc},
                 exporter_options(Grpc)),
    %% An already resolved exporter never re-reads environment configuration.
    os:putenv("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", "http://changed:4318/ignored"),
    ?assertMatch(#{endpoints := [<<"https://traces:4318/exact">>]},
                 otel_exporter_traces_otlp:merge_with_environment(exporter_options(Http))).

environment_can_disable_sdk(_Config) ->
    os:putenv("OTEL_SDK_DISABLED", "TRUE"),
    {ok, Runtime} = otel_configuration_source:resolve([]),
    ?assert(otel_configuration_sdk:disabled(Runtime)),
    {ok, _} = application:ensure_all_started(opentelemetry),
    ?assertEqual(undefined, whereis(otel_tracer_provider_global)).

environment_can_disable_export_and_propagation(_Config) ->
    set_environment([{"OTEL_TRACES_EXPORTER", "none"}, {"OTEL_PROPAGATORS", "none"}]),
    {ok, Runtime} = otel_configuration_source:resolve([]),
    ?assertMatch(#{processors := [{otel_batch_processor, #{exporter := none}}]},
                 otel_configuration_sdk:tracer_provider(Runtime)),
    ?assertEqual([], otel_configuration_sdk:text_map_propagators(Runtime)).

invalid_and_empty_values_use_defaults(_Config) ->
    set_environment([{"OTEL_CONFIG_FILE", ""}, {"OTEL_SDK_DISABLED", ""},
                     {"OTEL_TRACES_SAMPLER", "traceidratio"}, {"OTEL_TRACES_SAMPLER_ARG", "2"},
                     {"OTEL_BSP_MAX_QUEUE_SIZE", "-1"}, {"OTEL_BSP_EXPORT_TIMEOUT", "garbage"},
                     {"OTEL_EXPORTER_OTLP_ENDPOINT", "not a URL"},
                     {"OTEL_EXPORTER_OTLP_PROTOCOL", "unsupported"},
                     {"OTEL_PROPAGATORS", ""}]),
    {ok, Runtime} = otel_configuration_source:resolve([]),
    ?assertNot(otel_configuration_sdk:disabled(Runtime)),
    ?assertEqual([trace_context, baggage], otel_configuration_sdk:text_map_propagators(Runtime)),
    ?assertMatch(#{sampler := {trace_id_ratio_based, 1.0},
                   processors := [{otel_batch_processor, #{max_queue_size := 2048,
                                                           export_timeout := 30000}}]},
                 otel_configuration_sdk:tracer_provider(Runtime)),
    ?assertMatch(#{endpoints := [<<"http://localhost:4318/v1/traces">>]}, exporter_options(Runtime)).

explicit_application_configuration_is_authoritative(_Config) ->
    set_environment([{"OTEL_SDK_DISABLED", "true"}, {"OTEL_SERVICE_NAME", "ignored"},
                     {"OTEL_PROPAGATORS", "b3"}, {"OTEL_TRACES_SAMPLER", "always_off"},
                     {"OTEL_EXPORTER_OTLP_ENDPOINT", "http://ignored:4318"}]),
    AppEnv = [{tracer_provider, #{processors => [], sampler => always_on}}],
    {ok, Runtime} = otel_configuration_source:resolve(AppEnv),
    ?assertEqual(application_env, otel_configuration_model:source(otel_configuration_sdk:source(Runtime))),
    ?assertNot(otel_configuration_sdk:disabled(Runtime)),
    ?assertEqual([], otel_configuration_sdk:text_map_propagators(Runtime)),
    ?assertEqual(undefined, otel_configuration_sdk:resource(Runtime)),
    ?assertMatch(#{sampler := always_on, processors := []}, otel_configuration_sdk:tracer_provider(Runtime)),
    [application:set_env(opentelemetry, Key, Value) || {Key, Value} <- AppEnv],
    {ok, _} = application:ensure_all_started(opentelemetry),
    ?assert(is_pid(whereis(otel_tracer_provider_global))),
    ?assertMatch(#{'service.name' := <<"unknown_service:erl">>},
                 resource_attributes(otel_tracer_provider:resource())),
    %% A partial explicit configuration also keeps declarative no-op defaults.
    {ok, Partial} = otel_configuration_source:resolve([{resource, #{attributes => #{}}}]),
    ?assertEqual(undefined, otel_configuration_sdk:tracer_provider(Partial)).

invalid_explicit_configuration_does_not_fall_back(_Config) ->
    ?assertMatch({error, {application_configuration_error, _}},
                 otel_configuration_source:resolve([{processors, []}])),
    os:putenv("OTEL_CONFIG_FILE", "/nonexistent/otel-sdk.json"),
    ?assertMatch({error, {configuration_file_error, _, _}},
                 otel_configuration_source:resolve([])).

set_environment(Values) ->
    [os:putenv(Name, Value) || {Name, Value} <- Values],
    ok.

exporter_options(#{tracer_provider := #{processors := [{otel_batch_processor,
                                                       #{exporter := {opentelemetry_exporter, Options}}}]}}) ->
    Options.

resource_attributes(Resource) ->
    case otel_resource:attributes(Resource) of
        undefined -> error(missing_resource);
        Attributes -> otel_attributes:map(Attributes)
    end.
