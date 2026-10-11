-module(otel_configuration_source_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

-import(otel_configuration_test_utils, [resource_attributes/1]).
-include_lib("common_test/include/ct.hrl").

all() ->
    [parses_json_with_binary_keys,
     rejects_invalid_json,
     file_configuration_takes_precedence,
     empty_file_environment_uses_application_configuration,
     application_environment_matches_json,
     rejects_legacy_application_environment,
     rejects_native_typos_before_startup,
     application_startup_uses_shared_declarative_configuration,
     validates_json_distribution_before_startup,
     upstream_reference_starts_sdk,
     omitted_resource_ignores_legacy_detection].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    _ = application:unload(opentelemetry),
    ok.

init_per_testcase(_TestCase, Config) ->
    _ = application:stop(opentelemetry),
    SavedOSEnv = otel_configuration_test_utils:save_environment(),
    SavedSDKEnv = application:get_all_env(opentelemetry),
    clear_application_env(opentelemetry),
    [{saved_os_env, SavedOSEnv},
     {saved_sdk_env, SavedSDKEnv} | Config].

end_per_testcase(_TestCase, Config) ->
    _ = application:stop(opentelemetry),
    ok = otel_configuration_test_utils:restore_environment(
           proplists:get_value(saved_os_env, Config)),
    restore_application_env(opentelemetry,
                            proplists:get_value(saved_sdk_env, Config)),
    ok.

parses_json_with_binary_keys(_Config) ->
    Unknown = <<"configuration_source_unknown_",
                (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    JSON = <<"{\"file_format\":\"1.1\",\"", Unknown/binary, "\":true}">>,

    {ok, Parsed} = otel_configuration_source:parse_binary(JSON),

    ?assertMatch(#{<<"file_format">> := <<"1.1">>, Unknown := true}, Parsed).

rejects_invalid_json(Config) ->
    ?assertMatch({error, {configuration_decode_error, _}},
                 otel_configuration_source:parse_binary(<<"{invalid}">>)),
    ?assertEqual({error, {configuration_root_error, []}},
                 otel_configuration_source:parse_binary(<<"[]">>)),

    Missing = filename:join(?config(priv_dir, Config), "missing.json"),
    ?assertEqual({error, {configuration_file_error, Missing, {read_error, enoent}}},
                 otel_configuration_source:parse_file(Missing)),
    Invalid = filename:join(?config(priv_dir, Config), "invalid.json"),
    ok = file:write_file(Invalid, <<"{invalid}">>),
    ?assertMatch({error, {configuration_file_error, Invalid, {configuration_decode_error, _}}},
                 otel_configuration_source:parse_file(Invalid)).

file_configuration_takes_precedence(Config) ->
    PrivDir = ?config(priv_dir, Config),
    StableFile = write_configuration(PrivDir, "stable.json", "debug"),
    AppEnv = [{log_level, fatal}],

    os:putenv("OTEL_CONFIG_FILE", StableFile),
    os:putenv("OTEL_LOG_LEVEL", "fatal4"),
    {ok, Stable} = otel_configuration_source:resolve(AppEnv),
    ?assertEqual(<<"debug">>,
                 maps:get(<<"log_level">>, otel_configuration_model:root(
                                           otel_configuration_sdk:source(Stable)))),

    os:unsetenv("OTEL_CONFIG_FILE"),
    {ok, Application} = otel_configuration_source:resolve(AppEnv),
    ?assertEqual(fatal,
                 maps:get(log_level, otel_configuration_model:root(
                                       otel_configuration_sdk:source(Application)))).

empty_file_environment_uses_application_configuration(_Config) ->
    os:putenv("OTEL_CONFIG_FILE", ""),
    {ok, Application} = otel_configuration_source:resolve(
                          [{log_level, error}]),
    ?assertEqual(error,
                 maps:get(log_level, otel_configuration_model:root(
                                       otel_configuration_sdk:source(Application)))),

    os:putenv("OTEL_EXPERIMENTAL_CONFIG_FILE", "/not/read.json"),
    os:putenv("OTEL_LOG_LEVEL", "debug"),
    {ok, StillApplication} = otel_configuration_source:resolve([{log_level, info}]),
    ?assertEqual(info,
                 maps:get(log_level, otel_configuration_model:root(
                                       otel_configuration_sdk:source(StillApplication)))).

application_environment_matches_json(_Config) ->
    JsonConfiguration =
        #{<<"file_format">> => <<"1.1">>,
          <<"log_level">> => <<"debug">>,
          <<"propagator">> =>
              #{<<"composite">> => [#{<<"tracecontext">> => null},
                                      #{<<"baggage">> => null}]},
          <<"tracer_provider">> =>
              #{<<"processors">> =>
                    [#{<<"simple">> =>
                           #{<<"exporter">> => #{<<"otlp_http">> => null}}}],
                <<"sampler">> => #{<<"always_off">> => null}}},
    ApplicationEnvironment =
        [{log_level, debug},
         {propagator,
          #{composite => [trace_context, baggage]}},
         {tracer_provider,
          #{processors =>
                [{simple, #{exporter => {otlp_http, #{}}}}],
            sampler => always_off}}],

    {ok, ParsedJson} = otel_configuration_model:from_map(JsonConfiguration),
    {ok, ParsedApplication} =
        otel_configuration_model:from_application_env(ApplicationEnvironment),

    {ok, JsonRuntime} = otel_configuration_sdk:create(ParsedJson),
    {ok, ApplicationRuntime} = otel_configuration_sdk:create(ParsedApplication),
    ?assertEqual(maps:remove(source, JsonRuntime),
                 maps:remove(source, ApplicationRuntime)),
    ?assertEqual(otel_configuration_sdk:text_map_propagators(JsonRuntime),
                 otel_configuration_sdk:text_map_propagators(ApplicationRuntime)),
    JsonTracerProvider = #{} = otel_configuration_sdk:tracer_provider(JsonRuntime),
    ApplicationTracerProvider = #{} =
        otel_configuration_sdk:tracer_provider(ApplicationRuntime),
    ?assertEqual(otel_configuration_sdk:sampler(JsonTracerProvider),
                 otel_configuration_sdk:sampler(ApplicationTracerProvider)),
    [JsonProcessor] = otel_configuration_sdk:span_processors(JsonTracerProvider),
    [ApplicationProcessor] =
        otel_configuration_sdk:span_processors(ApplicationTracerProvider),
    ?assertEqual(element(1,
                         otel_configuration_sdk:span_processor_component(JsonProcessor)),
                 element(1,
                         otel_configuration_sdk:span_processor_component(
                           ApplicationProcessor))).

rejects_legacy_application_environment(_Config) ->
    ?assertEqual(
       {error,
        {application_configuration_error,
         {invalid_configuration, [processors], legacy_configuration_not_supported}}},
       otel_configuration_source:load(
         [{processors, [{otel_batch_processor, #{}}]}])).

rejects_native_typos_before_startup(_Config) ->
    lists:foreach(
      fun({AppEnv, Reason}) ->
              clear_application_env(opentelemetry),
              [application:set_env(opentelemetry, Key, Value) || {Key, Value} <- AppEnv],
              ?assertEqual({error, Reason}, otel_configuration_source:resolve(AppEnv)),
              ?assertEqual({error, {configuration_error, Reason}},
                           opentelemetry_app:start(normal, [])),
              ?assertEqual(undefined, whereis(opentelemetry_sup)),
              ?assertEqual(undefined, whereis(otel_tracer_provider_global))
      end,
      [{[{tracer_providers, #{}}],
        {invalid_configuration, [tracer_providers], unknown_property}},
       {[{resource, #{<<"service.name">> => <<"x">>}}],
        {invalid_configuration, [resource], legacy_configuration_not_supported}},
       {[{tracer_provider, #{processors => [{batch, #{exporter =>
                      {otlp_http, #{endpoints => [<<"https://collector/v1/traces">>]}}}}]}}],
        {invalid_configuration, [exporter, otlp_http, endpoints], unknown_property}}]).

application_startup_uses_shared_declarative_configuration(Config) ->
    PrivDir = ?config(priv_dir, Config),
    File = filename:join(PrivDir, "startup.json"),
    JSON = <<"{"
             "\"file_format\":\"1.1\","
             "\"resource\":{\"attributes\":[{\"name\":\"service.name\","
             "\"value\":\"declarative\"},"
             "{\"name\":\"service.instance.id\",\"value\":\"file-instance\"},"
             "{\"name\":\"process.runtime.name\",\"value\":\"configured-runtime\"}]},"
             "\"propagator\":{\"composite\":[]},"
             "\"meter_provider\":{\"readers\":[]}"
             "}">>,
    ok = file:write_file(File, JSON),
    os:putenv("OTEL_CONFIG_FILE", File),
    os:putenv("OTEL_PROPAGATORS", "b3"),
    os:putenv("OTEL_SERVICE_NAME", "environment-service"),
    os:putenv("OTEL_SERVICE_INSTANCE", "environment-instance"),
    os:putenv("OTEL_RESOURCE_ATTRIBUTES", "env.only=unwanted"),
    application:set_env(opentelemetry, log_level, fatal4),
    application:set_env(opentelemetry, resource, #{app => #{only => <<"unwanted">>}}),
    {ok, _} = application:ensure_all_started(opentelemetry),

    ?assertEqual(undefined, whereis(otel_tracer_provider_global)),
    Resource = otel_resource_detector:get_resource(),
    Attributes = resource_attributes(Resource),
    ?assertMatch(#{'service.name' := <<"declarative">>,
                   'service.instance.id' := <<"file-instance">>,
                   'process.runtime.name' := <<"configured-runtime">>,
                   'telemetry.sdk.language' := <<"erlang">>}, Attributes),
    ?assertNot(maps:is_key('env.only', Attributes)),
    ?assertNot(maps:is_key('app.only', Attributes)),

    ok = application:stop(opentelemetry).

validates_json_distribution_before_startup(Config) ->
    File = filename:join(?config(priv_dir, Config), "distribution.json"),
    os:putenv("OTEL_CONFIG_FILE", File),
    lists:foreach(
      fun({Fields, Reason}) ->
              ok = file:write_file(File,
                     ["{\"file_format\":\"1.1\",\"tracer_provider\":{\"processors\":[]},",
                      "\"distribution\":{\"erlang\":{", Fields, "}}}"]),
              ?assertEqual({error, Reason}, otel_configuration_source:resolve([])),
              ?assertEqual({error, {configuration_error, Reason}},
                           opentelemetry_app:start(normal, [])),
              ?assertEqual(undefined, whereis(opentelemetry_sup)),
              ?assertEqual(undefined, whereis(otel_resource_detector))
      end,
      [{"\"create_application_tracers\":\"false\"",
        {invalid_configuration, [distribution, erlang, create_application_tracers], <<"false">>}},
       {"\"resource_detectors\":[\"otel_resource_env_var\"]",
        {unsupported_configuration, [distribution, erlang, resource_detectors],
         [<<"otel_resource_env_var">>]}},
       {"\"resource_detector_timeout\":\"5000\"",
        {invalid_configuration, [distribution, erlang, resource_detector_timeout], <<"5000">>}}]),
    %% Real JSON booleans and numbers are safe to pass to the startup consumers.
    ok = file:write_file(File,
           <<"{\"file_format\":\"1.1\",\"tracer_provider\":{\"processors\":[]},"
             "\"distribution\":{\"erlang\":{\"create_application_tracers\":false,"
             "\"resource_detector_timeout\":0,\"resource_detectors\":[],\"deny_list\":[]}}}">>),
    {ok, _} = application:ensure_all_started(opentelemetry),
    ?assert(is_pid(whereis(otel_tracer_provider_global))).

upstream_reference_starts_sdk(Config) ->
    File = filename:join(?config(data_dir, Config), "otel-sdk-config.json"),
    {ok, Source} = otel_configuration_source:parse_file(File),
    ?assert(maps:is_key(<<"logger_provider">>, Source)),
    ?assert(maps:is_key(<<"meter_provider">>, Source)),
    os:putenv("OTEL_CONFIG_FILE", File),
    {ok, _} = application:ensure_all_started(opentelemetry),
    ?assert(is_pid(whereis(otel_tracer_provider_global))),
    ?assertMatch([{_, Pid, worker, _}] when is_pid(Pid),
                 supervisor:which_children(otel_span_processor_sup_global)),
    Attributes = resource_attributes(otel_tracer_provider:resource()),
    ?assertMatch(#{'service.name' := <<"unknown_service">>}, Attributes).

omitted_resource_ignores_legacy_detection(Config) ->
    File = write_configuration(?config(priv_dir, Config), "no-resource.json", "info"),
    os:putenv("OTEL_CONFIG_FILE", File),
    os:putenv("OTEL_SERVICE_NAME", "environment-service"),
    os:putenv("OTEL_SERVICE_INSTANCE", "environment-instance"),
    os:putenv("OTEL_RESOURCE_ATTRIBUTES", "env.only=unwanted"),
    application:set_env(opentelemetry, resource, #{app => #{only => <<"unwanted">>}}),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Attributes = resource_attributes(otel_resource_detector:get_resource()),
    ?assertMatch(#{'service.name' := <<"unknown_service:erl">>,
                   'telemetry.sdk.name' := <<"opentelemetry">>,
                   'telemetry.sdk.language' := <<"erlang">>,
                   'telemetry.sdk.version' := _}, Attributes),
    ?assertNot(maps:is_key('env.only', Attributes)),
    ?assertNotEqual(<<"environment-instance">>,
                    maps:get('service.instance.id', Attributes)).

write_configuration(Directory, Name, LogLevel) ->
    File = filename:join(Directory, Name),
    JSON = iolist_to_binary(["{\"file_format\":\"1.1\",\"log_level\":\"",
                             LogLevel, "\"}"]),
    ok = file:write_file(File, JSON),
    File.

clear_application_env(Application) ->
    [application:unset_env(Application, Key)
     || {Key, _} <- application:get_all_env(Application)],
    ok.

restore_application_env(Application, Environment) ->
    clear_application_env(Application),
    [application:set_env(Application, Key, Value) || {Key, Value} <- Environment],
    ok.
