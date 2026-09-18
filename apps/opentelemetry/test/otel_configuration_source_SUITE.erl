-module(otel_configuration_source_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [parses_json_without_atomizing_keys,
     rejects_invalid_json,
     file_configuration_takes_precedence,
     empty_file_environment_uses_application_configuration,
     application_environment_matches_json,
     rejects_legacy_application_environment,
     application_startup_uses_shared_declarative_configuration,
     omitted_resource_ignores_legacy_detection].

init_per_suite(Config) ->
    case code:ensure_loaded(json) of
        {module, json} -> Config;
        {error, _} -> {skip, "declarative JSON files require the OTP json module"}
    end.

end_per_suite(_Config) ->
    _ = application:unload(opentelemetry),
    ok.

init_per_testcase(_TestCase, Config) ->
    _ = application:stop(opentelemetry),
    SavedOSEnv = [{Name, os:getenv(Name)} || Name <- environment_names()],
    [os:unsetenv(Name) || Name <- environment_names()],
    SavedSDKEnv = application:get_all_env(opentelemetry),
    clear_application_env(opentelemetry),
    [{saved_os_env, SavedOSEnv},
     {saved_sdk_env, SavedSDKEnv} | Config].

end_per_testcase(_TestCase, Config) ->
    _ = application:stop(opentelemetry),
    [os:unsetenv(Name) || Name <- environment_names()],
    [restore_os_env(Name, Value)
     || {Name, Value} <- proplists:get_value(saved_os_env, Config)],
    restore_application_env(opentelemetry,
                            proplists:get_value(saved_sdk_env, Config)),
    ok.

parses_json_without_atomizing_keys(_Config) ->
    Unknown = <<"configuration_source_unknown_",
                (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    JSON = <<"{\"file_format\":\"1.1\",\"", Unknown/binary, "\":true}">>,
    ?assertException(error, badarg, binary_to_existing_atom(Unknown, utf8)),

    {ok, Parsed} = otel_configuration_source:parse_binary(JSON),

    ?assertMatch(#{<<"file_format">> := <<"1.1">>, Unknown := true}, Parsed),
    ?assertException(error, badarg, binary_to_existing_atom(Unknown, utf8)).

rejects_invalid_json(Config) ->
    ?assertMatch({error, {configuration_decode_error, _}},
                 otel_configuration_source:parse_binary(<<"{invalid}">>)),
    ?assertEqual({error, {configuration_root_error, []}},
                 otel_configuration_source:parse_binary(<<"[]">>)),

    Missing = filename:join(?config(priv_dir, Config), "missing.json"),
    ?assertEqual({error, {configuration_file_error, Missing, {read_error, enoent}}},
                 otel_configuration_source:parse_file(Missing)).

file_configuration_takes_precedence(Config) ->
    PrivDir = ?config(priv_dir, Config),
    StableFile = write_configuration(PrivDir, "stable.json", "debug"),
    AppEnv = [{log_level, fatal}],

    os:putenv("OTEL_CONFIG_FILE", StableFile),
    os:putenv("OTEL_LOG_LEVEL", "fatal4"),
    {ok, Stable} = otel_configuration_source:resolve(AppEnv),
    ?assertEqual(debug,
                 otel_configuration_sdk:value(log_level, Stable, undefined)),

    os:unsetenv("OTEL_CONFIG_FILE"),
    {ok, Application} = otel_configuration_source:resolve(AppEnv),
    ?assertEqual(fatal,
                 otel_configuration_sdk:value(log_level, Application, undefined)).

empty_file_environment_uses_application_configuration(_Config) ->
    os:putenv("OTEL_CONFIG_FILE", ""),
    {ok, Application} = otel_configuration_source:resolve(
                          [{log_level, error}]),
    ?assertEqual(error,
                 otel_configuration_sdk:value(log_level, Application, undefined)),

    os:putenv("OTEL_EXPERIMENTAL_CONFIG_FILE", "/not/read.json"),
    os:putenv("OTEL_LOG_LEVEL", "debug"),
    {ok, StillApplication} = otel_configuration_source:resolve([{log_level, info}]),
    ?assertEqual(info,
                 otel_configuration_sdk:value(log_level,
                                              StillApplication,
                                              undefined)).

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
                [{otel_simple_processor, #{exporter => {otlp_http, #{}}}}],
            sampler => always_off}}],

    {ok, ParsedJson} = otel_configuration_declarative:parse(JsonConfiguration),
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
    Attributes = resource_attributes_map(Resource),
    ?assertMatch(#{'service.name' := <<"declarative">>,
                   'service.instance.id' := <<"file-instance">>,
                   'process.runtime.name' := <<"configured-runtime">>,
                   'telemetry.sdk.language' := <<"erlang">>}, Attributes),
    ?assertNot(maps:is_key('env.only', Attributes)),
    ?assertNot(maps:is_key('app.only', Attributes)),

    ok = application:stop(opentelemetry).

omitted_resource_ignores_legacy_detection(Config) ->
    File = write_configuration(?config(priv_dir, Config), "no-resource.json", "info"),
    os:putenv("OTEL_CONFIG_FILE", File),
    os:putenv("OTEL_SERVICE_NAME", "environment-service"),
    os:putenv("OTEL_SERVICE_INSTANCE", "environment-instance"),
    os:putenv("OTEL_RESOURCE_ATTRIBUTES", "env.only=unwanted"),
    application:set_env(opentelemetry, resource, #{app => #{only => <<"unwanted">>}}),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Attributes = resource_attributes_map(otel_resource_detector:get_resource()),
    ?assertMatch(#{'service.name' := <<"unknown_service:erl">>,
                   'telemetry.sdk.name' := <<"opentelemetry">>,
                   'telemetry.sdk.language' := <<"erlang">>,
                   'telemetry.sdk.version' := _}, Attributes),
    ?assertNot(maps:is_key('env.only', Attributes)),
    ?assertNotEqual(<<"environment-instance">>,
                    maps:get('service.instance.id', Attributes)).

resource_attributes_map(Resource) ->
    case otel_resource:attributes(Resource) of
        undefined -> error(missing_resource);
        Attributes -> otel_attributes:map(Attributes)
    end.

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

restore_os_env(Name, false) ->
    os:unsetenv(Name);
restore_os_env(Name, Value) ->
    os:putenv(Name, Value).

environment_names() ->
    ["OTEL_CONFIG_FILE",
     "OTEL_EXPERIMENTAL_CONFIG_FILE",
     "OTEL_LOG_LEVEL",
     "OTEL_SERVICE_NAME",
     "OTEL_SERVICE_INSTANCE",
     "OTEL_RESOURCE_ATTRIBUTES",
     "OTEL_PROPAGATORS"].
