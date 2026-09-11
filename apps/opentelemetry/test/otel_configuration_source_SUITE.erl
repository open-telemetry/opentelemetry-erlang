-module(otel_configuration_source_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [parses_json_without_atomizing_keys,
     rejects_invalid_json,
     selects_one_configuration_source,
     empty_file_environment_is_unset,
     application_startup_uses_shared_declarative_configuration,
     omitted_resource_ignores_legacy_detection,
     exports_declarative_metrics].

init_per_suite(Config) ->
    case code:ensure_loaded(json) of
        {module, json} -> Config;
        {error, _} -> {skip, "declarative JSON files require the OTP json module"}
    end.

end_per_suite(_Config) ->
    _ = application:unload(opentelemetry_experimental),
    _ = application:unload(opentelemetry),
    ok.

init_per_testcase(_TestCase, Config) ->
    _ = application:stop(opentelemetry_experimental),
    _ = application:stop(opentelemetry),
    SavedOSEnv = [{Name, os:getenv(Name)} || Name <- environment_names()],
    [os:unsetenv(Name) || Name <- environment_names()],
    SavedSDKEnv = application:get_all_env(opentelemetry),
    SavedExperimentalEnv = application:get_all_env(opentelemetry_experimental),
    clear_application_env(opentelemetry),
    clear_application_env(opentelemetry_experimental),
    [{saved_os_env, SavedOSEnv},
     {saved_sdk_env, SavedSDKEnv},
     {saved_experimental_env, SavedExperimentalEnv} | Config].

end_per_testcase(_TestCase, Config) ->
    _ = application:stop(opentelemetry_experimental),
    _ = application:stop(opentelemetry),
    [os:unsetenv(Name) || Name <- environment_names()],
    [restore_os_env(Name, Value)
     || {Name, Value} <- proplists:get_value(saved_os_env, Config)],
    restore_application_env(opentelemetry,
                            proplists:get_value(saved_sdk_env, Config)),
    restore_application_env(opentelemetry_experimental,
                            proplists:get_value(saved_experimental_env, Config)),
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

selects_one_configuration_source(Config) ->
    PrivDir = ?config(priv_dir, Config),
    StableFile = write_configuration(PrivDir, "stable.json", "debug"),
    ExperimentalFile = write_configuration(PrivDir, "experimental.json", "warn"),
    ApplicationFile = write_configuration(PrivDir, "application.json", "error"),
    AppEnv = [{config_file, ApplicationFile}, {log_level, fatal}],

    os:putenv("OTEL_CONFIG_FILE", StableFile),
    os:putenv("OTEL_EXPERIMENTAL_CONFIG_FILE", ExperimentalFile),
    os:putenv("OTEL_LOG_LEVEL", "fatal4"),
    {ok, Stable} = otel_configuration_source:resolve(AppEnv),
    ?assertMatch(#{configuration_source := declarative, log_level := debug}, Stable),

    os:unsetenv("OTEL_CONFIG_FILE"),
    {ok, Experimental} = otel_configuration_source:resolve(AppEnv),
    ?assertMatch(#{configuration_source := declarative, log_level := warn}, Experimental),

    os:unsetenv("OTEL_EXPERIMENTAL_CONFIG_FILE"),
    {ok, Application} = otel_configuration_source:resolve(AppEnv),
    ?assertMatch(#{configuration_source := declarative, log_level := error}, Application),

    {ok, Legacy} = otel_configuration_source:resolve(
                     [{log_level, debug}]),
    ?assertMatch(#{configuration_source := legacy, log_level := fatal4}, Legacy).

empty_file_environment_is_unset(Config) ->
    PrivDir = ?config(priv_dir, Config),
    ExperimentalFile = write_configuration(PrivDir, "empty-experimental.json", "warn"),
    ApplicationFile = write_configuration(PrivDir, "empty-application.json", "error"),
    os:putenv("OTEL_CONFIG_FILE", ""),
    os:putenv("OTEL_EXPERIMENTAL_CONFIG_FILE", ExperimentalFile),
    {ok, Experimental} = otel_configuration_source:resolve(
                           [{config_file, ApplicationFile}]),
    ?assertMatch(#{configuration_source := declarative, log_level := warn}, Experimental),

    os:putenv("OTEL_EXPERIMENTAL_CONFIG_FILE", ""),
    {ok, Application} = otel_configuration_source:resolve(
                          [{config_file, ApplicationFile}]),
    ?assertMatch(#{configuration_source := declarative, log_level := error}, Application),

    os:putenv("OTEL_LOG_LEVEL", "debug"),
    {ok, Legacy} = otel_configuration_source:resolve([]),
    ?assertMatch(#{configuration_source := legacy, log_level := debug}, Legacy).

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
    application:set_env(opentelemetry_experimental, metrics_exporter, none),

    {ok, _} = application:ensure_all_started(opentelemetry_experimental),

    Runtime = otel_configuration:runtime(),
    ?assertMatch(#{configuration_source := declarative,
                   traces_enabled := false,
                   metrics_enabled := true,
                   log_level := info,
                   text_map_propagators := []},
                 Runtime),
    ?assertEqual(undefined, whereis(otel_tracer_provider_global)),
    ?assert(is_pid(whereis(otel_meter_provider_global))),
    Resource = otel_meter_provider:resource(),
    ?assertEqual(otel_resource_detector:get_resource(), Resource),
    Attributes = resource_attributes_map(Resource),
    ?assertMatch(#{'service.name' := <<"declarative">>,
                   'service.instance.id' := <<"file-instance">>,
                   'process.runtime.name' := <<"configured-runtime">>,
                   'telemetry.sdk.language' := <<"erlang">>}, Attributes),
    ?assertNot(maps:is_key('env.only', Attributes)),
    ?assertNot(maps:is_key('app.only', Attributes)),

    ok = application:stop(opentelemetry_experimental),
    ok = application:stop(opentelemetry),
    ?assertEqual(undefined, otel_configuration:runtime()).

omitted_resource_ignores_legacy_detection(Config) ->
    File = write_configuration(?config(priv_dir, Config), "no-resource.json", "info"),
    os:putenv("OTEL_CONFIG_FILE", File),
    os:putenv("OTEL_SERVICE_NAME", "environment-service"),
    os:putenv("OTEL_SERVICE_INSTANCE", "environment-instance"),
    os:putenv("OTEL_RESOURCE_ATTRIBUTES", "env.only=unwanted"),
    application:set_env(opentelemetry, resource, #{app => #{only => <<"unwanted">>}}),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Attributes = resource_attributes_map(otel_resource_detector:get_resource()),
    ?assertMatch(#{'service.name' := <<"unknown_service">>,
                   'telemetry.sdk.name' := <<"opentelemetry">>,
                   'telemetry.sdk.language' := <<"erlang">>,
                   'telemetry.sdk.version' := _}, Attributes),
    ?assertEqual(4, map_size(Attributes)).

exports_declarative_metrics(Config) ->
    File = filename:join(?config(priv_dir, Config), "metrics.json"),
    ok = file:write_file(File, <<
        "{\"file_format\":\"1.1\","
        "\"resource\":{\"attributes\":["
        "{\"name\":\"ints\",\"type\":\"int_array\",\"value\":[65,66]}]},"
        "\"meter_provider\":{\"readers\":[{\"periodic\":{\"interval\":60000,"
        "\"exporter\":{\"otlp_http\":{\"endpoint\":\"http://localhost:4318/v1/metrics\"}}}}]}}">>),
    os:putenv("OTEL_CONFIG_FILE", File),
    {ok, StartedApps} = application:ensure_all_started(opentelemetry_exporter),
    TestPid = self(),
    meck:new(httpc, [passthrough]),
    meck:expect(httpc, request,
        fun(post, {Address, _, "application/x-protobuf", Body}, _, _, _) ->
            Decoded = opentelemetry_exporter_metrics_service_pb:decode_msg(
                        Body, export_metrics_service_request),
            TestPid ! {exported_metrics, Address, Decoded},
            {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}}
        end),
    try
        {ok, _} = application:ensure_all_started(opentelemetry_experimental),
        Meter = opentelemetry_experimental:get_meter(),
        _ = otel_meter:create_counter(Meter, declarative_requests, #{}),
        ok = otel_counter:add(otel_ctx:new(), Meter, declarative_requests, 7, #{}),
        ok = otel_meter_server:force_flush(),
        receive
            {exported_metrics, Address, #{resource_metrics := [ResourceMetrics]}} ->
                ?assertEqual("http://localhost:4318/v1/metrics", Address),
                ?assertMatch(#{scope_metrics := [#{metrics :=
                    [#{name := <<"declarative_requests">>,
                       data := {sum, #{data_points := [#{value := {as_int, 7}}]}}}]}]},
                    ResourceMetrics),
                #{resource := #{attributes := Attributes}} = ResourceMetrics,
                ?assert(lists:member(#{key => <<"ints">>, value =>
                    #{value => {array_value, #{values =>
                        [#{value => {int_value, 65}}, #{value => {int_value, 66}}]}}}},
                    Attributes))
        after 5000 ->
            ct:fail(no_metric_export)
        end,
        ?assert(meck:validate(httpc))
    after
        application:stop(opentelemetry_experimental),
        application:stop(opentelemetry),
        meck:unload(httpc),
        [application:stop(App) || App <- lists:reverse(StartedApps)]
    end.

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
