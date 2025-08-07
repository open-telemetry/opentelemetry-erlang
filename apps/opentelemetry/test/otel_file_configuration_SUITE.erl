-module(otel_file_configuration_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [parse_test, app_env_test, os_env_test].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

parse_test(Config) ->
    DataDir = ?config(data_dir, Config),
    JsonFile = filename:join(DataDir, "out.json"),

    Json = otel_file_configuration:parse_file(JsonFile),

    ?assertMatch(#{disabled := false}, Json).

app_env_test(Config) ->
    try
        DataDir = ?config(data_dir, Config),
        JsonFile = filename:join(DataDir, "out.json"),

        application:load(opentelemetry),
        application:set_env(opentelemetry, config_file, JsonFile),

        {ok, _} = application:ensure_all_started(opentelemetry)

    after
        application:unset_env(opentelemetry, config_file),
        application:stop(opentelemetry)
    end,

    ok.

os_env_test(Config) ->
    try
        DataDir = ?config(data_dir, Config),
        JsonFile = filename:join(DataDir, "out.json"),

        os:putenv("OTEL_EXPERIMENTAL_CONFIG_FILE", JsonFile),

        {ok, _} = application:ensure_all_started(opentelemetry)
    after
        os:unsetenv("OTEL_EXPERIMENTAL_CONFIG_FILE"),
        application:stop(opentelemetry)
    end,

    ok.
