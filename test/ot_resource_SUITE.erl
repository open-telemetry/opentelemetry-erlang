-module(ot_resource_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_span.hrl").
-include("ot_test_utils.hrl").
-include("ot_tracer.hrl").

all() ->
    [os_env_resource].

init_per_suite(Config) ->
    os:putenv("OTEL_RESOURCE_LABELS", "service.name=cttest,service.version=1.1.1"),

    application:load(opentelemetry),
    application:set_env(opentelemetry, processors, [{ot_batch_processor, [{scheduled_delay_ms, 1}]}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config.

end_per_suite(_Config) ->
    _ = application:stop(opentelemetry).

init_per_testcase(_, Config) ->
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    ot_batch_processor:set_exporter(ot_exporter_tab, Tid),
    [{tid, Tid} | Config].

os_env_resource(_Config) ->
    {_, Tracer} = opentelemetry:get_tracer(),

    ?assertMatch({resource, #{"service.name" := "cttest",
                              "service.version" := "1.1.1"}}, Tracer#tracer.resource),

    ok.
