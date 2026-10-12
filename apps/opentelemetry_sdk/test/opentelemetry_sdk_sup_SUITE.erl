-module(opentelemetry_sdk_sup_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [registered_processes_belong_to_sdk,
     starts_without_opentelemetry].

registered_processes_belong_to_sdk(_Config) ->
    _ = application:load(opentelemetry_sdk),
    _ = application:load(opentelemetry),
    {ok, Registered} = application:get_key(opentelemetry_sdk, registered),
    ?assertEqual([opentelemetry_sdk_sup,
                  otel_tracer_provider_sup,
                  otel_span_sup,
                  otel_span_sweeper,
                  otel_resource_detector],
                 Registered),
    ?assertEqual({ok, {opentelemetry_sdk_app, []}},
                 application:get_key(opentelemetry_sdk, mod)),
    ?assertEqual({ok, []}, application:get_key(opentelemetry, registered)),
    _ = application:unload(opentelemetry),
    ok.

starts_without_opentelemetry(_Config) ->
    try
        _ = application:stop(opentelemetry),
        _ = application:unload(opentelemetry),
        ?assertEqual(undefined, application:get_key(opentelemetry, vsn)),

        application:set_env(opentelemetry_sdk, tracer_provider, #{processors => []}),
        {ok, _} = application:ensure_all_started(opentelemetry_sdk),
        SdkSup = whereis(opentelemetry_sdk_sup),

        Resource = otel_resource_detector:get_resource(),
        Attributes = otel_attributes:map(otel_resource:attributes(Resource)),
        {ok, SdkVsn} = application:get_key(opentelemetry_sdk, vsn),
        NamedResource = otel_resource:create([{'service.name', <<"named-provider">>}]),
        {ok, NamedProvider} = otel_tracer_provider:start(
                                direct_sdk_test, NamedResource, #{processors => []}),

        ?assert(is_process_alive(SdkSup)),
        ?assert(is_pid(whereis(otel_tracer_provider_global))),
        ?assert(is_process_alive(NamedProvider)),
        ?assertEqual(NamedResource, otel_tracer_provider:resource(direct_sdk_test)),
        ?assertEqual(unicode:characters_to_binary(SdkVsn),
                     maps:get('telemetry.sdk.version', Attributes)),
        ?assertEqual(undefined, application:get_key(opentelemetry, vsn))
    after
        _ = application:stop(opentelemetry_sdk),
        _ = application:unset_env(opentelemetry_sdk, tracer_provider)
    end.
