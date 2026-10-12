-module(opentelemetry_sdk_sup_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [starts_without_opentelemetry].

starts_without_opentelemetry(_Config) ->
    try
        _ = application:stop(opentelemetry),
        _ = application:unload(opentelemetry),
        ?assertEqual(undefined, application:get_key(opentelemetry, vsn)),

        {ok, _} = application:ensure_all_started(opentelemetry_sdk),
        {ok, Configuration} = otel_configuration_source:resolve([]),
        {ok, SdkSup} = opentelemetry_sdk_sup:start_link(Configuration),
        unlink(SdkSup),

        Resource = otel_resource_detector:get_resource(),
        Attributes = otel_attributes:map(otel_resource:attributes(Resource)),
        {ok, SdkVsn} = application:get_key(opentelemetry_sdk, vsn),

        ?assert(is_process_alive(SdkSup)),
        ?assertEqual(unicode:characters_to_binary(SdkVsn),
                     maps:get('telemetry.sdk.version', Attributes)),
        ?assertEqual(undefined, application:get_key(opentelemetry, vsn))
    after
        stop_sdk_supervisor(),
        _ = application:stop(opentelemetry_sdk)
    end.

stop_sdk_supervisor() ->
    case whereis(opentelemetry_sdk_sup) of
        undefined ->
            ok;
        SdkSup ->
            Monitor = monitor(process, SdkSup),
            unlink(SdkSup),
            exit(SdkSup, shutdown),
            receive
                {'DOWN', Monitor, process, SdkSup, shutdown} ->
                    ok
            after 5000 ->
                    error({supervisor_shutdown_timeout, SdkSup})
            end
    end.
