-module(otel_runtime_configuration_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [defaults_and_overrides].

defaults_and_overrides(_Config) ->
    Defaults = otel_configuration:resolve(#{}),
    Resolved = otel_configuration:resolve(#{log_level => debug}),

    ?assertMatch(#{configuration_source := legacy,
                   create_application_tracers := true,
                   traces_enabled := true,
                   metrics_enabled := true,
                   log_level := info,
                   processors := [{otel_batch_processor, _}]},
                 Defaults),
    ?assertEqual(debug, maps:get(log_level, Resolved)),
    ?assertEqual(maps:get(processors, Defaults), maps:get(processors, Resolved)),
    ?assertEqual(maps:size(Defaults), maps:size(Resolved)).
