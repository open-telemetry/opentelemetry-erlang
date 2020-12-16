-module(otel_resource_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_span.hrl").
-include("otel_test_utils.hrl").
-include("otel_tracer.hrl").

%% TODO: negative testing. What a valid value is is still in flux so nothing bothering
%% to write tests to limit what can be a value only have them become valid values.
all() ->
    [startup, os_env_resource, app_env_resource, combining, crash_detector, timeout_detector].

startup(_Config) ->
    os:putenv("OTEL_RESOURCE_ATTRIBUTES", "service.name=cttest,service.version=1.1.1"),

    {ok, _} = application:ensure_all_started(opentelemetry),
    {_, Tracer} = opentelemetry:get_tracer(),
    Resource = otel_tracer_provider:resource(),
    _ = application:stop(opentelemetry),

    ?assertMatch({otel_resource, [{<<"service.name">>, <<"cttest">>},
                                {<<"service.version">>, <<"1.1.1">>}]}, Resource),

    ?assertMatch({otel_resource, [{<<"service.name">>, <<"cttest">>},
                                {<<"service.version">>, <<"1.1.1">>}]}, Tracer#tracer.resource),
    ok.

crash_detector(_Config) ->
    application:load(opentelemetry),
    application:set_env(opentelemetry, resource, #{<<"c">> => <<"d">>}),
    os:putenv("OTEL_RESOURCE_ATTRIBUTES", "service.name=cttest,service.version=2.1.1"),

    otel_resource_detector:start_link([{resource_detectors, [otel_resource_env_var,
                                                             {otel_resource_detector_test, error},
                                                             otel_resource_app_env]},
                                       {resource_detectors_timeout, 100}]),

    {_, ResourceList} = otel_resource_detector:get_resource(),

    ?assertListsMatch([{<<"service.name">>, <<"cttest">>},
                       {<<"service.version">>, <<"2.1.1">>},
                       {<<"c">>, <<"d">>}], ResourceList),

    ok.

timeout_detector(_Config) ->
    application:load(opentelemetry),
    application:set_env(opentelemetry, resource, #{<<"e">> => <<"f">>}),
    os:putenv("OTEL_RESOURCE_ATTRIBUTES", "service.name=cttest,service.version=3.1.1"),

    otel_resource_detector:start_link([{resource_detectors, [otel_resource_env_var,
                                                             {otel_resource_detector_test, sleep},
                                                             otel_resource_app_env]},
                                       {resource_detectors_timeout, 100}]),

    {_, ResourceList} = otel_resource_detector:get_resource(),

    ?assertListsMatch([{<<"service.name">>, <<"cttest">>},
                       {<<"service.version">>, <<"3.1.1">>},
                       {<<"e">>, <<"f">>}], ResourceList),

    {_, []} = otel_resource_detector:get_resource(0),

    ok.

os_env_resource(_Config) ->
    Resource = otel_resource_env_var:parse("service.name=cttest,service.version=1.1.1"),
    Expected = [{"service.name", "cttest"}, {"service.version", "1.1.1"}],
    ?assertEqual(Expected, Resource),
    ok.

app_env_resource(_Config) ->
    Attributes = #{a => [{b,[{c,d}]}], service => #{name => <<"hello">>}},
    Expected = [{"a.b.c", d}, {"service.name", <<"hello">>}],

    %% sort because this is created from a map and need to make sure
    %% the order is always the same when we do the assertion
    ?assertEqual(Expected, lists:sort(otel_resource_app_env:parse(Attributes))),
    ok.

combining(_Config) ->
    Resource1 = otel_resource:create(otel_resource_app_env:parse([{service, [{name, <<"other-name">>},
                                                                         {version, "1.1.1"}]}])),
    Resource2 = otel_resource:create(otel_resource_env_var:parse("service.name=cttest,service.version=1.1.1")),

    Merged = otel_resource:merge(Resource1, Resource2),

    Expected = {otel_resource, [{<<"service.name">>, <<"other-name">>},
                              {<<"service.version">>, <<"1.1.1">>}]},
    ?assertEqual(Expected, Merged),
    ok.
