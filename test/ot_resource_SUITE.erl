-module(ot_resource_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_span.hrl").
-include("ot_test_utils.hrl").
-include("ot_tracer.hrl").

%% TODO: negative testing. What a valid value is is still in flux so nothing bothering
%% to write tests to limit what can be a value only have them become valid values.
all() ->
    [startup, os_env_resource, app_env_resource, combining].

startup(_Config) ->
    os:putenv("OTEL_RESOURCE_LABELS", "service.name=cttest,service.version=1.1.1"),

    {ok, _} = application:ensure_all_started(opentelemetry),
    {_, Tracer} = opentelemetry:get_tracer(),
    _ = application:stop(opentelemetry),

    ?assertMatch({ot_resource, [{"service.name", "cttest"},
                                {"service.version", "1.1.1"}]}, Tracer#tracer.resource),
    ok.

os_env_resource(_Config) ->
    Resource = ot_resource_env_var:parse("service.name=cttest,service.version=1.1.1"),
    Expected = [{"service.name", "cttest"}, {"service.version", "1.1.1"}],
    ?assertEqual(Expected, Resource),
    ok.

app_env_resource(_Config) ->
    Attributes = #{a => [{b,[{c,d}]}], service => #{name => <<"hello">>}},
    Expected = [{"a.b.c", d}, {"service.name", <<"hello">>}],

    %% sort because this is created from a map and need to make sure
    %% the order is always the same when we do the assertion
    ?assertEqual(Expected, lists:sort(ot_resource_app_env:parse(Attributes))),
    ok.

combining(_Config) ->
    Resource1 = ot_resource:create(ot_resource_app_env:parse([{service, [{name, <<"other-name">>},
                                                                         {version, "1.1.1"}]}])),
    Resource2 = ot_resource:create(ot_resource_env_var:parse("service.name=cttest,service.version=1.1.1")),

    Merged = ot_resource:merge(Resource1, Resource2),

    Expected = {ot_resource, [{"service.name","other-name"},
                              {"service.version","1.1.1"}]},
    ?assertEqual(Expected, Merged),
    ok.
