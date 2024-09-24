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
    [startup, startup_env_service_name, os_env_resource, app_env_resource, combining,
     combining_conflicting_schemas, crash_detector, timeout_detector, release_service_name,
     unknown_service_name, release_service_name_no_version, service_instance_id_env,
     service_instance_id_env_attributes, {group, net_kernel_node_name}, service_instance_id_node_id2,
     validate_keys, do_not_create_unneeded_atoms].

groups() ->
    [{net_kernel_node_name, [], [service_instance_id_node_id1,
                                 service_instance_id_node_name]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(net_kernel_node_name, Config) ->
    case os:getenv("CI") of
        "true" ->
            {skip, github_ci};
        _ ->
            Config
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(service_instance_id_node_name, Config) ->
    start_net_kernel_and_detector([test@instance, shortnames]),
    Config;
init_per_testcase(service_instance_id_node_id1, Config) ->
    start_net_kernel_and_detector([test@localhost, shortnames]),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(TestCase, _Config) when TestCase =:= service_instance_id_node_name ;
                                         TestCase =:= service_instance_id_node_id1 ->
    net_kernel:stop(),
    ok;
end_per_testcase(_, _) ->
    ok.

startup(_Config) ->
    try
        os:putenv("OTEL_RESOURCE_ATTRIBUTES", "service.name=cttest,service.version=1.1.1"),

        {ok, _} = application:ensure_all_started(opentelemetry),
        Resource = otel_tracer_provider:resource(),
        _ = application:stop(opentelemetry),

        ?assertMatch(#{'service.name' := <<"cttest">>,
                       'service.version' := <<"1.1.1">>}, otel_attributes:map(otel_resource:attributes(Resource))),
        ok
    after
        os:unsetenv("OTEL_RESOURCE_ATTRIBUTES"),
        application:stop(opentelemetry),
        application:unload(opentelemetry)
    end.

startup_env_service_name(_Config) ->
    try
        os:putenv("OTEL_SERVICE_NAME", "env-service-name"),
        os:putenv("OTEL_RESOURCE_ATTRIBUTES", "service.name=cttest,service.version=1.1.1"),

        {ok, _} = application:ensure_all_started(opentelemetry),
        Resource = otel_tracer_provider:resource(),
        _ = application:stop(opentelemetry),

        ?assertMatch(#{'service.name' := <<"env-service-name">>,
                       'service.version' := <<"1.1.1">>}, otel_attributes:map(otel_resource:attributes(Resource))),
        ok
    after
        os:unsetenv("OTEL_SERVICE_NAME"),
        os:unsetenv("OTEL_RESOURCE_ATTRIBUTES"),
        application:stop(opentelemetry),
        application:unload(opentelemetry)
    end.


crash_detector(_Config) ->
    try
        application:load(opentelemetry),
        application:set_env(opentelemetry, resource, #{<<"c">> => <<"d">>,
                                                       "sk" => "sv"}),
        os:putenv("OTEL_RESOURCE_ATTRIBUTES", "service.name=cttest,service.version=2.1.1"),

        otel_resource_detector:start_link(#{resource_detectors => [otel_resource_env_var,
                                                                   {otel_resource_detector_test, error},
                                                                   otel_resource_app_env],
                                            resource_detector_timeout => 100}),

        Resource = otel_resource_detector:get_resource(),

        ?assertMatch(#{'service.name' := <<"cttest">>,
                       'service.version' := <<"2.1.1">>,
                       c := <<"d">>,
                       sk := <<"sv">>}, otel_attributes:map(otel_resource:attributes(Resource))),

        ok
    after
        os:unsetenv("OTEL_RESOURCE_ATTRIBUTES"),
        application:unload(opentelemetry)
    end.

timeout_detector(_Config) ->
    try
        application:load(opentelemetry),
        application:set_env(opentelemetry, resource, #{<<"e">> => <<"f">>}),
        os:putenv("OTEL_RESOURCE_ATTRIBUTES", "service.name=cttest,service.version=3.1.1"),

        otel_resource_detector:start_link(#{resource_detectors => [otel_resource_env_var,
                                                                   {otel_resource_detector_test, sleep},
                                                                   otel_resource_app_env],
                                            resource_detector_timeout => 100}),

        Resource = otel_resource_detector:get_resource(),

        ?assertMatch(#{'service.name' := <<"cttest">>,
                       'service.version' := <<"3.1.1">>,
                       e := <<"f">>}, otel_attributes:map(otel_resource:attributes(Resource))),

        ?assertEqual(otel_resource:create([]), otel_resource_detector:get_resource(0)),

        ok
    after
        os:unsetenv("OTEL_RESOURCE_ATTRIBUTES"),
        application:unload(opentelemetry)
    end.

os_env_resource(_Config) ->
    Resource = otel_resource_env_var:parse("service.name=cttest,service.version=1.1.1"),
    Expected = [{"service.name", "cttest"}, {"service.version", "1.1.1"}],
    ?assertEqual(Expected, Resource),
    ok.

app_env_resource(_Config) ->
    Attributes = #{a => [{b,[{c,d}]}], service => #{name => <<"hello">>}},
    Expected = [{<<"a.b.c">>, d}, {<<"service.name">>, <<"hello">>}],

    %% sort because this is created from a map and need to make sure
    %% the order is always the same when we do the assertion
    ?assertEqual(Expected, lists:sort(otel_resource_app_env:parse(Attributes))),
    ok.

combining(_Config) ->
    Resource1 = otel_resource:create(otel_resource_app_env:parse([{service, [{name, <<"other-name">>},
                                                                             {version, "1.1.1"}]}]),
                                     <<"https://opentelemetry.io/schemas/1.8.0">>),
    Resource2 = otel_resource:create(otel_resource_env_var:parse("service.name=cttest,service.version=1.1.1"),
                                     <<"https://opentelemetry.io/schemas/1.8.0">>),

    Merged = otel_resource:merge(Resource1, Resource2),

    Expected = otel_attributes:new([{'service.name', <<"other-name">>},
                                    {'service.version', <<"1.1.1">>}], 128, 255),
    ?assertEqual(Expected, otel_resource:attributes(Merged)),
    ?assertEqual(<<"https://opentelemetry.io/schemas/1.8.0">>, otel_resource:schema_url(Merged)),
    ok.

combining_conflicting_schemas(_Config) ->
    Resource1 = otel_resource:create(otel_resource_app_env:parse([{service, [{name, <<"other-name">>},
                                                                             {version, "1.1.1"}]}]), <<"https://opentelemetry.io/schemas/1.8.0">>),
    Resource2 = otel_resource:create(otel_resource_env_var:parse("service.name=cttest,service.version=1.1.1"),
                                     <<"https://opentelemetry.io/schemas/1.7.0">>),

    Merged = otel_resource:merge(Resource1, Resource2),

    Expected = otel_attributes:new([{'service.name', <<"other-name">>},
                                    {'service.version', <<"1.1.1">>}], 128, 255),
    ?assertEqual(Expected, otel_resource:attributes(Merged)),
    ?assertEqual(undefined, otel_resource:schema_url(Merged)),
    ok.

unknown_service_name(_Config) ->
    try
        os:unsetenv("OTEL_RESOURCE_ATTRIBUTES"),

        application:unload(opentelemetry),
        application:load(opentelemetry),
        application:set_env(opentelemetry, resource, #{<<"e">> => <<"f">>}),

        otel_resource_detector:start_link(#{resource_detectors => [otel_resource_env_var,
                                                                   otel_resource_app_env],
                                            resource_detector_timeout => 100}),

        Resource = otel_resource_detector:get_resource(),
        ?assertMatch(#{'service.name' := <<"unknown_service:erl">>,
                       'process.runtime.name' := <<"BEAM">>,
                       'process.executable.name' := <<"erl">>,
                       e := <<"f">>}, otel_attributes:map(otel_resource:attributes(Resource))),

        ok
    after
        os:unsetenv("OTEL_RESOURCE_ATTRIBUTES")
    end.

release_service_name(_Config) ->
    try
        os:putenv("RELEASE_NAME", "rel-cttest"),
        os:putenv("RELEASE_VSN", "0.1.0"),
        application:unload(opentelemetry),
        application:load(opentelemetry),
        application:set_env(opentelemetry, resource, #{<<"e">> => <<"f">>}),

        otel_resource_detector:start_link(#{resource_detectors => [otel_resource_env_var,
                                                                   otel_resource_app_env],
                                            resource_detector_timeout => 100}),

        Resource = otel_resource_detector:get_resource(),
        ?assertMatch(#{'service.name' := <<"rel-cttest">>,
                       'service.version' := <<"0.1.0">>,
                       e := <<"f">>}, otel_attributes:map(otel_resource:attributes(Resource))),

        ok
    after
        os:unsetenv("RELEASE_VSN"),
        os:unsetenv("RELEASE_NAME")
    end.

service_instance_id_env(_Config) ->
    try
        os:putenv("OTEL_SERVICE_INSTANCE", "test@instance"),
        application:unload(opentelemetry),
        application:load(opentelemetry),
        application:set_env(opentelemetry, resource, #{<<"e">> => <<"f">>}),

        otel_resource_detector:start_link(#{resource_detectors => [otel_resource_env_var,
                                                                   otel_resource_app_env],
                                            resource_detector_timeout => 100}),

        Resource = otel_resource_detector:get_resource(),
        ?assertMatch(#{'service.instance.id' := <<"test@instance">>,
                       e := <<"f">>}, otel_attributes:map(otel_resource:attributes(Resource))),

        ok
    after
        os:unsetenv("OTEL_SERVICE_INSTANCE")
    end.

service_instance_id_env_attributes(_Config) ->
    try
        os:putenv("OTEL_RESOURCE_ATTRIBUTES", "service.instance.id=test@instance"),
        application:unload(opentelemetry),
        application:load(opentelemetry),
        application:set_env(opentelemetry, resource, #{<<"e">> => <<"f">>}),

        otel_resource_detector:start_link(#{resource_detectors => [otel_resource_env_var,
                                                                   otel_resource_app_env],
                                            resource_detector_timeout => 100}),

        Resource = otel_resource_detector:get_resource(),
        ?assertMatch(#{'service.instance.id' := <<"test@instance">>,
                       e := <<"f">>}, otel_attributes:map(otel_resource:attributes(Resource))),

        ok
    after
        os:unsetenv("OTEL_RESOURCE_ATTRIBUTES")
    end.

service_instance_id_node_name(_Config) ->
    Resource = otel_resource_detector:get_resource(),
    ?assertMatch(#{'service.instance.id' := <<"test@instance">>,
                   e := <<"f">>}, otel_attributes:map(otel_resource:attributes(Resource))),

    ok.

service_instance_id_node_id1(_Config) ->
    Resource = otel_resource_detector:get_resource(),
    ResourceMap = otel_attributes:map(otel_resource:attributes(Resource)),
    ?assertNotMatch(#{'service.instance.id' := <<"test@localhost">>}, ResourceMap),
    ?assert(maps:is_key('service.instance.id', ResourceMap)),

    ok.

service_instance_id_node_id2(_Config) ->
    application:unload(opentelemetry),
    application:load(opentelemetry),
    application:set_env(opentelemetry, resource, #{<<"e">> => <<"f">>}),

    otel_resource_detector:start_link(#{resource_detectors => [otel_resource_env_var,
                                                                otel_resource_app_env],
                                        resource_detector_timeout => 100}),

    Resource = otel_resource_detector:get_resource(),
    ResourceMap = otel_attributes:map(otel_resource:attributes(Resource)),
    ?assertNotMatch(#{'service.instance.id' := <<"nonode@nohost">>}, ResourceMap),
    ?assert(maps:is_key('service.instance.id', ResourceMap)),
    ok.

release_service_name_no_version(_Config) ->
    try
        os:putenv("RELEASE_NAME", "rel-cttest"),
        application:unload(opentelemetry),
        application:load(opentelemetry),
        application:set_env(opentelemetry, resource, #{<<"e">> => <<"f">>}),

        otel_resource_detector:start_link(#{resource_detectors => [otel_resource_env_var,
                                                                   otel_resource_app_env],
                                            resource_detector_timeout => 100}),

        Resource = otel_resource_detector:get_resource(),
        ?assertMatch(#{'service.name' := <<"rel-cttest">>,
                       e := <<"f">>}, otel_attributes:map(otel_resource:attributes(Resource))),
        ?assertNot(maps:is_key('service.version', otel_attributes:map(otel_resource:attributes(Resource)))),

        ok
    after
        os:unsetenv("RELEASE_NAME")
    end.

validate_keys(_Config) ->
    application:unload(opentelemetry),
    application:load(opentelemetry),
    application:set_env(opentelemetry, resource, #{
                                                   <<"e">> => <<"f">>,
                                                   service => #{
                                                                <<"name">> => <<"name">>,
                                                                alias => <<"alias">>
                                                               },
                                                   g => <<"h">>,
                                                   '' => <<"i">>,
                                                   'ə' => <<"j">>
                                                  }),

    otel_resource_detector:start_link(#{resource_detectors => [otel_resource_app_env],
                                        resource_detector_timeout => 100}),

    Resource = otel_resource_detector:get_resource(),
    ?assert(otel_resource:is_key(e, Resource)),
    ?assert(otel_resource:is_key('service.name', Resource)),
    ?assert(otel_resource:is_key(<<"service.alias">>, Resource)),
    ?assert(otel_resource:is_key("service.alias", Resource)),
    ?assert(otel_resource:is_key('service.alias', Resource)),
    ?assert(otel_resource:is_key(g, Resource)),
    ?assertNot(otel_resource:is_key('ə', Resource)),
    ?assertNot(otel_resource:is_key('', Resource)),

    ok.

do_not_create_unneeded_atoms(_Config) ->
    NonAtomBinary = fun NonAtomBinaryF(0) ->
                            error(max_iterations);
                        NonAtomBinaryF(N) ->
                            Candidate = << <<($a + rand:uniform(6) - 1)>> || _ <- lists:seq(1,8) >>,
                            try binary_to_existing_atom(Candidate, latin1) of
                                _ -> NonAtomBinaryF(N - 1)
                            catch error:badarg -> Candidate
                            end
                    end(100),
    otel_resource_detector:start_link(#{resource_detectors => [otel_resource_app_env],
                                        resource_detector_timeout => 100}),
    Resource = otel_resource_detector:get_resource(),
    ?assertNot(otel_resource:is_key(NonAtomBinary, Resource)),
    ?assertError(badarg, binary_to_existing_atom(NonAtomBinary, latin1)),
    ok.

%%

start_net_kernel_and_detector(NetKernelArgs) ->
    ?assertMatch({ok, _}, net_kernel:start(NetKernelArgs)),
    application:unload(opentelemetry),
    application:load(opentelemetry),
    application:set_env(opentelemetry, resource, #{<<"e">> => <<"f">>}),

    otel_resource_detector:start_link(#{resource_detectors => [otel_resource_env_var,
                                                               otel_resource_app_env],
                                        resource_detector_timeout => 100}),
    ok.
