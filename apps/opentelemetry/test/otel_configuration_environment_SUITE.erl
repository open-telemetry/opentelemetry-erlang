-module(otel_configuration_environment_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

-import(otel_configuration_test_utils, [resource_attributes/1, add_warning_handler/2, warnings/0]).
-include("otel_tracer.hrl").

all() ->
    [zero_config_starts_sdk,
     environment_configures_sdk,
     environment_exports_spans,
     trace_exporter_selection,
     environment_exports_to_console,
     otlp_signal_precedence,
     environment_can_disable_sdk,
     environment_can_disable_export_and_propagation,
     invalid_and_empty_values_use_defaults,
     shared_key_value_syntax,
     malformed_key_value_entries,
     explicit_application_configuration_is_authoritative,
     invalid_explicit_configuration_does_not_fall_back].

init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(opentelemetry_exporter),
    [{started_apps, Started} | Config].

end_per_suite(Config) ->
    _ = application:unload(opentelemetry),
    [application:stop(App) || App <- lists:reverse(proplists:get_value(started_apps, Config))],
    ok.

init_per_testcase(_Case, Config) ->
    _ = application:stop(opentelemetry),
    _ = application:load(opentelemetry),
    Saved = otel_configuration_test_utils:save_environment(),
    AppEnv = application:get_all_env(opentelemetry),
    [application:unset_env(opentelemetry, Key) || {Key, _} <- AppEnv],
    [{saved_environment, Saved}, {saved_app_env, AppEnv} | Config].

end_per_testcase(_Case, Config) ->
    _ = application:stop(opentelemetry),
    ok = otel_configuration_test_utils:restore_environment(
           proplists:get_value(saved_environment, Config)),
    [application:unset_env(opentelemetry, Key) || {Key, _} <- application:get_all_env(opentelemetry)],
    [application:set_env(opentelemetry, Key, Value)
     || {Key, Value} <- proplists:get_value(saved_app_env, Config)],
    ok.

zero_config_starts_sdk(_Config) ->
    {ok, Runtime} = otel_configuration_source:resolve([]),
    ?assertNot(maps:is_key(log_level, otel_configuration_model:root(
                                      otel_configuration_sdk:source(Runtime)))),
    ?assertEqual(environment, otel_configuration_model:source(otel_configuration_sdk:source(Runtime))),
    ?assertEqual([trace_context, baggage], otel_configuration_sdk:text_map_propagators(Runtime)),
    ?assertMatch(#{sampler := {parent_based, #{root := always_on}},
                   processors := [{otel_batch_processor, #{schedule_delay := 5000,
                                                           export_timeout := 30000,
                                                           max_queue_size := 2048}}]},
                 otel_configuration_sdk:tracer_provider(Runtime)),
    ?assertMatch(#{protocol := http_protobuf,
                   endpoints := [<<"http://localhost:4318/v1/traces">>],
                   configuration_resolved := true}, exporter_options(Runtime)),
    {ok, _} = application:ensure_all_started(opentelemetry),
    ?assert(is_pid(whereis(otel_tracer_provider_global))),
    ?assertMatch([{_, Pid, worker, _}] when is_pid(Pid),
                 supervisor:which_children(otel_span_processor_sup_global)),
    ?assertEqual({otel_propagator_text_map_composite,
                  [otel_propagator_trace_context, otel_propagator_baggage]},
                 opentelemetry:get_text_map_injector()).

environment_configures_sdk(_Config) ->
    set_environment([{"OTEL_LOG_LEVEL", "debug"},
                     {"OTEL_EXPORTER_OTLP_ENDPOINT", "http://collector:4318/prefix/"},
                     {"OTEL_SERVICE_NAME", "checkout"},
                     {"OTEL_SERVICE_INSTANCE", "instance-1"},
                     {"OTEL_RESOURCE_ATTRIBUTES", "service.name=lower-priority,region=ca,note=a%3Db"},
                     {"OTEL_TRACES_SAMPLER", "parentbased_traceidratio"},
                     {"OTEL_TRACES_SAMPLER_ARG", "0.25"},
                     {"OTEL_PROPAGATORS", "b3,baggage,b3"},
                     {"OTEL_BSP_SCHEDULE_DELAY_MILLIS", "42"},
                     {"OTEL_BSP_EXPORT_TIMEOUT", "123"},
                     {"OTEL_BSP_MAX_QUEUE_SIZE", "256"},
                     {"OTEL_ATTRIBUTE_COUNT_LIMIT", "64"},
                     {"OTEL_SPAN_EVENT_COUNT_LIMIT", "12"}]),
    {ok, Runtime} = otel_configuration_source:resolve([]),
    ?assertEqual(debug, maps:get(log_level, otel_configuration_model:root(
                                           otel_configuration_sdk:source(Runtime)))),
    ?assertNot(maps:is_key(log_level, Runtime)),
    ?assertMatch(#{sampler := {parent_based, #{root := {trace_id_ratio_based, 0.25}}},
                   processors := [{otel_batch_processor, #{schedule_delay := 42,
                                                           export_timeout := 123,
                                                           max_queue_size := 256}}]},
                 otel_configuration_sdk:tracer_provider(Runtime)),
    ?assertMatch(#{endpoints := [<<"http://collector:4318/prefix/v1/traces">>]}, exporter_options(Runtime)),
    os:putenv("OTEL_BSP_SCHEDULE_DELAY", "43"),
    {ok, Modern} = otel_configuration_source:resolve([]),
    ?assertMatch(#{processors := [{otel_batch_processor, #{schedule_delay := 43}}]},
                 otel_configuration_sdk:tracer_provider(Modern)),
    ?assertMatch(#{attribute_count_limit := 64, event_count_limit := 12},
                 otel_configuration_sdk:span_limits(Runtime)),
    {ok, _} = application:ensure_all_started(opentelemetry),
    ?assertMatch(#{'service.name' := <<"checkout">>, 'service.instance.id' := <<"instance-1">>,
                   region := <<"ca">>, note := <<"a=b">>},
                 resource_attributes(otel_tracer_provider:resource())),
    ?assertEqual({otel_propagator_text_map_composite,
                  [{otel_propagator_b3, b3single}, otel_propagator_baggage]},
                 opentelemetry:get_text_map_injector()),
    {otel_tracer_default, #tracer{sampler=Sampler}} = opentelemetry:get_tracer(),
    ?assertNotEqual(nomatch, binary:match(otel_sampler:description(Sampler), <<"0.25">>)).

environment_exports_spans(_Config) ->
    set_environment([{"OTEL_EXPORTER_OTLP_ENDPOINT", "http://collector:4318/prefix"},
                     {"OTEL_EXPORTER_OTLP_HEADERS", "test-header=environment"},
                     {"OTEL_BSP_SCHEDULE_DELAY", "60000"},
                     {"OTEL_SERVICE_NAME", "environment-export-test"}]),
    Parent = self(),
    ok = meck:new(httpc, [passthrough]),
    ok = meck:expect(httpc, request,
                     fun(post, {Address, Headers, "application/x-protobuf", Body}, _, _, _) ->
                             Parent ! {export_request, Address, Headers, Body},
                             {ok, {{"1.1", 200, ""}, [], <<>>}}
                     end),
    try
        {ok, _} = application:ensure_all_started(opentelemetry),
        Span = otel_tracer:start_span(opentelemetry:get_tracer(), <<"environment-span">>, #{}),
        otel_span:end_span(Span),
        otel_tracer_provider:force_flush(),
        receive
            {export_request, Address, Headers, Body} ->
                ?assertEqual("http://collector:4318/prefix/v1/traces", Address),
                ?assert(lists:member({"test-header", "environment"}, Headers)),
                ?assert(byte_size(Body) > 0)
        after 5000 -> error(no_environment_export)
        end,
        ?assert(meck:validate(httpc))
    after
        application:stop(opentelemetry),
        meck:unload(httpc)
    end.

trace_exporter_selection(_Config) ->
    Handler = trace_exporter_warning_test,
    ok = add_warning_handler(Handler, otel_configuration_env_var),
    try
        lists:foreach(
          fun({Value, Expected, Warnings}) ->
                  case Value of
                      false -> os:unsetenv("OTEL_TRACES_EXPORTER");
                      _ -> os:putenv("OTEL_TRACES_EXPORTER", Value)
                  end,
                  {ok, Runtime} = otel_configuration_source:resolve([]),
                  #{processors := [{otel_batch_processor, #{exporter := Exporter}}]} =
                      otel_configuration_sdk:tracer_provider(Runtime),
                  case Expected of
                      otlp -> ?assertMatch({opentelemetry_exporter, #{protocol := http_protobuf}}, Exporter);
                      _ -> ?assertEqual(Expected, Exporter)
                  end,
                  ?assertEqual(Warnings, warnings())
          end, [{false, otlp, []}, {"", otlp, []}, {"otlp", otlp, []},
                {"console", {otel_exporter_stdout, #{}}, []}, {"none", none, []},
                {"consol", none, ["OTEL_TRACES_EXPORTER"]},
                {"zipkin", none, ["OTEL_TRACES_EXPORTER"]},
                {"console,otlp", none, ["OTEL_TRACES_EXPORTER"]}])
    after
        logger:remove_handler(Handler)
    end.

environment_exports_to_console(_Config) ->
    set_environment([{"OTEL_TRACES_EXPORTER", "console"},
                     {"OTEL_BSP_SCHEDULE_DELAY", "60000"}]),
    Parent = self(),
    ok = meck:new(otel_exporter_stdout, [passthrough]),
    ok = meck:expect(otel_exporter_stdout, export,
                     fun(Table, Resource, State) ->
                             Result = meck:passthrough([Table, Resource, State]),
                             Parent ! {console_export, ets:info(Table, size)},
                             Result
                     end),
    try
        {ok, _} = application:ensure_all_started(opentelemetry),
        Span = otel_tracer:start_span(opentelemetry:get_tracer(), <<"console-span">>, #{}),
        otel_span:end_span(Span),
        otel_tracer_provider:force_flush(),
        receive
            {console_export, Count} -> ?assertEqual(1, Count)
        after 5000 -> error(no_console_export)
        end,
        ?assert(meck:validate(otel_exporter_stdout))
    after
        application:stop(opentelemetry),
        meck:unload(otel_exporter_stdout)
    end.

otlp_signal_precedence(_Config) ->
    set_environment([{"OTEL_EXPORTER_OTLP_ENDPOINT", "http://general:4318/base"},
                     {"OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", "https://traces:4318/exact"},
                     {"OTEL_EXPORTER_OTLP_PROTOCOL", "grpc"},
                     {"OTEL_EXPORTER_OTLP_TRACES_PROTOCOL", "http/protobuf"},
                     {"OTEL_EXPORTER_OTLP_HEADERS", "generic=discard"},
                     {"OTEL_EXPORTER_OTLP_TRACES_HEADERS", "trace=kept%3Dvalue"},
                     {"OTEL_EXPORTER_OTLP_COMPRESSION", "none"},
                     {"OTEL_EXPORTER_OTLP_TRACES_COMPRESSION", "gzip"}]),
    {ok, Http} = otel_configuration_source:resolve([]),
    ?assertMatch(#{endpoints := [<<"https://traces:4318/exact">>], protocol := http_protobuf,
                   headers := [{<<"trace">>, <<"kept=value">>}], compression := gzip},
                 exporter_options(Http)),
    os:unsetenv("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"),
    %% Invalid signal-specific values are ignored, allowing the general value.
    os:putenv("OTEL_EXPORTER_OTLP_TRACES_PROTOCOL", "unsupported"),
    {ok, Grpc} = otel_configuration_source:resolve([]),
    ?assertMatch(#{endpoints := [<<"http://general:4318/base">>], protocol := grpc},
                 exporter_options(Grpc)),
    %% An already resolved exporter never re-reads environment configuration.
    os:putenv("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", "http://changed:4318/ignored"),
    ?assertMatch(#{endpoints := [<<"https://traces:4318/exact">>]},
                 otel_exporter_traces_otlp:merge_with_environment(exporter_options(Http))).

environment_can_disable_sdk(_Config) ->
    os:putenv("OTEL_SDK_DISABLED", "TRUE"),
    {ok, Runtime} = otel_configuration_source:resolve([]),
    ?assert(otel_configuration_sdk:disabled(Runtime)),
    {ok, _} = application:ensure_all_started(opentelemetry),
    ?assertEqual(undefined, whereis(otel_tracer_provider_global)).

environment_can_disable_export_and_propagation(_Config) ->
    set_environment([{"OTEL_TRACES_EXPORTER", "none"}, {"OTEL_PROPAGATORS", "none"}]),
    {ok, Runtime} = otel_configuration_source:resolve([]),
    ?assertMatch(#{processors := [{otel_batch_processor, #{exporter := none}}]},
                 otel_configuration_sdk:tracer_provider(Runtime)),
    ?assertEqual([], otel_configuration_sdk:text_map_propagators(Runtime)).

invalid_and_empty_values_use_defaults(_Config) ->
    set_environment([{"OTEL_CONFIG_FILE", ""}, {"OTEL_SDK_DISABLED", ""},
                     {"OTEL_TRACES_SAMPLER", "traceidratio"}, {"OTEL_TRACES_SAMPLER_ARG", "2"},
                     {"OTEL_BSP_MAX_QUEUE_SIZE", "-1"}, {"OTEL_BSP_EXPORT_TIMEOUT", "garbage"},
                     {"OTEL_EXPORTER_OTLP_ENDPOINT", "not a URL"},
                     {"OTEL_EXPORTER_OTLP_PROTOCOL", "unsupported"},
                     {"OTEL_PROPAGATORS", ""}]),
    {ok, Runtime} = otel_configuration_source:resolve([]),
    ?assertNot(otel_configuration_sdk:disabled(Runtime)),
    ?assertEqual([trace_context, baggage], otel_configuration_sdk:text_map_propagators(Runtime)),
    ?assertMatch(#{sampler := {trace_id_ratio_based, 1.0},
                   processors := [{otel_batch_processor, #{max_queue_size := 2048,
                                                           export_timeout := 30000}}]},
                 otel_configuration_sdk:tracer_provider(Runtime)),
    ?assertMatch(#{endpoints := [<<"http://localhost:4318/v1/traces">>]}, exporter_options(Runtime)).

shared_key_value_syntax(_Config) ->
    Raw = <<" name%2Ckey = value%3Dpart ,percent=100%25,once=%252C,"
            "plus=a+b,equals=a=b,empty=,quoted=\"text\","
            "unicode=caf%C3%A9,literal=caf", 16#C3, 16#A9>>,
    Expected = [{<<"name,key">>, <<"value=part">>}, {<<"percent">>, <<"100%">>},
                {<<"once">>, <<"%2C">>}, {<<"plus">>, <<"a+b">>},
                {<<"equals">>, <<"a=b">>}, {<<"empty">>, <<>>},
                {<<"quoted">>, <<"\"text\"">>},
                {<<"unicode">>, <<"caf", 16#C3, 16#A9>>},
                {<<"literal">>, <<"caf", 16#C3, 16#A9>>}],
    lists:foreach(
      fun({Input, Pairs}) ->
              String = case unicode:characters_to_list(Input) of
                           Characters when is_list(Characters) -> Characters;
                           _ -> error(invalid_test_input)
                       end,
              os:putenv("OTEL_RESOURCE_ATTRIBUTES", String),
              os:putenv("OTEL_EXPORTER_OTLP_HEADERS", String),
              ?assertEqual(Pairs, otel_resource_env_var:parse(String)),
              {ok, Environment} = otel_configuration_source:resolve([]),
              {ok, Declarative} = otel_configuration_test_utils:resolve_declarative(
                                   #{file_format => <<"1.1">>,
                                     resource => #{attributes_list => Input}}),
              Attributes = resource_attributes(otel_resource:create(Pairs)),
              ?assertEqual(Attributes, resource_attributes(
                                         otel_configuration_sdk:resource(Environment))),
              ?assertEqual(Attributes, resource_attributes(
                                         otel_configuration_sdk:resource(Declarative))),
              ?assertEqual(Attributes, resource_attributes(otel_resource_env_var:get_resource(#{}))),
              ?assertEqual(Pairs, maps:get(headers, exporter_options(Environment)))
      end, [{Raw, Expected}, {<<>>, []}]).

malformed_key_value_entries(_Config) ->
    %% Raw malformed bytes must also return errors if trimming encounters them
    %% before the decoder's UTF-8 check. Valid neighbors are still retained.
    lists:foreach(
      fun(Bad) ->
              {Pairs, Errors} = otel_configuration_key_value_list:parse(
                                  <<"before=one,", Bad/binary, ",after=two">>),
              ?assertEqual([{<<"before">>, <<"one">>}, {<<"after">>, <<"two">>}], Pairs),
              ?assertMatch([{invalid_utf8, _}], Errors)
      end, [<<255, "=value">>, <<"name=", 255>>, <<"name=a", 255, "z">>,
            <<"name=", 16#C3>>, <<"name=%FF">>]),
    ?assertEqual({[{<<" key ">>, <<" value ">>}], []},
                 otel_configuration_key_value_list:parse(<<" %20key%20 = %20value%20 ">>)),
    Valid = [{<<"before">>, <<"one">>}, {<<"after">>, <<"two">>}],
    lists:foreach(
      fun({Bad, Reason}) ->
              Input = <<"before=one,", Bad/binary, ",after=two">>,
              String = binary_to_list(Input),
              os:putenv("OTEL_RESOURCE_ATTRIBUTES", String),
              os:putenv("OTEL_EXPORTER_OTLP_HEADERS", String),
              ?assertEqual(Valid, otel_resource_env_var:parse(String)),
              {ok, Environment} = otel_configuration_source:resolve([]),
              ?assertEqual(resource_attributes(otel_resource:create(Valid)),
                           resource_attributes(otel_configuration_sdk:resource(Environment))),
              ?assertEqual(Valid, maps:get(headers, exporter_options(Environment))),
              ?assertEqual({error, {invalid_configuration, [resource, attributes_list], Reason}},
                           otel_configuration_test_utils:resolve_declarative(
                             #{file_format => <<"1.1">>,
                               resource => #{attributes_list => Input}}))
      end, [{<<"bad=%">>, {invalid_percent_encoding, <<"%">>}},
            {<<"bad=%GG">>, {invalid_percent_encoding, <<"%GG">>}},
            {<<"bad=%FF">>, {invalid_utf8, <<"%FF">>}},
            {<<"bad=%C3">>, {invalid_utf8, <<"%C3">>}},
            {<<"%GG=value">>, {invalid_percent_encoding, <<"%GG">>}},
            {<<"=value">>, <<"=value">>}, {<<"missing">>, <<"missing">>}, {<<>>, <<>>}]).

explicit_application_configuration_is_authoritative(_Config) ->
    set_environment([{"OTEL_SDK_DISABLED", "true"}, {"OTEL_SERVICE_NAME", "ignored"},
                     {"OTEL_PROPAGATORS", "b3"}, {"OTEL_TRACES_SAMPLER", "always_off"},
                     {"OTEL_EXPORTER_OTLP_ENDPOINT", "http://ignored:4318"}]),
    AppEnv = [{tracer_provider, #{processors => [], sampler => always_on}}],
    {ok, Runtime} = otel_configuration_source:resolve(AppEnv),
    ?assertEqual(application_env, otel_configuration_model:source(otel_configuration_sdk:source(Runtime))),
    ?assertNot(otel_configuration_sdk:disabled(Runtime)),
    ?assertEqual([], otel_configuration_sdk:text_map_propagators(Runtime)),
    ?assertEqual(undefined, otel_configuration_sdk:resource(Runtime)),
    ?assertMatch(#{sampler := always_on, processors := []}, otel_configuration_sdk:tracer_provider(Runtime)),
    [application:set_env(opentelemetry, Key, Value) || {Key, Value} <- AppEnv],
    {ok, _} = application:ensure_all_started(opentelemetry),
    ?assert(is_pid(whereis(otel_tracer_provider_global))),
    ?assertMatch(#{'service.name' := <<"unknown_service:erl">>},
                 resource_attributes(otel_tracer_provider:resource())),
    %% A partial explicit configuration also keeps declarative no-op defaults.
    {ok, Partial} = otel_configuration_source:resolve([{resource, #{attributes => #{}}}]),
    ?assertEqual(undefined, otel_configuration_sdk:tracer_provider(Partial)).

invalid_explicit_configuration_does_not_fall_back(_Config) ->
    ?assertMatch({error, {application_configuration_error, _}},
                 otel_configuration_source:resolve([{processors, []}])),
    os:putenv("OTEL_CONFIG_FILE", "/nonexistent/otel-sdk.json"),
    ?assertMatch({error, {configuration_file_error, _, _}},
                 otel_configuration_source:resolve([])).

set_environment(Values) ->
    [os:putenv(Name, Value) || {Name, Value} <- Values],
    ok.

exporter_options(#{tracer_provider := #{processors := [{otel_batch_processor,
                                                       #{exporter := {opentelemetry_exporter, Options}}}]}}) ->
    Options.
