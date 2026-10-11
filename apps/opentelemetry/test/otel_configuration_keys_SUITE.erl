-module(otel_configuration_keys_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

-import(otel_configuration_test_utils, [add_warning_handler/2, warnings/0, foreach_case/2]).

all() -> [native_unknown_keys_fail, json_unknown_keys_warn, does_not_create_atoms,
          native_tuples_and_named_providers, legacy_resource_shape,
          custom_properties_are_opaque, rejects_binary_module_names, rejects_runtime_exporter_markers].

native_unknown_keys_fail(_Config) ->
    %% Each schema location is distinct coverage. Binary key lookup is shared,
    %% so one nested binary-key case suffices alongside the atom-key cases.
    BinaryCase = json_keys(exporter(otlp_http, #{tls => #{ca_files => <<"ca.pem">>}})),
    foreach_case(
      fun({Configuration, Path}) ->
              ?assertEqual({error, {invalid_configuration, Path, unknown_property}},
                           resolve_native(Configuration))
      end, cases() ++ [{BinaryCase, [exporter, otlp_http, tls, <<"ca_files">>]}]).

json_unknown_keys_warn(_Config) ->
    Handler = unknown_key_test,
    ok = add_warning_handler(Handler, otel_configuration_path),
    try
        lists:foreach(
          fun({Configuration, Path}) ->
                  Json = (json_keys(Configuration))#{<<"file_format">> => <<"1.1">>},
                  {ok, _} = otel_configuration_test_utils:resolve_declarative(Json),
                  ?assertEqual([binary_leaf(Path)], warnings())
          end, cases()),
        Unknown = <<"unknown_native_option_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
        ?assertEqual({error, {invalid_configuration, [Unknown], unknown_property}},
                     resolve_native(#{Unknown => <<"sensitive value">>})),
        ?assertMatch({ok, _}, otel_configuration_test_utils:resolve_declarative(
                               #{<<"file_format">> => <<"1.1">>, Unknown => <<"sensitive value">>})),
        ?assertEqual([[Unknown]], warnings())
    after
        logger:remove_handler(Handler)
    end.

native_tuples_and_named_providers(_Config) ->
    lists:foreach(
      fun(Processor) ->
              Provider = #{processors => [{Processor, #{exporter =>
                                  {otlp_http, #{endpoints => [<<"https://collector/v1/traces">>]}}}}]},
              Error = {error, {invalid_configuration, [exporter, otlp_http, endpoints], unknown_property}},
              ?assertEqual(Error, otel_configuration_sdk:create_tracer_provider(Provider)),
              ?assertEqual(Error, otel_tracer_provider_sup:start(invalid_options_provider, Provider)),
              ?assertEqual(undefined, whereis(otel_tracer_provider_invalid_options_provider))
      end, [batch, simple, otel_batch_processor, otel_simple_processor]),
    ?assertEqual({error, {invalid_configuration,
                         [tracer_provider, sampler, parent_based, remote_parent_smpled], unknown_property}},
                 otel_configuration_sdk:create_tracer_provider(
                   #{processors => [], sampler => {parent_based, #{remote_parent_smpled => always_off}}})),
    ?assertMatch({ok, #{sampler := {parent_based, #{root := always_off}}}},
                 otel_configuration_sdk:create_tracer_provider(
                   #{processors => [], sampler => {parent_based, #{<<"root">> => always_off}}})).

legacy_resource_shape(_Config) ->
    lists:foreach(
      fun(Attributes) ->
              ?assertEqual({error, {invalid_configuration, [resource], legacy_configuration_not_supported}},
                           resolve_native(#{resource => Attributes}))
      end, [#{<<"service.name">> => <<"x">>}, #{'service.name' => <<"x">>}]),
    ?assertMatch({ok, _}, resolve_native(#{resource => #{attributes => #{<<"service.name">> => <<"x">>}}})),
    ?assertMatch({ok, _}, resolve_native(#{resource => #{}})).

custom_properties_are_opaque(_Config) ->
    Options = #{endpoints => [], remote_parent_smpled => custom_value, arbitrary => #{nested => true}},
    {ok, Runtime} = resolve_native(
        #{resource => #{attributes => #{<<"tracer_providers">> => <<"attribute value">>}},
          propagator => #{composite => [{custom_propagator, Options}]},
          tracer_provider =>
              #{processors => [{custom_processor, Options},
                               {batch, #{exporter => {custom_exporter, Options}}}],
                sampler => {custom_sampler, Options}, id_generator => custom_generator},
          meter_provider => #{future_reader_setting => Options},
          logger_provider => #{future_logger_setting => Options}}),
    ?assertMatch(#{processors := [{custom_processor, Options},
                                  {otel_batch_processor, #{exporter := {custom_exporter, Options}}}],
                   sampler := {custom_sampler, Options}, id_generator := custom_generator},
                 otel_configuration_sdk:tracer_provider(Runtime)),
    ?assertEqual([{custom_propagator, Options}], otel_configuration_sdk:text_map_propagators(Runtime)),
    %% Transport-library options in the explicit module form remain opaque.
    ?assertMatch({ok, _}, resolve_native(exporter(opentelemetry_exporter,
                              #{channel_opts => Options, httpc_options => [{custom_option, true}]}))),
    lists:foreach(
      fun({Component, Expected}) ->
              ?assertMatch({ok, #{processors := [{otel_simple_processor,
                              #{exporter := {custom_exporter, Expected}}}]}},
                           otel_configuration_sdk:create_tracer_provider(
                             #{processors => [{simple, #{exporter => Component}}]}))
      end, [{{custom_exporter, null}, null}, {{custom_exporter, undefined}, undefined},
            {{custom_exporter, self()}, self()}, {#{custom_exporter => null}, #{}},
            {#{custom_exporter => undefined}, undefined}, {#{custom_exporter => Options}, Options}]).

cases() ->
    [{#{tracer_providers => #{}}, [tracer_providers]},
     {#{resource => #{atributes => #{}}}, [resource, atributes]},
     {#{resource => #{attributes => [#{name => <<"a">>, value => <<"b">>, valeu => 1}]}},
      [resource, attributes, valeu]},
     {#{propagator => #{composit => []}}, [propagator, composit]},
     {#{propagator => #{composite => [#{tracecontext => #{typo => true}}]}},
      [propagator, composite, tracecontext, typo]},
     {#{attribute_limits => #{attribute_counts_limit => 4}}, [attribute_limits, attribute_counts_limit]},
     {provider(#{processor => []}), [tracer_provider, processor]},
     {provider(#{limits => #{attribute_per_event_limit => 4}}), [tracer_provider, limits, attribute_per_event_limit]},
     {provider(#{processors => [#{batch => #{scheduled_delay_ms => 1, exporter => #{console => null}}}]}),
      [tracer_provider, processors, batch, scheduled_delay_ms]},
     {provider(#{processors => [#{simple => #{exporting_timeout_ms => 1, exporter => #{console => null}}}]}),
      [tracer_provider, processors, simple, exporting_timeout_ms]},
     {exporter(otlp_http, #{endpoints => [<<"https://collector/v1/traces">>]}), [exporter, otlp_http, endpoints]},
     {exporter(otlp_grpc, #{ssl_options => []}), [exporter, otlp_grpc, ssl_options]},
     {exporter(otlp_http, #{tls => #{ca_files => <<"ca.pem">>}}), [exporter, otlp_http, tls, ca_files]},
     {exporter(otlp_grpc, #{tls => #{ca_files => <<"ca.pem">>}}), [exporter, otlp_grpc, tls, ca_files]},
     {exporter(otlp_http, #{headers => [#{name => <<"a">>, value => <<"b">>, valeu => 1}]}),
      [exporter, otlp_http, headers, valeu]},
     {exporter(console, #{typo => true}), [exporter, console, typo]},
     {provider(#{sampler => #{parent_based => #{remote_parent_smpled => #{always_on => null}}}}),
      [tracer_provider, sampler, parent_based, remote_parent_smpled]},
     {provider(#{sampler => #{parent_based => #{root => #{trace_id_ratio_based => #{ration => 0.5}}}}}),
      [tracer_provider, sampler, parent_based, root, trace_id_ratio_based, ration]},
     {provider(#{sampler => #{always_on => #{typo => true}}}), [tracer_provider, sampler, always_on, typo]},
     {provider(#{id_generator => #{random => #{typo => true}}}), [tracer_provider, id_generator, random, typo]},
     {#{distribution => #{erlagn => #{}}}, [distribution, erlagn]},
     {#{distribution => #{erlang => #{resource_detector_timeot => 100}}}, [distribution, erlang, resource_detector_timeot]},
     {#{distribution => #{erlang => #{sweeper => #{span_ttl_ms => 100}}}}, [distribution, erlang, sweeper, span_ttl_ms]}].

provider(Options) -> #{tracer_provider => maps:merge(#{processors => []}, Options)}.
exporter(Kind, Options) -> provider(#{processors => [#{batch => #{exporter => #{Kind => Options}}}]}).

resolve_native(Configuration) ->
    {ok, Model} = otel_configuration_model:from_application_env(maps:to_list(Configuration)),
    otel_configuration_sdk:create(Model).

json_keys(Map) when is_map(Map) ->
    maps:from_list([{json_key(Key), json_keys(Value)} || {Key, Value} <- maps:to_list(Map)]);
json_keys(List) when is_list(List) -> [json_keys(Value) || Value <- List];
json_keys(Value) -> Value.

json_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
json_key(Key) -> Key.

binary_leaf(Path) -> lists:droplast(Path) ++ [json_key(lists:last(Path))].

rejects_binary_module_names(_Config) ->
    Cases = [{provider(#{processors => [#{Name => #{}}]}),
              [tracer_provider, processors], Name}
             || Name <- [<<"otel_batch_processor">>, <<"always_on">>, <<"periodic">>]] ++
            [{exporter(<<"opentelemetry_exporter">>, #{}),
              [tracer_provider, processors, exporter], <<"opentelemetry_exporter">>}],
    lists:foreach(
      fun({Configuration, Path, Name}) ->
              Expected = {error, {unsupported_configuration, Path, Name}},
              ?assertEqual(Expected, resolve_native(Configuration)),
              ?assertEqual(Expected, otel_configuration_test_utils:resolve_declarative(
                                       (json_keys(Configuration))#{<<"file_format">> => <<"1.1">>}))
      end, Cases).

rejects_runtime_exporter_markers(_Config) ->
    lists:foreach(
      fun(Key) ->
              lists:foreach(
                fun(Component) ->
                        Configuration = #{processors => [{simple, #{exporter => Component}}]},
                        Error = {error, {invalid_configuration,
                                        [exporter, opentelemetry_exporter, Key], unknown_property}},
                        ?assertEqual(Error, resolve_native(provider(Configuration))),
                        ?assertEqual(Error, otel_configuration_sdk:create_tracer_provider(Configuration)),
                        ?assertEqual(Error, otel_tracer_provider_sup:start(invalid_marker_provider, Configuration)),
                        ?assertEqual(undefined, whereis(otel_tracer_provider_invalid_marker_provider))
                end, [{opentelemetry_exporter, #{Key => true}},
                      #{opentelemetry_exporter => #{Key => false}}])
      end, [configuration_resolved, <<"configuration_resolved">>]),
    %% Only the SDK supplies the built-in marker; custom properties stay opaque.
    ?assertMatch({ok, #{processors := [{otel_simple_processor,
                      #{exporter := {opentelemetry_exporter, #{configuration_resolved := true}}}}]}},
                 otel_configuration_sdk:create_tracer_provider(
                   #{processors => [{simple, #{exporter => {opentelemetry_exporter, #{}}}}]})),
    ?assertMatch({ok, #{processors := [{otel_simple_processor,
                      #{exporter := {custom_exporter, #{configuration_resolved := false}}}}]}},
                 otel_configuration_sdk:create_tracer_provider(
                   #{processors => [{simple, #{exporter =>
                        {custom_exporter, #{configuration_resolved => false}}}}]})).

%% Check atom safety once across the distinct name-resolution paths, including
%% decoding JSON and the native unknown-property error path.
does_not_create_atoms(_Config) ->
    Name = <<"unregistered_configuration_name_",
             (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    ?assertException(error, badarg, binary_to_existing_atom(Name, utf8)),
    ?assertEqual({error, {invalid_configuration, [Name], unknown_property}},
                 resolve_native(#{Name => true})),
    foreach_case(
      fun({Input, Expected}) ->
              {ok, Parsed} = otel_configuration_source:parse_binary(
                               iolist_to_binary(json:encode(Input#{file_format => <<"1.1">>}))),
              Result = otel_configuration_test_utils:resolve_declarative(Parsed),
              case Expected of
                  ok -> ?assertMatch({ok, _}, Result);
                  _ -> ?assertEqual(Expected, Result)
              end
      end, [{#{Name => true}, ok},
            {provider(#{processors => [#{batch => #{exporter => #{console => #{}}, Name => null}}]}), ok},
            {provider(#{processors => [#{Name => #{}}]}),
             {error, {unsupported_configuration, [tracer_provider, processors], Name}}},
            {exporter(Name, #{}),
             {error, {unsupported_configuration, [tracer_provider, processors, exporter], Name}}},
            {#{distribution => #{erlang => #{resource_detectors => [Name]}}},
             {error, {unsupported_configuration, [distribution, erlang, resource_detectors], [Name]}}}]),
    ?assertException(error, badarg, binary_to_existing_atom(Name, utf8)).
