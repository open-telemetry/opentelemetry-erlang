-module(otel_declarative_configuration_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

-import(otel_configuration_test_utils, [resource_attributes/1, configured_resource/1,
                                        add_warning_handler/2, warnings/0, foreach_case/2]).

all() ->
    [declarative_defaults_are_authoritative,
     resolves_trace_configuration,
     normalizes_exporter_enums,
     component_forms_are_equivalent,
     normalizes_unicode_configuration,
     client_tls_uses_system_trust,
     warns_about_empty_http_endpoint_paths,
     preserves_resource_attribute_types,
     supports_atom_keys,
     applies_null_limit_defaults,
     null_defaults_are_preserved,
     undefined_is_not_a_null_alias,
     distinguishes_missing_and_null_limits,
     decodes_key_value_list_escaping,
     rejects_invalid_key_value_list_escaping,
     preserves_model_semantics,
     resolves_builtin_processor_aliases,
     resolves_console_exporter,
     rejects_invalid_console_exporter_settings,
     rejects_invalid_builtin_processor_settings,
     rejects_unknown_builtin_processor_settings,
     resolves_erlang_application_extensions,
     validates_erlang_distribution_values,
     rejects_json_erlang_names,
     resolves_standalone_tracer_provider,
     resolves_otlp_module_options,
     standalone_provider_limits_warn,
     resolves_ratio_sampler_forms,
     rejects_invalid_ratio_sampler_options,
     normalizes_json_sweeper_settings,
     configured_resource_takes_precedence,
     warns_about_unimplemented_settings,
     rejects_invalid_unimplemented_settings,
     rejects_unsupported_configuration].

declarative_defaults_are_authoritative(_Config) ->
    Input = #{<<"file_format">> => <<"1.1">>},
    {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(
                       Input),

    ?assertEqual([], otel_configuration_sdk:text_map_propagators(Resolved)),
    ?assertEqual(undefined,
                 otel_configuration_sdk:value(tracer_provider,
                                              Resolved,
                                              undefined)),
    ?assertEqual(undefined, otel_configuration_sdk:resource(Resolved)).

resolves_trace_configuration(_Config) ->
    Input =
        #{<<"file_format">> => <<"1.1">>,
          <<"disabled">> => true,
          <<"log_level">> => <<"debug3">>,
          <<"resource">> =>
              #{<<"attributes_list">> => <<"service.name=from-list,region=ca">>,
                <<"attributes">> =>
                    [#{<<"name">> => <<"service.name">>,
                       <<"value">> => <<"configured">>},
                     #{<<"name">> => <<"ignored">>, <<"value">> => null}],
                <<"schema_url">> => <<"https://opentelemetry.io/schemas/1.27.0">>},
          <<"attribute_limits">> =>
              #{<<"attribute_count_limit">> => 64,
                <<"attribute_value_length_limit">> => 99},
          <<"propagator">> =>
              #{<<"composite">> => [#{<<"tracecontext">> => null}],
                <<"composite_list">> => <<"baggage,tracecontext,b3">>},
          <<"tracer_provider">> =>
              #{<<"processors">> =>
                    [#{<<"simple">> =>
                           #{<<"exporter">> =>
                                 #{<<"otlp_http">> =>
                                       #{<<"endpoint">> => <<"https://collector/v1/traces">>,
                                         <<"headers_list">> => <<"low=one,shared=list">>,
                                         <<"headers">> =>
                                             [#{<<"name">> => <<"shared">>,
                                                <<"value">> => <<"explicit">>}],
                                         <<"compression">> => <<"gzip">>,
                                         <<"tls">> =>
                                             #{<<"ca_file">> => <<"/tmp/ca.pem">>}}}}},
                     #{<<"batch">> =>
                           #{<<"schedule_delay">> => 25,
                             <<"export_timeout">> => 50,
                             <<"max_queue_size">> => 100,
                             <<"exporter">> => #{<<"otlp_grpc">> => null}}}],
                <<"limits">> =>
                    #{<<"attribute_count_limit">> => 7,
                      <<"attribute_value_length_limit">> => null,
                      <<"event_count_limit">> => 8,
                      <<"link_count_limit">> => 9,
                      <<"event_attribute_count_limit">> => 10,
                      <<"link_attribute_count_limit">> => 11},
                <<"sampler">> =>
                    #{<<"parent_based">> =>
                          #{<<"root">> =>
                                #{<<"trace_id_ratio_based">> => #{<<"ratio">> => 0.25}},
                            <<"remote_parent_not_sampled">> =>
                                #{<<"always_off">> => null}}},
                <<"id_generator">> => #{<<"random">> => null}}},

    {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(Input),

    ?assertEqual([trace_context, baggage, b3],
                 otel_configuration_sdk:text_map_propagators(Resolved)),
    TracerProvider = #{} = otel_configuration_sdk:tracer_provider(Resolved),
    ?assertEqual({parent_based,
                  #{root => {trace_id_ratio_based, 0.25},
                    remote_parent_not_sampled => always_off}},
                 otel_configuration_sdk:sampler(TracerProvider)),
    ?assertEqual(otel_id_generator,
                 otel_configuration_sdk:id_generator(TracerProvider)),
    ?assertEqual(#{attribute_count_limit => 7,
                   attribute_value_length_limit => infinity,
                   event_count_limit => 8,
                   link_count_limit => 9,
                   event_attribute_count_limit => 10,
                   link_attribute_count_limit => 11},
                 otel_configuration_sdk:span_limits(Resolved)),
    [Simple, Batch] = otel_configuration_sdk:span_processors(TracerProvider),
    {otel_simple_processor, SimpleConfig} =
        otel_configuration_sdk:span_processor_component(Simple),
    SimpleExporterComponent = {opentelemetry_exporter, _} =
        otel_configuration_sdk:span_exporter_component(SimpleConfig),
    {opentelemetry_exporter, _} =
        otel_configuration_sdk:span_exporter(SimpleExporterComponent),
    {opentelemetry_exporter, #{} = SimpleExporterOptions} = SimpleExporterComponent,
    ?assertMatch(#{endpoints := [<<"https://collector/v1/traces">>],
                   protocol := http_protobuf,
                   compression := gzip}, SimpleExporterOptions),
    {otel_batch_processor, BatchConfig} =
        otel_configuration_sdk:span_processor_component(Batch),
    ?assertMatch(#{schedule_delay := 25,
                   export_timeout := 50,
                   max_queue_size := 100}, BatchConfig),

    Resource = configured_resource(Resolved),
    ?assertEqual(<<"https://opentelemetry.io/schemas/1.27.0">>,
                 otel_resource:schema_url(Resource)),
    ?assertMatch(#{'service.name' := <<"configured">>, region := <<"ca">>},
                 resource_attributes(Resource)).

client_tls_uses_system_trust(_Config) ->
    {ok, StartedApps} = application:ensure_all_started(tls_certificate_check),
    try
        check_client_tls_uses_system_trust()
    after
        [application:stop(App) || App <- lists:reverse(StartedApps)]
    end.

check_client_tls_uses_system_trust() ->
    KeyFile = "/tmp/client-key.pem",
    CertFile = "/tmp/client-cert.pem",
    {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(
        #{file_format => <<"1.1">>,
          tracer_provider => #{processors => [#{simple => #{exporter =>
              #{otlp_http => #{endpoint => <<"https://collector/v1/traces">>,
                               tls => #{key_file => KeyFile,
                                        cert_file => CertFile}}}}}]}}),
    TracerProvider = #{} = otel_configuration_sdk:tracer_provider(Resolved),
    [Processor] = otel_configuration_sdk:span_processors(TracerProvider),
    {otel_simple_processor, ProcessorConfig} =
        otel_configuration_sdk:span_processor_component(Processor),
    ExporterComponent = {opentelemetry_exporter, _} =
        otel_configuration_sdk:span_exporter_component(ProcessorConfig),
    {opentelemetry_exporter, _} =
        otel_configuration_sdk:span_exporter(ExporterComponent),
    {opentelemetry_exporter, #{} = ExporterOptions} = ExporterComponent,
    Endpoints = [<<"https://collector/v1/traces">>],
    SSLOptions = {system_defaults, [{keyfile, KeyFile}, {certfile, CertFile}]},
    #{endpoints := Endpoints, ssl_options := SSLOptions} = ExporterOptions,

    [#{ssl_options := MergedOptions}] =
        otel_exporter_otlp:endpoints(Endpoints, SSLOptions),
    ?assertEqual(KeyFile, proplists:get_value(keyfile, MergedOptions)),
    ?assertEqual(CertFile, proplists:get_value(certfile, MergedOptions)),
    SystemDefaults = tls_certificate_check:options("collector"),
    ?assert(lists:all(fun(Option) -> lists:member(Option, MergedOptions) end,
                      SystemDefaults)).

supports_atom_keys(_Config) ->
    {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(
                       #{file_format => "1.1",
                         propagator => #{composite => [#{tracecontext => null}]},
                         tracer_provider =>
                             #{processors =>
                                   [#{simple =>
                                          #{exporter => #{otlp_http => null}}}],
                               sampler => #{always_off => null}}}),

    ?assertEqual([trace_context],
                 otel_configuration_sdk:text_map_propagators(Resolved)),
    TracerProvider = #{} = otel_configuration_sdk:tracer_provider(Resolved),
    ?assertEqual(always_off, otel_configuration_sdk:sampler(TracerProvider)),
    [Processor] = otel_configuration_sdk:span_processors(TracerProvider),
    {otel_simple_processor, ProcessorConfig} =
        otel_configuration_sdk:span_processor_component(Processor),
    ExporterComponent = {opentelemetry_exporter, _} =
        otel_configuration_sdk:span_exporter_component(ProcessorConfig),
    {opentelemetry_exporter, _} =
        otel_configuration_sdk:span_exporter(ExporterComponent),
    {opentelemetry_exporter, #{} = ExporterOptions} = ExporterComponent,
    ?assertMatch(#{protocol := http_protobuf,
                   endpoints := [<<"http://localhost:4318/v1/traces">>]},
                 ExporterOptions).

applies_null_limit_defaults(_Config) ->
    {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(
                       #{<<"file_format">> => <<"1.1">>,
                         <<"attribute_limits">> =>
                             #{<<"attribute_count_limit">> => 42,
                               <<"attribute_value_length_limit">> => 12},
                         <<"tracer_provider">> =>
                             #{<<"processors">> =>
                                   [#{<<"batch">> =>
                                          #{<<"export_timeout">> => 0,
                                            <<"exporter">> =>
                                                #{<<"otlp_http">> => null}}}],
                               <<"limits">> =>
                                   #{<<"attribute_count_limit">> => null,
                                     <<"attribute_value_length_limit">> => null}}}),

    ?assertMatch(#{attribute_count_limit := 128,
                   attribute_value_length_limit := infinity},
                 otel_configuration_sdk:span_limits(Resolved)),
    TracerProvider = #{} = otel_configuration_sdk:tracer_provider(Resolved),
    [Processor] = otel_configuration_sdk:span_processors(TracerProvider),
    {otel_batch_processor, BatchConfig} =
        otel_configuration_sdk:span_processor_component(Processor),
    ?assertEqual(0, otel_configuration_sdk:value(export_timeout, BatchConfig, undefined)).

distinguishes_missing_and_null_limits(_Config) ->
    Global = #{attribute_count_limit => 64, attribute_value_length_limit => 12},
    lists:foreach(
      fun({Limits, Count, Length}) ->
              Native = [{attribute_limits, Global},
                        {tracer_provider, #{processors => [], limits => Limits}}],
              {ok, Model} = otel_configuration_model:from_application_env(Native),
              {ok, Runtime} = otel_configuration_sdk:create(Model),
              ?assertMatch(#{attribute_count_limit := Count,
                             attribute_value_length_limit := Length},
                           otel_configuration_sdk:span_limits(Runtime))
      end, [{#{}, 64, 12},
            {null, 64, 12},
            {#{attribute_count_limit => null, attribute_value_length_limit => null},
             128, infinity},
            {#{attribute_count_limit => 0, attribute_value_length_limit => 0}, 0, 0}]),
    %% JSON keys have the same omission/null semantics as native atom keys.
    lists:foreach(
      fun({Limits, Count, Length}) ->
              {ok, Runtime} = otel_configuration_test_utils:resolve_declarative(
                                #{<<"file_format">> => <<"1.1">>,
                                  <<"attribute_limits">> =>
                                      #{<<"attribute_count_limit">> => 64,
                                        <<"attribute_value_length_limit">> => 12},
                                  <<"tracer_provider">> =>
                                      #{<<"processors">> => [], <<"limits">> => Limits}}),
              ?assertMatch(#{attribute_count_limit := Count,
                             attribute_value_length_limit := Length},
                           otel_configuration_sdk:span_limits(Runtime))
      end, [{#{}, 64, 12},
            {null, 64, 12},
            {#{<<"attribute_count_limit">> => null, <<"attribute_value_length_limit">> => null},
             128, infinity},
            {#{<<"attribute_count_limit">> => 0, <<"attribute_value_length_limit">> => 0}, 0, 0}]).

decodes_key_value_list_escaping(_Config) ->
    {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(
        #{file_format => <<"1.1">>,
          resource =>
              #{attributes_list =>
                    <<"resource%2Ckey=resource%3Dvalue,percent=100%25,shared=from-list">>,
                attributes =>
                    [#{name => <<"shared">>, value => <<"explicit">>}]},
          tracer_provider =>
              #{processors =>
                    [#{simple =>
                           #{exporter =>
                                 #{otlp_http =>
                                       #{headers_list =>
                                             <<"header%2Ckey=header%3Dvalue,shared=from-list">>,
                                         headers =>
                                             [#{name => <<"shared">>,
                                                value => <<"explicit">>}]}}}}]}}),

    Resource = configured_resource(Resolved),
    ?assertMatch(#{'resource,key' := <<"resource=value">>,
                   percent := <<"100%">>,
                   shared := <<"explicit">>},
                 resource_attributes(Resource)),
    TracerProvider = #{} = otel_configuration_sdk:tracer_provider(Resolved),
    [Processor] = otel_configuration_sdk:span_processors(TracerProvider),
    {otel_simple_processor, ProcessorConfig} =
        otel_configuration_sdk:span_processor_component(Processor),
    ExporterComponent = {opentelemetry_exporter, _} =
        otel_configuration_sdk:span_exporter_component(ProcessorConfig),
    {opentelemetry_exporter, _} =
        otel_configuration_sdk:span_exporter(ExporterComponent),
    {opentelemetry_exporter, #{} = ExporterOptions} = ExporterComponent,
    ?assertEqual([{<<"header,key">>, <<"header=value">>},
                  {<<"shared">>, <<"explicit">>}],
                 maps:get(headers, ExporterOptions)).

rejects_invalid_key_value_list_escaping(_Config) ->
    ?assertMatch(
       {error, {invalid_configuration, [resource, attributes_list], _}},
       otel_configuration_test_utils:resolve_declarative(
         #{file_format => <<"1.1">>,
           resource => #{attributes_list => <<"valid=value,broken=%">>}})),
    ?assertMatch(
       {error, {invalid_configuration, [exporter, otlp_http, headers_list], _}},
       otel_configuration_test_utils:resolve_declarative(
         #{file_format => <<"1.1">>,
           tracer_provider =>
               #{processors =>
                     [#{simple =>
                            #{exporter =>
                                  #{otlp_http =>
                                        #{headers_list => <<"broken=%GG">>}}}}]}})).

preserves_model_semantics(_Config) ->
    Unknown = <<"model_unknown_",
                (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Configuration = #{<<"file_format">> => <<"1.1">>,
                      <<"propagator">> => null,
                      Unknown => #{<<"value">> => true}},
    {ok, Runtime} = otel_configuration_test_utils:resolve_declarative(Configuration),
    Root = otel_configuration_model:root(otel_configuration_sdk:source(Runtime)),
    ?assertEqual(Configuration, Root),
    ?assertNot(maps:is_key(<<"tracer_provider">>, Root)).

resolves_builtin_processor_aliases(_Config) ->
    {ok, Model} = otel_configuration_model:from_application_env(
                    [{tracer_provider,
                      #{processors =>
                            [{batch, #{exporter => none}},
                             {simple, #{exporter => none}},
                             {otel_batch_processor, #{exporter => none}},
                             {otel_simple_processor, #{exporter => none}}]}}]),
    {ok, Resolved} = otel_configuration_sdk:create(Model),
    TracerProvider = #{} = otel_configuration_sdk:tracer_provider(Resolved),
    Processors = otel_configuration_sdk:span_processors(TracerProvider),
    ?assertEqual([otel_batch_processor,
                  otel_simple_processor,
                  otel_batch_processor,
                  otel_simple_processor],
                 [Module || {Module, _} <- Processors]).

resolves_console_exporter(_Config) ->
    Expected = {otel_exporter_stdout, #{}},
    foreach_case(
      fun({Source, Kind, Component}) ->
              Provider = #{processors => [{Kind, #{exporter => Component}}]},
              {ok, Model} = case Source of
                                native -> otel_configuration_model:from_application_env(
                                            [{tracer_provider, Provider}]);
                                json -> otel_configuration_model:from_map(
                                          #{<<"file_format">> => <<"1.1">>,
                                            <<"tracer_provider">> =>
                                                #{<<"processors">> =>
                                                    [#{atom_to_binary(Kind, utf8) =>
                                                        #{<<"exporter">> => Component}}]}})
                            end,
              {ok, Runtime} = otel_configuration_sdk:create(Model),
              #{processors := [{_, #{exporter := Exporter}}]} =
                  otel_configuration_sdk:tracer_provider(Runtime),
              ?assertEqual(Expected, Exporter)
      end, [{native, simple, {console, #{}}},
            {native, batch, {console, null}},
            {native, simple, #{console => null}},
            {native, batch, #{console => #{}}},
            {json, simple, #{<<"console">> => #{}}},
            {json, batch, #{<<"console">> => null}}]),
    ?assertEqual({otel_exporter_stdout, []}, otel_exporter:init(Expected)).

rejects_invalid_console_exporter_settings(_Config) ->
    lists:foreach(
      fun(Options) ->
              Reason = case Options of
                           Map when is_map(Map) ->
                               [{Key, _}] = maps:to_list(Map),
                               {invalid_configuration, [exporter, console, Key], unknown_property};
                           _ -> {invalid_configuration, [exporter, console], Options}
                       end,
              lists:foreach(
                fun(Component) ->
                        ?assertEqual({error, Reason},
                                     otel_configuration_sdk:create_tracer_provider(
                                       #{processors => [{simple, #{exporter => Component}}]}))
                end, [{console, Options}, #{console => Options}, #{<<"console">> => Options}])
      end, [false, 1, <<"stdout">>, [], #{unsupported => true}, #{<<"unsupported">> => true}]).

rejects_invalid_builtin_processor_settings(_Config) ->
    assert_invalid_processor_setting(batch, schedule_delay, -1),
    assert_invalid_processor_setting(batch, schedule_delay, <<"1">>),
    assert_invalid_processor_setting(batch, export_timeout, -1),
    assert_invalid_processor_setting(simple, export_timeout, -1),
    assert_invalid_processor_setting(batch, max_queue_size, 0),
    assert_invalid_processor_setting(batch, check_table_size, -1),

    {ok, Model} = otel_configuration_model:from_application_env(
                    [{tracer_provider,
                      #{processors =>
                            [{batch,
                              #{exporter => none,
                                schedule_delay => 0,
                                export_timeout => 0,
                                max_queue_size => 1,
                                check_table_size => infinity}}]}}]),
    ?assertMatch({ok, _}, otel_configuration_sdk:create(Model)).

rejects_unknown_builtin_processor_settings(_Config) ->
    %% Cover each renamed key and each component representation without taking
    %% the Cartesian product of independent normalization rules.
    foreach_case(
      fun({Kind, Key, Component}) ->
              ?assertEqual({error, {invalid_configuration,
                                   [tracer_provider, processors, Kind, Key], unknown_property}},
                           otel_configuration_sdk:create_tracer_provider(#{processors => [Component]}))
      end, [{batch, scheduled_delay_ms, {batch, #{exporter => none, scheduled_delay_ms => 10}}},
            {batch, exporting_timeout_ms, {otel_batch_processor, #{exporter => none, exporting_timeout_ms => 10}}},
            {batch, <<"check_table_size_ms">>, #{batch => #{exporter => none, <<"check_table_size_ms">> => 10}}},
            {batch, misspelled_option, #{otel_batch_processor => #{exporter => none, misspelled_option => 10}}},
            {simple, <<"scheduled_delay_ms">>, {simple, #{exporter => none, <<"scheduled_delay_ms">> => 10}}},
            {simple, exporting_timeout_ms, {otel_simple_processor, #{exporter => none, exporting_timeout_ms => 10}}},
            {simple, check_table_size_ms, #{simple => #{exporter => none, check_table_size_ms => 10}}},
            {simple, misspelled_option, #{otel_simple_processor => #{exporter => none, misspelled_option => 10}}}]),
    %% Valid batch-only settings must not silently pass through simple.
    ?assertEqual({error, {invalid_configuration,
                         [tracer_provider, processors, simple, schedule_delay], unknown_property}},
                 otel_configuration_sdk:create_tracer_provider(
                   #{processors => [{simple, #{exporter => none, schedule_delay => 10}}]})),
    Unknown = <<"unknown_processor_option_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    %% JSON unknown properties warn, while native unknown properties above fail.
    ?assertMatch({ok, _},
                 otel_configuration_test_utils:resolve_declarative(
                   #{<<"file_format">> => <<"1.1">>,
                     <<"tracer_provider">> =>
                         #{<<"processors">> =>
                               [#{<<"batch">> => #{Unknown => null,
                                                   <<"exporter">> => #{<<"otlp_http">> => null}}}]}})),
    %% Third-party processors define their own properties, including these names.
    Custom = #{scheduled_delay_ms => 10, Unknown => null},
    ?assertMatch({ok, #{processors := [{custom_span_processor, Custom},
                                      {custom_span_processor, Custom}]}},
                 otel_configuration_sdk:create_tracer_provider(
                   #{processors => [{custom_span_processor, Custom},
                                    #{custom_span_processor => Custom}]})).

assert_invalid_processor_setting(Kind, Key, Value) ->
    {ok, Model} = otel_configuration_model:from_application_env(
                    [{tracer_provider,
                      #{processors =>
                            [{Kind, #{exporter => none, Key => Value}}]}}]),
    ?assertEqual({error,
                  {invalid_configuration,
                   [tracer_provider, processors, Kind, Key], Value}},
                 otel_configuration_sdk:create(Model)).

resolves_erlang_application_extensions(_Config) ->
    {ok, Model} = otel_configuration_model:from_application_env(
                    [{distribution,
                      #{erlang =>
                            #{create_application_tracers => false,
                              deny_list => [kernel, {stdlib, "1.0"}],
                              resource_detectors => [otel_resource_env_var,
                                                     {otel_resource_detector_test, custom_options}],
                              resource_detector_timeout => 123,
                              sweeper => #{interval => 10}}}},
                     {propagator,
                      #{composite => [custom_text_map_propagator]}},
                     {tracer_provider,
                      #{processors =>
                            [{custom_span_processor, #{custom => value}}],
                        sampler => {custom_sampler, #{sample => true}},
                        id_generator => custom_id_generator}}]),
    {ok, Resolved} = otel_configuration_sdk:create(Model),
    Erlang = otel_configuration_sdk:erlang_distribution(Resolved),
    ?assertMatch(#{create_application_tracers := false,
                   deny_list := [kernel, {stdlib, "1.0"}],
                   resource_detectors := [otel_resource_env_var,
                                          {otel_resource_detector_test, custom_options}],
                   resource_detector_timeout := 123,
                   sweeper := #{interval := 10}}, Erlang),
    ?assertEqual([custom_text_map_propagator],
                 otel_configuration_sdk:text_map_propagators(Resolved)),
    TracerProvider = #{} = otel_configuration_sdk:tracer_provider(Resolved),
    ?assertEqual([kernel, {stdlib, "1.0"}], maps:get(deny_list, TracerProvider)),
    [Processor] = otel_configuration_sdk:span_processors(TracerProvider),
    ?assertEqual({custom_span_processor, #{custom => value}},
                 otel_configuration_sdk:span_processor_component(Processor)),
    ?assertEqual({custom_sampler, #{sample => true}},
                 otel_configuration_sdk:sampler(TracerProvider)),
    ?assertEqual(custom_id_generator,
                 otel_configuration_sdk:id_generator(TracerProvider)).

validates_erlang_distribution_values(_Config) ->
    lists:foreach(
      fun(Bool) ->
              {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(
                  #{<<"file_format">> => <<"1.1">>,
                    <<"distribution">> => #{<<"erlang">> =>
                        #{<<"create_application_tracers">> => Bool,
                          <<"resource_detector_timeout">> => 0,
                          <<"resource_detectors">> => [], <<"deny_list">> => []}}}),
              ?assertEqual(#{create_application_tracers => Bool,
                             resource_detector_timeout => 0,
                             resource_detectors => [], deny_list => []},
                           otel_configuration_sdk:erlang_distribution(Resolved))
      end, [true, false]),
    lists:foreach(
      fun({Key, Value}) ->
              {ok, Native} = otel_configuration_model:from_application_env(
                              [{distribution, #{erlang => #{Key => Value}}}]),
              {ok, Json} = otel_configuration_model:from_map(
                            #{<<"file_format">> => <<"1.1">>,
                              <<"distribution">> => #{<<"erlang">> =>
                                  #{atom_to_binary(Key, utf8) => Value}}}),
              lists:foreach(
                fun(Model) ->
                        ?assertEqual({error, {invalid_configuration, [distribution, erlang, Key], Value}},
                                     otel_configuration_sdk:create(Model))
                end, [Native, Json])
      end, [{create_application_tracers, <<"false">>},
            {create_application_tracers, "false"}, {create_application_tracers, 1},
            {resource_detector_timeout, <<"5000">>},
            {resource_detector_timeout, -1}, {resource_detector_timeout, 1.5},
            {resource_detectors, <<"otel_resource_env_var">>}, {deny_list, <<"kernel">>}]),
    lists:foreach(
      fun({Key, Entry}) ->
              {ok, Model} = otel_configuration_model:from_application_env(
                              [{distribution, #{erlang => #{Key => [Entry]}}}]),
              ?assertEqual({error, {invalid_configuration, [distribution, erlang, Key], Entry}},
                           otel_configuration_sdk:create(Model))
      end, [{resource_detectors, <<"otel_resource_env_var">>},
            {resource_detectors, {<<"otel_resource_env_var">>, #{}}},
            {resource_detectors, #{}}, {deny_list, <<"kernel">>},
            {deny_list, {kernel, <<"1.0">>}}, {deny_list, {kernel, [bad_version]}}]),
    {ok, Nulls} = otel_configuration_test_utils:resolve_declarative(
                   #{file_format => <<"1.1">>, distribution => #{erlang =>
                       #{create_application_tracers => null, resource_detector_timeout => null,
                         resource_detectors => null, deny_list => null}}}),
    ?assertEqual(#{}, otel_configuration_sdk:erlang_distribution(Nulls)).

rejects_json_erlang_names(_Config) ->
    Unknown = <<"unknown_detector_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    lists:foreach(
      fun({Key, Value}) ->
              ?assertEqual({error, {unsupported_configuration, [distribution, erlang, Key], Value}},
                           otel_configuration_test_utils:resolve_declarative(
                             #{<<"file_format">> => <<"1.1">>,
                               <<"distribution">> => #{<<"erlang">> =>
                                   #{atom_to_binary(Key, utf8) => Value}}}))
      end, [{resource_detectors, [<<"otel_resource_env_var">>]},
            {resource_detectors, [Unknown]}, {deny_list, [<<"kernel">>]}]).

resolves_standalone_tracer_provider(_Config) ->
    Native = #{processors => [{batch, #{exporter => {otlp_http, #{}}}}]},
    {ok, Model} = otel_configuration_model:from_application_env(
                    [{tracer_provider, Native}]),
    {ok, Runtime} = otel_configuration_sdk:create(Model),
    {ok, Provider} = otel_configuration_sdk:create_tracer_provider(Native),
    ?assertEqual(otel_configuration_sdk:tracer_provider(Runtime), Provider),
    ?assertMatch(#{processors :=
                      [{otel_batch_processor,
                        #{exporter := {opentelemetry_exporter,
                                       #{protocol := http_protobuf,
                                         configuration_resolved := true}}}}],
                   sampler := {parent_based, #{root := always_on}},
                   id_generator := otel_id_generator,
                   deny_list := []}, Provider),
    ?assertEqual({error, {invalid_configuration,
                         [tracer_provider, processors], missing}},
                 otel_configuration_sdk:create_tracer_provider(#{})).

resolves_ratio_sampler_forms(_Config) ->
    foreach_case(
      fun({Form, Ratio}) ->
              ?assertMatch({ok, #{sampler := {trace_id_ratio_based, Ratio}}},
                           otel_configuration_sdk:create_tracer_provider(
                             #{processors => [], sampler => Form}))
      end, [{{trace_id_ratio_based, 0.25}, 0.25},
            {{trace_id_ratio_based, 1}, 1.0},
            {{trace_id_ratio_based, #{ratio => 0.25}}, 0.25},
            {#{trace_id_ratio_based => #{<<"ratio">> => 0.25}}, 0.25},
            {{trace_id_ratio_based, #{}}, 1.0},
            {#{trace_id_ratio_based => #{ratio => null}}, 1.0}]),
    ?assertMatch({ok, #{sampler := {parent_based,
                                   #{root := {trace_id_ratio_based, 0.25}}}}},
                 otel_configuration_sdk:create_tracer_provider(
                   #{processors => [],
                     sampler => {parent_based,
                                 #{root => {trace_id_ratio_based, #{ratio => 0.25}}}}})).

rejects_invalid_ratio_sampler_options(_Config) ->
    lists:foreach(
      fun({Options, Key, Reason}) ->
              lists:foreach(
                fun(Form) ->
                        ?assertEqual(
                           {error, {invalid_configuration,
                                    [tracer_provider, sampler, trace_id_ratio_based, Key], Reason}},
                           otel_configuration_sdk:create_tracer_provider(
                             #{processors => [], sampler => Form}))
                end, [{trace_id_ratio_based, Options}, #{trace_id_ratio_based => Options}])
      end, [{#{ratio => <<"0.25">>}, ratio, <<"0.25">>},
            {#{ration => 0.25}, ration, unknown_property}]).

normalizes_json_sweeper_settings(_Config) ->
    {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(
        #{<<"file_format">> => <<"1.1">>,
          <<"distribution">> =>
              #{<<"erlang">> =>
                    #{<<"sweeper">> =>
                          #{<<"interval">> => 123,
                            <<"span_ttl">> => 456,
                            <<"storage_size">> => 789,
                            <<"strategy">> => <<"end_span">>}}}}),
    ?assertEqual(#{interval => 123,
                   span_ttl => 456,
                   storage_size => 789,
                   strategy => end_span},
                 otel_configuration_sdk:value(
                   sweeper,
                   otel_configuration_sdk:erlang_distribution(Resolved),
                   undefined)),

    ?assertMatch(
       {error,
        {invalid_configuration,
         [distribution, erlang, sweeper, interval], -1}},
       otel_configuration_test_utils:resolve_declarative(
         #{<<"file_format">> => <<"1.1">>,
           <<"distribution">> =>
               #{<<"erlang">> =>
                     #{<<"sweeper">> => #{<<"interval">> => -1}}}})).

preserves_resource_attribute_types(_Config) ->
    {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(
        #{file_format => <<"1.1">>, resource => #{attributes =>
            [#{name => <<"ints">>, type => <<"int_array">>, value => [65, 66]},
             #{name => <<"strings">>, type => <<"string_array">>,
               value => [<<"a">>, <<"b">>]},
             #{name => <<"bools">>, type => <<"bool_array">>, value => [true, false]},
             #{name => <<"doubles">>, type => <<"double_array">>, value => [1, 2.5]},
             #{name => <<"double">>, type => <<"double">>, value => 1}]}}),
    Resource = configured_resource(Resolved),
    Attributes = otel_resource:attributes(Resource),
    ?assertEqual(#{ints => [65, 66], strings => [<<"a">>, <<"b">>],
                   bools => [true, false], doubles => [1.0, 2.5], double => 1.0},
                 resource_attributes(Resource)),
    Proto = maps:from_list([{Key, Value} || #{key := Key, value := Value} <-
                                            otel_otlp_common:to_attributes(Attributes)]),
    ?assertMatch(#{<<"ints">> := #{value := {array_value, #{values :=
                       [#{value := {int_value, 65}}, #{value := {int_value, 66}}]}}},
                   <<"strings">> := #{value := {array_value, #{values :=
                       [#{value := {string_value, <<"a">>}},
                        #{value := {string_value, <<"b">>}}]}}},
                   <<"bools">> := #{value := {array_value, #{values :=
                       [#{value := {bool_value, true}}, #{value := {bool_value, false}}]}}},
                   <<"doubles">> := #{value := {array_value, #{values :=
                       [#{value := {double_value, 1.0}}, #{value := {double_value, 2.5}}]}}},
                   <<"double">> := #{value := {double_value, 1.0}}}, Proto),
    %% Legacy character lists continue to mean strings.
    ?assertEqual(#{ints => <<"AB">>, strings => <<"ab">>},
                 resource_attributes(otel_resource:create(
                     [{ints, [65, 66]}, {strings, [<<"a">>, <<"b">>]}]))).

configured_resource_takes_precedence(_Config) ->
    ok = application:load(opentelemetry),
    {ok, Model} = otel_configuration_model:from_application_env(
        [{resource,
          #{attributes =>
                #{<<"configured.resource">> => <<"configured">>,
                  <<"configured.only">> => <<"present">>}}},
         {distribution,
          #{erlang =>
                #{resource_detectors =>
                      [{otel_resource_detector_test,
                        {attributes,
                         [{<<"configured.resource">>, <<"detected">>},
                          {<<"detector.only">>, <<"present">>}]}}],
                  resource_detector_timeout => 100}}}]),
    {ok, RuntimeConfiguration} = otel_configuration_sdk:create(Model),
    {ok, Pid} = otel_resource_detector:start_link(
                  RuntimeConfiguration),
    try
        Resource = otel_resource_detector:get_resource(),
        ?assertMatch(#{'configured.resource' := <<"configured">>,
                       'configured.only' := <<"present">>,
                       'detector.only' := <<"present">>},
                     resource_attributes(Resource))
    after
        gen_statem:stop(Pid),
        application:unload(opentelemetry)
    end.

warns_about_empty_http_endpoint_paths(_Config) ->
    Handler = endpoint_warning_test,
    ok = add_warning_handler(Handler, otel_configuration_path),
    try
        lists:foreach(
          fun({Transport, Endpoint, ExpectedWarnings}) ->
                  lists:foreach(
                    fun(Component) ->
                            {ok, Provider} = otel_configuration_sdk:create_tracer_provider(
                                               #{processors => [{simple, #{exporter => Component}}]}),
                            [{otel_simple_processor,
                              #{exporter := {opentelemetry_exporter, Options = #{}}}}] =
                                otel_configuration_sdk:span_processors(Provider),
                            ?assertEqual([unicode:characters_to_binary(Endpoint)],
                                         maps:get(endpoints, Options)),
                            ?assertEqual(ExpectedWarnings, warnings())
                    end, [{Transport, #{endpoint => Endpoint}},
                          #{atom_to_binary(Transport, utf8) => #{<<"endpoint">> => Endpoint}}])
          end,
          [{otlp_http, <<"http://localhost:4318">>, [[exporter, otlp_http, endpoint]]},
           {otlp_http, "https://collector:4318?token=secret", [[exporter, otlp_http, endpoint]]},
           {otlp_http, <<"http://localhost:4318/v1/traces">>, []},
           {otlp_http, <<"https://collector/custom/traces">>, []},
           {otlp_http, <<"https://collector/">>, []},
           {otlp_grpc, <<"http://localhost:4317">>, []}]),
        ?assertMatch({ok, _}, otel_configuration_sdk:create_tracer_provider(
                               #{processors => [{batch, #{exporter => {otlp_http, #{}}}}]})),
        ?assertEqual([], warnings())
    after
        logger:remove_handler(Handler)
    end.

warns_about_unimplemented_settings(_Config) ->
    Handler = configuration_warning_test,
    ok = add_warning_handler(Handler, otel_configuration_path),
    try
        Input = #{file_format => <<"1.1">>,
                  log_level => <<"debug3">>,
                  attribute_limits => #{attribute_value_depth_limit => 64},
                  resource => #{'detection/development' => #{}},
                  logger_provider => #{processors => []},
                  meter_provider => #{readers => []},
                  tracer_provider =>
                      #{'tracer_configurator/development' => #{},
                        limits => #{attribute_value_depth_limit => 64},
                        processors =>
                            [{batch, #{max_export_batch_size => 512,
                                       exporter =>
                                           {otlp_grpc,
                                            #{endpoint => <<"https://collector:4317">>,
                                              timeout => 10000,
                                              max_request_size => 0,
                                              max_response_size => 1,
                                              tls => #{insecure => true}}}}}]}},
        {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(Input),
        ?assertNot(maps:is_key(log_level, Resolved)),
        #{processors := [{otel_batch_processor, Processor}]} =
            otel_configuration_sdk:tracer_provider(Resolved),
        ?assertNot(maps:is_key(max_export_batch_size, Processor)),
        ?assertMatch({opentelemetry_exporter,
                      #{protocol := grpc,
                        endpoints := [<<"https://collector:4317">>],
                        ssl_options := undefined}}, maps:get(exporter, Processor)),
        Expected = [[log_level], [logger_provider], [meter_provider],
                    [resource, 'detection/development'],
                    [attribute_limits, attribute_value_depth_limit],
                    [tracer_provider, limits, attribute_value_depth_limit],
                    [tracer_provider, 'tracer_configurator/development'],
                    [tracer_provider, processors, batch, max_export_batch_size],
                    [exporter, otlp_grpc, timeout],
                    [exporter, otlp_grpc, max_request_size],
                    [exporter, otlp_grpc, max_response_size],
                    [exporter, otlp_grpc, tls, insecure]],
        ?assertEqual(lists:sort(Expected), lists:sort(warnings())),
        %% Null properties retain default behavior without warning.
        {ok, _} = otel_configuration_test_utils:resolve_declarative(
                    #{file_format => <<"1.1">>, log_level => null, logger_provider => null,
                      meter_provider => null,
                      attribute_limits => #{attribute_value_depth_limit => null},
                      tracer_provider =>
                          #{limits => #{attribute_value_depth_limit => null},
                            processors =>
                                [{batch, #{max_export_batch_size => null,
                                           exporter =>
                                               {otlp_grpc, #{timeout => null,
                                                             tls => #{insecure => null}}}}}]}}),
        ?assertEqual([], warnings())
    after
        logger:remove_handler(Handler)
    end.

rejects_invalid_unimplemented_settings(_Config) ->
    Cases = [{[log_level], <<"invalid">>},
             {[attribute_limits, attribute_value_depth_limit], 0},
             {[tracer_provider, limits, attribute_value_depth_limit], -1},
             {[logger_provider], false},
             {[meter_provider], []}],
    lists:foreach(
      fun({Path, Value}) ->
              Input = maps:merge(#{file_format => <<"1.1">>}, nested_property(Path, Value)),
              ?assertEqual({error, {invalid_configuration, Path, Value}},
                           otel_configuration_test_utils:resolve_declarative(Input))
      end, Cases),
    lists:foreach(
      fun(Value) -> assert_invalid_processor_setting(batch, max_export_batch_size, Value) end,
      [0, -1, 1.5, <<"512">>]),
    lists:foreach(
      fun({Transport, Path, Value}) ->
              Config = nested_property(Path, Value),
              Input = #{processors => [{batch, #{exporter => {Transport, Config}}}]},
              ?assertEqual({error, {invalid_configuration,
                                   [exporter, Transport | Path], Value}},
                           otel_configuration_sdk:create_tracer_provider(Input))
      end,
      [{otlp_http, [timeout], -1},
       {otlp_grpc, [timeout], <<"10000">>},
       {otlp_http, [max_request_size], -1},
       {otlp_grpc, [max_response_size], 0},
       {otlp_grpc, [tls, insecure], <<"true">>}]).

nested_property([Key], Value) -> #{Key => Value};
nested_property([Key | Rest], Value) -> #{Key => nested_property(Rest, Value)}.

rejects_unsupported_configuration(_Config) ->
    ?assertEqual({error, {unsupported_file_format, <<"2.0">>}},
                 otel_configuration_test_utils:resolve_declarative(
                   #{<<"file_format">> => <<"2.0">>})),
    %% An unresolved component is still an error, not an ignored property.
    ?assertEqual({error, {unsupported_configuration,
                         [tracer_provider, processors], <<"not_registered">>}},
                 otel_configuration_test_utils:resolve_declarative(
                   #{<<"file_format">> => <<"1.1">>,
                     <<"tracer_provider">> =>
                         #{<<"processors">> => [#{<<"not_registered">> => #{}}]}})),
    ?assertEqual({error, {unsupported_configuration,
                         [tracer_provider, processors, exporter], <<"not_registered">>}},
                 otel_configuration_sdk:create_tracer_provider(
                   #{processors => [{batch, #{exporter => #{<<"not_registered">> => #{}}}}]})).

normalizes_exporter_enums(_Config) ->
    Provider = fun(Options) ->
                       otel_configuration_sdk:create_tracer_provider(
                         #{processors => [{simple, #{exporter => {otlp_http, Options}}}]})
               end,
    lists:foreach(
      fun(Encoding) ->
              lists:foreach(
                fun({Compression, Expected}) ->
                        ?assertMatch({ok, #{processors := [{otel_simple_processor,
                                      #{exporter := {opentelemetry_exporter,
                                                     #{compression := Expected}}}}]}},
                                     Provider(#{encoding => Encoding, compression => Compression}))
                end, [{none, undefined}, {<<"none">>, undefined}, {"none", undefined},
                      {gzip, gzip}, {<<"gzip">>, gzip}, {"gzip", gzip}])
      end, [protobuf, <<"protobuf">>, "protobuf"]),
    lists:foreach(
      fun({Key, Value}) ->
              ?assertEqual({error, {unsupported_configuration, [exporter, otlp_http, Key], Value}},
                           Provider(#{Key => Value}))
      end, [{encoding, json}, {encoding, #{}}, {compression, zstd}, {compression, 42}]).

component_forms_are_equivalent(_Config) ->
    lists:foreach(
      fun(Processor) ->
              lists:foreach(
                fun(Exporter) ->
                        Tuple = #{processors => [{Processor, #{exporter => {Exporter, #{}}}}],
                                  sampler => {parent_based, #{root => {trace_id_ratio_based, #{ratio => 0.25}}}},
                                  id_generator => {random, #{}}},
                        Map = #{processors => [#{Processor => #{exporter => #{Exporter => #{}}}}],
                                sampler => #{parent_based => #{root => #{trace_id_ratio_based => #{ratio => 0.25}}}},
                                id_generator => #{random => #{}}},
                        {ok, Provider} = otel_configuration_sdk:create_tracer_provider(Tuple),
                        ?assertEqual({ok, Provider}, otel_configuration_sdk:create_tracer_provider(Map))
                end, [otlp_http, otlp_grpc, console])
      end, [batch, simple]),
    lists:foreach(
      fun({Name, Expected}) ->
              lists:foreach(
                fun(Component) ->
                        {ok, Model} = otel_configuration_model:from_application_env(
                                        [{propagator, #{composite => [Component]}}]),
                        {ok, Runtime} = otel_configuration_sdk:create(Model),
                        ?assertEqual([Expected], otel_configuration_sdk:text_map_propagators(Runtime))
                end, [Name, {Name, #{}}, #{Name => #{}}])
      end, [{tracecontext, trace_context}, {trace_context, trace_context},
            {baggage, baggage}, {b3, b3}, {b3multi, b3multi}]),
    Custom = #{opaque => #{keep => <<"as-is">>}},
    lists:foreach(
      fun(Component) ->
              ?assertMatch({ok, #{processors := [{custom_processor, Custom}]}},
                           otel_configuration_sdk:create_tracer_provider(#{processors => [Component]}))
      end, [{custom_processor, Custom}, #{custom_processor => Custom}]).

normalizes_unicode_configuration(_Config) ->
    lists:foreach(
      fun(Invalid) ->
              ?assertEqual({error, {unsupported_file_format, Invalid}},
                           otel_configuration_model:from_map(#{file_format => Invalid})),
              ?assertEqual({error, {invalid_configuration, [], Invalid}},
                           otel_configuration_sdk:create_tracer_provider(
                             #{processors => [{simple, #{exporter =>
                                  {otlp_http, #{endpoint => Invalid}}}}]}))
      end, [<<255>>, [16#d800], [16#110000], [bad]]),
    CaFile = "/tmp/caf" ++ [16#e9] ++ ".pem",
    {ok, Provider} = otel_configuration_sdk:create_tracer_provider(
                      #{processors => [{simple, #{exporter =>
                           {otlp_http, #{tls => #{ca_file => CaFile}}}}}]}),
    ?assertMatch(#{processors := [{otel_simple_processor,
                      #{exporter := {opentelemetry_exporter,
                            #{ssl_options := [{cacertfile, CaFile}]}}}}]}, Provider),
    {ok, Model} = otel_configuration_model:from_application_env(
                    [{propagator, #{composite_list =>
                          <<" tracecontext , ,baggage ">>}}]),
    {ok, Runtime} = otel_configuration_sdk:create(Model),
    ?assertEqual([trace_context, baggage], otel_configuration_sdk:text_map_propagators(Runtime)),
    {ok, Unknown} = otel_configuration_model:from_application_env(
                     [{propagator, #{composite_list => <<" ",16#c3,16#a9," ">>}}]),
    ?assertEqual({error, {unsupported_configuration, [propagator, composite_list], <<16#c3,16#a9>>}},
                 otel_configuration_sdk:create(Unknown)).

standalone_provider_limits_warn(_Config) ->
    {ok, Expected} = otel_configuration_sdk:create_tracer_provider(#{processors => []}),
    Handler = standalone_limits_test,
    ok = add_warning_handler(Handler, otel_configuration_path),
    try
        ?assertEqual({ok, Expected}, otel_configuration_sdk:create_tracer_provider(
                                      #{processors => [], limits => #{attribute_count_limit => 1}})),
        ?assertEqual([[tracer_provider, limits]], warnings()),
        ?assertEqual({ok, Expected}, otel_configuration_sdk:create_tracer_provider(
                                      #{processors => [], limits => null})),
        ?assertEqual([], warnings())
    after
        logger:remove_handler(Handler)
    end.

null_defaults_are_preserved(_Config) ->
    Base = #{resource => #{}, propagator => #{},
             tracer_provider => #{processors =>
                 [{simple, #{exporter => {otlp_http, #{tls => #{}}}}}]}},
    WithNulls = #{resource => #{attributes_list => null, schema_url => null},
                  propagator => #{composite_list => null},
                  tracer_provider => #{processors =>
                      [{simple, #{exporter => {otlp_http,
                          #{headers_list => null,
                            tls => #{ca_file => null, key_file => null, cert_file => null}}}}}]}},
    Resolve = fun(Input) ->
                      {ok, Resolved} = otel_configuration_test_utils:resolve_declarative(
                                         Input#{file_format => <<"1.1">>}),
                      maps:remove(source, Resolved)
              end,
    ?assertEqual(Resolve(Base), Resolve(WithNulls)),
    %% A lone client key or certificate still fails, including when its partner
    %% is explicitly null rather than omitted.
    lists:foreach(
      fun(Tls) ->
              ?assertMatch({error, {invalid_configuration, [exporter, otlp_http, tls], _}},
                           otel_configuration_sdk:create_tracer_provider(
                             #{processors => [{simple, #{exporter => {otlp_http, #{tls => Tls}}}}]}))
      end, [#{key_file => <<"key.pem">>, cert_file => null},
            #{key_file => null, cert_file => <<"cert.pem">>}]).

undefined_is_not_a_null_alias(_Config) ->
    lists:foreach(
      fun({Input, Path}) ->
              {ok, Model} = otel_configuration_model:from_application_env(maps:to_list(Input)),
              ?assertEqual({error, {invalid_configuration, Path, undefined}},
                           otel_configuration_sdk:create(Model))
      end, [{#{disabled => undefined}, [disabled]},
            {#{meter_provider => undefined}, [meter_provider]},
            {#{tracer_provider => #{processors => [{simple, undefined}]}},
             [tracer_provider, processors, simple]},
            {#{distribution => #{erlang => #{sweeper => #{interval => undefined}}}},
             [distribution, erlang, sweeper, interval]}]).

%% Built-in module options use the same defaults and validation as aliases.
%% Environment isolation is checked at the actual exporter merge boundary.
resolves_otlp_module_options(_Config) ->
    Endpoint = <<"https://collector.example/v1/traces">>,
    foreach_case(
      fun({Component, Protocol, ExpectedEndpoint}) ->
              ?assertMatch({ok, #{processors := [{otel_simple_processor,
                                  #{exporter := {opentelemetry_exporter,
                                    #{endpoints := [ExpectedEndpoint], headers := [],
                                      protocol := Protocol, compression := undefined,
                                      ssl_options := undefined, configuration_resolved := true}}}}]}},
                           otel_configuration_sdk:create_tracer_provider(
                             #{processors => [{simple, #{exporter => Component}}]}))
      end, [{{opentelemetry_exporter, #{}}, http_protobuf, <<"http://localhost:4318/v1/traces">>},
            {#{opentelemetry_exporter => #{protocol => http_protobuf}}, http_protobuf,
             <<"http://localhost:4318/v1/traces">>},
            {{opentelemetry_exporter, #{protocol => grpc}}, grpc, <<"http://localhost:4317">>},
            {#{opentelemetry_exporter => #{endpoints => [Endpoint]}}, http_protobuf, Endpoint}]),
    foreach_case(
      fun({Component, Path, Reason}) ->
              ?assertEqual({error, {invalid_configuration, Path, Reason}},
                           otel_configuration_sdk:create_tracer_provider(
                             #{processors => [{simple, #{exporter => Component}}]}))
      end, [{{opentelemetry_exporter, null}, [tracer_provider, processors, exporter], null},
            {#{opentelemetry_exporter => #{protocol => invalid_protocol}},
             [tracer_provider, processors, exporter, protocol], invalid_protocol}]).
