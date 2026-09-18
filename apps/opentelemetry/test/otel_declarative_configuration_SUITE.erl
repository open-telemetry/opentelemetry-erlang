-module(otel_declarative_configuration_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [declarative_defaults_are_authoritative,
     resolves_trace_configuration,
     client_tls_uses_system_trust,
     preserves_resource_attribute_types,
     supports_atom_keys,
     applies_null_limit_defaults,
     decodes_key_value_list_escaping,
     rejects_invalid_key_value_list_escaping,
     preserves_model_semantics,
     resolves_erlang_application_extensions,
     configured_resource_takes_precedence,
     rejects_unsupported_configuration,
     ignores_unknown_top_level_properties_without_creating_atoms].

declarative_defaults_are_authoritative(_Config) ->
    Input = #{<<"file_format">> => <<"1.1">>},
    {ok, Resolved} = otel_configuration_declarative:resolve(
                       Input),

    ?assertEqual(Input,
                 otel_configuration_model:root(
                   otel_configuration_sdk:source(Resolved))),
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

    {ok, Resolved} = otel_configuration_declarative:resolve(Input),

    ?assertEqual(Input,
                 otel_configuration_model:root(
                   otel_configuration_sdk:source(Resolved))),
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
    SimpleExporterOptions =
        otel_configuration_sdk:otlp_exporter_options(SimpleExporterComponent),
    ?assertMatch(#{endpoints := [<<"https://collector/v1/traces">>],
                   protocol := http_protobuf,
                   compression := gzip}, SimpleExporterOptions),
    {otel_batch_processor, BatchConfig} =
        otel_configuration_sdk:span_processor_component(Batch),
    ?assertMatch(#{schedule_delay := 25,
                   export_timeout := 50,
                   max_queue_size := 100}, BatchConfig),

    Resource = eqwalizer:dynamic_cast(otel_configuration_sdk:resource(Resolved)),
    ?assertEqual(<<"https://opentelemetry.io/schemas/1.27.0">>,
                 otel_resource:schema_url(Resource)),
    ?assertMatch(#{'service.name' := <<"configured">>, region := <<"ca">>},
                 resource_attributes_map(Resource)).

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
    {ok, Resolved} = otel_configuration_declarative:resolve(
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
    ExporterOptions = otel_configuration_sdk:otlp_exporter_options(ExporterComponent),
    ?assertEqual({system_defaults,
                  [{keyfile, KeyFile}, {certfile, CertFile}]},
                 maps:get(ssl_options, ExporterOptions)),

    [#{ssl_options := MergedOptions}] =
        otel_exporter_otlp:endpoints(maps:get(endpoints, ExporterOptions),
                                     maps:get(ssl_options, ExporterOptions)),
    ?assertEqual(KeyFile, proplists:get_value(keyfile, MergedOptions)),
    ?assertEqual(CertFile, proplists:get_value(certfile, MergedOptions)),
    SystemDefaults = tls_certificate_check:options("collector"),
    ?assert(lists:all(fun(Option) -> lists:member(Option, MergedOptions) end,
                      SystemDefaults)).

supports_atom_keys(_Config) ->
    {ok, Resolved} = otel_configuration_declarative:resolve(
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
    ExporterOptions = otel_configuration_sdk:otlp_exporter_options(ExporterComponent),
    ?assertMatch(#{protocol := http_protobuf,
                   endpoints := [<<"http://localhost:4318/v1/traces">>]},
                 ExporterOptions).

applies_null_limit_defaults(_Config) ->
    {ok, Resolved} = otel_configuration_declarative:resolve(
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

decodes_key_value_list_escaping(_Config) ->
    {ok, Resolved} = otel_configuration_declarative:resolve(
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

    Resource = eqwalizer:dynamic_cast(otel_configuration_sdk:resource(Resolved)),
    ?assertMatch(#{'resource,key' := <<"resource=value">>,
                   percent := <<"100%">>,
                   shared := <<"explicit">>},
                 resource_attributes_map(Resource)),
    TracerProvider = #{} = otel_configuration_sdk:tracer_provider(Resolved),
    [Processor] = otel_configuration_sdk:span_processors(TracerProvider),
    {otel_simple_processor, ProcessorConfig} =
        otel_configuration_sdk:span_processor_component(Processor),
    ExporterComponent = {opentelemetry_exporter, _} =
        otel_configuration_sdk:span_exporter_component(ProcessorConfig),
    {opentelemetry_exporter, _} =
        otel_configuration_sdk:span_exporter(ExporterComponent),
    ExporterOptions = otel_configuration_sdk:otlp_exporter_options(ExporterComponent),
    ?assertEqual([{<<"header,key">>, <<"header=value">>},
                  {<<"shared">>, <<"explicit">>}],
                 maps:get(headers, ExporterOptions)).

rejects_invalid_key_value_list_escaping(_Config) ->
    ?assertMatch(
       {error, {invalid_configuration, [resource, attributes_list], _}},
       otel_configuration_declarative:resolve(
         #{file_format => <<"1.1">>,
           resource => #{attributes_list => <<"valid=value,broken=%">>}})),
    ?assertMatch(
       {error, {invalid_configuration, [exporter, otlp_http, headers_list], _}},
       otel_configuration_declarative:resolve(
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
    {ok, Model} = otel_configuration_declarative:parse(Configuration),
    Root = otel_configuration_model:root(Model),
    ?assertEqual(Configuration, Root),
    ?assertNot(maps:is_key(<<"tracer_provider">>, Root)),
    ?assertException(error, badarg, binary_to_existing_atom(Unknown, utf8)).

resolves_erlang_application_extensions(_Config) ->
    {ok, Model} = otel_configuration_model:from_application_env(
                    [{distribution,
                      #{erlang =>
                            #{create_application_tracers => false,
                              deny_list => [kernel],
                              resource_detectors => [otel_resource_env_var],
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
                   deny_list := [kernel],
                   resource_detectors := [otel_resource_env_var],
                   resource_detector_timeout := 123,
                   sweeper := #{interval := 10}}, Erlang),
    ?assertEqual([custom_text_map_propagator],
                 otel_configuration_sdk:text_map_propagators(Resolved)),
    TracerProvider = #{} = otel_configuration_sdk:tracer_provider(Resolved),
    [Processor] = otel_configuration_sdk:span_processors(TracerProvider),
    ?assertEqual({custom_span_processor, #{custom => value}},
                 otel_configuration_sdk:span_processor_component(Processor)),
    ?assertEqual({custom_sampler, #{sample => true}},
                 otel_configuration_sdk:sampler(TracerProvider)),
    ?assertEqual(custom_id_generator,
                 otel_configuration_sdk:id_generator(TracerProvider)).

preserves_resource_attribute_types(_Config) ->
    {ok, Resolved} = otel_configuration_declarative:resolve(
        #{file_format => <<"1.1">>, resource => #{attributes =>
            [#{name => <<"ints">>, type => <<"int_array">>, value => [65, 66]},
             #{name => <<"strings">>, type => <<"string_array">>,
               value => [<<"a">>, <<"b">>]},
             #{name => <<"bools">>, type => <<"bool_array">>, value => [true, false]},
             #{name => <<"doubles">>, type => <<"double_array">>, value => [1, 2.5]},
             #{name => <<"double">>, type => <<"double">>, value => 1}]}}),
    Resource = otel_configuration_sdk:resource(Resolved),
    Attributes = otel_resource:attributes(Resource),
    ?assertEqual(#{ints => [65, 66], strings => [<<"a">>, <<"b">>],
                   bools => [true, false], doubles => [1.0, 2.5], double => 1.0},
                 resource_attributes_map(Resource)),
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
                 resource_attributes_map(otel_resource:create(
                     eqwalizer:dynamic_cast(
                       [{ints, [65, 66]}, {strings, [<<"a">>, <<"b">>]}])))).

resource_attributes_map(Resource) ->
    case otel_resource:attributes(Resource) of
        undefined -> error(missing_resource);
        Attributes -> otel_attributes:map(Attributes)
    end.

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
                     resource_attributes_map(Resource))
    after
        gen_statem:stop(Pid),
        application:unload(opentelemetry)
    end.

rejects_unsupported_configuration(_Config) ->
    ?assertEqual({error, {unsupported_file_format, <<"2.0">>}},
                 otel_configuration_declarative:resolve(
                   #{<<"file_format">> => <<"2.0">>})),
    ?assertMatch({error, {unsupported_configuration, [logger_provider], _}},
                 otel_configuration_declarative:resolve(
                   #{<<"file_format">> => <<"1.1">>,
                     <<"logger_provider">> => #{<<"processors">> => []}})),
    ?assertMatch({error, {unsupported_configuration,
                          [tracer_provider, processors, batch, max_export_batch_size], 512}},
                 otel_configuration_declarative:resolve(
                   #{<<"file_format">> => <<"1.1">>,
                     <<"tracer_provider">> =>
                         #{<<"processors">> =>
                               [#{<<"batch">> =>
                                      #{<<"max_export_batch_size">> => 512,
                                        <<"exporter">> => #{<<"otlp_http">> => null}}}]}})).

ignores_unknown_top_level_properties_without_creating_atoms(_Config) ->
    Unknown = <<"declarative_unknown_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    ?assertException(error, badarg, binary_to_existing_atom(Unknown, utf8)),

    {ok, _} = otel_configuration_declarative:resolve(
                #{<<"file_format">> => <<"1.1">>, Unknown => #{}}),

    ?assertException(error, badarg, binary_to_existing_atom(Unknown, utf8)).
