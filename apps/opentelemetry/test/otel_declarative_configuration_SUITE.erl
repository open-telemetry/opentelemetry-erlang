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
     resolves_metric_reader,
     configured_resource_is_detector_base,
     rejects_unsupported_configuration,
     ignores_unknown_top_level_properties_without_creating_atoms].

declarative_defaults_are_authoritative(_Config) ->
    {ok, Resolved} = otel_configuration_declarative:resolve(
                       #{<<"file_format">> => <<"1.1">>}),

    ?assertMatch(#{configuration_source := declarative,
                   traces_enabled := false,
                   metrics_enabled := false,
                   resource := undefined,
                   resource_detectors := [],
                   text_map_propagators := [],
                   traces_exporter := none,
                   metrics_exporter := none,
                   processors := [],
                   readers := []},
                 Resolved),

    LegacyDefaults = otel_configuration:resolve(#{}),
    ?assertMatch(#{resource_detectors := [otel_resource_env_var, otel_resource_app_env],
                   text_map_propagators := [trace_context, baggage],
                   processors := [{otel_batch_processor, _}]},
                 LegacyDefaults).

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

    ?assertMatch(#{sdk_disabled := true,
                   traces_enabled := true,
                   metrics_enabled := false,
                   log_level := debug3,
                   resource_detectors := [],
                   text_map_propagators := [trace_context, baggage, b3],
                   attribute_count_limit := 7,
                   attribute_value_length_limit := infinity,
                   event_count_limit := 8,
                   link_count_limit := 9,
                   attribute_per_event_limit := 10,
                   attribute_per_link_limit := 11,
                   sampler := {parent_based,
                               #{root := {trace_id_ratio_based, 0.25},
                                 remote_parent_not_sampled := always_off}},
                   processors :=
                       [{otel_simple_processor,
                         #{exporter :=
                               {opentelemetry_exporter,
                                #{endpoints := [<<"https://collector/v1/traces">>],
                                  headers := [{<<"low">>, <<"one">>},
                                              {<<"shared">>, <<"explicit">>}],
                                  protocol := http_protobuf,
                                  compression := gzip,
                                  ssl_options := [{cacertfile, "/tmp/ca.pem"}],
                                  configuration_source := declarative}}}},
                        {otel_batch_processor,
                         #{scheduled_delay_ms := 25,
                           exporting_timeout_ms := 50,
                           max_queue_size := 100,
                           exporter :=
                               {opentelemetry_exporter,
                                #{endpoints := [<<"http://localhost:4317">>],
                                  headers := [],
                                  protocol := grpc,
                                  compression := undefined,
                                  ssl_options := undefined,
                                  configuration_source := declarative}}}}]},
                 Resolved),

    Resource = maps:get(resource, Resolved),
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
    [{otel_simple_processor,
      #{exporter := {opentelemetry_exporter, ExporterOptions}}}] =
        maps:get(processors, Resolved),
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

    ?assertMatch(#{text_map_propagators := [trace_context],
                   sampler := always_off,
                   processors :=
                       [{otel_simple_processor,
                         #{exporter :=
                               {opentelemetry_exporter,
                                #{protocol := http_protobuf,
                                  endpoints := [<<"http://localhost:4318/v1/traces">>]}}}}]},
                 Resolved).

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
                   attribute_value_length_limit := infinity,
                   processors :=
                       [{otel_batch_processor,
                         #{exporting_timeout_ms := infinity}}]},
                 Resolved).

resolves_metric_reader(_Config) ->
    {ok, Resolved} = otel_configuration_declarative:resolve(
                       #{<<"file_format">> => <<"1.1">>,
                         <<"meter_provider">> =>
                             #{<<"readers">> =>
                                   [#{<<"periodic">> =>
                                          #{<<"interval">> => 123,
                                            <<"exporter">> =>
                                                #{<<"otlp_http">> =>
                                                      #{<<"temporality_preference">> =>
                                                            <<"delta">>}}}}],
                               <<"exemplar_filter">> => <<"always_on">>}}),

    ?assertMatch(#{traces_enabled := false,
                   metrics_enabled := true,
                   exemplars_enabled := true,
                   exemplar_filter := always_on,
                   readers :=
                       [#{module := otel_metric_reader,
                          config :=
                              #{export_interval_ms := 123,
                                exporter :=
                                    {otel_exporter_metrics_otlp,
                                     #{endpoints :=
                                           [<<"http://localhost:4318/v1/metrics">>],
                                       protocol := http_protobuf}},
                                default_temporality_mapping :=
                                    #{counter := temporality_delta,
                                      observable_counter := temporality_delta,
                                      histogram := temporality_delta,
                                      updown_counter := temporality_cumulative}}}]},
                 Resolved),
    {ok, Grpc} = otel_configuration_declarative:resolve(
                   #{file_format => <<"1.1">>,
                     meter_provider => #{readers =>
                         [#{periodic => #{exporter => #{otlp_grpc => null}}}]}}),
    ?assertMatch([#{config := #{exporter := {otel_exporter_metrics_otlp,
                                            #{protocol := grpc}}}}],
                 maps:get(readers, Grpc)).

preserves_resource_attribute_types(_Config) ->
    {ok, Resolved} = otel_configuration_declarative:resolve(
        #{file_format => <<"1.1">>, resource => #{attributes =>
            [#{name => <<"ints">>, type => <<"int_array">>, value => [65, 66]},
             #{name => <<"strings">>, type => <<"string_array">>,
               value => [<<"a">>, <<"b">>]},
             #{name => <<"bools">>, type => <<"bool_array">>, value => [true, false]},
             #{name => <<"doubles">>, type => <<"double_array">>, value => [1, 2.5]},
             #{name => <<"double">>, type => <<"double">>, value => 1}]}}),
    Resource = maps:get(resource, Resolved),
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
                     [{ints, [65, 66]}, {strings, [<<"a">>, <<"b">>]}]))).

resource_attributes_map(Resource) ->
    case otel_resource:attributes(Resource) of
        undefined -> error(missing_resource);
        Attributes -> otel_attributes:map(Attributes)
    end.

configured_resource_is_detector_base(_Config) ->
    ok = application:load(opentelemetry),
    Configured = otel_resource:create([{<<"configured.resource">>, <<"present">>}]),
    {ok, Pid} = otel_resource_detector:start_link(
                  #{resource_detectors => [],
                    resource_detector_timeout => 100,
                    resource => Configured}),
    try
        Resource = otel_resource_detector:get_resource(),
        ?assertMatch(#{'configured.resource' := <<"present">>},
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
