%%%------------------------------------------------------------------------
%% Copyright 2026, OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Adapts an already decoded and schema-validated OpenTelemetry declarative
%% configuration to the concrete runtime configuration consumed by this SDK.
%% Parsing YAML, substituting environment variables, and validating against the
%% JSON schema deliberately happen outside this module.
%% @private
%%%-------------------------------------------------------------------------
-module(otel_configuration_declarative).

-export([resolve/1]).

-type path() :: [atom()].
-type error_reason() :: {invalid_configuration, path(), term()}
                      | {unsupported_configuration, path(), term()}
                      | {unsupported_file_format, term()}.

-export_type([error_reason/0]).

-spec resolve(map()) -> {ok, otel_configuration:resolved()} | {error, error_reason()}.
resolve(Configuration) when is_map(Configuration) ->
    try
        validate_file_format(Configuration),
        Overrides0 = declarative_defaults(),
        Overrides1 = top_level(Configuration, Overrides0),
        Overrides2 = resource(Configuration, Overrides1),
        Overrides3 = attribute_limits(Configuration, Overrides2),
        Overrides4 = propagator(Configuration, Overrides3),
        Overrides5 = tracer_provider(Configuration, Overrides4),
        Overrides6 = meter_provider(Configuration, Overrides5),
        reject_present(logger_provider, Configuration, [logger_provider]),
        {ok, otel_configuration:resolve(Overrides6)}
    catch
        throw:{declarative_configuration_error, Reason} ->
            {error, Reason}
    end;
resolve(Configuration) ->
    {error, {invalid_configuration, [], Configuration}}.

%% A declarative configuration is authoritative. In particular, the legacy
%% environment-backed resource detectors and default propagators/exporters must
%% not leak into it when a section is omitted.
declarative_defaults() ->
    #{configuration_source => declarative,
      traces_enabled => false,
      metrics_enabled => false,
      resource_detectors => [],
      resource => undefined,
      text_map_propagators => [],
      traces_exporter => none,
      metrics_exporter => none,
      processors => [],
      readers => [],
      views => []}.

validate_file_format(Configuration) ->
    case find(file_format, Configuration) of
        {ok, Version} ->
            case re:run(to_binary(Version), <<"^1\\.[0-9]+(?:\\.[0-9]+)?(?:[-+].*)?$">>,
                        [{capture, none}]) of
                match -> ok;
                nomatch -> fail({unsupported_file_format, Version})
            end;
        error ->
            fail({invalid_configuration, [file_format], missing})
    end.

top_level(Configuration, Overrides) ->
    Overrides1 = put_if_value(disabled, sdk_disabled, Configuration, Overrides),
    case find(log_level, Configuration) of
        error -> Overrides1;
        {ok, null} -> Overrides1;
        {ok, undefined} -> Overrides1;
        {ok, Value} -> Overrides1#{log_level => log_level(Value)}
    end.

log_level(Value) ->
    enum(Value,
         [{<<"trace">>, trace}, {<<"trace2">>, trace2},
          {<<"trace3">>, trace3}, {<<"trace4">>, trace4},
          {<<"debug">>, debug}, {<<"debug2">>, debug2},
          {<<"debug3">>, debug3}, {<<"debug4">>, debug4},
          {<<"info">>, info}, {<<"info2">>, info2},
          {<<"info3">>, info3}, {<<"info4">>, info4},
          {<<"warn">>, warn}, {<<"warn2">>, warn2},
          {<<"warn3">>, warn3}, {<<"warn4">>, warn4},
          {<<"error">>, error}, {<<"error2">>, error2},
          {<<"error3">>, error3}, {<<"error4">>, error4},
          {<<"fatal">>, fatal}, {<<"fatal2">>, fatal2},
          {<<"fatal3">>, fatal3}, {<<"fatal4">>, fatal4}],
         [log_level]).

resource(Configuration, Overrides) ->
    case find(resource, Configuration) of
        error -> Overrides;
        {ok, null} -> Overrides;
        {ok, ResourceConfig} when is_map(ResourceConfig) ->
            reject_present('detection/development', ResourceConfig,
                           [resource, 'detection/development']),
            ListAttributes = resource_attribute_list(value(attributes_list, ResourceConfig, <<>>)),
            ExplicitAttributes = resource_attributes(value(attributes, ResourceConfig, [])),
            Attributes = merge_name_value_pairs(ListAttributes, ExplicitAttributes),
            SchemaUrl = value(schema_url, ResourceConfig, undefined),
            Overrides#{resource => otel_resource:create_from_attributes(
                                     Attributes, null_to_undefined(SchemaUrl))};
        {ok, Value} ->
            fail({invalid_configuration, [resource], Value})
    end.

resource_attributes(Attributes) when is_list(Attributes) ->
    lists:filtermap(
      fun(Attribute) when is_map(Attribute) ->
              Name = required(name, Attribute, [resource, attributes, name]),
              case required(value, Attribute, [resource, attributes, value]) of
                  null -> false;
                  AttributeValue ->
                      {true, {Name, resource_attribute_value(
                                      value(type, Attribute, string), AttributeValue)}}
              end;
         (Attribute) ->
              fail({invalid_configuration, [resource, attributes], Attribute})
      end, Attributes);
resource_attributes(Value) ->
    fail({invalid_configuration, [resource, attributes], Value}).

%% JSON represents numbers without distinguishing integer and double values.
%% Preserve the declared type even when a double is written without a decimal.
resource_attribute_value(Type, Value) ->
    case to_binary(Type) of
        <<"double">> -> float(Value);
        <<"double_array">> -> [float(Element) || Element <- Value];
        _ -> Value
    end.

resource_attribute_list(null) -> [];
resource_attribute_list(undefined) -> [];
resource_attribute_list(<<>>) -> [];
resource_attribute_list(Value) -> parse_key_value_list(Value).

attribute_limits(Configuration, Overrides) ->
    case find(attribute_limits, Configuration) of
        error -> Overrides;
        {ok, null} -> Overrides;
        {ok, Limits} when is_map(Limits) ->
            reject_non_null(attribute_value_depth_limit, Limits,
                            [attribute_limits, attribute_value_depth_limit]),
            Overrides1 = put_limit(attribute_count_limit, attribute_count_limit,
                                   Limits, Overrides, 128),
            put_limit(attribute_value_length_limit, attribute_value_length_limit,
                      Limits, Overrides1, infinity);
        {ok, Value} ->
            fail({invalid_configuration, [attribute_limits], Value})
    end.

propagator(Configuration, Overrides) ->
    case find(propagator, Configuration) of
        error -> Overrides;
        {ok, null} -> Overrides;
        {ok, Propagator} when is_map(Propagator) ->
            Composite = composite_propagators(value(composite, Propagator, [])),
            CompositeList = propagator_list(value(composite_list, Propagator, <<>>)),
            Overrides#{text_map_propagators => append_unique(Composite, CompositeList)};
        {ok, Value} ->
            fail({invalid_configuration, [propagator], Value})
    end.

composite_propagators(Propagators) when is_list(Propagators) ->
    [propagator_component(Propagator) || Propagator <- Propagators];
composite_propagators(Value) ->
    fail({invalid_configuration, [propagator, composite], Value}).

propagator_component(Component) when is_map(Component), map_size(Component) =:= 1 ->
    case first_key(Component) of
        tracecontext -> trace_context;
        baggage -> baggage;
        b3 -> b3;
        b3multi -> b3multi;
        Name -> fail({unsupported_configuration, [propagator, composite], Name})
    end;
propagator_component(Value) ->
    fail({invalid_configuration, [propagator, composite], Value}).

propagator_list(null) -> [];
propagator_list(undefined) -> [];
propagator_list(<<>>) -> [];
propagator_list(Value) ->
    [propagator_name(trim_binary(Name)) || Name <- binary:split(to_binary(Value), <<",">>, [global]),
                                              trim_binary(Name) =/= <<>>].

propagator_name(<<"tracecontext">>) -> trace_context;
propagator_name(<<"baggage">>) -> baggage;
propagator_name(<<"b3">>) -> b3;
propagator_name(<<"b3multi">>) -> b3multi;
propagator_name(Name) ->
    fail({unsupported_configuration, [propagator, composite_list], Name}).

tracer_provider(Configuration, Overrides) ->
    case find(tracer_provider, Configuration) of
        error -> Overrides;
        {ok, null} -> Overrides;
        {ok, TracerProvider} when is_map(TracerProvider) ->
            reject_present('tracer_configurator/development', TracerProvider,
                           [tracer_provider, 'tracer_configurator/development']),
            Overrides1 = tracer_limits(TracerProvider, Overrides),
            Overrides2 = tracer_sampler(TracerProvider, Overrides1),
            Overrides3 = tracer_id_generator(TracerProvider, Overrides2),
            Processors = required(processors, TracerProvider,
                                  [tracer_provider, processors]),
            Overrides3#{traces_enabled => true,
                        processors => span_processors(Processors)};
        {ok, Value} ->
            fail({invalid_configuration, [tracer_provider], Value})
    end.

tracer_limits(TracerProvider, Overrides) ->
    case find(limits, TracerProvider) of
        error -> Overrides;
        {ok, null} -> Overrides;
        {ok, Limits} when is_map(Limits) ->
            reject_non_null(attribute_value_depth_limit, Limits,
                            [tracer_provider, limits, attribute_value_depth_limit]),
            Mappings = [{attribute_value_length_limit, attribute_value_length_limit, infinity},
                        {attribute_count_limit, attribute_count_limit, 128},
                        {event_count_limit, event_count_limit, 128},
                        {link_count_limit, link_count_limit, 128},
                        {event_attribute_count_limit, attribute_per_event_limit, 128},
                        {link_attribute_count_limit, attribute_per_link_limit, 128}],
            lists:foldl(fun({Input, Output, Default}, Acc) ->
                                put_limit(Input, Output, Limits, Acc, Default)
                        end, Overrides, Mappings);
        {ok, Value} ->
            fail({invalid_configuration, [tracer_provider, limits], Value})
    end.

tracer_sampler(TracerProvider, Overrides) ->
    case find(sampler, TracerProvider) of
        error -> Overrides;
        {ok, null} -> Overrides;
        {ok, Sampler} -> Overrides#{sampler => sampler(Sampler, [tracer_provider, sampler])}
    end.

sampler(Sampler, Path) when is_map(Sampler), map_size(Sampler) =:= 1 ->
    case first_entry(Sampler) of
        {always_on, _} -> always_on;
        {always_off, _} -> always_off;
        {trace_id_ratio_based, Config0} ->
            Config = component_map(Config0, Path ++ [trace_id_ratio_based]),
            Ratio = number(value(ratio, Config, 1.0), Path ++ [trace_id_ratio_based, ratio]),
            {trace_id_ratio_based, float(Ratio)};
        {parent_based, Config0} ->
            Config = component_map(Config0, Path ++ [parent_based]),
            ParentOptions0 = sampler_option(root, Config, always_on, Path),
            ParentOptions1 = sampler_option(remote_parent_sampled, Config, undefined,
                                            Path, ParentOptions0),
            ParentOptions2 = sampler_option(remote_parent_not_sampled, Config, undefined,
                                            Path, ParentOptions1),
            ParentOptions3 = sampler_option(local_parent_sampled, Config, undefined,
                                            Path, ParentOptions2),
            ParentOptions = sampler_option(local_parent_not_sampled, Config, undefined,
                                           Path, ParentOptions3),
            {parent_based, ParentOptions};
        {Name, _} ->
            fail({unsupported_configuration, Path, Name})
    end;
sampler(Value, Path) ->
    fail({invalid_configuration, Path, Value}).

sampler_option(Key, Config, Default, Path) ->
    sampler_option(Key, Config, Default, Path, #{}).

sampler_option(Key, Config, Default, Path, Options) ->
    case find(Key, Config) of
        error when Default =:= undefined -> Options;
        error -> Options#{Key => Default};
        {ok, null} when Default =:= undefined -> Options;
        {ok, null} -> Options#{Key => Default};
        {ok, Child} -> Options#{Key => sampler(Child, Path ++ [parent_based, Key])}
    end.

tracer_id_generator(TracerProvider, Overrides) ->
    case find(id_generator, TracerProvider) of
        error -> Overrides;
        {ok, null} -> Overrides;
        {ok, Generator} when is_map(Generator), map_size(Generator) =:= 1 ->
            case first_key(Generator) of
                random -> Overrides#{id_generator => otel_id_generator};
                Name -> fail({unsupported_configuration, [tracer_provider, id_generator], Name})
            end;
        {ok, Value} ->
            fail({invalid_configuration, [tracer_provider, id_generator], Value})
    end.

span_processors(Processors) when is_list(Processors) ->
    [span_processor(Processor) || Processor <- Processors];
span_processors(Value) ->
    fail({invalid_configuration, [tracer_provider, processors], Value}).

span_processor(Processor) when is_map(Processor), map_size(Processor) =:= 1 ->
    case first_entry(Processor) of
        {batch, Config0} ->
            Config = component_map(Config0, [tracer_provider, processors, batch]),
            reject_non_null(max_export_batch_size, Config,
                            [tracer_provider, processors, batch, max_export_batch_size]),
            Exporter = required(exporter, Config,
                                [tracer_provider, processors, batch, exporter]),
            Options0 = otel_configuration:span_processor_defaults(otel_batch_processor),
            Options1 = Options0#{exporter => span_exporter(Exporter)},
            Options2 = put_defaulted(schedule_delay, scheduled_delay_ms, Config, Options1, 5000),
            Options3 = put_timeout(export_timeout, exporting_timeout_ms, Config, Options2, 30000),
            Options = put_defaulted(max_queue_size, max_queue_size, Config, Options3, 2048),
            {otel_batch_processor, Options};
        {simple, Config0} ->
            Config = component_map(Config0, [tracer_provider, processors, simple]),
            Exporter = required(exporter, Config,
                                [tracer_provider, processors, simple, exporter]),
            Options = (otel_configuration:span_processor_defaults(otel_simple_processor))#{
                        exporter => span_exporter(Exporter)},
            {otel_simple_processor, Options};
        {Name, _} ->
            fail({unsupported_configuration, [tracer_provider, processors], Name})
    end;
span_processor(Value) ->
    fail({invalid_configuration, [tracer_provider, processors], Value}).

span_exporter(Exporter) when is_map(Exporter), map_size(Exporter) =:= 1 ->
    case first_entry(Exporter) of
        {otlp_http, Config0} -> otlp_exporter(http, traces, Config0);
        {otlp_grpc, Config0} -> otlp_exporter(grpc, traces, Config0);
        {Name, _} -> fail({unsupported_configuration,
                           [tracer_provider, processors, exporter], Name})
    end;
span_exporter(Value) ->
    fail({invalid_configuration, [tracer_provider, processors, exporter], Value}).

otlp_exporter(Transport, Signal, Config0) ->
    Path = [exporter, otlp_transport(Transport)],
    Config = component_map(Config0, Path),
    reject_non_null(max_request_size, Config, Path ++ [max_request_size]),
    reject_non_null(max_response_size, Config, Path ++ [max_response_size]),
    reject_non_null(timeout, Config, Path ++ [timeout]),
    check_encoding(Transport, Config, Path),
    Endpoint = value(endpoint, Config, default_endpoint(Transport, Signal)),
    Headers = exporter_headers(Config),
    Compression = compression(value(compression, Config, none), Path ++ [compression]),
    SSLOptions = tls_options(value(tls, Config, undefined), Transport, Path ++ [tls]),
    Exporter = case Signal of
                   traces -> opentelemetry_exporter;
                   metrics -> otel_exporter_metrics_otlp
               end,
    {Exporter,
     #{endpoints => [Endpoint],
       headers => Headers,
       protocol => protocol(Transport),
       compression => Compression,
       ssl_options => SSLOptions,
       configuration_source => declarative}}.

check_encoding(grpc, _Config, _Path) -> ok;
check_encoding(http, Config, Path) ->
    case value(encoding, Config, protobuf) of
        protobuf -> ok;
        <<"protobuf">> -> ok;
        "protobuf" -> ok;
        Value -> fail({unsupported_configuration, Path ++ [encoding], Value})
    end.

exporter_headers(Config) ->
    FromList = parse_key_value_list(value(headers_list, Config, <<>>)),
    Explicit = header_pairs(value(headers, Config, [])),
    merge_name_value_pairs(FromList, Explicit).

header_pairs(Headers) when is_list(Headers) ->
    lists:filtermap(
      fun(Header) when is_map(Header) ->
              Name = required(name, Header, [exporter, headers, name]),
              case required(value, Header, [exporter, headers, value]) of
                  null -> false;
                  HeaderValue -> {true, {Name, HeaderValue}}
              end;
         (Header) -> fail({invalid_configuration, [exporter, headers], Header})
      end, Headers);
header_pairs(Value) ->
    fail({invalid_configuration, [exporter, headers], Value}).

tls_options(null, _Transport, _Path) -> undefined;
tls_options(undefined, _Transport, _Path) -> undefined;
tls_options(Tls, Transport, Path) when is_map(Tls) ->
    case Transport of
        grpc -> reject_non_null(insecure, Tls, Path ++ [insecure]);
        http -> ok
    end,
    Ca = value(ca_file, Tls, undefined),
    Key = value(key_file, Tls, undefined),
    Cert = value(cert_file, Tls, undefined),
    ClientOptions = case {Key, Cert} of
                        {undefined, undefined} -> [];
                        {null, null} -> [];
                        {undefined, null} -> [];
                        {null, undefined} -> [];
                        {K, C} when K =/= null, K =/= undefined,
                                    C =/= null, C =/= undefined ->
                            [{keyfile, to_list(K)}, {certfile, to_list(C)}];
                        Pair -> fail({invalid_configuration, Path, Pair})
                    end,
    case Ca of
        undefined -> ssl_options_with_system_defaults(ClientOptions);
        null -> ssl_options_with_system_defaults(ClientOptions);
        CaFile -> [{cacertfile, to_list(CaFile)} | ClientOptions]
    end;
tls_options(Value, _Transport, Path) ->
    fail({invalid_configuration, Path, Value}).

ssl_options_with_system_defaults([]) -> undefined;
ssl_options_with_system_defaults(ClientOptions) ->
    {system_defaults, ClientOptions}.

meter_provider(Configuration, Overrides) ->
    case find(meter_provider, Configuration) of
        error -> Overrides;
        {ok, null} -> Overrides;
        {ok, MeterProvider} when is_map(MeterProvider) ->
            reject_present(views, MeterProvider, [meter_provider, views]),
            reject_present('meter_configurator/development', MeterProvider,
                           [meter_provider, 'meter_configurator/development']),
            reject_present('view_matching_mode/development', MeterProvider,
                           [meter_provider, 'view_matching_mode/development']),
            Readers = required(readers, MeterProvider, [meter_provider, readers]),
            Overrides1 = Overrides#{metrics_enabled => true,
                                   exemplars_enabled => true,
                                   readers => metric_readers(Readers)},
            case find(exemplar_filter, MeterProvider) of
                error -> Overrides1;
                {ok, null} -> Overrides1;
                {ok, Filter} -> Overrides1#{exemplar_filter =>
                                                enum(Filter,
                                                     [{<<"always_on">>, always_on},
                                                      {<<"always_off">>, always_off},
                                                      {<<"trace_based">>, trace_based}],
                                                     [meter_provider, exemplar_filter])}
            end;
        {ok, Value} ->
            fail({invalid_configuration, [meter_provider], Value})
    end.

metric_readers(Readers) when is_list(Readers) ->
    [metric_reader(Reader) || Reader <- Readers];
metric_readers(Value) ->
    fail({invalid_configuration, [meter_provider, readers], Value}).

metric_reader(Reader) when is_map(Reader), map_size(Reader) =:= 1 ->
    case first_entry(Reader) of
        {periodic, Config0} ->
            Path = [meter_provider, readers, periodic],
            Config = component_map(Config0, Path),
            reject_non_null(timeout, Config, Path ++ [timeout]),
            reject_present('max_export_batch_size/development', Config,
                           Path ++ ['max_export_batch_size/development']),
            reject_present(producers, Config, Path ++ [producers]),
            reject_present(cardinality_limits, Config, Path ++ [cardinality_limits]),
            ExporterConfig = required(exporter, Config, Path ++ [exporter]),
            {Exporter, TemporalityMapping} = metric_exporter(ExporterConfig),
            #{module => otel_metric_reader,
              config => #{exporter => Exporter,
                          export_interval_ms => value(interval, Config, 60000),
                          default_temporality_mapping => TemporalityMapping}};
        {Name, _} ->
            fail({unsupported_configuration, [meter_provider, readers], Name})
    end;
metric_reader(Value) ->
    fail({invalid_configuration, [meter_provider, readers], Value}).

metric_exporter(Exporter) when is_map(Exporter), map_size(Exporter) =:= 1 ->
    case first_entry(Exporter) of
        {otlp_http, Config0} -> metric_otlp_exporter(http, Config0);
        {otlp_grpc, Config0} -> metric_otlp_exporter(grpc, Config0);
        {Name, _} ->
            fail({unsupported_configuration, [meter_provider, readers, exporter], Name})
    end;
metric_exporter(Value) ->
    fail({invalid_configuration, [meter_provider, readers, exporter], Value}).

metric_otlp_exporter(Transport, Config0) ->
    Path = [meter_provider, readers, exporter, otlp_transport(Transport)],
    Config = component_map(Config0, Path),
    HistogramAggregation = value(default_histogram_aggregation, Config,
                                 explicit_bucket_histogram),
    case enum(HistogramAggregation,
              [{<<"explicit_bucket_histogram">>, explicit_bucket_histogram},
               {<<"base2_exponential_bucket_histogram">>,
                base2_exponential_bucket_histogram}],
              Path ++ [default_histogram_aggregation]) of
        explicit_bucket_histogram -> ok;
        UnsupportedAggregation ->
            fail({unsupported_configuration,
                  Path ++ [default_histogram_aggregation], UnsupportedAggregation})
    end,
    Temporality = enum(value(temporality_preference, Config, cumulative),
                       [{<<"cumulative">>, cumulative},
                        {<<"delta">>, delta},
                        {<<"low_memory">>, low_memory}],
                       Path ++ [temporality_preference]),
    {otlp_exporter(Transport, metrics, Config), temporality_mapping(Temporality)}.

temporality_mapping(cumulative) ->
    maps:from_list([{Kind, temporality_cumulative} || Kind <- metric_kinds()]);
temporality_mapping(delta) ->
    #{counter => temporality_delta,
      observable_counter => temporality_delta,
      histogram => temporality_delta,
      observable_gauge => temporality_cumulative,
      updown_counter => temporality_cumulative,
      observable_updowncounter => temporality_cumulative};
temporality_mapping(low_memory) ->
    #{counter => temporality_delta,
      observable_counter => temporality_cumulative,
      histogram => temporality_delta,
      observable_gauge => temporality_cumulative,
      updown_counter => temporality_cumulative,
      observable_updowncounter => temporality_cumulative}.

metric_kinds() ->
    [counter, observable_counter, histogram, observable_gauge,
     updown_counter, observable_updowncounter].

put_if_value(InputKey, OutputKey, Input, Output) ->
    case find(InputKey, Input) of
        error -> Output;
        {ok, null} -> Output;
        {ok, undefined} -> Output;
        {ok, Value} -> Output#{OutputKey => Value}
    end.

put_limit(InputKey, OutputKey, Input, Output, NullDefault) ->
    case find(InputKey, Input) of
        error -> Output;
        {ok, null} -> Output#{OutputKey => NullDefault};
        {ok, undefined} -> Output#{OutputKey => NullDefault};
        {ok, Value} -> Output#{OutputKey => Value}
    end.

put_defaulted(InputKey, OutputKey, Input, Output, Default) ->
    Output#{OutputKey => value(InputKey, Input, Default)}.

put_timeout(InputKey, OutputKey, Input, Output, Default) ->
    case value(InputKey, Input, Default) of
        0 -> Output#{OutputKey => infinity};
        Timeout -> Output#{OutputKey => Timeout}
    end.

required(Key, Map, Path) ->
    case find(Key, Map) of
        {ok, Value} -> Value;
        error -> fail({invalid_configuration, Path, missing})
    end.

value(Key, Map, Default) ->
    case find(Key, Map) of
        {ok, null} -> Default;
        {ok, undefined} -> Default;
        {ok, Value} -> Value;
        error -> Default
    end.

find(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, _}=Found -> Found;
        error -> maps:find(atom_to_binary(Key, utf8), Map)
    end.

first_entry(Map) ->
    [{RawKey, Value}] = maps:to_list(Map),
    {known_key(RawKey), Value}.

first_key(Map) ->
    {Key, _} = first_entry(Map),
    Key.

known_key(Key) when is_atom(Key) -> Key;
known_key(<<"always_on">>) -> always_on;
known_key(<<"always_off">>) -> always_off;
known_key(<<"trace_id_ratio_based">>) -> trace_id_ratio_based;
known_key(<<"parent_based">>) -> parent_based;
known_key(<<"random">>) -> random;
known_key(<<"batch">>) -> batch;
known_key(<<"simple">>) -> simple;
known_key(<<"otlp_http">>) -> otlp_http;
known_key(<<"otlp_grpc">>) -> otlp_grpc;
known_key(<<"periodic">>) -> periodic;
known_key(<<"pull">>) -> pull;
known_key(<<"tracecontext">>) -> tracecontext;
known_key(<<"baggage">>) -> baggage;
known_key(<<"b3">>) -> b3;
known_key(<<"b3multi">>) -> b3multi;
known_key(Key) -> Key.

component_map(null, _Path) -> #{};
component_map(undefined, _Path) -> #{};
component_map(Config, _Path) when is_map(Config) -> Config;
component_map(Value, Path) -> fail({invalid_configuration, Path, Value}).

reject_present(Key, Map, Path) ->
    case find(Key, Map) of
        error -> ok;
        {ok, null} -> ok;
        {ok, undefined} -> ok;
        {ok, Value} -> fail({unsupported_configuration, Path, Value})
    end.

reject_non_null(Key, Map, Path) -> reject_present(Key, Map, Path).

parse_key_value_list(null) -> [];
parse_key_value_list(undefined) -> [];
parse_key_value_list(<<>>) -> [];
parse_key_value_list(Value) ->
    lists:filtermap(
      fun(Part) ->
              case binary:split(Part, <<"=">>) of
                  [Name, PairValue] ->
                      {true, {trim_binary(Name), trim_binary(PairValue)}};
                  _ -> false
              end
      end, binary:split(to_binary(Value), <<",">>, [global])).

merge_name_value_pairs(LowPriority, HighPriority) ->
    HighNames = [to_binary(Name) || {Name, _} <- HighPriority],
    [{Name, Value} || {Name, Value} <- LowPriority,
                      not lists:member(to_binary(Name), HighNames)] ++ HighPriority.

append_unique(First, Second) ->
    lists:foldl(fun(Item, Acc) ->
                        case lists:member(Item, Acc) of
                            true -> Acc;
                            false -> Acc ++ [Item]
                        end
                end, First, Second).

compression(none, _Path) -> undefined;
compression(<<"none">>, _Path) -> undefined;
compression("none", _Path) -> undefined;
compression(gzip, _Path) -> gzip;
compression(<<"gzip">>, _Path) -> gzip;
compression("gzip", _Path) -> gzip;
compression(Value, Path) -> fail({unsupported_configuration, Path, Value}).

protocol(http) -> http_protobuf;
protocol(grpc) -> grpc.

otlp_transport(http) -> otlp_http;
otlp_transport(grpc) -> otlp_grpc.

default_endpoint(http, traces) -> <<"http://localhost:4318/v1/traces">>;
default_endpoint(http, metrics) -> <<"http://localhost:4318/v1/metrics">>;
default_endpoint(grpc, traces) -> <<"http://localhost:4317">>;
default_endpoint(grpc, metrics) -> <<"http://localhost:4317">>.

enum(Value, Choices, Path) ->
    Binary = to_binary(Value),
    case lists:keyfind(Binary, 1, Choices) of
        {_, Atom} -> Atom;
        false -> fail({invalid_configuration, Path, Value})
    end.

number(Value, _Path) when is_integer(Value); is_float(Value) -> Value;
number(Value, Path) -> fail({invalid_configuration, Path, Value}).

null_to_undefined(null) -> undefined;
null_to_undefined(Value) -> Value.

trim_binary(Value) ->
    unicode:characters_to_binary(string:trim(to_list(Value))).

-spec to_binary(term()) -> binary().
to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_binary(Value) when is_list(Value) -> characters_to_binary(Value, <<>>);
to_binary(Value) -> fail({invalid_configuration, [], Value}).

characters_to_binary([], Acc) -> Acc;
characters_to_binary([Character | Rest], Acc)
  when is_integer(Character), Character >= 0, Character =< 16#10ffff ->
    characters_to_binary(Rest, <<Acc/binary, Character/utf8>>);
characters_to_binary(Value, _Acc) ->
    fail({invalid_configuration, [], Value}).

to_list(Value) -> binary_to_list(to_binary(Value)).

fail(Reason) -> throw({declarative_configuration_error, Reason}).
