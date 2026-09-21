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
%% Converts the lossless declarative configuration model into the idiomatic,
%% typed Erlang configuration consumed by SDK components.
%% @private
%%%-------------------------------------------------------------------------
-module(otel_configuration_sdk).

-export([create/1,
         value/3,
         disabled/1,
         tracer_provider/1,
         erlang_distribution/1,
         span_processors/1,
         span_exporter_component/1,
         resource/1,
         text_map_propagators/1,
         sampler/1,
         id_generator/1,
         span_processor_component/1,
         span_exporter/1,
         otlp_exporter_options/1,
         span_limits/1,
         source/1]).

-type path() :: [atom()].
-type error_reason() :: {invalid_configuration, path(), term()}
                      | {unsupported_configuration, path(), term()}.

-type component_properties() :: #{term() => term()}.
-type sampler_component() :: otel_sampler:sampler_spec().
-type id_generator_component() :: module().
-type span_exporter_component() :: {module(), term()}.
-type otlp_exporter_options() ::
        #{endpoints := [binary()],
          headers := [{binary(), binary()}],
          protocol := grpc | http_protobuf,
          compression := gzip | undefined,
          ssl_options := list() | {system_defaults, list()} | undefined,
          configuration_source := declarative}.
-type batch_processor_configuration() ::
        #{exporter => span_exporter_component() | none,
          schedule_delay => non_neg_integer(),
          export_timeout => non_neg_integer(),
          max_queue_size => non_neg_integer(),
          %% Erlang extension used to schedule the internal queue-size check.
          check_table_size => timeout(),
          term() => term()}.
-type simple_processor_configuration() ::
        #{exporter => span_exporter_component() | none,
          export_timeout => non_neg_integer(),
          term() => term()}.
-type span_processor_component() ::
        {module(), batch_processor_configuration() |
                   simple_processor_configuration() |
                   component_properties()}.
-type span_limits_configuration() ::
        #{attribute_value_length_limit => non_neg_integer() | infinity | null,
          attribute_count_limit => non_neg_integer() | null,
          event_count_limit => non_neg_integer() | null,
          link_count_limit => non_neg_integer() | null,
          event_attribute_count_limit => non_neg_integer() | null,
          link_attribute_count_limit => non_neg_integer() | null,
          attribute_value_depth_limit => non_neg_integer() | null,
          term() => term()}.
-type resolved_span_limits() ::
        #{attribute_value_length_limit := non_neg_integer() | infinity,
          attribute_count_limit := non_neg_integer(),
          event_count_limit := non_neg_integer(),
          link_count_limit := non_neg_integer(),
          event_attribute_count_limit := non_neg_integer(),
          link_attribute_count_limit := non_neg_integer()}.
-type tracer_provider_configuration() ::
        #{processors := [span_processor_component()],
          sampler := sampler_component(),
          id_generator := id_generator_component()}.
-type erlang_distribution_configuration() ::
        #{create_application_tracers => boolean(),
          deny_list => [term()],
          resource_detectors => [module() | {module(), term()}],
          resource_detector_timeout => non_neg_integer(),
          sweeper => component_properties(),
          term() => term()}.
-type distribution_configuration() ::
        #{erlang := erlang_distribution_configuration()}.
-type configuration() ::
        #{source := otel_configuration_model:t(),
          file_format := binary(),
          disabled := boolean(),
          resource := otel_resource:t() | undefined,
          text_map_propagators := [atom() | {module(), component_properties()}],
          span_limits := resolved_span_limits(),
          tracer_provider := tracer_provider_configuration() | undefined,
          distribution := distribution_configuration(),
          log_level => atom()}.

-export_type([error_reason/0,
              configuration/0,
              component_properties/0,
              sampler_component/0,
              id_generator_component/0,
              span_processor_component/0,
              batch_processor_configuration/0,
              simple_processor_configuration/0,
              span_exporter_component/0,
              otlp_exporter_options/0,
              span_limits_configuration/0,
              resolved_span_limits/0,
              tracer_provider_configuration/0,
              distribution_configuration/0,
              erlang_distribution_configuration/0]).

-spec create(otel_configuration_model:t()) ->
          {ok, configuration()} | {error, error_reason()}.
create(Model) ->
    Raw = otel_configuration_model:root(Model),
    try
        validate_top_level(Raw),
        validate_distribution(Raw),
        validate_attribute_limits(Raw),
        reject_present(logger_provider, Raw, [logger_provider]),
        Configuration0 =
            #{source => Model,
              file_format => otel_configuration_model:file_format(Model),
              disabled => resolve_disabled(Raw),
              resource => resolve_resource(Raw),
              text_map_propagators => resolve_text_map_propagators(Raw),
              span_limits => resolve_span_limits(Raw),
              tracer_provider => resolve_tracer_provider(Raw),
              distribution => resolve_distribution(Raw)},
        {ok, maybe_put_log_level(Raw, Configuration0)}
    catch
        throw:{declarative_configuration_error, Reason} ->
            {error, Reason}
    end.

-spec disabled(configuration()) -> boolean().
disabled(Configuration) ->
    maps:get(disabled, Configuration).

-spec tracer_provider(configuration()) -> tracer_provider_configuration() | undefined.
tracer_provider(Configuration) ->
    maps:get(tracer_provider, Configuration).

-spec erlang_distribution(configuration()) -> erlang_distribution_configuration().
erlang_distribution(Configuration) ->
    maps:get(erlang, maps:get(distribution, Configuration), #{}).

-spec span_processors(tracer_provider_configuration()) -> [span_processor_component()].
span_processors(TracerProvider) ->
    maps:get(processors, TracerProvider, []).

-spec span_exporter_component(map()) ->
          span_exporter_component() | none | undefined.
span_exporter_component(ProcessorConfiguration) ->
    case maps:get(exporter, ProcessorConfiguration, undefined) of
        {Module, _}=Exporter when is_atom(Module) -> Exporter;
        none -> none;
        undefined -> undefined;
        Value -> fail({invalid_configuration,
                       [tracer_provider, processors, exporter], Value})
    end.

-spec resource(configuration()) -> otel_resource:t() | undefined.
resource(Configuration) ->
    maps:get(resource, Configuration).

resolve_resource(Configuration) ->
    case find(resource, Configuration) of
        error -> undefined;
        {ok, null} -> undefined;
        {ok, ResourceConfig} when is_map(ResourceConfig) ->
            reject_present('detection/development', ResourceConfig,
                           [resource, 'detection/development']),
            ListAttributes = resource_attribute_list(value(attributes_list, ResourceConfig, <<>>)),
            SchemaUrl = value(schema_url, ResourceConfig, undefined),
            resolve_resource_attributes(find(attributes, ResourceConfig),
                                        ListAttributes,
                                        null_to_undefined(SchemaUrl));
        {ok, Value} ->
            fail({invalid_configuration, [resource], Value})
    end.

resolve_resource_attributes(error, ListAttributes, SchemaUrl) ->
    otel_resource:create_from_attributes(ListAttributes, SchemaUrl);
resolve_resource_attributes({ok, null}, ListAttributes, SchemaUrl) ->
    otel_resource:create_from_attributes(ListAttributes, SchemaUrl);
resolve_resource_attributes({ok, Attributes}, ListAttributes, SchemaUrl)
  when is_map(Attributes) ->
    Explicit = otel_resource:create(Attributes, SchemaUrl),
    FromList = otel_resource:create_from_attributes(ListAttributes, SchemaUrl),
    otel_resource:merge(Explicit, FromList);
resolve_resource_attributes({ok, Attributes}, ListAttributes, SchemaUrl) ->
    ExplicitAttributes = resource_attributes(Attributes),
    Merged = merge_name_value_pairs(ListAttributes, ExplicitAttributes),
    otel_resource:create_from_attributes(Merged, SchemaUrl).

-spec text_map_propagators(configuration()) ->
          [atom() | {module(), component_properties()}].
text_map_propagators(Configuration) ->
    maps:get(text_map_propagators, Configuration).

resolve_text_map_propagators(Configuration) ->
    case find(propagator, Configuration) of
        error -> [];
        {ok, null} -> [];
        {ok, Propagator} when is_map(Propagator) ->
            Composite = composite_propagators(value(composite, Propagator, [])),
            CompositeList = propagator_list(value(composite_list, Propagator, <<>>)),
            append_unique(Composite, CompositeList);
        {ok, Value} ->
            fail({invalid_configuration, [propagator], Value})
    end.

-spec sampler(tracer_provider_configuration()) -> otel_sampler:sampler_spec().
sampler(TracerProvider) ->
    maps:get(sampler, TracerProvider, {parent_based, #{root => always_on}}).

-spec id_generator(tracer_provider_configuration()) -> module().
id_generator(TracerProvider) ->
    maps:get(id_generator, TracerProvider, otel_id_generator).

resolve_id_generator(TracerProvider) ->
    case find(id_generator, TracerProvider) of
        error -> otel_id_generator;
        {ok, null} -> otel_id_generator;
        {ok, random} -> otel_id_generator;
        {ok, Generator} when is_atom(Generator) -> Generator;
        {ok, Generator} when is_map(Generator), map_size(Generator) =:= 1 ->
            case first_key(Generator) of
                random -> otel_id_generator;
                Name when is_atom(Name) -> Name;
                Name -> fail({unsupported_configuration, [tracer_provider, id_generator], Name})
            end;
        {ok, Invalid} ->
            fail({invalid_configuration, [tracer_provider, id_generator], Invalid})
    end.

-spec span_processor_component(span_processor_component()) ->
          {module(), batch_processor_configuration() |
                     simple_processor_configuration() |
                     component_properties()}.
span_processor_component({Module, Config}) when is_atom(Module), is_map(Config) ->
    {Module, Config};
span_processor_component(Invalid) ->
    fail({invalid_configuration, [tracer_provider, processors], Invalid}).

-spec span_limits(configuration()) -> resolved_span_limits().
span_limits(Configuration) ->
    maps:get(span_limits, Configuration).

resolve_span_limits(Configuration) ->
    Global = value(attribute_limits, Configuration, #{}),
    GlobalCount = limit(attribute_count_limit, Global, 128, 128),
    GlobalLength = limit(attribute_value_length_limit, Global, infinity, infinity),
    TracerProvider = value(tracer_provider, Configuration, #{}),
    Limits = value(limits, TracerProvider, #{}),
    #{attribute_count_limit =>
          limit(attribute_count_limit, Limits, GlobalCount, 128),
      attribute_value_length_limit =>
          limit(attribute_value_length_limit, Limits, GlobalLength, infinity),
      event_count_limit => limit(event_count_limit, Limits, 128, 128),
      link_count_limit => limit(link_count_limit, Limits, 128, 128),
      event_attribute_count_limit =>
          limit(event_attribute_count_limit, Limits, 128, 128),
      link_attribute_count_limit =>
          limit(link_attribute_count_limit, Limits, 128, 128)}.

-spec source(configuration()) -> otel_configuration_model:t().
source(Configuration) ->
    maps:get(source, Configuration).

limit(Key, Map, MissingDefault, NullDefault) ->
    case find(Key, Map) of
        error -> MissingDefault;
        {ok, null} -> NullDefault;
        {ok, undefined} -> NullDefault;
        {ok, Value} -> Value
    end.

validate_top_level(Configuration) ->
    case find(log_level, Configuration) of
        error -> ok;
        {ok, null} -> ok;
        {ok, undefined} -> ok;
        {ok, Value} -> _ = log_level(Value), ok
    end.

resolve_disabled(Configuration) ->
    case value(disabled, Configuration, false) of
        true -> true;
        false -> false;
        Value -> fail({invalid_configuration, [disabled], Value})
    end.

maybe_put_log_level(Configuration, Resolved) ->
    case find(log_level, Configuration) of
        error -> Resolved;
        {ok, null} -> Resolved;
        {ok, undefined} -> Resolved;
        {ok, Value} -> Resolved#{log_level => log_level(Value)}
    end.

validate_distribution(Configuration) ->
    case find(distribution, Configuration) of
        error -> ok;
        {ok, null} -> ok;
        {ok, Distribution} when is_map(Distribution) ->
            case find(erlang, Distribution) of
                error -> ok;
                {ok, null} -> ok;
                {ok, Erlang} when is_map(Erlang) -> validate_erlang_distribution(Erlang);
                {ok, Value} -> fail({invalid_configuration, [distribution, erlang], Value})
            end;
        {ok, Value} ->
            fail({invalid_configuration, [distribution], Value})
    end.

resolve_distribution(Configuration) ->
    case find(distribution, Configuration) of
        error -> #{erlang => #{}};
        {ok, null} -> #{erlang => #{}};
        {ok, Distribution} when is_map(Distribution) ->
            case find(erlang, Distribution) of
                error -> #{erlang => #{}};
                {ok, null} -> #{erlang => #{}};
                {ok, Erlang} when is_map(Erlang) ->
                    #{erlang => resolve_erlang_distribution(Erlang)}
            end
    end.

resolve_erlang_distribution(Erlang) ->
    lists:foldl(
      fun(Key, Acc) ->
              case find(Key, Erlang) of
                  {ok, Value} when Value =/= null, Value =/= undefined ->
                      Acc#{Key => Value};
                  _ -> Acc
              end
      end, #{}, [create_application_tracers,
                 deny_list,
                 resource_detectors,
                 resource_detector_timeout,
                 sweeper]).

validate_erlang_distribution(Erlang) ->
    case find(sweeper, Erlang) of
        error -> ok;
        {ok, null} -> ok;
        {ok, Sweeper} when is_map(Sweeper) -> ok;
        {ok, Value} ->
            fail({invalid_configuration, [distribution, erlang, sweeper], Value})
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
resource_attributes(Attributes) when is_map(Attributes) ->
    maps:to_list(Attributes);
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
resource_attribute_list(Value) ->
    parse_key_value_list(Value, [resource, attributes_list]).

validate_attribute_limits(Configuration) ->
    case find(attribute_limits, Configuration) of
        error -> ok;
        {ok, null} -> ok;
        {ok, Limits} when is_map(Limits) ->
            reject_non_null(attribute_value_depth_limit, Limits,
                            [attribute_limits, attribute_value_depth_limit]);
        {ok, Value} ->
            fail({invalid_configuration, [attribute_limits], Value})
    end.

composite_propagators(Propagators) when is_list(Propagators) ->
    [propagator_component(Propagator) || Propagator <- Propagators];
composite_propagators(Value) ->
    fail({invalid_configuration, [propagator, composite], Value}).

propagator_component(Component) when is_map(Component), map_size(Component) =:= 1 ->
    case first_entry(Component) of
        {tracecontext, _} -> trace_context;
        {baggage, _} -> baggage;
        {b3, _} -> b3;
        {b3multi, _} -> b3multi;
        {Name, Config} when is_atom(Name) -> erlang_component(Name, Config);
        {Name, _} -> fail({unsupported_configuration, [propagator, composite], Name})
    end;
propagator_component(trace_context) -> trace_context;
propagator_component(tracecontext) -> trace_context;
propagator_component(baggage) -> baggage;
propagator_component(b3) -> b3;
propagator_component(b3multi) -> b3multi;
propagator_component(Name) when is_atom(Name) -> Name;
propagator_component({Name, Config}) when is_atom(Name), is_map(Config) ->
    {Name, Config};
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

resolve_tracer_provider(Configuration) ->
    case find(tracer_provider, Configuration) of
        error -> undefined;
        {ok, null} -> undefined;
        {ok, TracerProvider} when is_map(TracerProvider) ->
            reject_present('tracer_configurator/development', TracerProvider,
                           [tracer_provider, 'tracer_configurator/development']),
            validate_tracer_limits(TracerProvider),
            Processors = required(processors, TracerProvider,
                                  [tracer_provider, processors]),
            #{processors => resolve_span_processors(Processors),
              sampler => resolve_sampler(TracerProvider),
              id_generator => resolve_id_generator(TracerProvider)};
        {ok, Value} ->
            fail({invalid_configuration, [tracer_provider], Value})
    end.

resolve_sampler(TracerProvider) ->
    case find(sampler, TracerProvider) of
        error -> {parent_based, #{root => always_on}};
        {ok, null} -> {parent_based, #{root => always_on}};
        {ok, SamplerConfig} -> sampler(SamplerConfig, [tracer_provider, sampler])
    end.

resolve_span_processors(Processors) when is_list(Processors) ->
    [resolve_span_processor(Processor) || Processor <- Processors];
resolve_span_processors(Value) ->
    fail({invalid_configuration, [tracer_provider, processors], Value}).

resolve_span_processor({Module, Config}) when is_atom(Module), is_map(Config) ->
    case Module of
        otel_batch_processor ->
            {Module, resolve_processor_config(batch, Config)};
        otel_simple_processor ->
            {Module, resolve_processor_config(simple, Config)};
        _ ->
            {Module, Config}
    end;
resolve_span_processor(Component) when is_map(Component), map_size(Component) =:= 1 ->
    case first_entry(Component) of
        {batch, Config0} ->
            {otel_batch_processor,
             resolve_processor_config(batch,
                                      component_map(Config0,
                                                    [tracer_provider, processors, batch]))};
        {simple, Config0} ->
            {otel_simple_processor,
             resolve_processor_config(simple,
                                      component_map(Config0,
                                                    [tracer_provider, processors, simple]))};
        {Name, Config} when is_atom(Name) ->
            {Name, null_to_map(Config)};
        {Name, _} ->
            fail({unsupported_configuration, [tracer_provider, processors], Name})
    end;
resolve_span_processor(Value) ->
    fail({invalid_configuration, [tracer_provider, processors], Value}).

resolve_processor_config(Kind, Config) ->
    Path = [tracer_provider, processors, Kind],
    case Kind of
        batch ->
            reject_non_null(max_export_batch_size, Config,
                            Path ++ [max_export_batch_size]);
        simple -> ok
    end,
    Exporter = required(exporter, Config, Path ++ [exporter]),
    Resolved0 = #{exporter => resolve_span_exporter(Exporter)},
    Keys = case Kind of
               batch -> [schedule_delay, export_timeout, max_queue_size, check_table_size];
               simple -> [export_timeout]
           end,
    copy_present(Keys, Config, Resolved0).

copy_present([], _Source, Target) -> Target;
copy_present([Key | Rest], Source, Target) ->
    case find(Key, Source) of
        {ok, Value} when Value =/= null, Value =/= undefined ->
            copy_present(Rest, Source, Target#{Key => Value});
        _ ->
            copy_present(Rest, Source, Target)
    end.

validate_tracer_limits(TracerProvider) ->
    case find(limits, TracerProvider) of
        error -> ok;
        {ok, null} -> ok;
        {ok, Limits} when is_map(Limits) ->
            reject_non_null(attribute_value_depth_limit, Limits,
                            [tracer_provider, limits, attribute_value_depth_limit]);
        {ok, Value} ->
            fail({invalid_configuration, [tracer_provider, limits], Value})
    end.

sampler(always_on, _Path) -> always_on;
sampler(always_off, _Path) -> always_off;
sampler({trace_id_ratio_based, Ratio}, Path) ->
    {trace_id_ratio_based, float(number(Ratio, Path ++ [trace_id_ratio_based, ratio]))};
sampler({parent_based, Options}, Path) when is_map(Options) ->
    {parent_based, maps:map(fun(_Key, Child) -> sampler(Child, Path) end, Options)};
sampler({Module, Config}, _Path) when is_atom(Module), is_map(Config) ->
    {Module, Config};
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
        {Name, Config} when is_atom(Name) ->
            {Name, null_to_map(Config)};
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

-spec span_exporter(span_exporter_component()) -> {module(), term()}.
span_exporter({Module, Config}) when is_atom(Module) ->
    {Module, Config};
span_exporter(Value) ->
    fail({invalid_configuration, [tracer_provider, processors, exporter], Value}).

-spec otlp_exporter_options(span_exporter_component()) -> otlp_exporter_options().
otlp_exporter_options(
  {opentelemetry_exporter,
   #{endpoints := Endpoints,
     headers := Headers,
     protocol := Protocol,
     compression := Compression,
     ssl_options := SSLOptions,
     configuration_source := declarative}}) ->
    #{endpoints => otlp_endpoints(Endpoints),
      headers => otlp_headers(Headers),
      protocol => otlp_protocol(Protocol),
      compression => otlp_compression(Compression),
      ssl_options => otlp_ssl_options(SSLOptions),
      configuration_source => declarative};
otlp_exporter_options(Exporter) ->
    fail({invalid_configuration,
          [tracer_provider, processors, exporter], Exporter}).

-spec otlp_endpoints(term()) -> [binary()].
otlp_endpoints(Endpoints) when is_list(Endpoints) ->
    [case Endpoint of
         Binary when is_binary(Binary) -> Binary;
         _ -> fail({invalid_configuration,
                    [tracer_provider, processors, exporter, endpoints], Endpoint})
     end || Endpoint <- Endpoints];
otlp_endpoints(Value) ->
    fail({invalid_configuration,
          [tracer_provider, processors, exporter, endpoints], Value}).

-spec otlp_headers(term()) -> [{binary(), binary()}].
otlp_headers(Headers) when is_list(Headers) ->
    [case Header of
         {Name, Value} when is_binary(Name), is_binary(Value) -> Header;
         _ -> fail({invalid_configuration,
                    [tracer_provider, processors, exporter, headers], Header})
     end || Header <- Headers];
otlp_headers(Value) ->
    fail({invalid_configuration,
          [tracer_provider, processors, exporter, headers], Value}).

-spec otlp_protocol(term()) -> grpc | http_protobuf.
otlp_protocol(grpc) -> grpc;
otlp_protocol(http_protobuf) -> http_protobuf;
otlp_protocol(Value) ->
    fail({invalid_configuration,
          [tracer_provider, processors, exporter, protocol], Value}).

-spec otlp_compression(term()) -> gzip | undefined.
otlp_compression(gzip) -> gzip;
otlp_compression(undefined) -> undefined;
otlp_compression(Value) ->
    fail({invalid_configuration,
          [tracer_provider, processors, exporter, compression], Value}).

-spec otlp_ssl_options(term()) -> list() | {system_defaults, list()} | undefined.
otlp_ssl_options(undefined) -> undefined;
otlp_ssl_options(Options) when is_list(Options) -> Options;
otlp_ssl_options({system_defaults, Options}) when is_list(Options) ->
    {system_defaults, Options};
otlp_ssl_options(Value) ->
    fail({invalid_configuration,
          [tracer_provider, processors, exporter, ssl_options], Value}).

resolve_span_exporter({opentelemetry_exporter, Options}) when is_map(Options) ->
    case maps:is_key(protocol, Options) of
        true -> {opentelemetry_exporter, Options};
        false -> fail({invalid_configuration,
                       [tracer_provider, processors, exporter], Options})
    end;
resolve_span_exporter(none) ->
    none;
resolve_span_exporter({otlp_http, Config}) ->
    otlp_exporter(http, Config);
resolve_span_exporter({otlp_grpc, Config}) ->
    otlp_exporter(grpc, Config);
resolve_span_exporter({Module, Config}) when is_atom(Module) ->
    {Module, Config};
resolve_span_exporter(Exporter) ->
    validate_span_exporter(Exporter).

validate_span_exporter(Exporter) when is_map(Exporter), map_size(Exporter) =:= 1 ->
    case first_entry(Exporter) of
        {otlp_http, Config0} -> otlp_exporter(http, Config0);
        {otlp_grpc, Config0} -> otlp_exporter(grpc, Config0);
        {Name, Config} when is_atom(Name) -> {Name, null_to_map(Config)};
        {Name, _} -> fail({unsupported_configuration,
                           [tracer_provider, processors, exporter], Name})
    end;
validate_span_exporter(Value) ->
    fail({invalid_configuration, [tracer_provider, processors, exporter], Value}).

otlp_exporter(Transport, Config0) ->
    Path = [exporter, otlp_transport(Transport)],
    Config = component_map(Config0, Path),
    reject_non_null(max_request_size, Config, Path ++ [max_request_size]),
    reject_non_null(max_response_size, Config, Path ++ [max_response_size]),
    reject_non_null(timeout, Config, Path ++ [timeout]),
    check_encoding(Transport, Config, Path),
    Endpoint = to_binary(value(endpoint, Config, default_endpoint(Transport))),
    Headers = exporter_headers(Config, Path),
    Compression = compression(value(compression, Config, none), Path ++ [compression]),
    SSLOptions = tls_options(value(tls, Config, undefined), Transport, Path ++ [tls]),
    {opentelemetry_exporter,
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

exporter_headers(Config, Path) ->
    FromList = parse_key_value_list(value(headers_list, Config, <<>>),
                                    Path ++ [headers_list]),
    Explicit = header_pairs(value(headers, Config, [])),
    merge_name_value_pairs(FromList, Explicit).

header_pairs(Headers) when is_list(Headers) ->
    lists:filtermap(
      fun(Header) when is_map(Header) ->
              Name = required(name, Header, [exporter, headers, name]),
              case required(value, Header, [exporter, headers, value]) of
                  null -> false;
                  HeaderValue -> {true, {to_binary(Name), to_binary(HeaderValue)}}
              end;
         ({Name, HeaderValue}) -> {true, {to_binary(Name), to_binary(HeaderValue)}};
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

parse_key_value_list(null, _Path) -> [];
parse_key_value_list(undefined, _Path) -> [];
parse_key_value_list(<<>>, _Path) -> [];
parse_key_value_list(Value, Path) ->
    [parse_key_value_pair(Part, Path)
     || Part <- binary:split(to_binary(Value), <<",">>, [global])].

parse_key_value_pair(Part, Path) ->
    case binary:split(Part, <<"=">>) of
        [RawName, RawValue] ->
            Name = trim_binary(RawName),
            PairValue = trim_binary(RawValue),
            case Name of
                <<>> -> fail({invalid_configuration, Path, Part});
                _ -> {percent_decode(Name, Path), percent_decode(PairValue, Path)}
            end;
        _ ->
            fail({invalid_configuration, Path, Part})
    end.

percent_decode(Value, Path) ->
    case percent_decode_binary(Value, <<>>) of
        {ok, Decoded} -> Decoded;
        {error, Reason} -> fail({invalid_configuration, Path, {Reason, Value}})
    end.

percent_decode_binary(<<$%, High, Low, Rest/binary>>, Acc) ->
    case {hex_value(High), hex_value(Low)} of
        {{ok, HighValue}, {ok, LowValue}} ->
            Octet = HighValue * 16 + LowValue,
            percent_decode_binary(Rest, <<Acc/binary, Octet>>);
        _ ->
            {error, invalid_percent_encoding}
    end;
percent_decode_binary(<<$%, _/binary>>, _Acc) ->
    {error, invalid_percent_encoding};
percent_decode_binary(<<Octet, Rest/binary>>, Acc) ->
    percent_decode_binary(Rest, <<Acc/binary, Octet>>);
percent_decode_binary(<<>>, Acc) ->
    case unicode:characters_to_list(Acc) of
        {error, _, _} -> {error, invalid_utf8};
        {incomplete, _, _} -> {error, invalid_utf8};
        _ -> {ok, Acc}
    end.

hex_value(Character) when Character >= $0, Character =< $9 ->
    {ok, Character - $0};
hex_value(Character) when Character >= $a, Character =< $f ->
    {ok, Character - $a + 10};
hex_value(Character) when Character >= $A, Character =< $F ->
    {ok, Character - $A + 10};
hex_value(_) ->
    error.

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

default_endpoint(http) -> <<"http://localhost:4318/v1/traces">>;
default_endpoint(grpc) -> <<"http://localhost:4317">>.

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

null_to_map(null) -> #{};
null_to_map(undefined) -> #{};
null_to_map(Value) -> Value.

erlang_component(Name, null) -> Name;
erlang_component(Name, undefined) -> Name;
erlang_component(Name, Config) -> {Name, Config}.

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
