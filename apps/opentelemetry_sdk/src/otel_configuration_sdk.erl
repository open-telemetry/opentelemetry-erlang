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

-import(otel_configuration_utils, [find/2, fail/1]).

-include_lib("kernel/include/logger.hrl").

-export([create/1,
         create_tracer_provider/1,
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
         span_limits/1,
         source/1]).

-type path() :: [atom() | binary()].
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
          configuration_resolved := true}.
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
%% Native options accepted by public provider startup, before resolution.
%% Runtime configuration below has required defaults and implementation modules.
-type processor_options() :: span_processor_component() |
                             #{atom() | binary() => component_properties() | null}.
-type sampler_options() :: always_on | always_off
                         | {trace_id_ratio_based, number() | component_properties()}
                         | {parent_based, component_properties()}
                         | {module(), component_properties()}
                         | #{atom() | binary() => component_properties() | null}.
-type tracer_provider_options() ::
        #{processors := [processor_options()],
          sampler => sampler_options() | null,
          id_generator => module() | {module(), component_properties()} |
                          #{atom() | binary() => component_properties() | null}}.

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
          id_generator := id_generator_component(),
          deny_list := [atom() | {atom(), string()}]}.
-type erlang_distribution_configuration() ::
        #{create_application_tracers => boolean(),
          deny_list => [atom() | {atom(), string()}],
          resource_detectors => [module() | {module(), term()}],
          resource_detector_timeout => non_neg_integer(),
          sweeper => sweeper_configuration(),
          term() => term()}.
-type sweeper_configuration() ::
        #{interval => timeout(),
          strategy => drop | end_span | failed_attribute_and_end_span |
                      fun((opentelemetry:span()) -> ok),
          span_ttl => timeout(),
          storage_size => non_neg_integer() | infinity}.
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
          distribution := distribution_configuration()}.

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
              resolved_span_limits/0,
              processor_options/0,
              sampler_options/0,
              tracer_provider_options/0,
              tracer_provider_configuration/0,
              distribution_configuration/0,
              erlang_distribution_configuration/0]).

-spec create(otel_configuration_model:t()) ->
          {ok, configuration()} | {error, error_reason()}.
create(Model) ->
    Raw = otel_configuration_model:root(Model),
    try
        otel_configuration_keys:validate(Raw, otel_configuration_model:source(Model)),
        warn_unsupported(log_level, Raw, [log_level], fun log_level/2),
        validate_distribution(Raw),
        validate_attribute_limits(Raw),
        warn_unsupported(logger_provider, Raw, [logger_provider], fun component_map/2),
        warn_unsupported(meter_provider, Raw, [meter_provider], fun component_map/2),
        Distribution = #{erlang := Erlang} = resolve_distribution(
                                               Raw, otel_configuration_model:source(Model)),
        Configuration =
            #{source => Model,
              file_format => otel_configuration_model:file_format(Model),
              disabled => resolve_disabled(Raw),
              resource => resolve_resource(Raw),
              text_map_propagators => resolve_text_map_propagators(Raw),
              span_limits => resolve_span_limits(Raw),
              tracer_provider => resolve_tracer_provider(Raw, Erlang),
              distribution => Distribution},
        {ok, Configuration}
    catch
        throw:{declarative_configuration_error, Reason} ->
            {error, Reason}
    end.

%% Resolves a standalone provider without constructing an SDK configuration.
-spec create_tracer_provider(map()) ->
          {ok, tracer_provider_configuration()} | {error, error_reason()}.
create_tracer_provider(Configuration) ->
    try
        otel_configuration_keys:validate_tracer_provider(Configuration),
        %% Span limits are global and can only be installed at SDK startup.
        warn_unsupported(limits, Configuration, [tracer_provider, limits], fun component_map/2),
        {ok, resolve_tracer_provider_configuration(Configuration, #{})}
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
            warn_unsupported('detection/development', ResourceConfig,
                             [resource, 'detection/development'], fun component_map/2),
            ListAttributes = resource_attribute_list(value(attributes_list, ResourceConfig, <<>>)),
            SchemaUrl = value(schema_url, ResourceConfig, undefined),
            resolve_resource_attributes(find(attributes, ResourceConfig),
                                        ListAttributes,
                                        SchemaUrl);
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
    maps:get(sampler, TracerProvider).

-spec id_generator(tracer_provider_configuration()) -> module().
id_generator(TracerProvider) ->
    maps:get(id_generator, TracerProvider).

resolve_id_generator(TracerProvider) ->
    case find(id_generator, TracerProvider) of
        error -> otel_id_generator;
        {ok, null} -> otel_id_generator;
        {ok, random} -> otel_id_generator;
        {ok, Generator} when is_atom(Generator) -> Generator;
        {ok, Generator} ->
            case component_entry(id_generator, Generator, [tracer_provider, id_generator]) of
                {random, _} -> otel_id_generator;
                {Name, _} when is_atom(Name) -> Name;
                {Name, _} -> fail({unsupported_configuration, [tracer_provider, id_generator], Name})
            end
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
    %% Omission inherits the general attribute limit. Explicit null uses the
    %% SpanLimits schema's defaultBehavior (128 / no length limit), as required
    %% by Create when the property has no separate nullBehavior.
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
        {ok, Value} -> Value
    end.

resolve_disabled(Configuration) ->
    case value(disabled, Configuration, false) of
        true -> true;
        false -> false;
        Value -> fail({invalid_configuration, [disabled], Value})
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

resolve_distribution(Configuration, Source) ->
    case find(distribution, Configuration) of
        error -> #{erlang => #{}};
        {ok, null} -> #{erlang => #{}};
        {ok, Distribution} when is_map(Distribution) ->
            case find(erlang, Distribution) of
                error -> #{erlang => #{}};
                {ok, null} -> #{erlang => #{}};
                {ok, Erlang} when is_map(Erlang) ->
                    #{erlang => resolve_erlang_distribution(Erlang, Source)}
            end
    end.

resolve_erlang_distribution(Erlang, Source) ->
    Path = [distribution, erlang],
    Resolved0 = copy_validated(create_application_tracers, Erlang, #{}, Path,
                               fun boolean_value/2),
    Resolved1 = copy_validated(resource_detector_timeout, Erlang, Resolved0, Path,
                               fun non_negative_integer/2),
    Resolved2 = copy_validated(deny_list, Erlang, Resolved1, Path,
                               fun(Value, P) -> native_distribution_entries(
                                                   Value, P, Source, fun deny_list_entry/2) end),
    Resolved = copy_validated(resource_detectors, Erlang, Resolved2, Path,
                              fun(Value, P) -> native_distribution_entries(
                                                  Value, P, Source, fun resource_detector/2) end),
    case find(sweeper, Erlang) of
        {ok, Sweeper} when is_map(Sweeper) ->
            Resolved#{sweeper => resolve_sweeper(Sweeper)};
        _ ->
            Resolved
    end.

%% JSON cannot select Erlang modules or application atoms. Empty lists are safe
%% and useful for explicitly disabling these distribution extensions.
native_distribution_entries([], _Path, _Source, _Validator) -> [];
native_distribution_entries(Values, Path, declarative, _Validator) when is_list(Values) ->
    fail({unsupported_configuration, Path, Values});
native_distribution_entries(Values, Path, _Source, Validator) when is_list(Values) ->
    [Validator(Value, Path) || Value <- Values];
native_distribution_entries(Value, Path, _Source, _Validator) ->
    fail({invalid_configuration, Path, Value}).

resource_detector(Module, _Path) when is_atom(Module) -> Module;
resource_detector({Module, _}=Detector, _Path) when is_atom(Module) -> Detector;
resource_detector(Value, Path) -> fail({invalid_configuration, Path, Value}).

deny_list_entry(Name, _Path) when is_atom(Name) -> Name;
deny_list_entry({Name, Version}=Entry, Path) when is_atom(Name), is_list(Version) ->
    case io_lib:char_list(Version) of
        true -> Entry;
        false -> fail({invalid_configuration, Path, Entry})
    end;
deny_list_entry(Value, Path) -> fail({invalid_configuration, Path, Value}).

resolve_sweeper(Sweeper) ->
    Path = [distribution, erlang, sweeper],
    Resolved0 = copy_validated(interval, Sweeper, #{}, Path,
                               fun non_negative_integer_or_infinity/2),
    Resolved1 = copy_validated(span_ttl, Sweeper, Resolved0, Path,
                               fun non_negative_integer_or_infinity/2),
    Resolved2 = copy_validated(storage_size, Sweeper, Resolved1, Path,
                               fun non_negative_integer_or_infinity/2),
    copy_validated(strategy, Sweeper, Resolved2, Path,
                   fun sweeper_strategy/2).

sweeper_strategy(Value, _Path) when is_function(Value, 1) -> Value;
sweeper_strategy(Value, Path) ->
    enum(Value,
         [{<<"drop">>, drop},
          {<<"end_span">>, end_span},
          {<<"failed_attribute_and_end_span">>, failed_attribute_and_end_span}],
         Path).

validate_erlang_distribution(Erlang) ->
    case find(sweeper, Erlang) of
        error -> ok;
        {ok, null} -> ok;
        {ok, Sweeper} when is_map(Sweeper) -> ok;
        {ok, Value} ->
            fail({invalid_configuration, [distribution, erlang, sweeper], Value})
    end.

log_level(Value, Path) ->
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
         Path).

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

resource_attribute_list(<<>>) -> [];
resource_attribute_list(Value) ->
    parse_key_value_list(Value, [resource, attributes_list]).

validate_attribute_limits(Configuration) ->
    case find(attribute_limits, Configuration) of
        error -> ok;
        {ok, null} -> ok;
        {ok, Limits} when is_map(Limits) ->
            warn_unsupported(attribute_value_depth_limit, Limits,
                             [attribute_limits, attribute_value_depth_limit],
                             fun positive_integer/2);
        {ok, Value} ->
            fail({invalid_configuration, [attribute_limits], Value})
    end.

composite_propagators(Propagators) when is_list(Propagators) ->
    [propagator_component(Propagator) || Propagator <- Propagators];
composite_propagators(Value) ->
    fail({invalid_configuration, [propagator, composite], Value}).

propagator_component(trace_context) -> trace_context;
propagator_component(tracecontext) -> trace_context;
propagator_component(Name) when is_atom(Name) -> Name;
propagator_component(Component) ->
    case component_entry(propagator, Component, [propagator, composite]) of
        {tracecontext, _} -> trace_context;
        {baggage, _} -> baggage;
        {b3, _} -> b3;
        {b3multi, _} -> b3multi;
        {Name, Config} when is_atom(Name) -> erlang_component(Name, Config);
        {Name, _} -> fail({unsupported_configuration, [propagator, composite], Name})
    end.

propagator_list(<<>>) -> [];
propagator_list(Value) ->
    [propagator_name(Name) || Part <- binary:split(to_binary(Value), <<",">>, [global]),
                              Name <- [trim_binary(Part)], Name =/= <<>>].

propagator_name(Name) ->
    case otel_configuration_keys:component_name(propagator, Name) of
        tracecontext -> trace_context;
        Propagator when is_atom(Propagator) -> Propagator;
        _ -> fail({unsupported_configuration, [propagator, composite_list], Name})
    end.

resolve_tracer_provider(Configuration, Erlang) ->
    case find(tracer_provider, Configuration) of
        error -> undefined;
        {ok, null} -> undefined;
        {ok, TracerProvider} ->
            resolve_tracer_provider_configuration(TracerProvider, Erlang)
    end.

resolve_tracer_provider_configuration(TracerProvider, Erlang) when is_map(TracerProvider) ->
    warn_unsupported('tracer_configurator/development', TracerProvider,
                     [tracer_provider, 'tracer_configurator/development'],
                     fun component_map/2),
    validate_tracer_limits(TracerProvider),
    Processors = required(processors, TracerProvider,
                          [tracer_provider, processors]),
    #{processors => resolve_span_processors(Processors),
      sampler => resolve_sampler(TracerProvider),
      id_generator => resolve_id_generator(TracerProvider),
      deny_list => maps:get(deny_list, Erlang, [])};
resolve_tracer_provider_configuration(Value, _Erlang) ->
    fail({invalid_configuration, [tracer_provider], Value}).

resolve_sampler(TracerProvider) ->
    case find(sampler, TracerProvider) of
        Missing when Missing =:= error; Missing =:= {ok, null} ->
            {parent_based, #{root => always_on}};
        {ok, SamplerConfig} -> sampler(SamplerConfig, [tracer_provider, sampler])
    end.

resolve_span_processors(Processors) when is_list(Processors) ->
    [resolve_span_processor(Processor) || Processor <- Processors];
resolve_span_processors(Value) ->
    fail({invalid_configuration, [tracer_provider, processors], Value}).

resolve_span_processor(Component) ->
    case component_entry(processor, Component, [tracer_provider, processors]) of
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
    end.

resolve_processor_config(Kind, Config) ->
    Path = [tracer_provider, processors, Kind],
    lists:foldl(
      fun({Key, exporter}, Acc) ->
              Acc#{Key => resolve_span_exporter(required(Key, Config, Path ++ [Key]))};
         ({Key, {unsupported, Rule}}, Acc) ->
              warn_unsupported(Key, Config, Path ++ [Key], processor_validator(Rule)),
              Acc;
         ({Key, Rule}, Acc) ->
              copy_validated(Key, Config, Acc, Path, processor_validator(Rule))
      end, #{}, otel_configuration_keys:processor_fields(Kind)).

processor_validator(non_negative_integer) -> fun non_negative_integer/2;
processor_validator(positive_integer) -> fun positive_integer/2;
processor_validator(non_negative_integer_or_infinity) -> fun non_negative_integer_or_infinity/2.

copy_validated(Key, Source, Target, Path, Validator) ->
    case find(Key, Source) of
        {ok, Value} when Value =/= null ->
            Target#{Key => Validator(Value, Path ++ [Key])};
        _ ->
            Target
    end.

non_negative_integer(Value, _Path) when is_integer(Value), Value >= 0 -> Value;
non_negative_integer(Value, Path) ->
    fail({invalid_configuration, Path, Value}).

positive_integer(Value, _Path) when is_integer(Value), Value > 0 -> Value;
positive_integer(Value, Path) ->
    fail({invalid_configuration, Path, Value}).

non_negative_integer_or_infinity(infinity, _Path) -> infinity;
non_negative_integer_or_infinity(<<"infinity">>, _Path) -> infinity;
non_negative_integer_or_infinity("infinity", _Path) -> infinity;
non_negative_integer_or_infinity(Value, Path) -> non_negative_integer(Value, Path).

validate_tracer_limits(TracerProvider) ->
    case find(limits, TracerProvider) of
        error -> ok;
        {ok, null} -> ok;
        {ok, Limits} when is_map(Limits) ->
            warn_unsupported(attribute_value_depth_limit, Limits,
                             [tracer_provider, limits, attribute_value_depth_limit],
                             fun positive_integer/2);
        {ok, Value} ->
            fail({invalid_configuration, [tracer_provider, limits], Value})
    end.

sampler(always_on, _Path) -> always_on;
sampler(always_off, _Path) -> always_off;
sampler({trace_id_ratio_based, Ratio}, Path) when not is_map(Ratio) ->
    {trace_id_ratio_based, float(number(Ratio, Path ++ [trace_id_ratio_based, ratio]))};
sampler(Sampler, Path) ->
    case component_entry(sampler, Sampler, Path) of
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
    end.

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

-spec otlp_protocol(term()) -> grpc | http_protobuf.
otlp_protocol(grpc) -> grpc;
otlp_protocol(http_protobuf) -> http_protobuf;
otlp_protocol(Value) ->
    fail({invalid_configuration,
          [tracer_provider, processors, exporter, protocol], Value}).

resolve_span_exporter(none) -> none;
resolve_span_exporter(Exporter) ->
    {Name, Options} = component_entry(exporter, Exporter, [tracer_provider, processors, exporter]),
    %% Maps use null for empty options; native module tuples may carry opaque
    %% terms, including null or undefined. The built-in module requires a map.
    Config = case is_map(Exporter) andalso Name =/= opentelemetry_exporter of
                 true -> null_to_map(Options);
                 false -> Options
             end,
    resolve_exporter_entry(Name, Config).

resolve_exporter_entry(opentelemetry_exporter, Options) when is_map(Options) ->
    Transport = case otlp_protocol(maps:get(protocol, Options, http_protobuf)) of
                    http_protobuf -> http;
                    grpc -> grpc
                end,
    {opentelemetry_exporter, Defaults} = otlp_exporter(Transport, #{}),
    %% Keep implementation-specific options while supplying the same
    %% defaults as aliases, without consulting another config source.
    Resolved = maps:merge(Defaults, Options),
    {opentelemetry_exporter, Resolved#{configuration_resolved => true}};
resolve_exporter_entry(opentelemetry_exporter, Value) ->
    fail({invalid_configuration, [tracer_provider, processors, exporter], Value});
resolve_exporter_entry(otlp_http, Config) -> otlp_exporter(http, Config);
resolve_exporter_entry(otlp_grpc, Config) -> otlp_exporter(grpc, Config);
resolve_exporter_entry(console, Config) -> console_exporter(Config);
resolve_exporter_entry(Module, Config) when is_atom(Module) -> {Module, Config};
resolve_exporter_entry(Name, _) ->
    fail({unsupported_configuration, [tracer_provider, processors, exporter], Name}).

console_exporter(Config0) ->
    _ = component_map(Config0, [exporter, console]),
    {otel_exporter_stdout, #{}}.

otlp_exporter(Transport, Config0) ->
    Path = [exporter, otlp_transport(Transport)],
    Config = component_map(Config0, Path),
    warn_unsupported(max_request_size, Config, Path ++ [max_request_size],
                     fun non_negative_integer/2),
    warn_unsupported(max_response_size, Config, Path ++ [max_response_size],
                     fun positive_integer/2),
    warn_unsupported(timeout, Config, Path ++ [timeout], fun non_negative_integer/2),
    check_encoding(Transport, Config, Path),
    Endpoint = to_binary(value(endpoint, Config, default_endpoint(Transport))),
    warn_empty_http_endpoint_path(Transport, Endpoint),
    Headers = exporter_headers(Config, Path),
    Compression = compression(value(compression, Config, none), Path ++ [compression]),
    SSLOptions = tls_options(value(tls, Config, undefined), Transport, Path ++ [tls]),
    {opentelemetry_exporter,
     #{endpoints => [Endpoint],
       headers => Headers,
       protocol => protocol(Transport),
       compression => Compression,
       ssl_options => SSLOptions,
       configuration_resolved => true}}.

warn_empty_http_endpoint_path(http, Endpoint) ->
    case uri_string:parse(Endpoint) of
        #{path := <<>>} ->
            ?LOG_WARNING("OTLP HTTP endpoint has an empty path and is used verbatim; "
                         "the traces endpoint usually requires /v1/traces. "
                         "No path is appended automatically.", [],
                         #{otel_configuration_path => [exporter, otlp_http, endpoint]});
        _ -> ok
    end;
warn_empty_http_endpoint_path(grpc, _Endpoint) ->
    ok.

check_encoding(grpc, _Config, _Path) -> ok;
check_encoding(http, Config, Path) ->
    enum(value(encoding, Config, protobuf), [{<<"protobuf">>, ok}],
         Path ++ [encoding], unsupported_configuration).

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

tls_options(undefined, _Transport, _Path) -> undefined;
tls_options(Tls, Transport, Path) when is_map(Tls) ->
    case Transport of
        grpc -> warn_unsupported(insecure, Tls, Path ++ [insecure], fun boolean_value/2);
        http -> ok
    end,
    Ca = value(ca_file, Tls, undefined),
    Key = value(key_file, Tls, undefined),
    Cert = value(cert_file, Tls, undefined),
    ClientOptions = case {Key, Cert} of
                        {undefined, undefined} -> [];
                        {K, C} when K =/= undefined, C =/= undefined ->
                            [{keyfile, to_list(K)}, {certfile, to_list(C)}];
                        Pair -> fail({invalid_configuration, Path, Pair})
                    end,
    case Ca of
        undefined -> ssl_options_with_system_defaults(ClientOptions);
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
        {ok, Value} -> Value;
        error -> Default
    end.

component_entry(Category, Component, Path) ->
    case otel_configuration_utils:component_entry(Component) of
        {ok, {Name, Config}} -> {otel_configuration_keys:component_name(Category, Name), Config};
        error -> fail({invalid_configuration, Path, Component})
    end.

component_map(null, _Path) -> #{};
component_map(Config, _Path) when is_map(Config) -> Config;
component_map(Value, Path) -> fail({invalid_configuration, Path, Value}).

%% Keep schema-valid optional settings in the source model even when this SDK
%% cannot apply them yet. Do not include values (which may be sensitive) in logs.
warn_unsupported(Key, Map, Path, Validator) ->
    case find(Key, Map) of
        error -> ok;
        {ok, null} -> ok;
        {ok, Value} ->
            _ = Validator(Value, Path),
            ?LOG_WARNING("Ignoring unsupported OpenTelemetry configuration property ~p",
                         [Path], #{otel_configuration_path => Path})
    end.

boolean_value(Value, _Path) when is_boolean(Value) -> Value;
boolean_value(Value, Path) -> fail({invalid_configuration, Path, Value}).

parse_key_value_list(Value, Path) ->
    case otel_configuration_key_value_list:parse(to_binary(Value)) of
        {Pairs, []} -> Pairs;
        {_, [Reason | _]} -> fail({invalid_configuration, Path, Reason})
    end.

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

compression(Value, Path) ->
    enum(Value, [{<<"none">>, undefined}, {<<"gzip">>, gzip}], Path,
         unsupported_configuration).

protocol(http) -> http_protobuf;
protocol(grpc) -> grpc.

otlp_transport(http) -> otlp_http;
otlp_transport(grpc) -> otlp_grpc.

default_endpoint(http) -> <<"http://localhost:4318/v1/traces">>;
default_endpoint(grpc) -> <<"http://localhost:4317">>.

enum(Value, Choices, Path) ->
    enum(Value, Choices, Path, invalid_configuration).

enum(Value, Choices, Path, ErrorKind) ->
    try to_binary(Value) of
        Binary ->
            case lists:keyfind(Binary, 1, Choices) of
                {_, Result} -> Result;
                false -> fail({ErrorKind, Path, Value})
            end
    catch
        throw:{declarative_configuration_error, _} -> fail({ErrorKind, Path, Value})
    end.

number(Value, _Path) when is_integer(Value); is_float(Value) -> Value;
number(Value, Path) -> fail({invalid_configuration, Path, Value}).

null_to_map(null) -> #{};
null_to_map(Value) -> Value.

erlang_component(Name, null) -> Name;
erlang_component(Name, Config) -> {Name, Config}.

trim_binary(Value) ->
    string:trim(to_binary(Value)).

-spec to_binary(term()) -> binary().
to_binary(Value) ->
    case otel_configuration_utils:to_binary(Value) of
        {ok, Binary} -> Binary;
        error -> fail({invalid_configuration, [], Value})
    end.

to_list(Value) ->
    case unicode:characters_to_list(to_binary(Value)) of
        Characters when is_list(Characters) -> Characters;
        _ -> fail({invalid_configuration, [], Value})
    end.
