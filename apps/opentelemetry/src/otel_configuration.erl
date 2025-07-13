%%%------------------------------------------------------------------------
%% Copyright 2021, OpenTelemetry Authors
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
%% Merges environment variable configuration values with application
%% configuration. The OS environment variables take precedence over the
%% application environment.
%% @private
%%%-------------------------------------------------------------------------
-module(otel_configuration).

-export([new/0,
         merge_with_os/1,
         merge_list_with_environment/3,
         transform/2,
         report_cb/1]).

-type log_level() :: atom().

-type attribute_limits() :: #{attribute_value_length_limit => integer() | undefined,
                              attribute_count_limit => integer() | undefined
                             }.

-type exporter_args() :: map().

-type exporter() :: {module(), exporter_args()}.

-type batch_span_processor() :: #{schedule_delay := integer(),
                                  export_timeout := integer(),
                                  max_queue_size := integer(),
                                  max_export_batch_size := integer(),
                                  exporter := exporter()}.

-type simple_span_processor() :: #{exporter := exporter()}.

-type span_processor_type() :: batch | simple | atom().

-type span_processor() :: batch_span_processor() |
                          simple_span_processor() |
                          otel_config_properties:t().
-type span_processors() :: #{span_processor_type() => span_processor()}.

-type propagator() :: tracecontext | baggage | b3 | b3multi | jaeger | ottrace | atom().

-type limits() :: #{%% Configure max attribute value size. Overrides .attribute_limits.attribute_value_length_limit.
                    %% Value must be non-negative.
                    %% If omitted or undefined, there is no limit.
                    attribute_value_length_limit => integer() | undefined,
                    %% Configure max attribute count. Overrides .attribute_limits.attribute_count_limit.
                    %% Value must be non-negative.
                    %% If omitted or undefined, 128 is used.
                    attribute_count_limit => integer() | undefined,
                    %% Configure max span event count.
                    %% Value must be non-negative.
                    %% If omitted or undefined, 128 is used.
                    event_count_limit => integer() | undefined,
                    %% Configure max span link count.
                    %% Value must be non-negative.
                    %% If omitted or undefined, 128 is used.
                    link_count_limit => integer() | undefined,
                    %% Configure max attributes per span event.
                    %% Value must be non-negative.
                    %% If omitted or undefined, 128 is used.
                    event_attribute_count_limit => integer() | undefined,
                    %% Configure max attributes per span link.
                    %% Value must be non-negative.
                    %% If omitted or undefined, 128 is used.
                    link_attribute_count_limit => integer() | undefined
                   }.

-type sampler() :: {always_on, #{}}
                 | {always_off, #{}}
                 | {trace_id_ratio_based, #{ratio => float()}}
                 | {parent_based, #{remote_parent_sampled => sampler(),
                                    remote_parent_not_sampled => sampler(),
                                    local_parent_sampled => sampler(),
                                    local_parent_not_sampled => sampler(),
                                    root => sampler()}}
                 | {atom(), otel_config_properties:t()}.

-type t() :: #{disabled => boolean(),
               log_level => log_level(),
               resource => #{attributes => opentelemetry:attributes_map()},
               attribute_limits => attribute_limits(),
               propagator := #{composite => [propagator()],
                               composite_list => string()},
               tracer_provider => #{processors => [span_processors()],
                                    limits => limits(),
                                    sampler => sampler()}
              }.

%% structure before the standard otel declarative configuration
%% -type old_t() :: #{sdk_disabled := boolean(),
%%                    log_level := atom(),
%%                    register_loaded_applications := boolean() | undefined,
%%                    create_application_tracers := boolean() | undefined,
%%                    id_generator := module(),
%%                    deny_list := [atom()],

%%                    resource_detectors := [module()],
%%                    resource_detector_timeout := integer(),

%%                    attribute_count_limit := integer(),
%%                    attribute_value_length_limit := integer() | infinity,

%%                    event_count_limit := integer(),
%%                    link_count_limit := integer(),

%%                    attribute_per_event_limit := integer(),
%%                    attribute_per_link_limit := integer(),

%%                    %% tracer provider
%%                    bsp_scheduled_delay_ms := integer() | undefined,
%%                    bsp_exporting_timeout_ms := integer() | undefined,
%%                    bsp_max_queue_size := integer() | undefined,
%%                    ssp_exporting_timeout_ms := integer() | undefined,
%%                    traces_exporter := {atom(), term()} | none | undefined,
%%                    processors := list(),
%%                    sampler := {atom(), term()},

%%                    text_map_propagators := [atom()],

%%                    metrics_exporter := {atom(), term()} | none | undefined,
%%                    views := list(), %% TODO: type should be `[otel_meter_server:view_config]'
%%                    %% when Metrics are moved out of the experimental app
%%                    readers := [#{id := atom(), module => module(), config => map()}],
%%                    exemplars_enabled := boolean(),
%%                    exemplar_filter := always_on | always_off | trace_based,
%%                    metric_producers := [{module(), term()}],

%%                    sweeper := #{interval => integer() | infinity,
%%                                 strategy => atom() | fun(),
%%                                 span_ttl => integer() | infinity,
%%                                 storage_size => integer() | infinity}
%%                   }.

-export_type([t/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/gradualizer.hrl").

-spec new() -> t().
new() ->
    ?assert_type(#{}, t()).

%% old() ->
%%     ?assert_type(#{sdk_disabled => false,
%%                    log_level => info,
%%                    register_loaded_applications => undefined,
%%                    create_application_tracers => undefined,
%%                    id_generator => otel_id_generator,
%%                    deny_list => [],
%%                    resource_detectors => [otel_resource_env_var,
%%                                           otel_resource_app_env],
%%                    resource_detector_timeout => 5000,
%%                    bsp_scheduled_delay_ms => undefined,
%%                    bsp_exporting_timeout_ms => undefined,
%%                    bsp_max_queue_size => undefined,
%%                    ssp_exporting_timeout_ms => undefined,
%%                    text_map_propagators => [trace_context, baggage],
%%                    traces_exporter => {opentelemetry_exporter, #{}},
%%                    metrics_exporter => {opentelemetry_exporter, #{}},
%%                    views => [],
%%                    readers => [],
%%                    exemplars_enabled => false,
%%                    exemplar_filter => trace_based,
%%                    metric_producers => [],
%%                    processors => [{otel_batch_processor, ?BATCH_PROCESSOR_DEFAULTS}],
%%                    sampler => {parent_based, #{root => always_on}},
%%                    sweeper => #{interval => timer:minutes(10),
%%                                 strategy => drop,
%%                                 span_ttl => timer:minutes(30),
%%                                 storage_size => infinity},
%%                    attribute_count_limit => 128,
%%                    attribute_value_length_limit => infinity,
%%                    event_count_limit => 128,
%%                    link_count_limit => 128,
%%                    attribute_per_event_limit => 128,
%%                    attribute_per_link_limit => 128}, old_t()).

%% constructs a config based on the flat format and merges with the new
%% nested configuration from the declarative configuration SIG
-spec merge_with_os(list()) -> t().
merge_with_os(Config) ->
    OldConfig = lists:foldl(fun(F, Acc) ->
                                    F(Config, Acc)
                            end, #{}, [fun span_limits/2,
                                       fun general/2,
                                       fun sampler/2,
                                       fun processors/2,
                                       fun sweeper/2]),

    convert_to_new(OldConfig, Config).

-spec convert_to_new(#{}, map()) -> t().
convert_to_new(OldConfig, Config) ->
    convert_tracer_provider(
      convert_propagator(
        convert_resource(
          convert_attribute_limits(
            convert_disabled(OldConfig, Config), Config), Config), Config), Config).

convert_tracer_provider(OldConfig, Config) ->
    %%                    bsp_scheduled_delay_ms := integer() | undefined,
    %%                    bsp_exporting_timeout_ms := integer() | undefined,
    %%                    bsp_max_queue_size := integer() | undefined,
    %%                    ssp_exporting_timeout_ms := integer() | undefined,
    %%                    traces_exporter := {atom(), term()} | none | undefined,
    %%                    processors := list(),
    %%                    sampler := {atom(), term()},
    case lists:keyfind(tracer_provider, 1, Config) of
        {tracer_provider, TracerProvider} ->
            OldConfig#{tracer_provider => TracerProvider};
        false ->
            case maps:take(processors, OldConfig) of
                error ->
                    {TracerProvider, OldConfig1} = convert_sampler(OldConfig),
                    TracerProvider1 = convert_span_limits(TracerProvider, OldConfig1),
                    OldConfig1#{tracer_provider => maps:merge(TracerProvider1)};
                {Processors, OldConfig1} ->
                    NewProcessors = lists:map(fun({otel_batch_processor, BatchConfig}) ->
                                                      {batch, convert_batch(BatchConfig)};
                                                 ({otel_simple_processor, SimpleConfig}) ->
                                                      {simple, convert_simple(SimpleConfig)}
                                              end, Processors),

                    {TracerProvider, OldConfig2} = convert_sampler(OldConfig1),
                    TracerProvider1 = convert_span_limits(TracerProvider, OldConfig2),

                    OldConfig2#{tracer_provider => TracerProvider1#{processors => NewProcessors}}
            end
    end.

convert_sampler(OldConfig) ->
    case maps:take(sampler, OldConfig) of
        {Sampler, OldConfig1} ->
            {#{sampler => Sampler}, OldConfig1};
        error ->
            {#{}, OldConfig}
    end.

convert_span_limits(TracerProvider, OldConfig) ->
    AttributeLimits = maps:get(attribute_limits, OldConfig, #{}),
    TracerProvider#{limits => new_map_updated_keys(maps:merge(OldConfig, AttributeLimits),
                                                   [{attribute_count_limit, attribute_count_limit},
                                                    {attribute_value_length_limit, attribute_value_length_limit},
                                                    {event_count_limit, event_count_limit},
                                                    {link_count_limit, link_count_limit},
                                                    {attribute_per_event_limit, event_attribute_count_limit},
                                                    {attribute_per_link_limit, link_attribute_count_limit}])}.

new_map_updated_keys(Config, Options) ->
    lists:foldl(fun({OldKey, NewKey}, ConfigAcc) ->
                        case maps:find(OldKey, Config) of
                            error ->
                                ConfigAcc;
                            {ok, Value} ->
                                ConfigAcc#{NewKey => Value}
                        end
                end, #{}, Options).


convert_simple(SimpleConfig) ->
    SimpleConfig1 = update_processor_config(SimpleConfig, [{exporting_timeout_ms, export_timeout}]),

    case maps:take(exporter, SimpleConfig1) of
        {ExporterConfig, SimpleConfig2} ->
            SimpleConfig2#{exporter => convert_exporter(ExporterConfig)};
        error ->
            SimpleConfig1
    end.

convert_batch(BatchConfig) ->
    BatchConfig1 = update_processor_config(BatchConfig, [{scheduled_delay_ms, schedule_delay},
                                                         {exporting_timeout_ms, export_timeout}
                                                         %% {max_queue_size, max_queue_size},
                                                         %% {max_export_batch_size},
                                                        ]),

    case maps:take(exporter, BatchConfig1) of
        error ->
            BatchConfig1;
        {ExporterValue, BatchConfig2} ->
            BatchConfig2#{exporter => convert_exporter(ExporterValue)}
    end.

update_processor_config(Config, Options) ->
    lists:foldl(fun({OldKey, NewKey}, ConfigAcc) ->
                        case maps:take(OldKey, ConfigAcc) of
                            error ->
                                ConfigAcc;
                            {Value, ConfigAcc1} ->
                                ConfigAcc1#{NewKey => Value}
                        end
                end, Config, Options).

convert_exporter(ExporterConfig) ->
    ExporterConfig.

%% convert the old `text_map_propagators' config into the new `propagator' config
%% based on the declarative file configuration of otel
convert_propagator(OldConfig, Config) ->
    case lists:keyfind(propagator, 1, Config) of
        false ->
            case maps:take(text_map_propagators, OldConfig) of
                {Value, OldConfig1} ->
                    OldConfig1#{propagator => #{composite => Value}};
                error ->
                    OldConfig
            end;
        {propagator, Value} ->
            OldConfig#{propagator => Value}
    end.

%% this one is different, it wasn't read from the old config before, only from application env by the detector
%% that detector is now a no-op and its parsing is done here. The old supported option for
%% adding static resource attributes is combined with the new. Anything under the key
%% `attributes' is expected to be of the form `#{name := binary(), value := binary()}' while
%% every other key/value pair is assumed to be an attribute
convert_resource(OldConfig, Config) ->
    case application:get_env(opentelemetry, resource) of
        {ok, ResourceAttributes} when is_list(ResourceAttributes) orelse is_map(ResourceAttributes) ->
            ResourceAttributes0 = case is_list(ResourceAttributes) of
                                      true ->
                                          maps:from_list(ResourceAttributes);
                                      false ->
                                          ResourceAttributes
                                  end,
            {NewConfigAttributes1, ResourceAttributes2}
                = try maps:take(attributes, ResourceAttributes0) of
                      {NewConfigAttributes, ResourceAttributes1} ->
                          {NewConfigAttributes, ResourceAttributes1};
                      error ->
                          {[], ResourceAttributes0}
                  catch
                      %% old form of resource attributes may not be a map but instead a list
                      %% we don't bother looking for an attributes key in that list
                      _:_ ->
                          {[], ResourceAttributes0}
                  end,

            ParsedAttributes = otel_resource_app_env:parse(ResourceAttributes2),

            OldConfig#{resource => #{attributes => ParsedAttributes ++ NewConfigAttributes1}};
         _ ->
            OldConfig
    end.

convert_disabled(OldConfig, Config) ->
    case maps:take(sdk_disabled, OldConfig) of
        {Value, OldConfig1} ->
            OldConfig1#{disabled => Value};
        error ->
            OldConfig
    end.

convert_attribute_limits(OldConfig, Config) ->
    case lists:keyfind(attribute_limits, 1, Config) of
        {attribute_limits, Limits} ->
            OldConfig#{attribute_limits => Limits};
        false ->
            OldConfig2 = case maps:take(attribute_count_limit, OldConfig) of
                             {AttributeCountLimit, OldConfig1} ->
                                 OldConfig1#{attribute_limits => #{attribute_count_limit => AttributeCountLimit}};
                             error ->
                                 OldConfig
                         end,


            case maps:take(attribute_value_length_limit, OldConfig2) of
                {AttributeValueLengthLimit, OldConfig3} ->
                    AttributeLimitsMap = maps:get(attribute_limits, OldConfig3, #{}),
                    OldConfig3#{attribute_limits => AttributeLimitsMap#{attribute_value_length_limit => AttributeValueLengthLimit}};
                error ->
                    OldConfig2
            end
    end.

    %% ConfigMap = new(),

    %% ConfigMap1 = lists:foldl(fun(F, Acc) ->
    %%                                  F(AppEnv, Acc)
    %%                          end, ConfigMap, [fun resource/2,
    %%                                           fun attribute_limits/2,
    %%                                           fun propagator/2,
    %%                                           fun tracer_provider/2,
    %%                                           fun sweeper/2]),



%% -spec resource(list(), t()) -> t().
%% resource(AppEnv, ConfigMap) ->
%%     Resource = proplists:get_value(resource, AppEnv, []),
%%     ConfigMap.

%% -spec attribute_limits(list(), t()) -> t().
%% attribute_limits(AppEnv, ConfigMap) ->
%%     AttributeLimits = proplists:get_value(attribute_limits, AppEnv, []),
%%     ConfigMap.

%% -spec propagator(list(), t()) -> t().
%% propagator(AppEnv, ConfigMap) ->
%%     Propagator = proplists:get_value(propagator, AppEnv, []),
%%     ConfigMap.

%% -spec tracer_provider(list(), t()) -> t().
%% tracer_provider(AppEnv, ConfigMap) ->
%%     TracerProvider = proplists:get_value(tracer_provider, AppEnv, []),
%%     ConfigMap.

%% backwards compatability

-spec span_limits(list(), t()) -> t().
span_limits(AppEnv, ConfigMap) ->
    merge_list_with_environment(config_mappings(span_limits), AppEnv, ConfigMap).

-spec general(list(), t()) -> t().
general(AppEnv, ConfigMap) ->
    Config = merge_list_with_environment(config_mappings(general_sdk), AppEnv, ConfigMap),

    %% merge the old `register_loaded_applications' with the new config key
    %% `create_application_tracers' that has replaced it
    Config1 = maps:update_with(create_application_tracers,
                               fun(undefined) ->
                                       %% `create_application_tracers' isn't set so update
                                       %% with the `register_loaded_applications' value
                                       %% or `true' if it too isn't set
                                       case maps:get(register_loaded_applications, Config, undefined) of
                                           undefined ->
                                               true;
                                           Bool ->
                                               Bool
                                       end;
                                  (Bool) ->
                                       Bool
                               end, undefined, Config),

    ?assert_type(Config1, t()).

-spec sweeper(list(), t()) -> t().
sweeper(AppEnv, ConfigMap) ->
    DefaultSweeperConfig = maps:get(sweeper, ConfigMap, #{}),
    AppEnvSweeper = proplists:get_value(sweeper, AppEnv, #{}),

    %% convert sweeper config to a list to utilize the merge_list_with_environment function
    SweeperConfig = merge_list_with_environment(config_mappings(sweeper),
                                                maps:to_list(AppEnvSweeper),
                                                DefaultSweeperConfig),
    ConfigMap#{sweeper => SweeperConfig}.

-spec processors(list(), t()) -> t().
processors(AppEnv, ConfigMap) ->
    SpanProcessors = case transform(span_processor, proplists:get_value(span_processor, AppEnv)) of
                         undefined ->
                             Processors = proplists:get_value(processors, AppEnv, maps:get(processors, ConfigMap, [])),
                             transform(span_processors, Processors);
                         SpanProcessor ->
                             case proplists:get_value(processors, AppEnv) of
                                 undefined ->
                                     ok;
                                 _ ->
                                     ?LOG_INFO("both processors and span_processor set in configuration but "
                                               "only one should be used. span_processor will be ignored.")
                             end,

                             [SpanProcessor]
                     end,

    ProcessorsConfig = lists:map(fun({Name, Opts}) ->
                                         Opts1 = merge_processor_config(Name, Opts, ConfigMap, AppEnv),
                                         {Name, Opts1}
                                 end, SpanProcessors),

    ConfigMap#{processors => ProcessorsConfig}.

%% use the top level app env and os env configuration to set/override processor config values
merge_processor_config(otel_batch_processor, Opts, ConfigMap, AppEnv) ->
    BatchEnvMapping = [{bsp_scheduled_delay_ms, scheduled_delay_ms},
                       {bsp_exporting_timeout_ms, exporting_timeout_ms},
                       {bsp_max_queue_size, max_queue_size},
                       {traces_exporter, exporter}],
    merge_processor_config_(BatchEnvMapping, Opts, ConfigMap, AppEnv);
merge_processor_config(otel_simple_processor, Opts, ConfigMap, AppEnv) ->
    SimpleEnvMapping = [{ssp_exporting_timeout_ms, exporting_timeout_ms},
                        {traces_exporter, exporter}],
    merge_processor_config_(SimpleEnvMapping, Opts, ConfigMap, AppEnv);
merge_processor_config(_, Opts, _, _) ->
    Opts.

merge_processor_config_(EnvMapping, Opts, ConfigMap, AppEnv) ->
    Mappings = config_mappings(general_sdk),

    lists:foldl(fun({K, V}, Acc) ->
                        case maps:get(K, ConfigMap, undefined) of
                            undefined ->
                                Acc;
                            Value ->
                                %% use default only if the config isn't found in Opts
                                %% but if the value in ConfigMap isn't the default use it
                                IsDefault = is_default(K, AppEnv, Mappings),
                                case (IsDefault andalso not maps:is_key(V, Opts)) orelse not IsDefault of
                                    true ->
                                        Acc#{V => Value};
                                    false ->
                                        Acc
                                end
                        end
                end, Opts, EnvMapping).

%% return true if the user (through application or os environment) configured
%% a certain setting
is_default(Key, AppEnv, Mappings) ->
    {OSVarName, Key, _TransformType} = lists:keyfind(Key, 2, Mappings),
    not lists:keymember(Key, 1, AppEnv) andalso os:getenv(OSVarName) =:= false.

%% sampler configuration is unique since it has the _ARG that is a sort of
%% sub-configuration of the sampler config, and isn't a list.
-spec sampler(list(), t()) -> t().
sampler(AppEnv, ConfigMap) ->
    OSVar = "OTEL_TRACES_SAMPLER",
    Key = sampler,
    Transform = sampler,
    case os:getenv("OTEL_TRACES_SAMPLER") of
        false ->
            case proplists:get_value(Key, AppEnv) of
                undefined ->
                    ConfigMap;
                Sampler ->
                    update_config_map(OSVar, Key, Transform, Sampler, ConfigMap)
            end;
        OSEnvSampler->
            SamplerTuple = {OSEnvSampler, os:getenv("OTEL_TRACES_SAMPLER_ARG")},
            update_config_map(OSVar, Key, Transform, SamplerTuple, ConfigMap)
    end.

%% requires `ConfigMap' contains every key and its default
%% will replace the defaults with the value from either the OS environment or application
%% environment, with the OS environment taking precedence.
-spec merge_list_with_environment([{OSVar, Key, Transform}], AppEnv, ConfigMap) -> ConfigMap
              when OSVar :: string(),
                   Key :: atom(),
                   Transform :: atom(),
                   AppEnv :: [{atom(), term()}],
                   ConfigMap :: map().
merge_list_with_environment(ConfigMappings, AppEnv, ConfigMap) ->
    lists:foldl(fun({OSVar, Key, Transform}, Acc) ->
                        case os:getenv(OSVar) of
                            false ->
                                %% not in the OS environment so check application environment
                                case lists:keyfind(Key, 1, AppEnv) of
                                    false ->
                                        %% not in the application env so leave default
                                        Acc;
                                    {_, Value} ->
                                        %% transform even the value from the
                                        %% application environment to ensure it
                                        %% is of the right type/format
                                        update_config_map(OSVar, Key, Transform, Value, Acc)
                                end;
                            OSVal ->
                                update_config_map(OSVar, Key, Transform, OSVal, Acc)
                        end
                end, ConfigMap, ConfigMappings).

update_config_map(OSVar, Key, Transform, Value, ConfigMap) ->
    try transform(Transform, Value) of
        TransformedValue ->
            ConfigMap#{Key => TransformedValue}
    catch
        Kind:Reason:StackTrace ->
            ?LOG_INFO(#{source => transform,
                        kind => Kind,
                        reason => Reason,
                        os_var => OSVar,
                        key => Key,
                        transform => Transform,
                        value => Value,
                        stacktrace => StackTrace},
                      #{report_cb => fun ?MODULE:report_cb/1}),
            ConfigMap
    end.

report_cb(#{source := transform,
            kind := Kind,
            reason := Reason,
            os_var := OSVar,
            key := Key,
            transform := Transform,
            value := Value,
            stacktrace := StackTrace}) ->
    {"Transforming configuration value failed: os_var=~ts key=~ts transform=~ts value=~ts exception=~ts",
     [OSVar, Key, Transform, Value, otel_utils:format_exception(Kind, Reason, StackTrace)]}.

config_mappings(general_sdk) ->
    [{"OTEL_SDK_DISABLED", sdk_disabled, boolean},
     {"OTEL_LOG_LEVEL", log_level, existing_atom},

     %% `register_loaded_applications' is kept for backwards compatibility
     {"OTEL_REGISTER_LOADED_APPLICATIONS", register_loaded_applications, boolean},
     {"OTEL_CREATE_APPLICATION_TRACERS", create_application_tracers, boolean},

     {"OTEL_ID_GENERATOR", id_generator, existing_atom},
     {"OTEL_DENY_LIST", deny_list, existing_atom_list},
     {"OTEL_PROPAGATORS", text_map_propagators, propagators},
     {"OTEL_TRACES_EXPORTER", traces_exporter, exporter},
     {"OTEL_METRICS_EXPORTER", metrics_exporter, exporter},
     {"OTEL_METRIC_VIEWS", views, views},
     {"OTEL_METRIC_READERS", readers, readers},
     {"OTEL_ERLANG_X_EXEMPLARS_ENABLED", exemplars_enabled, boolean},
     {"OTEL_METRICS_EXEMPLAR_FILTER", exemplar_filter, exemplar_filter},
     {"OTEL_ERLANG_X_METRIC_PRODUCERS", metric_producers, metric_producers},
     {"OTEL_RESOURCE_DETECTORS", resource_detectors, existing_atom_list},
     {"OTEL_RESOURCE_DETECTOR_TIMEOUT", resource_detector_timeout, integer},

     {"OTEL_BSP_SCHEDULE_DELAY_MILLIS", bsp_scheduled_delay_ms, integer},
     {"OTEL_BSP_EXPORT_TIMEOUT_MILLIS", bsp_exporting_timeout_ms, integer},
     {"OTEL_BSP_MAX_QUEUE_SIZE", bsp_max_queue_size, integer},
     %% the following is not supported yet
     %% {"OTEL_BSP_MAX_EXPORT_BATCH_SIZE", bsp_max_export_batch_size, integer}

     {"OTEL_SSP_EXPORT_TIMEOUT_MILLIS", ssp_exporting_timeout_ms, integer}
    ];
config_mappings(span_limits) ->
    [{"OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT", attribute_count_limit, integer},
     {"OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT", attribute_value_length_limit, integer_infinity},
     {"OTEL_SPAN_EVENT_COUNT_LIMIT", event_count_limit, integer},
     {"OTEL_SPAN_LINK_COUNT_LIMIT", link_count_limit, integer},
     {"OTEL_EVENT_ATTRIBUTE_COUNT_LIMIT", attribute_per_event_limit, integer},
     {"OTEL_LINK_ATTRIBUTE_COUNT_LIMIT", attribute_per_link_limit, integer}%% ,
     %% {"OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT", attribute_value_length_limit, integer},
     %% {"OTEL_ATTRIBUTE_COUNT_LIMIT", attribute_per_link_limit, integer}
    ];
config_mappings(sweeper) ->
    [{"OTEL_SPAN_SWEEPER_INTERVAL", interval, integer_infinity},
     {"OTEL_SPAN_SWEEPER_STRATEGY", strategy, atom_or_fun},
     {"OTEL_SPAN_SWEEPER_SPAN_TTL", span_ttl, integer_infinity},
     {"OTEL_SPAN_SWEEPER_STORAGE_SIZE", storage_size, integer_infinity}
    ].

transform(_, undefined) ->
    undefined;
transform(existing_atom_list, [A | _]=List) when is_atom(A) ->
    List;
transform(existing_atom_list, String) when is_list(String) ->
    List = string:split(String, ",", all),
    lists:filtermap(fun(A) ->
                            try transform(existing_atom, string:trim(A)) of
                                Value ->
                                    {true, Value}
                            catch
                                _:_ ->
                                    false
                            end
                    end, List);
transform(exporter, Exporter) when Exporter =:= "otlp" ; Exporter =:= otlp ->
    {opentelemetry_exporter, #{}};
transform(exporter, Exporter) when Exporter =:= "jaeger" ; Exporter =:= jaeger ->
    ?LOG_WARNING("configuring jaeger exporter through OTEL_TRACES_EXPORTER is not yet supported ", []),
    none;
transform(exporter, Exporter)  when Exporter =:= "zipkin" ; Exporter =:= zipkin ->
    ?LOG_WARNING("configuring zipkin exporter through OTEL_TRACES_EXPORTER is not yet supported ", []),
    none;
transform(exporter, Exporter) when Exporter =:= "none" ; Exporter =:= none ->
    none;
transform(exporter, Value={Term, _}) when is_atom(Term) ->
    Value;
transform(exporter, UnknownExporter) when is_list(UnknownExporter) ->
    ?LOG_WARNING("unknown exporter ~p. falling back to default otlp", [UnknownExporter]),
    {opentelemetry_exporter, #{}};

transform(otlp_protocol, Proto) when Proto =:= "grpc"; Proto =:= grpc ->
  grpc;
transform(otlp_protocol, Proto) when Proto =:= "http/protobuf"; Proto =:= "http_protobuf"; Proto =:= http_protobuf ->
  http_protobuf;
transform(otlp_protocol, Proto) when Proto =:= "http/json"; Proto =:= "http_json"; Proto =:= http_json ->
  ?LOG_WARNING("The http/json transport protocol is not supported by the OTLP exporter, spans will not be exported.", []),
  undefined;
transform(otlp_protocol, Proto) ->
  ?LOG_WARNING("unknown ~ts transport protocol, spans will not be exported.", [Proto]),
  undefined;

transform(integer_infinity, infinity) ->
    infinity;
transform(integer_infinity, Value) ->
    transform(integer, Value);
transform(integer, Value) when is_integer(Value) ->
    Value;
transform(integer, Value) when is_list(Value) ->
    list_to_integer(Value);
transform(existing_atom, Value) when is_atom(Value) ->
    Value;
transform(existing_atom, Value) when is_list(Value) ->
    list_to_existing_atom(Value);
transform(atom_or_fun, Value) when is_list(Value) ->
    list_to_existing_atom(Value);
transform(atom_or_fun, Value) when is_atom(Value) ->
    Value;
transform(atom_or_fun, Value) when is_function(Value) ->
    Value;
transform(boolean, Value) when is_boolean(Value) ->
    Value;
transform(boolean, "true") ->
    true;
transform(boolean, "false") ->
    false;
transform(url, Value=#{}) ->
    Value;
transform(url, Value) ->
    uri_string:parse(Value);
%% convert sampler string to usable configuration term
transform(sampler, {"parentbased_always_on", _}) ->
    {parent_based, #{root => {always_on, #{}}}};
transform(sampler, {"parentbased_always_off", _}) ->
    {parent_based, #{root => {always_off, #{}}}};
transform(sampler, {"always_on", _}) ->
    {always_on, #{}};
transform(sampler, {"always_off", _}) ->
    {always_off, #{}};
transform(sampler, {"traceidratio", false}) ->
    {trace_id_ratio_based, #{ratio => 1.0}};
transform(sampler, {"traceidratio", Probability}) ->
    {trace_id_ratio_based, #{ratio => probability_string_to_float(Probability)}};
transform(sampler, {"parentbased_traceidratio", false}) ->
    {parent_based, #{root => {trace_id_ratio_based, #{ratio => 1.0}}}};
transform(sampler, {"parentbased_traceidratio", Probability}) ->
    {parent_based, #{root => {trace_id_ratio_based, #{ratio => probability_string_to_float(Probability)}}}};
transform(sampler, Value) ->
    Value;
transform(key_value_list, Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true ->
            Pairs = string:split(Value, ",", all),
            lists:filtermap(fun(Pair) ->
                                    case string:split(Pair, "=", leading) of
                                        [K, V] ->
                                            V1 = re:replace(string:trim(V), "^\"|\"$", "", [global, {return, list}]),
                                            {true, {string:trim(K), V1}};
                                        _ ->
                                            false
                                    end
                            end, Pairs);
        false ->
            Value
    end;
transform(propagators, [P | _]=Propagators) when is_atom(P) ->
    lists:filtermap(fun(Propagator) ->
                            case transform(propagator, Propagator) of
                                undefined ->
                                    false;
                                Value ->
                                    {true, Value}
                            end
                    end, Propagators);
transform(propagators, PropagatorsString) when is_list(PropagatorsString) ->
    Propagators = string:split(PropagatorsString, ",", all),
    lists:filtermap(fun(Propagator) when is_list(Propagator) ->
                          case transform(propagator, string:trim(Propagator)) of
                              undefined ->
                                  false;
                              Value ->
                                  {true, Value}
                          end
                  end, Propagators);

transform(propagator, Value) when Value =:= "tracecontext" ;
                                  Value =:= tracecontext ;
                                  Value =:= trace_context ->
    trace_context;
transform(propagator, Value) when Value =:= "baggage" ;
                                  Value =:= baggage ->
    baggage;
transform(propagator, Value) when Value =:= "b3multi" ;
                                  Value =:= b3multi ->
    b3multi;
transform(propagator, Value) when Value =:= "b3" ;
                                  Value =:= b3 ->
    b3;
%% TODO: support jager propagator format
%% transform(propagator, "jaeger") ->
%%     jaeger;
transform(propagator, Propagator) when is_atom(Propagator) ->
    Propagator;
transform(propagator, Propagator) ->
    ?LOG_WARNING("Ignoring unknown propagator ~ts in OS environment variable $OTEL_PROPAGATORS",
                 [Propagator]),
    undefined;
transform(span_processors, Processors) when is_list(Processors) ->
    [transform(span_processor, Processor) || Processor <- Processors];
transform(span_processors, Unknown) ->
    ?LOG_WARNING("processors value must be a list, but ~ts given. No span processors will be used", [Unknown]),
    [];
transform(span_processor, batch) ->
    {otel_batch_processor, #{}};
transform(span_processor, simple) ->
    {otel_simple_processor, #{}};
transform(span_processor, SpanProcessor) ->
    SpanProcessor;
transform(readers, Readers) ->
    Readers;
transform(exemplar_filter, ExemplarFilter) when ExemplarFilter =:= "always_on" ;
                                                ExemplarFilter =:= always_on ->
    always_on;
transform(exemplar_filter, ExemplarFilter) when ExemplarFilter =:= "always_off" ;
                                                ExemplarFilter =:= always_off ->
    always_off;
transform(exemplar_filter, _) ->
    trace_based;
transform(metric_producers, MetricProducers) when is_list(MetricProducers) ->
    lists:filtermap(fun({ProducerModule, ProducerConfig}) when is_atom(ProducerModule) ->
                            {true, {ProducerModule, ProducerConfig}};
                       (ProducerModule) when is_atom(ProducerModule) ->
                            {true, {ProducerModule, []}};
                       (_) ->
                            false
                    end, MetricProducers);
transform(views, Views) ->
    Views.


probability_string_to_float(Probability) ->
    try list_to_float(Probability) of
        Float ->
            Float
    catch
        error:badarg when Probability =:= "1" ->
            1.0;
         error:badarg when Probability =:= "0" ->
            0.0;
        error:badarg ->
            ?LOG_WARNING("Unable to convert $OTEL_TRACE_SAMPLER_ARG string value ~ts to float, using 1.0"),
            1.0
    end.
