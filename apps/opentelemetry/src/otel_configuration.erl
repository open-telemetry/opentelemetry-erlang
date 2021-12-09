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
%% @doc Merges environment variable configuration values with application
%% configuration. The OS environment variables take precedence over the
%% application environment.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_configuration).

-export([merge_with_os/1,
         merge_list_with_environment/3,
         report_cb/1]).

-include_lib("kernel/include/logger.hrl").

%% required configuration
%% using a map instead of a record because there can be more values
-type t() :: #{log_level := atom(),
               register_loaded_applications := boolean(),
               text_map_propagators := [atom()],
               traces_exporter := {atom(), term()},
               processors := list(),
               attribute_count_limit := integer(),
               attribute_value_length_limit := integer() | infinity,
               event_count_limit := integer(),
               link_count_limit := integer(),
               attribute_per_event_limit := integer(),
               attribute_per_link_limit := integer()}.

new() ->
    #{log_level => info,
      register_loaded_applications => true,
      text_map_propagators => [trace_context, baggage],
      traces_exporter => {opentelemetry_exporter, #{}},
      processors => [{otel_batch_processor, #{scheduled_delay_ms => 5000,
                                              exporting_timeout_ms => 30000,
                                              max_queue_size => 2048,
                                              exporter => {opentelemetry_exporter, #{}}}}],
      sampler => {parent_based, #{root => always_on}},
      attribute_count_limit => 128,
      attribute_value_length_limit => infinity,
      event_count_limit => 128,
      link_count_limit => 128,
      attribute_per_event_limit => 128,
      attribute_per_link_limit => 128}.

-spec merge_with_os(list()) -> t().
merge_with_os(AppEnv) ->
    ConfigMap = new(),

    lists:foldl(fun(F, Acc) ->
                        F(AppEnv, Acc)
                end, ConfigMap, [fun span_limits/2,
                                 fun general/2,
                                 fun sampler/2,
                                 fun processors/2]).

-spec span_limits(list(), t()) -> t().
span_limits(AppEnv, ConfigMap) ->
    merge_list_with_environment(config_mappings(span_limits), AppEnv, ConfigMap).

-spec general(list(), t()) -> t().
general(AppEnv, ConfigMap) ->
    merge_list_with_environment(config_mappings(general_sdk), AppEnv, ConfigMap).

-spec processors(list(), t()) -> t().
processors(AppEnv, ConfigMap) ->
    %% builtin processors have OS environment configuration per type of processor
    %% so we must do an environment merge even with the default processor from
    %% the ConfigMap to ensure we get all the user's configuration
    Processors = proplists:get_value(processors, AppEnv, maps:get(processors, ConfigMap)),

    ProcessorsConfig = lists:map(fun({Name, Opts}) ->
                                         {Name, merge_with_environment(config_mappings(Name), Opts)}
                                 end, Processors),

    ConfigMap#{processors := ProcessorsConfig}.

%% sampler configuration is unique since it has the _ARG that is a sort of
%% sub-configuration of the sampler config, and isn't a list.
-spec sampler(list(), t()) -> t().
sampler(AppEnv, ConfigMap) ->
    case os:getenv("OTEL_TRACES_SAMPLER") of
        false ->
            case proplists:get_value(sampler, AppEnv) of
                undefined ->
                    ConfigMap;
                Sampler ->
                    try transform(sampler, Sampler) of
                        TransformedSampler ->
                            ConfigMap#{sampler := TransformedSampler}
                    catch
                        Kind:Reason:StackTrace ->
                            ?LOG_INFO(#{source => transform,
                                        kind => Kind,
                                        reason => Reason,
                                        os_var => "OTEL_TRACES_SAMPLER",
                                        key => sampler,
                                        transform => sampler,
                                        value => Sampler,
                                        stacktrace => StackTrace},
                                      #{report_cb => fun ?MODULE:report_cb/1}),
                            ConfigMap
                    end
            end;
        OSEnvSampler->
            SamplerTuple = {OSEnvSampler, os:getenv("OTEL_TRACES_SAMPLER_ARG")},
            try transform(sampler, SamplerTuple) of
                TransformedSampler ->
                    ConfigMap#{sampler := TransformedSampler}
            catch
                Kind:Reason:StackTrace ->
                    ?LOG_INFO(#{source => transform,
                                kind => Kind,
                                reason => Reason,
                                os_var => "OTEL_TRACES_SAMPLER",
                                key => sampler,
                                transform => sampler,
                                value => SamplerTuple,
                                stacktrace => StackTrace},
                              #{report_cb => fun ?MODULE:report_cb/1}),
                    ConfigMap
            end
    end.

-spec merge_list_with_environment([{OSVar, Key, Transform}], list(), t()) -> t()
              when OSVar :: string(),
                   Key :: atom(),
                   Transform :: atom().
merge_list_with_environment(ConfigMappings, AppEnv, ConfigMap) ->
    lists:foldl(fun({OSVar, Key, Transform}, Acc) ->
                        case os:getenv(OSVar) of
                            false ->
                                case lists:keyfind(Key, 1, AppEnv) of
                                    false ->
                                        Acc;
                                    {_, Value} ->
                                        %% transform even the value from the
                                        %% application environment to ensure it
                                        %% is of the right type/format
                                        try transform(Transform, Value) of
                                            TransformedValue ->
                                                Acc#{Key := TransformedValue}
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
                                                Acc
                                        end
                                end;
                            OSVal ->
                                try transform(Transform, OSVal) of
                                    TransformedValue ->
                                        Acc#{Key := TransformedValue}
                                catch
                                    Kind:Reason:StackTrace ->
                                        ?LOG_INFO(#{source => transform,
                                                    kind => Kind,
                                                    reason => Reason,
                                                    os_var => OSVar,
                                                    key => Key,
                                                    transform => Transform,
                                                    value => OSVal,
                                                    stacktrace => StackTrace},
                                                  #{report_cb => fun ?MODULE:report_cb/1}),
                                        Acc
                                end
                        end
                end, ConfigMap, ConfigMappings).

-spec merge_with_environment([{OSVar, Key, Transform}], map()) -> map()
              when OSVar :: string(),
                   Key :: atom(),
                   Transform :: atom().
merge_with_environment(ConfigMappings, Opts) ->
    lists:foldl(fun({OSVar, Key, Transform}, Acc) ->
                        case os:getenv(OSVar) of
                            false ->
                                Acc;
                            OSVal ->
                                try transform(Transform, OSVal) of
                                    Transformed ->
                                        %% unlike the top level config `t()' not every config
                                        %% key is guaranteed to exist here because it could
                                        %% have been read from the application environment.
                                        %% so use `=>' and not `:='
                                        Acc#{Key => Transformed}
                                catch
                                    Kind:Reason:StackTrace ->
                                        ?LOG_INFO(#{source => transform,
                                                    kind => Kind,
                                                    reason => Reason,
                                                    os_var => OSVar,
                                                    key => Key,
                                                    transform => Transform,
                                                    value => OSVal,
                                                    stacktrace => StackTrace},
                                                  #{report_cb => fun ?MODULE:report_cb/1}),
                                        Acc
                                end
                        end
                end, Opts, ConfigMappings).

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
    [{"OTEL_LOG_LEVEL", log_level, existing_atom},
     {"OTEL_REGISTER_LOADED_APPLICATIONS", register_loaded_applications, boolean},
     {"OTEL_PROPAGATORS", text_map_propagators, propagators},
     {"OTEL_TRACES_EXPORTER", traces_exporter, exporter},
     {"OTEL_METRICS_EXPORTER", metrics_exporter, exporter}];
config_mappings(otel_batch_processor) ->
    [{"OTEL_BSP_SCHEDULE_DELAY_MILLIS", scheduled_delay_ms, integer}, %% 5000,
     {"OTEL_BSP_EXPORT_TIMEOUT_MILLIS", exporting_timeout_ms, integer}, %% 30000,
     {"OTEL_BSP_MAX_QUEUE_SIZE", max_queue_size, integer}, %% 2048,
     %% a second usage of OTEL_TRACES_EXPORTER to set the exporter used by batch processor
     {"OTEL_TRACES_EXPORTER", exporter, exporter} %% "otlp",
     %% the following are not supported yet
     %% {"OTEL_BSP_MAX_EXPORT_BATCH_SIZE", max_export_batch_size, 512}
    ];
config_mappings(span_limits) ->
    [{"OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT", attribute_count_limit, integer},
     {"OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT", attribute_value_length_limit, integer_infinity},
     {"OTEL_SPAN_EVENT_COUNT_LIMIT", event_count_limit, integer},
     {"OTEL_SPAN_LINK_COUNT_LIMIT", link_count_limit, integer},
     {"OTEL_EVENT_ATTRIBUTE_COUNT_LIMIT", attribute_per_event_limit, integer},
     {"OTEL_LINK_ATTRIBUTE_COUNT_LIMIT", attribute_per_link_limit, integer}%% ,
     %% {"OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT", attribute_value_length_limit, 128, integer},
     %% {"OTEL_ATTRIBUTE_COUNT_LIMIT", attribute_per_link_limit, 128, integer}
    ];
config_mappings(_) ->
     [].

transform(_, undefined) ->
    undefined;
transform(exporter, "otlp") ->
    {opentelemetry_exporter, #{}};
transform(exporter, "jaeger") ->
    ?LOG_WARNING("configuring jaeger exporter through OTEL_TRACES_EXPORTER is not yet supported ", []),
    undefined;
transform(exporter, "zipkin") ->
    ?LOG_WARNING("configuring zipkin exporter through OTEL_TRACES_EXPORTER is not yet supported ", []),
    undefined;
transform(exporter, "none") ->
    undefined;
transform(exporter, Value={Term, _}) when is_atom(Term) ->
    Value;
transform(exporter, UnknownExporter) when is_list(UnknownExporter) ->
    ?LOG_WARNING("unknown exporter ~p. falling back to default otlp", [UnknownExporter]),
    {opentelemetry_exporter, #{}};

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
    {parent_based, #{root => always_on}};
transform(sampler, {"parentbased_always_off", _}) ->
    {parent_based, #{root => always_off}};
transform(sampler, {"always_on", _}) ->
    always_on;
transform(sampler, {"always_off", _}) ->
    always_off;
transform(sampler, {"traceidratio", false}) ->
    {trace_id_ratio_based, 1.0};
transform(sampler, {"traceidratio", Probability}) ->
    {trace_id_ratio_based, probability_string_to_float(Probability)};
transform(sampler, {"parentbased_traceidratio", false}) ->
    {parent_based, #{root => {trace_id_ratio_based, 1.0}}};
transform(sampler, {"parentbased_traceidratio", Probability}) ->
    {parent_based, #{root => {trace_id_ratio_based, probability_string_to_float(Probability)}}};
transform(sampler, Value) ->
    Value;
transform(key_value_list, Value) when is_list(Value) ->
    case io_lib:printable_list(Value) of
        true ->
            Pairs = string:split(Value, ",", all),
            lists:filtermap(fun(Pair) ->
                                    case string:split(Pair, "=", all) of
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
transform(propagator, Propagator) ->
    ?LOG_WARNING("Ignoring unknown propagator ~ts in OS environment variable $OTEL_PROPAGATORS",
                 [Propagator]),
    undefined.

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
