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
         merge_list_with_environment/2]).

-include_lib("kernel/include/logger.hrl").

-spec merge_with_os(list()) -> list().
merge_with_os(Opts) ->
    general(
      sampler(
        processors(Opts))).

general(Opts) ->
    merge_list_with_environment(config_mappings(general_sdk), Opts).

processors(AppEnvOpts) ->
    Processors = proplists:get_value(processors, AppEnvOpts, [{otel_batch_processor, #{}}]),

    Config = lists:map(fun({Name, Opts}) ->
                               {Name, merge_with_environment(config_mappings(Name), Opts)}
                       end, Processors),

    lists:keystore(processors, 1, AppEnvOpts, {processors, Config}).

%% sampler configuration is unique since it has the _ARG that is a sort of
%% sub-configuration of the sampler config, and isn't a list.
sampler(AppEnvOpts) ->
    Sampler = proplists:get_value(sampler, AppEnvOpts, {parent_based, #{root => always_on}}),

    Sampler1 = case os:getenv("OTEL_TRACES_SAMPLER") of
                   false ->
                       Sampler;
                   OSEnvSampler->
                       transform(sampler, {OSEnvSampler, os:getenv("OTEL_TRACES_SAMPLER_ARG")})
               end,

    lists:keystore(sampler, 1, AppEnvOpts, {sampler, Sampler1}).

-spec merge_list_with_environment([{OSVar, Key, Default, Transform}], list()) -> list()
              when OSVar :: string(),
                   Key :: atom(),
                   Default :: term(),
                   Transform :: atom().
merge_list_with_environment(ConfigMappings, Opts) ->
    lists:foldl(fun({OSVar, Key, Default, Transform}, Acc) ->
                        case os:getenv(OSVar) of
                            false ->
                                case lists:keyfind(Key, 1, Opts) of
                                    false ->
                                        %% set to Default if it doesn't exist
                                        [{Key, transform(Transform, Default)} | Acc];
                                    _ ->
                                        Acc
                                end;
                            Value ->
                                lists:keystore(Key, 1, Acc, {Key, transform(Transform, Value)})
                        end
                end, Opts, ConfigMappings).

-spec merge_with_environment([{OSVar, Key, Default, Transform}], map()) -> map()
              when OSVar :: string(),
                   Key :: atom(),
                   Default :: term(),
                   Transform :: atom().
merge_with_environment(ConfigMappings, Opts) ->
    lists:foldl(fun({OSVar, Key, Default, Transform}, Acc) ->
                        case os:getenv(OSVar) of
                            false ->
                                %% set to Default if it doesn't exist
                                maps:update_with(Key, fun(X) -> X end,
                                                 transform(Transform, Default), Acc);
                            Value ->
                                maps:put(Key, transform(Transform, Value), Acc)
                        end
                end, Opts, ConfigMappings).

config_mappings(general_sdk) ->
    [{"OTEL_LOG_LEVEL", log_level, "info", existing_atom},
     {"OTEL_PROPAGATORS", propagators, "tracecontext,baggage", propagators},
     {"OTEL_TRACES_EXPORTER", traces_exporter, "otlp", exporter},
     {"OTEL_METRICS_EXPORTER", metrics_exporter, undefined, exporter}];
config_mappings(otel_batch_processor) ->
    [{"OTEL_BSP_SCHEDULE_DELAY_MILLIS", scheduled_delay_ms, 5000, integer},
     {"OTEL_BSP_EXPORT_TIMEOUT_MILLIS", exporting_timeout_ms, 30000, integer},
     {"OTEL_BSP_MAX_QUEUE_SIZE", max_queue_size, 2048, integer},
     %% a second usage of OTEL_TRACES_EXPORTER to set the exporter used by batch processor
     {"OTEL_TRACES_EXPORTER", exporter, "otlp", exporter}
     %% the following are not supported yet
     %% {"OTEL_BSP_MAX_EXPORT_BATCH_SIZE", max_export_batch_size, 512}
    ];
%% span limit not supported
%% config_mappings(span_limits) ->
%%     [{"OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT", attribute_count_limit, 1000, integer},
%%      {"OTEL_SPAN_EVENT_COUNT_LIMIT", event_count_limit, 1000, integer},
%%      {"OTEL_SPAN_LINK_COUNT_LIMIT", link_count_limit 1000, integer}];
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
transform(exporter, "prometheus") ->
    prometheus;
transform(exporter, UnknownExporter) ->
    ?LOG_WARNING("unknown exporter ~p. falling back to default otlp", [UnknownExporter]),
    {opentelemetry_exporter, #{}};
transform(integer, Value) when is_integer(Value) ->
    Value;
transform(integer, Value) when is_list(Value) ->
    list_to_integer(Value);
transform(existing_atom, Value) when is_list(Value) ->
    list_to_existing_atom(Value);
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

transform(propagator, "tracecontext") ->
    fun otel_tracer_default:w3c_propagators/0;
transform(propagator, "baggage") ->
    fun otel_baggage:get_text_map_propagators/0;
transform(propagator, Propagator) ->
    ?LOG_WARNING("Ignoring uknown propagator ~ts in OS environment variable $OTEL_PROPAGATORS",
                 [Propagator]),
    undefined;
transform(key_value_list, Value) when is_list(Value) ->
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

transform(string, Value) ->
    Value.

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
