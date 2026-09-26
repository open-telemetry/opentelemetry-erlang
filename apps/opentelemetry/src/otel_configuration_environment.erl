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
%% Builds native declarative configuration only for zero-config startup.
%% Environment values are read here, before SDK components are initialized.
%% @private
%%%------------------------------------------------------------------------
-module(otel_configuration_environment).

-export([configuration/0]).

-include_lib("kernel/include/logger.hrl").

-spec configuration() -> map().
configuration() ->
    #{file_format => <<"1.1">>,
      disabled => read("OTEL_SDK_DISABLED", fun boolean/1, false),
      log_level => read("OTEL_LOG_LEVEL", fun log_level/1, info),
      resource => resource(),
      propagator => #{composite => propagators()},
      attribute_limits => limits(attribute_limits),
      tracer_provider =>
          #{processors => [{batch, batch()}],
            sampler => sampler(),
            limits => limits(span_limits),
            id_generator => read("OTEL_ID_GENERATOR", fun list_to_existing_atom/1,
                                 otel_id_generator)},
      distribution => #{erlang => erlang_distribution()}}.

batch() ->
    #{exporter => exporter(),
      schedule_delay => read_first(["OTEL_BSP_SCHEDULE_DELAY", "OTEL_BSP_SCHEDULE_DELAY_MILLIS"],
                                    fun non_negative_integer/1, 5000),
      export_timeout => read_first(["OTEL_BSP_EXPORT_TIMEOUT", "OTEL_BSP_EXPORT_TIMEOUT_MILLIS"],
                                    fun non_negative_integer/1, 30000),
      max_queue_size => read("OTEL_BSP_MAX_QUEUE_SIZE", fun positive_integer/1, 2048)}.

exporter() ->
    case read("OTEL_TRACES_EXPORTER", fun(V) -> choice(V, [{"otlp", otlp}, {"none", none}]) end, otlp) of
        none -> none;
        otlp ->
            Transport = read_first(otlp_names("PROTOCOL"), fun protocol/1, otlp_http),
            DefaultEndpoint = case Transport of
                                  otlp_http -> <<"http://localhost:4318/v1/traces">>;
                                  otlp_grpc -> <<"http://localhost:4317">>
                              end,
            %% Only the general HTTP endpoint gets the signal path appended.
            General = read("OTEL_EXPORTER_OTLP_ENDPOINT",
                           fun(V) -> general_endpoint(V, Transport) end, DefaultEndpoint),
            Endpoint = read("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", fun endpoint/1, General),
            {Transport,
             #{endpoint => Endpoint,
               headers => read_first(otlp_names("HEADERS"),
                                      fun(V) -> pairs(V, "OTEL_EXPORTER_OTLP_HEADERS") end, []),
               compression => read_first(otlp_names("COMPRESSION"),
                                          fun(V) -> choice(V, [{"gzip", gzip}, {"none", none}]) end,
                                          none)}}
    end.

otlp_names(Suffix) ->
    ["OTEL_EXPORTER_OTLP_TRACES_" ++ Suffix, "OTEL_EXPORTER_OTLP_" ++ Suffix].

protocol(Value) ->
    choice(Value, [{"grpc", otlp_grpc}, {"http/protobuf", otlp_http},
                   {"http_protobuf", otlp_http}]).

endpoint(Value) ->
    Binary = to_binary(Value),
    #{scheme := Scheme, host := Host} = uri_string:parse(Binary),
    true = (Scheme =:= <<"http">> orelse Scheme =:= <<"https">>) andalso Host =/= <<>>,
    Binary.

general_endpoint(Value, otlp_grpc) -> endpoint(Value);
general_endpoint(Value, otlp_http) ->
    Uri = #{} = uri_string:parse(endpoint(Value)),
    Path = string:trim(to_binary(maps:get(path, Uri, <<>>)), trailing, "/"),
    to_binary(uri_string:recompose(Uri#{path => <<Path/binary, "/v1/traces">>})).

resource() ->
    Attributes0 = maps:from_list(read("OTEL_RESOURCE_ATTRIBUTES",
                                      fun(V) -> pairs(V, "OTEL_RESOURCE_ATTRIBUTES") end, [])),
    Attributes1 = put_resource_attribute("OTEL_SERVICE_NAME", <<"service.name">>, Attributes0),
    #{attributes => put_resource_attribute("OTEL_SERVICE_INSTANCE", <<"service.instance.id">>, Attributes1)}.

put_resource_attribute(Name, Key, Attributes) ->
    case read(Name, fun to_binary/1, undefined) of
        undefined -> Attributes;
        Value -> Attributes#{Key => Value}
    end.

pairs(Value, Name) ->
    lists:filtermap(
      fun(Entry) ->
              try
                  [Key0, Value0] = string:split(Entry, "=", leading),
                  Key = string:trim(Key0),
                  true = Key =/= "",
                  {true, {to_binary(Key),
                          to_binary(uri_string:percent_decode(to_binary(string:trim(Value0))))}}
              catch
                  error:_ -> warn(Name), false
              end
      end, string:split(Value, ",", all)).

propagators() ->
    read("OTEL_PROPAGATORS",
         fun(Value) ->
                 lists:uniq(lists:filtermap(
                   fun("none") -> false;
                      (Name) ->
                           try choice(Name, [{"tracecontext", trace_context}, {"baggage", baggage},
                                              {"b3", b3}, {"b3multi", b3multi}]) of
                               Propagator -> {true, Propagator}
                           catch error:_ -> warn("OTEL_PROPAGATORS"), false
                           end
                   end, [string:trim(V) || V <- string:split(Value, ",", all)]))
         end, [trace_context, baggage]).

sampler() ->
    Kind = read("OTEL_TRACES_SAMPLER",
                fun(V) -> choice(V, [{"always_on", always_on}, {"always_off", always_off},
                                      {"traceidratio", traceidratio},
                                      {"parentbased_always_on", parentbased_always_on},
                                      {"parentbased_always_off", parentbased_always_off},
                                      {"parentbased_traceidratio", parentbased_traceidratio}]) end,
                parentbased_always_on),
    case Kind of
        always_on -> always_on;
        always_off -> always_off;
        parentbased_always_on -> {parent_based, #{root => always_on}};
        parentbased_always_off -> {parent_based, #{root => always_off}};
        traceidratio -> {trace_id_ratio_based, sampler_ratio()};
        parentbased_traceidratio -> {parent_based, #{root => {trace_id_ratio_based, sampler_ratio()}}}
    end.

sampler_ratio() -> read("OTEL_TRACES_SAMPLER_ARG", fun ratio/1, 1.0).

ratio(Value) ->
    Number = case string:to_float(Value) of
                 {Float, []} -> Float;
                 _ -> float(list_to_integer(Value))
             end,
    true = Number >= 0.0 andalso Number =< 1.0,
    Number.

limits(attribute_limits) ->
    #{attribute_count_limit => read("OTEL_ATTRIBUTE_COUNT_LIMIT", fun non_negative_integer/1, 128),
      attribute_value_length_limit => read("OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT", fun non_negative_integer/1, infinity)};
limits(span_limits) ->
    %% Omitted span limits inherit the global attribute limits during resolution.
    mappings([{"OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT", attribute_count_limit},
              {"OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT", attribute_value_length_limit},
              {"OTEL_SPAN_EVENT_COUNT_LIMIT", event_count_limit},
              {"OTEL_SPAN_LINK_COUNT_LIMIT", link_count_limit},
              {"OTEL_EVENT_ATTRIBUTE_COUNT_LIMIT", event_attribute_count_limit},
              {"OTEL_LINK_ATTRIBUTE_COUNT_LIMIT", link_attribute_count_limit}],
             fun non_negative_integer/1).

mappings(Mappings, Parser) ->
    lists:foldl(fun({Name, Key}, Acc) ->
                        case read(Name, Parser, undefined) of
                            undefined -> Acc;
                            Value -> Acc#{Key => Value}
                        end
                end, #{}, Mappings).

erlang_distribution() ->
    #{create_application_tracers =>
          read_first(["OTEL_CREATE_APPLICATION_TRACERS", "OTEL_REGISTER_LOADED_APPLICATIONS"],
                     fun boolean/1, true),
      deny_list => read("OTEL_DENY_LIST", fun atom_list/1, []),
      resource_detectors => read("OTEL_RESOURCE_DETECTORS", fun atom_list/1, []),
      resource_detector_timeout => read("OTEL_RESOURCE_DETECTOR_TIMEOUT", fun non_negative_integer/1, 5000),
      sweeper => maps:merge(
                   mappings([{"OTEL_SPAN_SWEEPER_INTERVAL", interval},
                             {"OTEL_SPAN_SWEEPER_SPAN_TTL", span_ttl},
                             {"OTEL_SPAN_SWEEPER_STORAGE_SIZE", storage_size}], fun timeout/1),
                   mappings([{"OTEL_SPAN_SWEEPER_STRATEGY", strategy}],
                            fun(V) -> choice(V, [{"drop", drop}, {"end_span", end_span},
                                                 {"failed_attribute_and_end_span", failed_attribute_and_end_span}]) end))}.

atom_list(Value) ->
    [list_to_existing_atom(string:trim(V)) || V <- string:split(Value, ",", all)].

to_binary(Value) ->
    case unicode:characters_to_binary(Value) of
        Binary when is_binary(Binary) -> Binary;
        _ -> error(invalid_unicode)
    end.

boolean(Value) -> choice(string:lowercase(Value), [{"true", true}, {"false", false}]).

log_level(Value) ->
    choice(Value, [{"trace", trace}, {"debug", debug}, {"info", info},
                   {"warn", warn}, {"warning", warn}, {"error", error}, {"fatal", fatal}]).

non_negative_integer(Value) ->
    Integer = list_to_integer(Value),
    true = Integer >= 0,
    Integer.

positive_integer(Value) ->
    Integer = non_negative_integer(Value),
    true = Integer > 0,
    Integer.

timeout("infinity") -> infinity;
timeout(Value) -> non_negative_integer(Value).

choice(Value, Choices) ->
    {Value, Result} = lists:keyfind(Value, 1, Choices),
    Result.

read(Name, Parser, Default) -> read_first([Name], Parser, Default).

read_first([], _Parser, Default) -> Default;
read_first([Name | Rest], Parser, Default) ->
    case os:getenv(Name) of
        Unset when Unset =:= false; Unset =:= "" -> read_first(Rest, Parser, Default);
        Value ->
            try Parser(Value)
            catch error:_ -> warn(Name), read_first(Rest, Parser, Default)
            end
    end.

warn(Name) ->
    ?LOG_WARNING("Ignoring invalid or unsupported OpenTelemetry environment variable ~ts", [Name]).
