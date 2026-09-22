%% Copyright 2026, OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% http://www.apache.org/licenses/LICENSE-2.0
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @private
%% Checks property names only in maps interpreted by the stable SDK. Value and
%% component validation remains in otel_configuration_sdk. Custom component
%% properties, attribute maps, and retained signal configuration are opaque.
-module(otel_configuration_keys).

-import(otel_configuration_utils, [find/2, fail/1]).

-export([validate/2, validate_tracer_provider/1, component_name/2, processor_fields/1]).

-include_lib("kernel/include/logger.hrl").

-spec validate(map(), otel_configuration_model:source()) -> ok.
validate(Configuration, Source) ->
    walk(sdk, Configuration, [], Source).

-spec validate_tracer_provider(map()) -> ok.
validate_tracer_provider(Configuration) ->
    walk(tracer_provider, Configuration, [tracer_provider], application_env).

walk({list, Kind}, Values, Path, Source) when is_list(Values) ->
    lists:foreach(fun(Value) -> walk(Kind, Value, Path, Source) end, Values);
walk({list, _}, _Value, _Path, _Source) -> ok;
walk({component, Category}, Value, Path, Source) ->
    case otel_configuration_utils:component_entry(Value) of
        {ok, {Name, Config}} -> component(Category, Name, Config, Path, Source);
        error -> ok
    end;
walk(Kind, Map, Path, Source) when is_map(Map) ->
    Fields = fields(Kind),
    Allowed = Fields ++ [atom_to_binary(Key, utf8) || Key <- Fields],
    check_resource_shape(Kind, Map, Allowed, Path, Source),
    maps:foreach(
      fun(Key, _Value) when is_atom(Key); is_binary(Key) ->
              case lists:member(Key, Allowed) of
                  true -> ok;
                  false -> unknown_key(Source, Path ++ [Key])
              end;
         (Key, _Value) -> fail({invalid_configuration, Path, {invalid_property_name, Key}})
      end, Map),
    lists:foreach(
      fun({Key, ChildKind}) ->
              case find(Key, Map) of
                  {ok, Value} -> walk(ChildKind, Value, Path ++ [Key], Source);
                  error -> ok
              end
      end, children(Kind));
walk(_Kind, _Value, _Path, _Source) -> ok.

component(Category, Name, Config, Path, Source) ->
    case component_kind(Category, Name) of
        {ok, Kind} ->
            ComponentPath = case Category of
                                exporter -> [exporter, Kind];
                                _ -> Path ++ [Kind]
                            end,
            walk(Kind, Config, ComponentPath, Source);
        error -> ok
    end.

%% One component-name table drives both validation and resolution. Native module
%% aliases are accepted only as atoms; JSON cannot select Erlang module names.
-spec component_name(atom(), term()) -> term().
component_name(Category, Name) ->
    case component_kind(Category, Name) of
        {ok, Kind} -> Kind;
        error -> Name
    end.

component_kind(Category, Name) ->
    case lists:search(fun({Alias, _}) -> component_matches(Alias, Name) end,
                      components(Category)) of
        {value, {_, Kind}} -> {ok, Kind};
        false -> error
    end.

component_matches({native, Name}, Value) -> Value =:= Name;
component_matches(Name, Value) -> Value =:= Name orelse Value =:= atom_to_binary(Name, utf8).

unknown_key(declarative, Path) ->
    ?LOG_WARNING("Ignoring unknown OpenTelemetry configuration property ~p", [Path],
                 #{otel_configuration_path => Path});
unknown_key(_Source, Path) ->
    fail({invalid_configuration, Path, unknown_property}).

%% resource remains a valid top-level key. Only an unmistakable legacy flat
%% attribute map gets the migration error; ordinary misspellings use unknown_key.
check_resource_shape(resource, Map, Allowed, Path, Source) when Source =/= declarative ->
    Keys = maps:keys(Map),
    case not lists:any(fun(Key) -> lists:member(Key, Allowed) end, Keys)
         andalso lists:any(fun dotted_key/1, Keys) of
        true -> fail({invalid_configuration, Path, legacy_configuration_not_supported});
        false -> ok
    end;
check_resource_shape(_, _, _, _, _) -> ok.

dotted_key(Key) when is_atom(Key) -> dotted_key(atom_to_binary(Key, utf8));
dotted_key(Key) when is_binary(Key) -> binary:match(Key, <<".">>) =/= nomatch;
dotted_key(_) -> false.

fields(sdk) ->
    [file_format, disabled, log_level, resource, propagator, attribute_limits,
     tracer_provider, meter_provider, logger_provider, distribution];
fields(resource) -> [attributes, attributes_list, schema_url, 'detection/development'];
fields(attribute) -> [name, type, value];
fields(header) -> [name, value];
fields(propagator) -> [composite, composite_list];
fields(attribute_limits) -> [attribute_count_limit, attribute_value_length_limit, attribute_value_depth_limit];
fields(limits) -> fields(attribute_limits) ++
    [event_count_limit, link_count_limit, event_attribute_count_limit, link_attribute_count_limit];
fields(tracer_provider) -> [processors, sampler, limits, id_generator, 'tracer_configurator/development'];
fields(Kind) when Kind =:= batch; Kind =:= simple ->
    [Key || {Key, _Rule} <- processor_fields(Kind)];
fields(otlp_http) -> [encoding | fields(otlp_grpc)];
fields(otlp_grpc) -> [endpoint, headers, headers_list, compression, tls, timeout, max_request_size, max_response_size];
fields(opentelemetry_exporter) ->
    [endpoints, headers, protocol, compression, ssl_options, channel_opts, httpc_options];
fields(http_tls) -> [ca_file, key_file, cert_file];
fields(grpc_tls) -> [insecure | fields(http_tls)];
fields(parent_based) -> [root, remote_parent_sampled, remote_parent_not_sampled,
                       local_parent_sampled, local_parent_not_sampled];
fields(trace_id_ratio_based) -> [ratio];
fields(distribution) -> [erlang];
fields(erlang) -> [create_application_tracers, deny_list, resource_detectors, resource_detector_timeout, sweeper];
fields(sweeper) -> [interval, span_ttl, storage_size, strategy];
fields(console) -> [];
fields(always_on) -> [];
fields(always_off) -> [];
fields(random) -> [];
fields(tracecontext) -> [];
fields(baggage) -> [];
fields(b3) -> [];
fields(b3multi) -> [].

children(sdk) -> [{resource, resource}, {propagator, propagator}, {attribute_limits, attribute_limits},
                  {tracer_provider, tracer_provider}, {distribution, distribution}];
children(resource) -> [{attributes, {list, attribute}}];
children(propagator) -> [{composite, {list, {component, propagator}}}];
children(tracer_provider) -> [{processors, {list, {component, processor}}},
                              {sampler, {component, sampler}}, {limits, limits},
                              {id_generator, {component, id_generator}}];
children(batch) -> [{exporter, {component, exporter}}];
children(simple) -> children(batch);
children(otlp_http) -> [{tls, http_tls}, {headers, {list, header}}];
children(otlp_grpc) -> [{tls, grpc_tls}, {headers, {list, header}}];
children(parent_based) -> [{Key, {component, sampler}} || Key <- fields(parent_based)];
children(distribution) -> [{erlang, erlang}];
children(erlang) -> [{sweeper, sweeper}];
children(_) -> [].

components(processor) -> [{batch, batch}, {simple, simple},
                          {{native, otel_batch_processor}, batch}, {{native, otel_simple_processor}, simple}];
components(exporter) -> [{otlp_http, otlp_http}, {otlp_grpc, otlp_grpc}, {console, console},
                         {{native, opentelemetry_exporter}, opentelemetry_exporter}];
components(sampler) -> [{always_on, always_on}, {always_off, always_off},
                        {trace_id_ratio_based, trace_id_ratio_based}, {parent_based, parent_based}];
components(id_generator) -> [{random, random}];
components(propagator) -> [{tracecontext, tracecontext}, {{native, trace_context}, tracecontext},
                           {baggage, baggage}, {b3, b3}, {b3multi, b3multi}].

%% Property names and value rules live together for the built-in processors.
-spec processor_fields(batch | simple) ->
          [{atom(), exporter | non_negative_integer | positive_integer |
                    non_negative_integer_or_infinity | {unsupported, positive_integer}}].
processor_fields(batch) ->
    [{max_export_batch_size, {unsupported, positive_integer}},
     {exporter, exporter},
     {export_timeout, non_negative_integer},
     {schedule_delay, non_negative_integer},
     {max_queue_size, positive_integer},
     {check_table_size, non_negative_integer_or_infinity}];
processor_fields(simple) ->
    [{exporter, exporter}, {export_timeout, non_negative_integer}].
