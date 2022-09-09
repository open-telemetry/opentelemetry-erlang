%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(otel_otlp_common).

-export([to_instrumentation_scope_proto/1,
         to_attributes/1,
         to_any_value/1,
         to_key_value_list/1,
         to_binary/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

to_instrumentation_scope_proto(undefined) ->
    #{};
to_instrumentation_scope_proto(#instrumentation_scope{name=Name,
                                                      version=Version,
                                                      schema_url=undefined}) ->
    #{scope => #{name => Name,
                 version => Version}};
to_instrumentation_scope_proto(#instrumentation_scope{name=Name,
                                                      version=Version,
                                                      schema_url=SchemaUrl}) ->
    #{scope => #{name => Name,
                 version => Version},
      schema_url => SchemaUrl}.

-spec to_attributes(otel_attributes:t()) -> [opentelemetry_exporter_trace_service_pb:key_value()].
to_attributes(Attributes) ->
    maps:fold(fun(Key, Value, Acc) ->
                      [#{key => to_binary(Key),
                         value => to_any_value(Value)} | Acc]
              end, [], otel_attributes:map(Attributes)).

to_any_value(Value) when is_binary(Value) ->
    %% TODO: there is a bytes_value type we don't currently support bc we assume string
    #{value => {string_value, Value}};
to_any_value(Value) when is_atom(Value) ->
    #{value => {string_value, to_binary(Value)}};
to_any_value(Value) when is_integer(Value) ->
    #{value => {int_value, Value}};
to_any_value(Value) when is_float(Value) ->
    #{value => {double_value, Value}};
to_any_value(Value) when is_boolean(Value) ->
    #{value => {bool_value, Value}};
to_any_value(Value) when is_map(Value) ->
    #{value => {kvlist_value, to_key_value_list(maps:to_list(Value))}};
to_any_value(Value) when is_tuple(Value) ->
    #{value => {array_value, to_array_value(tuple_to_list(Value))}};
to_any_value(Value) when is_list(Value) ->
    case is_proplist(Value) of
        true ->
            #{value => {kvlist_value, to_key_value_list(Value)}};
        false ->
            #{value => {array_value, to_array_value(Value)}}
    end.

to_key_value_list(List) ->
    #{values => to_key_value_list(List, [])}.

to_key_value_list([], Acc) ->
    Acc;
to_key_value_list([{Key, Value} | Rest], Acc) when is_atom(Key) ; is_binary(Key) ->
    to_key_value_list(Rest, [to_key_value(Key, Value) | Acc]);
to_key_value_list([_ | Rest], Acc) ->
    to_key_value_list(Rest, Acc).

to_key_value(Key, Value) ->
    #{key => to_binary(Key),
      value => to_any_value(Value)}.

to_array_value(List) when is_list(List) ->
    #{values => [to_any_value(V) || V <- List]};
to_array_value(_) ->
    #{values => []}.

is_proplist([]) ->
    true;
is_proplist([{K, _} | L]) when is_atom(K) ; is_binary(K) ->
    is_proplist(L);
is_proplist(_) ->
    false.

to_binary(Term) when is_atom(Term) ->
    erlang:atom_to_binary(Term, unicode);
to_binary(Term) ->
    unicode:characters_to_binary(Term).
