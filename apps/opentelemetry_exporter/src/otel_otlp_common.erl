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

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

-spec to_attributes(opentelemetry:attributes_map() | otel_attributes:t() | undefined) -> [opentelemetry_exporter_trace_service_pb:key_value()].
to_attributes(Attributes) when is_map(Attributes) ->
    maps:fold(fun(Key, Value, Acc) ->
        case to_any_value(Value) of
            invalid ->
                ?LOG_DEBUG("OpenTelemetry exporter: discarded invalid attribute, ~p", [{Key, Value}]),
                Acc;
            ParsedValue ->
                [#{key => to_binary(Key), value => ParsedValue} | Acc]
        end
    end, [], Attributes);
to_attributes(Attributes) when is_list(Attributes) ->
    to_attributes(maps:from_list(Attributes));
to_attributes(Attributes) when is_tuple(Attributes) ->
    to_attributes(otel_attributes:map(Attributes));
to_attributes(undefined) ->
    [].

to_any_value(Value) when is_binary(Value) ->
    case unicode:characters_to_binary(Value) of
        {Failure, _, _} when Failure =:= error ;
                             Failure =:= incomplete ->
            #{value => {bytes_value, Value}};
        String ->
            #{value => {string_value, String}}
    end;
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
    case to_array_value(tuple_to_list(Value)) of
        invalid -> invalid;
        ArrayValue -> #{value => {array_value, ArrayValue}}
    end;
to_any_value(Value) when is_list(Value) ->
    case is_proplist(Value) of
        true ->
            #{value => {kvlist_value, to_key_value_list(Value)}};
        false ->
            case to_array_value(Value) of
                invalid -> invalid;
                ArrayValue -> #{value => {array_value, ArrayValue}}
            end
    end;
to_any_value(Value) ->
    #{value => {string_value, to_binary(io_lib:format("~p", [Value]))}}.

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
    case to_array_value(List, []) of
        invalid -> invalid;
        Values -> #{values => Values}
    end;
to_array_value(_) ->
    #{values => []}.


to_array_value([Elem | _List], _Acc) when is_tuple(Elem); is_list(Elem) ->
    invalid;
to_array_value([Elem | List], Acc) ->
    to_array_value(List, [to_any_value(Elem) | Acc]);
to_array_value([], Acc) ->
    lists:reverse(Acc).

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


-ifdef(TEST).

to_attributes_test_() ->
    [
        ?_assertEqual([
            #{
                key => <<"bin_list">>,
                value => #{
                    value => {array_value, #{
                        values => [
                            #{value => {string_value,<<"a">>}},
                            #{value => {string_value,<<"b">>}}
                        ]
                    }}
                }
            }
        ], to_attributes(#{bin_list => [<<"a">>, <<"b">>]})),
        ?_assertEqual([
            #{
                key => <<"int_list">>,
                value => #{
                    value => {array_value, #{
                        values => [
                            #{value => {int_value,1}},
                            #{value => {int_value,2}}
                        ]
                    }}
                }
            }
        ], to_attributes(#{int_list => [1, 2]})),
        ?_assertEqual([
            #{
                key => <<"float_list">>,
                value => #{
                    value => {array_value, #{
                        values => [
                            #{value => {double_value,1.2}},
                            #{value => {double_value,3.4}}
                        ]
                    }}
                }
            }
        ], to_attributes(#{float_list => [1.2, 3.4]})),
        ?_assertEqual([
            #{
                key => <<"bool_list">>,
                value => #{
                    value => {array_value, #{
                        values => [
                            #{value => {string_value,<<"true">>}},
                            #{value => {string_value,<<"false">>}}
                        ]
                    }}
                }
            }
        ], to_attributes(#{bool_list => [true, false]})),
        ?_assertEqual([], to_attributes(#{attr => [1, [2, 3]]}))
    ].

-endif.