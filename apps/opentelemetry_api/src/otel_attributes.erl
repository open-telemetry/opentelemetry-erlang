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
%% @doc Functions to work with Attributes.
%%
%% An Attribute is a key-value pair with string or atom keys.
%% See <a href="https://opentelemetry.io/docs/specs/otel/common/#attribute">the specification</a>.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_attributes).

-export([new/3,
         set/2,
         set/3,
         dropped/1,
         map/1,
         is_valid_attribute/2,
         process_attributes/1]).

-define(is_allowed_key(Key), (is_atom(Key) orelse (is_binary(Key) andalso Key =/= <<"">>))).
-define(is_allowed_value(Value), (is_atom(Value) orelse
                                  is_boolean(Value) orelse
                                  is_number(Value) orelse
                                  is_binary(Value) orelse
                                  is_list(Value))).
-record(attributes, {
                     count_limit :: integer(),
                     value_length_limit :: integer() | infinity,
                     dropped :: integer(),
                     map :: map()
                    }).

-type t() :: #attributes{}.

-export_type([t/0]).

%% @doc Creates a new `Attributes' from `Pairs' with the given count and value length limits.
%%
%% `Pairs' can be a list of key-value pairs or a map. If `Pairs' is not a list or map, the
%% function returns an empty `Attributes'.
-spec new(
    [opentelemetry:attribute()] | opentelemetry:attributes_map(),
    integer(),
    integer() | infinity
) -> t().
new(Pairs, CountLimit, ValueLengthLimit) when is_list(Pairs) ->
    new(maps:from_list(Pairs), CountLimit, ValueLengthLimit);
new(Map, CountLimit, ValueLengthLimit) when is_map(Map) ->
    update_attributes(Map, #attributes{count_limit=CountLimit,
                                        value_length_limit=ValueLengthLimit,
                                        dropped=0,
                                        map=#{}});
new(_, CountLimit, ValueLengthLimit) ->
    #attributes{count_limit=CountLimit,
                value_length_limit=ValueLengthLimit,
                dropped=0,
                map=#{}}.

%% @doc Sets the given key-value pairs in the given `Attributes'. Overrides 
%% the existing value for a given key if it already exists in `Attributes'.
%%
%% `NewListOrMap' can be a list of key-value pairs or a map. If `NewListOrMap' is not a list
%% or map, the function returns `Attributes' as is. Returns the updated `Attributes'.
-spec set([opentelemetry:attribute()] | opentelemetry:attributes_map(), t()) -> t().
set(NewListOrMap, Attributes) when is_list(NewListOrMap) ->
    set(maps:from_list(NewListOrMap), Attributes);
set(NewMap, Attributes) when is_map(NewMap) ->
    update_attributes(NewMap, Attributes);
set(_, Attributes) ->
    Attributes.

%% @doc Sets the given key-value pair in the given `Attributes'.
%%
%% Overrides the existing value under `Key' if `Key' already exists.
%% Returns the updated `Attributes'.
-spec set(opentelemetry:attribute_key(), opentelemetry:attribute_value(), t()) -> t().
set(Key, Value, Attributes) ->
    update_attribute(Key, Value, Attributes).

%% @doc Returns the count of dropped attributes in the given `Attributes'.
dropped(#attributes{dropped=Dropped}) ->
    Dropped.

%% @doc Returns the Attributes in the form of a map.
%%
%% For example:
%% ```
%% otel_attributes:new([], 10, 10),
%% otel_attributes:set(<<"key">>, <<"value">>, Attributes),
%% otel_attributes:map(Attributes).
%% %=> #{<<"key">> => <<"value">>}
%% '''
-spec map(t()) -> map().
map(#attributes{map=Map}) ->
    Map.

%%

update_attributes(List, Attributes) ->
    maps:fold(fun update_attribute/3, Attributes, List).

%% add key/value if the size is still under the limit or the key is already in the map
update_attribute(Key, Value, Attributes=#attributes{count_limit=CountLimit,
                                                    value_length_limit=ValueLengthLimit,
                                                    map=Map})
  when is_binary(Value) , (map_size(Map) < CountLimit orelse is_map_key(Key, Map)) ->
    Attributes#attributes{map=Map#{Key => maybe_truncate_binary(Value, ValueLengthLimit)}};
%% value is a list of binaries, so potentially truncate
update_attribute(Key, [Value1 | _Rest] = Value, Attributes=#attributes{count_limit=CountLimit,
                                                    value_length_limit=ValueLengthLimit,
                                                    map=Map})
  when is_binary(Value1) , (map_size(Map) < CountLimit orelse is_map_key(Key, Map)) ->
    Attributes#attributes{map=Map#{Key => [maybe_truncate_binary(V, ValueLengthLimit) || V <- Value]}};
%% already in the map and not a binary so update
update_attribute(Key, Value, Attributes=#attributes{map=Map}) when is_map_key(Key, Map) ->
    Attributes#attributes{map=Map#{Key := Value}};
%% we've already started dropping, so just increment
update_attribute(_Key, _Value, Attributes=#attributes{dropped=Dropped})
  when Dropped > 0 ->
    Attributes#attributes{dropped=Dropped + 1};
%% met or exceeded the limit
update_attribute(_Key, _Value, Attributes=#attributes{count_limit=CountLimit,
                                                      dropped=Dropped,
                                                      map=Map})
  when map_size(Map) >= CountLimit ->
    Attributes#attributes{dropped=Dropped + 1};
%% new attribute
update_attribute(Key, Value, Attributes=#attributes{map=Map}) ->
    Attributes#attributes{map=Map#{Key => Value}}.

maybe_truncate_binary(Value, infinity) ->
  Value;
maybe_truncate_binary(Value, ValueLengthLimit) ->
    case string:length(Value) > ValueLengthLimit of
        true ->
            string:slice(Value, 0, ValueLengthLimit);
        false ->
            Value
    end.

%% @doc Checks whether the given key-value pair makes for a valid attribute.
%%
%% For example:
%% ```
%% otel_attributes:is_valid_attribute(<<"key">>, <<"value">>).
%% %=> true
%%
%% otel_attributes:is_valid_attribute(atom_key, <<"value">>).
%% %=> true
%%
%% otel_attributes:is_valid_attribute(123, <<"value">>).
%% %=> false
%% '''
-spec is_valid_attribute(opentelemetry:attribute_key(), opentelemetry:attribute_value()) -> boolean().
is_valid_attribute(Key, Value) when is_tuple(Value) , ?is_allowed_key(Key) ->
    is_valid_attribute(Key, tuple_to_list(Value));
%% lists as attribute values must be primitive types and homogeneous
is_valid_attribute(Key, [Value1 | _Rest] = Values) when is_binary(Value1) , ?is_allowed_key(Key) ->
    lists:all(fun is_binary/1, Values);
is_valid_attribute(Key, [Value1 | _Rest] = Values) when is_boolean(Value1) , ?is_allowed_key(Key) ->
    lists:all(fun is_boolean/1, Values);
is_valid_attribute(Key, [Value1 | _Rest] = Values) when is_atom(Value1) , ?is_allowed_key(Key) ->
    lists:all(fun is_valid_atom_value/1, Values);
is_valid_attribute(Key, [Value1 | _Rest] = Values) when is_integer(Value1) , ?is_allowed_key(Key) ->
    lists:all(fun is_integer/1, Values);
is_valid_attribute(Key, [Value1 | _Rest] = Values) when is_float(Value1) , ?is_allowed_key(Key) ->
    lists:all(fun is_float/1, Values);
is_valid_attribute(_Key, Value) when is_list(Value) ->
    false;
is_valid_attribute(Key, []) when ?is_allowed_key(Key) ->
    true;
is_valid_attribute(Key, Value) when ?is_allowed_key(Key) , ?is_allowed_value(Value) ->
    true;
is_valid_attribute(_, _) ->
    false.

is_valid_atom_value(undefined) ->
    false;
is_valid_atom_value(nil) ->
    false;
is_valid_atom_value(Value) ->
    is_atom(Value) andalso (is_boolean(Value) == false).

%% @private
-spec process_attributes(eqwalizer:dynamic()) -> opentelemetry:attributes_map().
process_attributes(Attributes) when is_map(Attributes) ->
    maps:fold(fun process_attribute/3, #{}, Attributes);
process_attributes([]) -> #{};
process_attributes(Attributes) when is_list(Attributes) ->
    process_attributes(maps:from_list(Attributes));
process_attributes(_) ->
    #{}.

process_attribute(Key, Value, Map) when is_tuple(Value) ->
    List = tuple_to_list(Value),
    case is_valid_attribute(Key, List) of
        true ->
            maps:put(Key, Value, Map);
        false ->
            Map
    end;
process_attribute(Key, Value, Map) ->
    case is_valid_attribute(Key, Value) of
        true ->
            maps:put(Key, Value, Map);
        false ->
            Map
    end.
