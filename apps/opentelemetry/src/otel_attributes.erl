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
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(otel_attributes).

-export([new/3,
         set/2,
         set/3,
         dropped/1,
         map/1]).

-record(attributes, {
                     count_limit :: integer(),
                     value_length_limit :: integer() | infinity,
                     dropped :: integer(),
                     map :: map()
                    }).

-type t() :: #attributes{}.

-export_type([t/0]).

new(List, CountLimit, ValueLengthLimit) when is_list(List) ->
    new(maps:from_list(List), CountLimit, ValueLengthLimit);
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

set(NewList, Attributes) when is_list(NewList) ->
    set(maps:from_list(NewList), Attributes);
set(NewMap, Attributes) when is_map(NewMap) ->
    update_attributes(NewMap, Attributes);
set(_, Attributes) ->
    Attributes.

set(Key, Value, Attributes) ->
    update_attribute(Key, Value, Attributes).

dropped(#attributes{dropped=Dropped}) ->
    Dropped.

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
