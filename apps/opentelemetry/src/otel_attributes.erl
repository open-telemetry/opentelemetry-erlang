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
                                                    dropped=Dropped,
                                                    map=Map})
  when (map_size(Map) < CountLimit orelse is_map_key(Key, Map)),
       (is_binary(Key) orelse is_atom(Key)) ->
    case is_allowed(Value, ValueLengthLimit) of
        {true, V} ->
            Attributes#attributes{map=Map#{Key => V}};

        %% Value isn't a primitive or a list/array so drop it
        false ->
            Attributes#attributes{dropped=Dropped + 1}
    end;
%% Key must be a binary string or atom
update_attribute(_Key, _Value, Attributes=#attributes{dropped=Dropped}) ->
    Attributes#attributes{dropped=Dropped + 1}.

%% if value is a primitive or a list/array then just return it
is_allowed(Value, _) when is_atom(Value) ;
                          is_integer(Value) ;
                          is_float(Value) ;
                          is_list(Value) ->
    {true, Value};
%% if a binary then it is a string and we may need to trim its length
is_allowed(Value, ValueLengthLimit) when is_binary(Value) ->
    case string:length(Value) > ValueLengthLimit of
        true ->
            {true, string:slice(Value, 0, ValueLengthLimit)};
        false ->
            {true, Value}
    end;
is_allowed(_, _) ->
    false.
