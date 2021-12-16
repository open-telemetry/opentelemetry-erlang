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

new(Map, CountLimit, ValueLengthLimit) when is_map(Map) ->
    new(maps:to_list(Map), CountLimit, ValueLengthLimit);
new(List, CountLimit, ValueLengthLimit) when is_list(List) ->
    update_attributes(List, #attributes{count_limit=CountLimit,
                                        value_length_limit=ValueLengthLimit,
                                        dropped=0,
                                        map=#{}});
new(_, CountLimit, ValueLengthLimit) ->
    #attributes{count_limit=CountLimit,
                value_length_limit=ValueLengthLimit,
                dropped=0,
                map=#{}}.

set(NewMap, Attributes) when is_map(NewMap) ->
    %% convert to a list so dropping for size is easier in `update_attributes'
    set(maps:to_list(NewMap), Attributes);
set(NewList, Attributes) when is_list(NewList) ->
    update_attributes(NewList, Attributes);
set(_, Attributes) ->
    Attributes.

set(Key, Value, Attributes) ->
    update_attribute(Key, Value, Attributes).

dropped(#attributes{dropped=Dropped}) ->
    Dropped.

map(#attributes{map=Map}) ->
    Map.

%%

%% TODO: this is partly wrong. We must check each element in the List and not just drop
%% them all. If the key already exists in the Attributes then its value must be replaced
%% instead of dropping the new attribute.
update_attributes(List, Attributes=#attributes{count_limit=CountLimit,
                                               map=Map}) when map_size(Map) < CountLimit ->
    Limit = CountLimit - map_size(Map),
    update_attributes_(List, Limit, Attributes);
update_attributes(List, Attributes=#attributes{dropped=Dropped}) ->
    Attributes#attributes{dropped=Dropped + length(List)}.

update_attributes_([], _, Attributes) ->
    Attributes;
update_attributes_(List, 0, Attributes=#attributes{dropped=Dropped}) ->
    %% we've hit the limit on the number of attributes so drop the rest
    Attributes#attributes{dropped=Dropped + length(List)};
update_attributes_([{Key, Value} | Rest], Limit, Attributes=#attributes{value_length_limit=ValueLengthLimit,
                                                                        dropped=Dropped,
                                                                        map=Map}) ->
    case is_allowed(Value, ValueLengthLimit) of
        {true, V} ->
            update_attributes_(Rest, Limit - 1, Attributes#attributes{map=Map#{Key => V}});

        %% Value isn't a primitive or a list/array so drop it
        false ->
            %% don't subtract from the limit since the attribute was dropped
            update_attributes_(Rest, Limit, Attributes#attributes{dropped=Dropped + 1})
    end.

update_attribute(Key, Value, Attributes=#attributes{count_limit=CountLimit,
                                                    value_length_limit=ValueLengthLimit,
                                                    dropped=Dropped,
                                                    map=Map}) when map_size(Map) < CountLimit ,
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
