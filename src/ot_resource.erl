%%%------------------------------------------------------------------------
%% Copyright 2019, OpenTelemetry Authors
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
%%
%% @end
%%%-----------------------------------------------------------------------
-module(ot_resource).

-export([create/1,
         merge/2,
         attributes/1]).

-type key() :: io_lib:latin1_string().
-type value() :: io_lib:latin1_string() | integer() | float() | boolean().
-type resource() :: {ot_resource, [{key(), value()}]}.
-opaque t() :: resource().

-export_type([t/0]).

-define(MAX_LENGTH, 255).

%% verifies each key and value and drops any that don't pass verification
-spec create(#{key() => value()} | [{key(), value()}]) -> t().
create(Map) when is_map(Map) ->
    create(maps:to_list(Map));
create(List) when is_list(List) ->
    {ot_resource, lists:filtermap(fun({K, V}) ->
                                    %% TODO: log an info or debug message when dropping?
                                    case check_key(K) andalso check_value(V) of
                                        {true, Value} ->
                                            {true, {to_binary(K), Value}};
                                        _ ->
                                            false
                                    end
                               end, lists:ukeysort(1, List))}.

-spec attributes(t()) -> [{key(), value()}].
attributes({ot_resource, Resource}) ->
    Resource.

%% In case of collision the current, first argument, resource takes precedence.
-spec merge(t(), t()) -> t().
merge({ot_resource, CurrentResource}, {ot_resource, OtherResource}) ->
    {ot_resource, lists:ukeymerge(1, CurrentResource, OtherResource)}.

%%

%% all resource strings, key or value, must be latin1 with length less than 255
check_string(S) ->
    string:length(S) =< ?MAX_LENGTH.

%% a resource key must be a non-empty latin1 string
check_key(K=[_|_]) ->
    check_string(K);
check_key(_) ->
    false.

%% a resource value can be a latin1 string, integer, float or boolean
check_value(V) when is_integer(V) ;
                    is_float(V) ;
                    is_boolean(V) ->
    {true, V};
check_value(V) when is_binary(V) ->
    check_string_value(V);
check_value(V) when is_list(V) ->
    try unicode:characters_to_binary(V) of
        B ->
            check_string_value(B)
    catch
        error:badarg ->
            %% must be an array attribute value
            {true, V}
    end;
check_value(_) ->
    false.

check_string_value(V) ->
    case check_string(V) of
        true ->
            {true, V};
        false ->
            false
    end.

to_binary(K) when is_binary(K) ->
    K;
to_binary(K) when is_list(K) ->
    list_to_binary(K).
