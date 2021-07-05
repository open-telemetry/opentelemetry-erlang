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
-module(otel_resource).

-export([create/1,
         create/2,
         merge/2,
         attributes/1,
         schema_url/1]).

-type key() :: io_lib:latin1_string().
-type value() :: io_lib:latin1_string() | integer() | float() | boolean().
-type schema() :: uri_string:uri_string() | undefined.
-type resource() :: {otel_resource, schema(), [{key(), value()}]}.
-opaque t() :: resource().

-export_type([t/0]).

-define(MAX_LENGTH, 255).

%% verifies each key and value and drops any that don't pass verification
-spec create(#{key() => value()} | [{key(), value()}]) -> t().
create(Map) when is_map(Map) ->
    create(Map, undefined);
create(List) when is_list(List) ->
    create(List, undefined).

%% verifies each key and value and drops any that don't pass verification
-spec create(#{key() => value()} | [{key(), value()}], schema() ) -> t().
create(Map, SchemaUrl) when is_map(Map) ->
    create(maps:to_list(Map), SchemaUrl);
create(List, SchemaUrl) when is_list(List) ->
    {otel_resource, SchemaUrl, lists:filtermap(fun({K, V}) ->
                                    %% TODO: log an info or debug message when dropping?
                                    case check_key(K) andalso check_value(V) of
                                        {true, Value} ->
                                            {true, {to_binary(K), Value}};
                                        _ ->
                                            false
                                    end
                               end, lists:ukeysort(1, List))}.

-spec attributes(t()) -> [{key(), value()}].
attributes({otel_resource, _, Resource}) ->
    Resource.

-spec schema_url(t()) -> schema().
schema_url({otel_resource, SchemaUrl, _}) ->
    SchemaUrl.

%% In case of collision the current, first argument, resource takes precedence.
%% In case of schema conflict, it will be set to undefined
-spec merge(t(), t()) -> t().
merge({otel_resource, CurrentSchema, CurrentAttributes}, {otel_resource, OtherSchema, OtherAttributes}) ->
    %% check schema url
    Schema = check_schema(CurrentSchema, OtherSchema),
    {otel_resource, Schema, lists:ukeymerge(1, CurrentAttributes, OtherAttributes)}.

%% check two schemas. Current behavior is undefined if schemas do not match
-spec check_schema(schema(), schema()) -> schema().
check_schema(A, A) ->
    A;
check_schema(undefined, A) ->
    A;
check_schema(A, undefined) ->
    A;
check_schema(_, _) ->
    undefined.

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
