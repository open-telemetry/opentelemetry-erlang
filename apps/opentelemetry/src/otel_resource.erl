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
         schema_url/1,
         attributes/1]).

-include("otel_resource.hrl").

-type key() :: unicode:latin1_binary().
%% values allowed in attributes of a resource are limited
-type value() :: unicode:latin1_binary() | integer() | float() | boolean().
-type schema_url() :: uri_string:uri_string().

-define(MAX_LENGTH, 255).

-record(resource, {schema_url :: schema_url() | undefined,
                   attributes :: otel_attributes:t()}).
-type t() :: #resource{}.

-spec create(#{key() => value()} | [{key(), value()}]) -> t().
create(Attributes) ->
    create(Attributes, undefined).

%% verifies each key and value and drops any that don't pass verification
-spec create(#{key() => value()} | [{key(), value()}], schema_url() | undefined) -> t().
create(Map, SchemaUrl) when is_map(Map) ->
    create(maps:to_list(Map), SchemaUrl);
create(List, SchemaUrl) when is_list(List) ->
    List1 = lists:filtermap(fun({K, V}) ->
                                    %% TODO: log an info or debug message when dropping?
                                    case check_key(K) andalso check_value(V) of
                                        {true, Value} ->
                                            {true, {to_binary(K), Value}};
                                        _ ->
                                            false
                                    end
                            end, lists:ukeysort(1, List)),
    #resource{schema_url=SchemaUrl,
              attributes=otel_attributes:new(List1, 128, 255)}.

-spec schema_url(t()) -> schema_url() | undefined.
schema_url(#resource{schema_url=Schema}) ->
    Schema.

-spec attributes(t()) -> otel_attributes:t().
attributes(#resource{attributes=Attributes}) ->
    Attributes.

%% in case of collision the updating, first argument, resource takes precedence.
-spec merge(t(), t()) -> t().
merge(#resource{schema_url=NewSchemaUrl,
                attributes=NewAttributes}, Current=#resource{schema_url=CurrentSchemaUrl,
                                                             attributes=CurrentAttributes}) ->
    SchameUrl = merge_schema_url(NewSchemaUrl, CurrentSchemaUrl),
    NewMap = otel_attributes:map(NewAttributes),
    Current#resource{schema_url=SchameUrl,
                     attributes=otel_attributes:set(NewMap, CurrentAttributes)}.

%%

%% when merging resources the schemas are checked to verify they match.
%% if they do match then the schema is set to `undefined' and the attributes are kept.
merge_schema_url(SchemaUrl, undefined) ->
    SchemaUrl;
merge_schema_url(undefined, SchemaUrl) ->
    SchemaUrl;
merge_schema_url(NewSchemaUrl, CurrentSchemaUrl) ->
    merge_schema_url_(uri_string:normalize(NewSchemaUrl), uri_string:normalize(CurrentSchemaUrl)).

merge_schema_url_(SchemaUrl, SchemaUrl) ->
    SchemaUrl;
merge_schema_url_(_, _) ->
    undefined.

%% all resource strings, key or value, must be latin1 with length less than 255
check_string(S) ->
    string:length(S) =< ?MAX_LENGTH.

%% a resource key must be a non-empty latin1 string
check_key(K) when is_binary(K) ; is_list(K) ->
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
