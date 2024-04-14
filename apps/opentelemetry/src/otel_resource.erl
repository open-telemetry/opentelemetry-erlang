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
%% @doc A Resource is attributes representing the entity producing
%% telemetry. For example, by default the language (Erlang), name of this
%% library (`opentelemetry'), and version of this library are included in
%% the Resource.
%%
%% This module provides the functional interface for working with the
%% resource record.
%%
%% The `opentelemetry' library supports <i>resource detectors</i> to
%% detect attributes to include in the Resource. See {@link otel_resource_detector}
%% for the behaviour to detect resources, and the {@link otel_resource_app_env}
%% and {@link otel_resource_env_var} modules for built-in implementations.
%%
%% See the <a href="https://opentelemetry.io/docs/concepts/resources/">
%% OpenTelemetry Resource documentation</a> for more information on Resources.
%% @end
%%%-----------------------------------------------------------------------
-module(otel_resource).

-export([create/1,
         create/2,
         merge/2,
         schema_url/1,
         attributes/1,
         is_key/2]).

-type key() :: unicode:latin1_binary() | atom().
%% values allowed in attributes of a resource are limited

-type value() :: unicode:latin1_binary() | integer() | float() | boolean().
%% A resource value.

-type schema_url() :: uri_string:uri_string().
%% A schema URL for the resource.

-define(MAX_LENGTH, 255).

-record(resource, {schema_url :: schema_url() | undefined,
                   attributes :: otel_attributes:t()}).

-type t() :: #resource{} | undefined.
%% The type that represents a resource.

-export_type([t/0]).

%% @equiv create(Attributes, undefined)
-spec create(#{key() => value()} | [{key(), value()}]) -> t().
create(Attributes) ->
    create(Attributes, undefined).

%% @doc Creates a new resources from the given map or list of `Attributes' and
%% with the given `SchemaUrl'.
%%
%%
%% This function verifies each key and value, and drops any that don't pass verification.
-spec create(#{key() => value()} | [{key(), value()}], schema_url() | undefined) -> t().
create(Map, SchemaUrl) when is_map(Map) ->
    create(maps:to_list(Map), SchemaUrl);
create(List, SchemaUrl) when is_list(List) ->
    List1 = lists:filtermap(fun({K, V}) ->
                                    %% TODO: log an info or debug message when dropping?
                                    case try_check_key(K, true) of
                                        {true, Key} ->
                                            case check_value(V) of
                                                {true, Value} ->
                                                    {true, {Key, Value}};
                                                _ ->
                                                    false
                                            end;
                                        _ ->
                                            false
                                    end
                            end, lists:ukeysort(1, List)),
    #resource{schema_url=SchemaUrl,
              attributes=otel_attributes:new(List1, 128, 255)}.

%% @doc Returns the schema URL of the resource.
-spec schema_url(t()) -> schema_url() | undefined.
schema_url(#resource{schema_url=Schema}) ->
    Schema;
schema_url(_) ->
    undefined.

%% @doc Returns the attributes of the given `Resource'.
%%
%% This function returns `undefined' only in case `Resource' is an invalid argument
%% (not a resource record).
-spec attributes(t()) -> otel_attributes:t() | undefined.
attributes(#resource{attributes=Attributes}) ->
    Attributes;
attributes(_) ->
    undefined.

%% @doc Returns `true' if `Key' is valid and part of the given resource.
-spec is_key(key(), t()) -> boolean().
is_key(K, #resource{attributes=Attributes}) ->
    case try_check_key(K, false) of
        {true, Key} ->
            maps:is_key(Key, otel_attributes:map(Attributes));
        _ ->
            false
    end;
is_key(_, _) ->
    false.

%% @doc Merges the two given resources.
%%
%% In case of collision, the first argument (`Resource') takes precedence.
-spec merge(t(), t()) -> t().
merge(#resource{schema_url=NewSchemaUrl,
                attributes=NewAttributes}, CurrentResource=#resource{schema_url=CurrentSchemaUrl,
                                                             attributes=CurrentAttributes}) ->
    SchameUrl = merge_schema_url(NewSchemaUrl, CurrentSchemaUrl),
    NewMap = otel_attributes:map(NewAttributes),
    CurrentResource#resource{schema_url=SchameUrl,
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

%% a resource key must be a non-empty latin1 string or atom, stored as atom
try_check_key(K, UnconditionalBinaryToAtom) ->
    try check_key(K, UnconditionalBinaryToAtom)
    catch error:badarg -> false
    end.

check_key(<<>>, _) ->
    false;
check_key(K, UnconditionalBinaryToAtom) when is_binary(K) ->
    case check_string_value(K) of
        {true, Key} when UnconditionalBinaryToAtom ->
            {true, binary_to_atom(Key, latin1)};
        {true, Key} ->
            {true, binary_to_existing_atom(Key, latin1)};
        false ->
            false
    end;
check_key(K, UnconditionalBinaryToAtom) when is_list(K) ->
    check_key(list_to_binary(K), UnconditionalBinaryToAtom);
check_key(K, _) when is_atom(K) ->
    % Atoms have an length upper limit of 255 utf8 codepoints, so the
    % latin1 representation, if it exists, shares that limit
    _ = atom_to_binary(K, latin1),
    {true, K};
check_key(_, _) ->
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
    %% all resource strings, key or value, must be latin1 with length less than 255
    case string:length(V) =< ?MAX_LENGTH of
        true ->
            {true, V};
        false ->
            false
    end.
