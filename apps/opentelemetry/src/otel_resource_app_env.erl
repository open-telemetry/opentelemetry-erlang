%%%------------------------------------------------------------------------
%% Copyright 2020, OpenTelemetry Authors
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
%% @doc Adds attributes to the `Resource' based on the value of `resource'
%% in the `opentelemetry' application's environment.
%%
%%     [{service, #{name => "service-name",
%%                  namespace => "service-namespace"}]
%%
%% Results in the `Resource' attributes `service.name' and `service.namespace'.
%% @end
%%%-----------------------------------------------------------------------
-module(otel_resource_app_env).

-behaviour(otel_resource_detector).

-export([get_resource/1,
         parse/1]).

get_resource(_Config) ->
    Attributes = parse(application:get_env(opentelemetry, resource, #{})),
    otel_resource:create(Attributes).

%%

parse(Attributes) when is_map(Attributes) ->
    parse(maps:to_list(Attributes));
parse(Attributes) when is_list(Attributes) ->
    lists:flatmap(fun({Key, Values}) when is_list(Key) ; is_binary(Key) ; is_atom(Key) ->
                          parse_values(to_string(Key), Values);
                     (_) ->
                          %% ignore anything else
                          []
                  end, Attributes);
parse(_) ->
    %% must be a map or list. ignore and return empty if it isn't
    [].

parse_values(Key, Values) when is_map(Values) ->
    parse_values(Key, maps:to_list(Values));
parse_values(Key, Values) when is_list(Values) ->
    case io_lib:printable_unicode_list(Values) of
        true ->
            [{unicode:characters_to_binary(Key), unicode:characters_to_binary(Values)}];
        false ->
            lists:flatmap(fun({SubKey, Value=[{_,_}|_]}) ->
                                  %% list of tuples means we have more subkeys
                                  parse_values([Key, ".", to_string(SubKey)], Value);
                             ({SubKey, Value}) when is_map(Value) ->
                                  %% map value means we have more subkeys
                                  parse_values([Key, ".", to_string(SubKey)], Value);
                             ({SubKey, Value})->
                                  [{unicode:characters_to_binary([Key, ".", to_string(SubKey)]), Value}]
                          end, Values)
    end;
parse_values(Key, Value) ->
    [{unicode:characters_to_binary(Key), Value}].

-spec to_string(atom() | binary() | list()) -> binary().
to_string(K) when is_atom(K) ->
    atom_to_binary(K, utf8);
to_string(K) when is_list(K) ->
    list_to_binary(K);
to_string(K) ->
    K.
