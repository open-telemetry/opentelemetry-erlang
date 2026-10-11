%%%------------------------------------------------------------------------
%% Copyright 2026, OpenTelemetry Authors
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
%% @private
%% Shared syntax helpers for configuration input.
-module(otel_configuration_utils).
-export([component_entry/1, find/2, fail/1, to_binary/1]).

-spec component_entry(term()) -> {ok, {term(), term()}} | error.
component_entry({Name, Options}) -> {ok, {Name, Options}};
component_entry(Map) when is_map(Map), map_size(Map) =:= 1 ->
    [Entry] = maps:to_list(Map),
    {ok, Entry};
component_entry(_) -> error.

%% Native atom keys take precedence over their JSON spellings.
-spec find(atom(), map()) -> {ok, term()} | error.
find(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, _}=Found -> Found;
        error -> maps:find(atom_to_binary(Key, utf8), Map)
    end.

-spec fail(term()) -> no_return().
fail(Reason) -> throw({declarative_configuration_error, Reason}).

%% Conversion is shared; callers decide whether invalid input is a model error,
%% a configuration error, or an invalid environment value to warn about and skip.
-spec to_binary(dynamic()) -> {ok, binary()} | error.
to_binary(Value) when is_atom(Value) -> {ok, atom_to_binary(Value, utf8)};
to_binary(Value) when is_binary(Value); is_list(Value) ->
    try unicode:characters_to_binary(Value) of
        Binary when is_binary(Binary) -> {ok, Binary};
        _ -> error
    catch
        error:badarg -> error
    end;
to_binary(_) -> error.
