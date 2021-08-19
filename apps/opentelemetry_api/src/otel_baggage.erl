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
%% @doc Baggage is used to annotate telemetry, adding context and
%% information to metrics, traces, and logs. It is represented by a set
%% of name/value pairs describing user-defined properties.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_baggage).

-export([set/1,
         set/2,
         set/3,
         set/4,
         get_all/0,
         get_all/1,
         clear/0,
         clear/1]).

%% keys and values are UTF-8 binaries
-type key() :: unicode:unicode_binary().
-type value() :: unicode:unicode_binary().
-type metadata() :: [unicode:unicode_binary() | {unicode:unicode_binary(), unicode:unicode_binary()}].

-type t() :: #{key() => {value(), metadata()}}.

-export_type([t/0,
              key/0,
              value/0]).

-define(BAGGAGE_KEY, '$__otel_baggage_ctx_key').

-spec set(#{key() => value()} | [{key(), value()}]) -> ok.
set(KeyValues) when is_list(KeyValues) ->
    set(maps:from_list(KeyValues));
set(KeyValues) when is_map(KeyValues) ->
    Baggage = otel_ctx:get_value(?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(KeyValues)));
set(_) ->
    ok.

%% Ctx will never be a list or binary so we can tell if a context is passed by checking that
-spec set(otel_ctx:t() | key() | unicode:charlist(), #{key() => value()} | [{key(), value()}] | value()) -> otel_ctx:t().
set(Key, Value) when is_list(Key) ; is_binary(Key) ->
    set(Key, Value, []);
set(Ctx, KeyValues) when is_list(KeyValues) ->
    set(Ctx, maps:from_list(KeyValues));
set(Ctx, KeyValues) when is_map(KeyValues) ->
    Baggage = otel_ctx:get_value(Ctx, ?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(Ctx, ?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(KeyValues))).

-spec set(otel_ctx:t() | key(), key() | value(), value() | list()) -> ok | otel_ctx:t().
set(Key, Value, Metadata) when is_list(Key) ; is_binary(Key) ->
    Baggage = otel_ctx:get_value(?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(#{Key => {Value, Metadata}})));
set(Ctx, Key, Value) ->
    set(Ctx, Key, Value, []).

-spec set(otel_ctx:t(), key(), value(), metadata()) -> otel_ctx:t().
set(Ctx, Key, Value, Metadata) ->
    Baggage = otel_ctx:get_value(Ctx, ?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(Ctx, ?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(#{Key => {Value, Metadata}}))).

-spec get_all() -> t().
get_all() ->
    otel_ctx:get_value(?BAGGAGE_KEY, #{}).

-spec get_all(otel_ctx:t()) -> t().
get_all(Ctx) ->
    otel_ctx:get_value(Ctx, ?BAGGAGE_KEY, #{}).

-spec clear() -> ok.
clear() ->
    otel_ctx:set_value(?BAGGAGE_KEY, #{}).

-spec clear(otel_ctx:t()) -> otel_ctx:t().
clear(Ctx) ->
    otel_ctx:set_value(Ctx, ?BAGGAGE_KEY, #{}).

%%

%% checks the keys, values and metadata are valid and drops them if they are not
%% all strings are converted to binaries
verify_baggage(KeyValues) ->
    maps:fold(fun(K, V, Acc) ->
                      %% TODO: filter out keys with invalid characters here
                      case to_binary(K) of
                          error ->
                              Acc;
                          BinKey ->
                              case update_metadata(V) of
                                  {true, ValueMetadata} ->
                                      Acc#{BinKey => ValueMetadata};
                                  _ ->
                                      Acc
                              end
                      end
              end, #{}, KeyValues).

to_binary(String) when is_list(String)->
    %% wrap in a `try' in case the list is not a printable list
    %% this can go away if we start checking the key with a regex
    %% to ensure it only contains certain characters
    try unicode:characters_to_binary(String) catch _:_ -> error end;
to_binary(String) when is_atom(String) ->
    atom_to_binary(String, utf8);
to_binary(String) when is_binary(String) ->
    String;
to_binary(_) ->
    error.

update_metadata({Value, Metadata}) when is_list(Value) ->
    case to_binary(Value) of
        error ->
            false;
        BinValue ->
            update_metadata(BinValue, Metadata)
    end;
update_metadata(Value) when is_list(Value) ->
    case to_binary(Value) of
        error ->
            false;
        BinValue ->
            update_metadata(BinValue, [])
    end;
update_metadata({Value, Metadata}) ->
    update_metadata(Value, Metadata);
update_metadata(Value) ->
    update_metadata(Value, []).

update_metadata(Value, Metadata) when (is_binary(Value) orelse is_list(Value)) andalso is_list(Metadata) ->
    {true, {Value, lists:filtermap(fun verify_metadata/1, Metadata)}};
update_metadata(_, _) ->
    false.


verify_metadata({MK, MV}) when is_binary(MK) , is_binary(MV) ->
    true;
verify_metadata(M) when is_binary(M) ->
    true;
verify_metadata(_) ->
    false.
