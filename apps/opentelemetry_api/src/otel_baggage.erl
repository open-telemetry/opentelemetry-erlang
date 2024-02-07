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
%%
%% The baggage can be stored either in the <i>current context</i> (with {@link set/1} or
%% {@link set/3}, for example) or in an explicit Context (see {@link otel_ctx}).
%% @end
%%%-------------------------------------------------------------------------
-module(otel_baggage).

-export([set/1,
         set/2,
         set/3,
         set/4,
         set_to/2,
         set_to/3,
         set_to/4,
         get_all/0,
         get_all/1,
         clear/0,
         clear/1]).

%% Keys and values are UTF-8 binaries

-type key() :: unicode:unicode_binary().
%% The type for the baggage key, which is a UTF-8 binary.

-type value() :: unicode:unicode_binary().
%% The type for the baggage value, which is a UTF-8 binary.

-type input_key() :: key() | unicode:charlist().
%% An input key, that is, a key that is then converted to a UTF-8 binary.

-type input_value() :: value() | unicode:charlist() | atom().
%% An input value, that is, a value that is then converted to a UTF-8 binary.

-type metadata() :: [unicode:unicode_binary() | {unicode:unicode_binary(), unicode:unicode_binary()}].
%% The type for the baggage metadata, which is a list of UTF-8 binaries or a list of
%% tuples of UTF-8 binaries (as key-value pairs).

-type t() :: #{key() => {value(), metadata()}}.
%% The type for the baggage.

-export_type([t/0,
              key/0,
              value/0]).

-define(BAGGAGE_KEY, '$__otel_baggage_ctx_key').

-include("gradualizer.hrl").

%% @doc Sets the given key-value pairs in the current baggage.
%%
%% If you need to set <i>metadata</i> for the key-value pair, use {@link set/3} instead.
-spec set(#{key() => value()} | [{key(), value()}]) -> ok.
set(KeyValues) when is_list(KeyValues) ->
    set(maps:from_list(KeyValues));
set(KeyValues) when is_map(KeyValues) ->
    Baggage = otel_ctx:get_value(?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(KeyValues)));
set(_) ->
    ok.

%% @doc Sets the given key-value pair in the current baggage, or sets the
%% given key-value pairs in the baggage for the given context.
%%
%% Returns `ok' when using the `set(Key, Value)' form, or the updated context when
%% using the `set(Ctx, KeyValues)' form.
%% @end
%% Ctx will never be a list or binary so we can tell if a context is passed by checking that
-spec set(otel_ctx:t() | input_key(), #{input_key() => input_value()} | [{input_key(), input_value()}] | input_value()) -> otel_ctx:t() | ok.
set(Key, Value) when (is_list(Key) orelse is_binary(Key)) andalso is_binary(Value) ->
    ?assert_type(set(Key, Value, []), ok | undefined | #{any() => any()});
%% drop bad value
set(Key, Value) when (is_list(Key) orelse is_binary(Key)) andalso not is_binary(Value) ->
    ok;
set(Ctx, KeyValues) when is_list(KeyValues) ->
    %% eqwalizer:ignore I know what I'm doing
    ?assert_type(set(Ctx, maps:from_list(KeyValues)), ok | undefined | #{any() => any()});
set(Ctx, KeyValues) when is_map(KeyValues) andalso (is_map(Ctx) orelse Ctx =:= undefined)->
    Baggage = otel_ctx:get_value(Ctx, ?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(Ctx, ?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(KeyValues))).

%% @doc Sets the given key-value pairs in the baggage for the given context.
%%
%% Returns the updated context.
-spec set_to(otel_ctx:t(), #{input_key() => input_value()} | [{input_key(), input_value()}]) -> otel_ctx:t().
set_to(Ctx, KeyValues) when is_list(KeyValues) ->
    set_to(Ctx, maps:from_list(KeyValues));
set_to(Ctx, KeyValues) when is_map(KeyValues) ->
    Baggage = otel_ctx:get_value(Ctx, ?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(Ctx, ?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(KeyValues))).

%% @doc Sets the given key-value pair in the current baggage (with the
%% associated metadata), or sets the given key-value pair in the baggage for the
%% given context.
%%
%% Returns `ok' when using the `set(Key, Value, Metadata)' form, or the updated
%% context when using the `set(Ctx, Key, Value)' form.
-spec set(otel_ctx:t() | input_key(), input_key() | input_value(), input_value() | metadata()) -> otel_ctx:t() | ok.
set(Key, Value, Metadata) when (is_list(Key) orelse is_binary(Key)) andalso is_binary(Value) ->
    Baggage = otel_ctx:get_value(?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(#{Key => {Value, Metadata}})));
%% drop bad value
set(Key, Value, _Metadata) when (is_list(Key) orelse is_binary(Key)) andalso not is_binary(Value) ->
    ok;
set(Ctx, Key, Value) ->
    set_to(?assert_type(Ctx, otel_ctx:t()),
           ?assert_type(Key, input_key()),
           ?assert_type(Value, input_value()),
           []).

%% @doc Sets the given key-value pair in the baggage for the given context.
%%
%% Returns the updated context.
-spec set_to(otel_ctx:t(), input_key(), input_value()) -> otel_ctx:t().
set_to(Ctx, Key, Value) ->
    set_to(Ctx, Key, Value, []).

%% @doc Sets the given key-value pair in the baggage for the given context, with the
%% associated metadata.
%%
%% Returns the updated context.
-spec set(otel_ctx:t(), input_key(), input_value(), metadata()) -> otel_ctx:t().
set(Ctx, Key, Value, Metadata) when is_binary(Value) ->
    Baggage = otel_ctx:get_value(Ctx, ?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(Ctx, ?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(#{Key => {Value, Metadata}})));
%% drop bad value
set(Ctx, _, _, _) ->
    Ctx.

%% @doc Sets the given key-value pair in the baggage for the given context, with the
%% associated metadata.
%%
%% Returns the updated context.
-spec set_to(otel_ctx:t(), input_key(), input_value(), metadata()) -> otel_ctx:t().
set_to(Ctx, Key, Value, Metadata) when is_binary(Value) ->
    Baggage = otel_ctx:get_value(Ctx, ?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(Ctx, ?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(#{Key => {Value, Metadata}})));
%% drop bad value
set_to(Ctx, _, _, _) ->
    Ctx.

%% @doc Returns the baggage from the process dictionary.
-spec get_all() -> t().
get_all() ->
    otel_ctx:get_value(?BAGGAGE_KEY, #{}).

%% @doc Returns the baggage for the given context.
-spec get_all(otel_ctx:t()) -> t().
get_all(Ctx) ->
    otel_ctx:get_value(Ctx, ?BAGGAGE_KEY, #{}).

%% @doc Clears the baggage, removing all the current key-value pairs.
-spec clear() -> ok.
clear() ->
    otel_ctx:set_value(?BAGGAGE_KEY, #{}).

%% @doc Clears the baggage for the given context, removing all the current key-value pairs.
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
