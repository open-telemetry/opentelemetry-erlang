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
         clear/1,
         get_text_map_propagators/0]).

%% keys and values are UTF-8 binaries
-type key() :: unicode:unicode_binary().
-type value() :: unicode:unicode_binary().
-type metadata() :: [unicode:unicode_binary() | {unicode:unicode_binary(), unicode:unicode_binary()}].

-type t() :: #{key() => {value(), metadata()}}.

-export_type([t/0,
              key/0,
              value/0]).

-define(DEC2HEX(X),
        if ((X) >= 0) andalso ((X) =< 9) -> (X) + $0;
           ((X) >= 10) andalso ((X) =< 15) -> (X) + $A - 10
        end).

-define(HEX2DEC(X),
        if ((X) >= $0) andalso ((X) =< $9) -> (X) - $0;
           ((X) >= $A) andalso ((X) =< $F) -> (X) - $A + 10;
           ((X) >= $a) andalso ((X) =< $f) -> (X) - $a + 10
        end).

-define(BAGGAGE_KEY, '$__otel_baggage_ctx_key').
-define(BAGGAGE_HEADER, <<"baggage">>).

-spec set(#{key() => value()} | [{key(), value()}]) -> ok.
set(KeyValues) when is_list(KeyValues) ->
    set(maps:from_list(KeyValues));
set(KeyValues) when is_map(KeyValues) ->
    Baggage = otel_ctx:get_value(?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(KeyValues)));
set(_) ->
    ok.

%% Ctx will never be a list or binary so we can tell if a context is passed by checking that
-spec set(otel_ctx:t() | key(), #{key() => value()} | [{key(), value()}] | value()) -> otel_ctx:t().
set(Key, Value) when is_list(Key) ; is_binary(Key) ->
    set(Key, Value, []);
set(Ctx, KeyValues) when is_list(KeyValues) ->
    set(Ctx, maps:from_list(KeyValues));
set(Ctx, KeyValues) when is_map(KeyValues) ->
    Baggage = otel_ctx:get_value(Ctx, ?BAGGAGE_KEY, #{}),
    otel_ctx:set_value(Ctx, ?BAGGAGE_KEY, maps:merge(Baggage, verify_baggage(KeyValues))).

-spec set(otel_ctx:t(), key(), value()) -> ok | otel_ctx:t().
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

-spec get_text_map_propagators() -> {otel_propagator:text_map_extractor(), otel_propagator:text_map_injector()}.
get_text_map_propagators() ->
    ToText = fun(Baggage) when is_map(Baggage) ->
                     case maps:fold(fun(Key, Value, Acc) ->
                                            [$,, [encode_key(Key), "=", encode_value(Value)] | Acc]
                                    end, [], Baggage) of
                         [$, | List] ->
                             [{?BAGGAGE_HEADER, unicode:characters_to_binary(List)}];
                         _ ->
                             []
                     end;
                (_) ->
                     []
             end,
    FromText = fun(Headers, CurrentBaggage) ->
                       case lookup(?BAGGAGE_HEADER, Headers) of
                           undefined ->
                               CurrentBaggage;
                           String ->
                               Pairs = string:lexemes(String, [$,]),
                               lists:foldl(fun(Pair, Acc) ->
                                                   [Key, Value] = string:split(Pair, "="),
                                                   Acc#{decode_key(Key) =>
                                                            decode_value(Value)}
                                           end, CurrentBaggage, Pairs)
                       end
               end,
    Inject = otel_ctx:text_map_injector(?BAGGAGE_KEY, ToText),
    Extract = otel_ctx:text_map_extractor(?BAGGAGE_KEY, FromText),
    {Extract, Inject}.

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

%% find a header in a list, ignoring case
lookup(_, []) ->
    undefined;
lookup(Header, [{H, Value} | Rest]) ->
    case string:equal(Header, H, true, none) of
        true ->
            Value;
        false ->
            lookup(Header, Rest)
    end.

encode_key(Key) ->
    form_urlencode(Key, [{encoding, utf8}]).

encode_value({Value, Metadata}) ->
    EncodedMetadata = encode_metadata(Metadata),
    EncodedValue = form_urlencode(Value, [{encoding, utf8}]),
    unicode:characters_to_binary(lists:join(<<";">>, [EncodedValue | EncodedMetadata])).

encode_metadata(Metadata) when is_list(Metadata) ->
    lists:filtermap(fun({MK, MV}) when is_binary(MK) , is_binary(MV)  ->
                            {true, [MK, <<"=">>, MV]};
                       (M) when is_binary(M) ->
                            {true, M};
                       (_) ->
                            false
                    end, Metadata);
encode_metadata(_) ->
    [].

decode_key(Key) ->
    percent_decode(string:trim(unicode:characters_to_binary(Key))).

decode_value(ValueAndMetadata) ->
    [Value | MetadataList] = string:lexemes(ValueAndMetadata, [$;]),
    {string_decode(Value), lists:filtermap(fun metadata_decode/1, MetadataList)}.

metadata_decode(Metadata) ->
    case string:split(Metadata, "=") of
        [MetadataKey] ->
            {true, string_decode(MetadataKey)};
        [MetadataKey, MetadataValue] ->
            {true, {string_decode(MetadataKey), string_decode(MetadataValue)}};
        _ ->
            false
    end.

string_decode(S) ->
    percent_decode(string:trim(unicode:characters_to_binary(S))).

%% TODO: call `uri_string:percent_decode' and remove this when OTP-23 is
%% the oldest version we maintain support for
-spec percent_decode(URI) -> Result when
      URI :: uri_string:uri_string(),
      Result :: uri_string:uri_string() |
                {error, {invalid, {atom(), {term(), term()}}}}.
percent_decode(URI) when is_list(URI) orelse
                         is_binary(URI) ->
    raw_decode(URI).

%% TODO: call `uri_string:percent_encode' when it is added to OTP and
%% available in the oldest version we support
form_urlencode(Cs, [{encoding, Encoding}])
  when is_list(Cs), Encoding =:= utf8; Encoding =:= unicode ->
    B = convert_to_binary(Cs, utf8, Encoding),
    html5_byte_encode(B);
form_urlencode(Cs, [{encoding, Encoding}])
  when is_binary(Cs), Encoding =:= utf8; Encoding =:= unicode ->
    html5_byte_encode(Cs);
form_urlencode(Cs, [{encoding, Encoding}]) when is_list(Cs); is_binary(Cs) ->
    throw({error,invalid_encoding, Encoding});
form_urlencode(Cs, _) ->
    throw({error,invalid_input, Cs}).

html5_byte_encode(B) ->
    html5_byte_encode(B, <<>>).
%%
html5_byte_encode(<<>>, Acc) ->
    Acc;
html5_byte_encode(<<$ ,T/binary>>, Acc) ->
    html5_byte_encode(T, <<Acc/binary,$+>>);
html5_byte_encode(<<H,T/binary>>, Acc) ->
    case is_url_char(H) of
        true ->
            html5_byte_encode(T, <<Acc/binary,H>>);
        false ->
            <<A:4,B:4>> = <<H>>,
            html5_byte_encode(T, <<Acc/binary,$%,(?DEC2HEX(A)),(?DEC2HEX(B))>>)
    end;
html5_byte_encode(H, _Acc) ->
    throw({error,invalid_input, H}).


%% Return true if input char can appear in form-urlencoded string
%% Allowed chararacters:
%%   0x2A, 0x2D, 0x2E, 0x30 to 0x39, 0x41 to 0x5A,
%%   0x5F, 0x61 to 0x7A
is_url_char(C)
  when C =:= 16#2A; C =:= 16#2D;
       C =:= 16#2E; C =:= 16#5F;
       16#30 =< C, C =< 16#39;
       16#41 =< C, C =< 16#5A;
       16#61 =< C, C =< 16#7A -> true;
is_url_char(_) -> false.

%% Convert to binary
convert_to_binary(Binary, InEncoding, OutEncoding) ->
    case unicode:characters_to_binary(Binary, InEncoding, OutEncoding) of
        {error, _List, RestData} ->
            throw({error, invalid_input, RestData});
        {incomplete, _List, RestData} ->
            throw({error, invalid_input, RestData});
        Result ->
            Result
    end.

-spec raw_decode(list()|binary()) -> list() | binary() | uri_string:error().
raw_decode(Cs) ->
    raw_decode(Cs, <<>>).
%%
raw_decode(L, Acc) when is_list(L) ->
    try
        B0 = unicode:characters_to_binary(L),
        B1 = raw_decode(B0, Acc),
        unicode:characters_to_list(B1)
    catch
        throw:{error, Atom, RestData} ->
            {error, Atom, RestData}
    end;
raw_decode(<<$%,C0,C1,Cs/binary>>, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true ->
            B = ?HEX2DEC(C0)*16+?HEX2DEC(C1),
            raw_decode(Cs, <<Acc/binary, B>>);
        false ->
            throw({error,invalid_percent_encoding,<<$%,C0,C1>>})
    end;
raw_decode(<<C,Cs/binary>>, Acc) ->
    raw_decode(Cs, <<Acc/binary, C>>);
raw_decode(<<>>, Acc) ->
    check_utf8(Acc).

%% Returns Cs if it is utf8 encoded.
check_utf8(Cs) ->
    case unicode:characters_to_list(Cs) of
        {incomplete,_,_} ->
            throw({error,invalid_utf8,Cs});
        {error,_,_} ->
            throw({error,invalid_utf8,Cs});
        _ -> Cs
    end.

-spec is_hex_digit(char()) -> boolean().
is_hex_digit(C)
  when $0 =< C, C =< $9;$a =< C, C =< $f;$A =< C, C =< $F -> true;
is_hex_digit(_) -> false.
