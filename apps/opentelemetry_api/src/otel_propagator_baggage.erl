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
%% @doc An implementation of {@link otel_propagator_text_map} that injects and
%% extracts baggage using the
%% <a href="https://w3c.github.io/baggage/">W3C Baggage format</a>.
%%
%% This propagator along with {@link otel_propagator_trace_context} are used
%% by default. The global TextMap Propagators can be configured in the
%% application environment:
%%
%% ```
%% {text_map_propagators, [trace_context, baggage]},
%% '''
%%
%% Or by calling {@link opentelemetry:set_text_map_propagator/1}.
%% @end
%%%-----------------------------------------------------------------------
-module(otel_propagator_baggage).

-behaviour(otel_propagator_text_map).

-export([fields/1,
         inject/4,
         extract/5]).

-include("opentelemetry.hrl").

-define(DEC2HEX(X),
        if ((X) >= 0) andalso ((X) =< 9) -> (X) + $0;
           ((X) >= 10) andalso ((X) =< 15) -> (X) + $A - 10
        end).

-define(HEX2DEC(X),
        if ((X) >= $0) andalso ((X) =< $9) -> (X) - $0;
           ((X) >= $A) andalso ((X) =< $F) -> (X) - $A + 10;
           ((X) >= $a) andalso ((X) =< $f) -> (X) - $a + 10
        end).

-define(BAGGAGE_HEADER, <<"baggage">>).

fields(_) ->
    [?BAGGAGE_HEADER].

-spec inject(Context, Carrier, CarrierSetFun, Options) -> Carrier
              when Context :: otel_ctx:t(),
                   Carrier :: otel_propagator:carrier(),
                   CarrierSetFun :: otel_propagator_text_map:carrier_set(),
                   Options :: otel_propagator_text_map:propagator_options().
inject(Ctx, Carrier, CarrierSet, _Options) ->
    Baggage = otel_baggage:get_all(Ctx),
    case maps:fold(fun(Key, Value, Acc) ->
                           [$,, [encode_key(Key), "=", encode_value(Value)] | Acc]
                   end, [], Baggage) of
        [$, | List] ->
            CarrierSet(?BAGGAGE_HEADER, unicode:characters_to_binary(List), Carrier);
        _ ->
            Carrier
    end.

-spec extract(Context, Carrier, CarrierKeysFun, CarrierGetFun, Options) -> Context
              when Context :: otel_ctx:t(),
                   Carrier :: otel_propagator:carrier(),
                   CarrierKeysFun :: otel_propagator_text_map:carrier_keys(),
                   CarrierGetFun :: otel_propagator_text_map:carrier_get(),
                   Options :: otel_propagator_text_map:propagator_options().
extract(Ctx, Carrier, _CarrierKeysFun, CarrierGet, _Options) ->
    case CarrierGet(?BAGGAGE_HEADER, Carrier) of
        undefined ->
            Ctx;
        String ->
            Pairs = string:lexemes(String, [$,]),
            DecodedBaggage =
                lists:foldl(fun(Pair, Acc) ->
                                    [Key, Value] = string:split(Pair, "="),
                                    Acc#{decode_key(Key) => decode_value(Value)}

                            end, #{}, Pairs),
            otel_baggage:set(Ctx, DecodedBaggage)
    end.

%%

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
