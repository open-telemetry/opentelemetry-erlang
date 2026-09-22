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
%% Shared comma-separated key/value syntax for attributes and headers.
%% Names and values are trimmed, percent-decoded once, and checked for UTF-8.
%% Quotes and plus signs are literal; only the first equals sign separates a pair.
-module(otel_configuration_key_value_list).

-export([parse/1]).

-type parse_error() :: binary() | {invalid_percent_encoding | invalid_utf8, binary()}.

%% Valid pairs and errors are each returned in source order. The caller decides
%% whether to report and skip malformed entries or fail configuration.
%% Input values are never logged here because they may contain credentials.
-spec parse(binary()) -> {[{binary(), binary()}], [parse_error()]}.
parse(<<>>) -> {[], []};
parse(Value) ->
    lists:foldr(
      fun(Part, {Pairs, Errors}) ->
              case parse_pair(Part) of
                  {ok, Pair} -> {[Pair | Pairs], Errors};
                  {error, Reason} -> {Pairs, [Reason | Errors]}
              end
      end, {[], []}, binary:split(Value, <<",">>, [global])).

parse_pair(Part) ->
    case binary:split(Part, <<"=">>) of
        [RawName, RawValue] ->
            %% Trimming can reject malformed UTF-8 at the boundaries. The
            %% decoder checks the complete name and value after percent decoding.
            try {string:trim(RawName), string:trim(RawValue)} of
                {<<>>, _} -> {error, Part};
                {Name, Value} -> decode_pair(Name, Value)
            catch
                error:_ -> {error, {invalid_utf8, Part}}
            end;
        _ -> {error, Part}
    end.

decode_pair(Name, Value) ->
    case percent_decode_binary(Name, <<>>) of
        {error, Reason} -> {error, {Reason, Name}};
        {ok, DecodedName} ->
            case percent_decode_binary(Value, <<>>) of
                {error, Reason} -> {error, {Reason, Value}};
                {ok, DecodedValue} -> {ok, {DecodedName, DecodedValue}}
            end
    end.

percent_decode_binary(<<$%, High, Low, Rest/binary>>, Acc) ->
    case {hex_value(High), hex_value(Low)} of
        {{ok, HighValue}, {ok, LowValue}} ->
            Octet = HighValue * 16 + LowValue,
            percent_decode_binary(Rest, <<Acc/binary, Octet>>);
        _ ->
            {error, invalid_percent_encoding}
    end;
percent_decode_binary(<<$%, _/binary>>, _Acc) ->
    {error, invalid_percent_encoding};
percent_decode_binary(<<Octet, Rest/binary>>, Acc) ->
    percent_decode_binary(Rest, <<Acc/binary, Octet>>);
percent_decode_binary(<<>>, Acc) ->
    case unicode:characters_to_list(Acc) of
        {error, _, _} -> {error, invalid_utf8};
        {incomplete, _, _} -> {error, invalid_utf8};
        _ -> {ok, Acc}
    end.

hex_value(Character) when Character >= $0, Character =< $9 ->
    {ok, Character - $0};
hex_value(Character) when Character >= $a, Character =< $f ->
    {ok, Character - $a + 10};
hex_value(Character) when Character >= $A, Character =< $F ->
    {ok, Character - $A + 10};
hex_value(_) ->
    error.
