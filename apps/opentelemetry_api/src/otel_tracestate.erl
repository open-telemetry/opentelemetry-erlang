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
%% @doc tracestate provides additional vendor-specific trace identification
%% information across different distributed tracing systems. It represents an
%% immutable list consisting of key/value pairs, each pair is referred to as a
%% list-member.
%%
%% Keys and values are strings of up to 256 printable US-ASCII characters,
%% conforming to the W3C spec https://www.w3.org/TR/trace-context/#tracestate-field
%%
%% @end
%%%-----------------------------------------------------------------------
-module(otel_tracestate).

-export([new/0,
         new/1,
         get/2,
         add/3,
         remove/2,
         update/3,
         decode_header/1,
         encode_header/1
        ]).

-type members() :: [{unicode:latin1_chardata(), unicode:latin1_chardata()}].

-record(tracestate, {members :: members()}).
-type t() :: #tracestate{}.

-export_type([t/0,
              members/0]).

%% See https://www.w3.org/TR/trace-context/#tracestate-header
%% for the limits and string requirements that make up the regexes
-define(MAX_MEMBERS, 32).
%% re:compile("^(([a-z][_0-9a-z\-\*\/]{0,255})|([a-z0-9][_0-9a-z-*/]{0,240}@[a-z][_0-9a-z-*/]{0,13}))$")
-define(KEY_MP, {re_pattern,3,0,0,
                 <<69,82,67,80,59,1,0,0,16,0,0,0,1,0,0,0,255,255,255,255,
                   255,255,255,255,0,0,0,0,0,0,3,0,0,0,64,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,131,0,247,
                   27,133,0,84,0,1,133,0,76,0,2,110,0,0,0,0,0,0,0,0,0,0,
                   0,0,254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,
                   0,0,0,0,0,164,255,3,0,0,0,128,254,255,255,7,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,0,0,104,0,0,0,255,120,0,76,119,0,
                   155,133,0,149,0,3,110,0,0,0,0,0,0,255,3,0,0,0,0,254,
                   255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,0,0,0,
                   0,164,255,3,0,0,0,128,254,255,255,7,0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,109,0,0,0,240,29,64,110,0,0,0,0,0,0,0,0,
                   0,0,0,0,254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                   110,0,0,0,0,0,164,255,3,0,0,0,128,254,255,255,7,0,0,0,
                   0,0,0,0,0,0,0,0,0,0,0,0,0,104,0,0,0,13,120,0,149,120,
                   0,239,25,120,0,247,0>>}).
%% re:compile("^([ -+--<>-~]{0,255}[!-+--<>-~])$")
-define(VALUE_MP, {re_pattern,1,0,0,
                   <<69,82,67,80,152,0,0,0,16,0,0,0,1,0,0,0,255,255,255,
                     255,255,255,255,255,0,0,0,0,0,0,1,0,0,0,64,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,131,0,
                     84,27,133,0,76,0,1,110,0,0,0,0,255,239,255,223,255,
                     255,255,255,255,255,255,127,0,0,0,0,0,0,0,0,0,0,0,0,0,
                     0,0,0,104,0,0,0,255,110,0,0,0,0,254,239,255,223,255,
                     255,255,255,255,255,255,127,0,0,0,0,0,0,0,0,0,0,0,0,0,
                     0,0,0,120,0,76,25,120,0,84,0>>}).

-define(IS_STRING, (is_atom(Key) orelse is_string(Key) orelse is_binary(Key))).

-spec new() -> t().
new() ->
    #tracestate{members=[]}.

-spec new([{unicode:latin1_chardata(), unicode:latin1_chardata()}]) -> t().
new(List) ->
    Members = [Element || {Key, Value}=Element <- List, is_valid(Key, Value)],
    #tracestate{members=Members}.

-spec add(unicode:latin1_chardata(), unicode:latin1_chardata(), t()) -> t().
add(Key, Value, Tracestate=#tracestate{members=TracestateList}) ->
    case is_valid(Key, Value) of
        true ->
            Tracestate#tracestate{members=[{Key, Value} | TracestateList]};
        false ->
            Tracestate
    end.

-spec get(unicode:latin1_chardata(), t()) -> unicode:latin1_chardata().
get(Key, #tracestate{members=TracestateList}) ->
    case lists:keyfind(Key, 1, TracestateList) of
        false ->
            "";
        {_, Value} ->
            Value
    end.

-spec remove(unicode:latin1_chardata(), t()) -> t().
remove(Key, Tracestate=#tracestate{members=TracestateList}) ->
    Tracestate#tracestate{members=lists:keydelete(Key, 1, TracestateList)}.

-spec update(unicode:latin1_chardata(), unicode:latin1_chardata(), t()) -> t().
update(Key, Value, Tracestate=#tracestate{members=TracestateList}) ->
    case is_valid(Key, Value) of
        true ->
            Tracestate#tracestate{members=[{Key, Value} | lists:keydelete(Key, 1, TracestateList)]};
        false ->
            Tracestate
    end.

-spec decode_header(string() | undefined) -> t().
decode_header(undefined) ->
    new();
decode_header(Value) ->
    #tracestate{members=parse_pairs(string:lexemes(Value, [$,]))}.

-spec encode_header(t()) -> unicode:latin1_binary().
encode_header(#tracestate{members=Entries=[_|_]}) ->
    StateHeaderValue = lists:join($,, [[Key, $=, Value] || {Key, Value} <- Entries]),
    otel_utils:assert_to_binary(StateHeaderValue);
encode_header(_) ->
    <<>>.

%%

is_valid(Key, Value) ->
    try
        re:run(Key, ?KEY_MP, [{capture, none}]) =:= match
            andalso re:run(Value, ?VALUE_MP, [{capture, none}]) =:= match
    catch
        _:_ ->
            false
    end.

parse_pairs(Pairs) when length(Pairs) =< ?MAX_MEMBERS ->
    parse_pairs(Pairs, []);
parse_pairs(_) ->
    [].

parse_pairs([], Acc) ->
    Acc;
parse_pairs([Pair | Rest], Acc) ->
    case split(string:trim(Pair)) of
        {K, V} ->
            case re:run(K, ?KEY_MP) =/= nomatch
                andalso re:run(V, ?VALUE_MP) =/= nomatch
            of
                false ->
                    [];
                true ->
                    %% replace existing key value or append to the end of the list
                    parse_pairs(Rest, lists:keystore(K, 1, Acc, {K, V}))
            end;
        undefined ->
            []
    end.

split(Pair) ->
    case string:split(Pair, "=", all) of
        [Key, Value] when Value =/= [] andalso Value =/= <<>> ->
            {otel_utils:assert_to_binary(Key),
             otel_utils:assert_to_binary(Value)};
        _ ->
            undefined
    end.
