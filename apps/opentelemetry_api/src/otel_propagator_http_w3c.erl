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
%% @end
%%%-----------------------------------------------------------------------
-module(otel_propagator_http_w3c).

-behaviour(otel_propagator).

-export([inject/1,
         encode/1,
         extract/2,
         decode/1]).

-include("opentelemetry.hrl").

-define(VERSION, <<"00">>).

-define(ZERO_TRACEID, <<"00000000000000000000000000000000">>).
-define(ZERO_SPANID, <<"0000000000000000">>).

-define(HEADER_KEY, <<"traceparent">>).
-define(STATE_HEADER_KEY, <<"tracestate">>).

-define(KEY_MP, element(2, re:compile("^[a-z0-9][a-z0-9_*/-]{0,255}$|^([a-z0-9_*/-]{1,241})(@[a-z0-9_*/-]{1,14})$"))).
-define(VALUE_MP, element(2, re:compile("^[ -~]{0,256}$"))).

-define(MAX_TRACESTATE_PAIRS, 32).

-spec inject(opentelemetry:span_ctx() | undefined) -> otel_propagator:text_map().
inject(#span_ctx{trace_id=TraceId,
                 span_id=SpanId})
  when TraceId =:= 0 orelse SpanId =:= 0 ->
    [];
inject(SpanCtx=#span_ctx{}) ->
    EncodedValue = encode(SpanCtx),
    [{?HEADER_KEY, EncodedValue} | encode_tracestate(SpanCtx)];
inject(undefined) ->
    [].

-spec encode(opentelemetry:span_ctx()) -> binary().
encode(#span_ctx{trace_id=TraceId,
                 span_id=SpanId,
                 trace_flags=TraceOptions}) ->
    Options = case TraceOptions band 1 of 1 -> <<"01">>; _ -> <<"00">> end,
    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),
    iolist_to_binary([?VERSION, "-", EncodedTraceId, "-", EncodedSpanId, "-", Options]).

encode_tracestate(#span_ctx{tracestate=undefined}) ->
    [];
encode_tracestate(#span_ctx{tracestate=Entries}) ->
    StateHeaderValue = lists:join($,, [[Key, $=, Value] || {Key, Value} <- Entries]),
    [{?STATE_HEADER_KEY, unicode:characters_to_binary(StateHeaderValue)}].

-spec extract(otel_propagator:text_map(), term()) -> opentelemetry:span_ctx()| undefined.
extract(Headers, _) when is_list(Headers) ->
    case header_take(?HEADER_KEY, Headers) of
        [{_, Value} | RestHeaders] ->
            case header_member(?HEADER_KEY, RestHeaders) of
                true ->
                    %% duplicate traceparent header found
                    undefined;
                false ->
                    case decode(string:trim(Value)) of
                        undefined ->
                            undefined;
                        SpanCtx ->
                            Tracestate = tracestate_from_headers(Headers),
                            SpanCtx#span_ctx{tracestate=Tracestate}
                    end
            end;
        _ ->
            undefined
    end;
extract(_, _) ->
    undefined.

tracestate_from_headers(Headers) ->
    %% could be multiple tracestate headers. Combine them all with comma separators
    case combine_headers(?STATE_HEADER_KEY, Headers) of
        [] ->
            undefined;
        FieldValue ->
            tracestate_decode(FieldValue)
    end.

combine_headers(Key, Headers) ->
    lists:foldl(fun({K, V}, Acc) ->
                        case string:equal(Key, string:casefold(K)) of
                            true ->
                                [Acc,  $, | V];
                            false ->
                                Acc
                        end
                end, [], Headers).

split(Pair) ->
    case string:split(Pair, "=", all) of
        [Key, Value] when Value =/= [] andalso Value =/= <<>> ->
            {iolist_to_binary(Key), iolist_to_binary(Value)};
        _ ->
            undefined
    end.

%% note: version ff (255) not allowed by spec
decode(TraceContext) when is_list(TraceContext) ->
    decode(list_to_binary(TraceContext));
decode(<<_:2/binary, "-", TraceId:32/binary, "-", SpanId:16/binary, _/binary>>)
  when TraceId =:= ?ZERO_TRACEID orelse SpanId =:= ?ZERO_SPANID ->
    undefined;
decode(<<Version:2/binary, "-", TraceId:32/binary, "-", SpanId:16/binary, "-", Opts:2/binary>>)
  when Version >= ?VERSION andalso Version =/= <<"ff">> ->
    to_span_ctx(Version, TraceId, SpanId, Opts);
%% future versions could have more after Opts, so allow for a trailing -
decode(<<Version:2/binary, "-", TraceId:32/binary, "-", SpanId:16/binary, "-", Opts:2/binary, "-", _/binary>>)
  when Version > ?VERSION andalso Version =/= <<"ff">> ->
        to_span_ctx(Version, TraceId, SpanId, Opts);
decode(_) ->
    undefined.

to_span_ctx(Version, TraceId, SpanId, Opts) ->
    try
        %% verify version is hexadecimal
        _ = binary_to_integer(Version, 16),
        otel_tracer:from_remote_span(binary_to_integer(TraceId, 16),
                                     binary_to_integer(SpanId, 16),
                                     case Opts of <<"01">> -> 1; <<"00">> -> 0; _ -> error(badarg) end)
    catch
        %% to integer from base 16 string failed
        error:badarg ->
            undefined
    end.

tracestate_decode(Value) ->
    parse_pairs(string:lexemes(Value, [$,])).

parse_pairs(Pairs) when length(Pairs) =< ?MAX_TRACESTATE_PAIRS ->
    parse_pairs(Pairs, []);
parse_pairs(_) ->
    undefined.

parse_pairs([], Acc) ->
    Acc;
parse_pairs([Pair | Rest], Acc) ->
    case split(string:trim(Pair)) of
        {K, V} ->
            case re:run(K, ?KEY_MP) =/= nomatch
                andalso not lists:keymember(K, 1, Acc)
                andalso re:run(V, ?VALUE_MP) =/= nomatch
            of
                false ->
                    undefined;
                true ->
                    parse_pairs(Rest, Acc ++ [{K, V}])
            end;
        undefined ->
            undefined
    end.
%%

header_take(Key, Headers) ->
    lists:dropwhile(fun({K, _}) ->
                            not string:equal(Key, string:casefold(K))
                    end, Headers).

header_member(_, []) ->
    false;
header_member(Key, [{K, _} | T]) ->
    string:equal(Key, string:casefold(K)) orelse header_member(Key, T).
