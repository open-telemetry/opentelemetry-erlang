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
%% @doc An implementation of {@link otel_propagator_text_map} that injects and
%% extracts trace context using the
%% <a href="https://www.w3.org/TR/trace-context/">W3C TraceContext format</a>.
%%
%% This propagator along with {@link otel_propagator_baggage} are used
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
-module(otel_propagator_trace_context).

-behaviour(otel_propagator_text_map).

-export([fields/1,
         inject/4,
         extract/5]).

-include("opentelemetry.hrl").

-define(VERSION, <<"00">>).

-define(ZERO_TRACEID, <<"00000000000000000000000000000000">>).
-define(ZERO_SPANID, <<"0000000000000000">>).

-define(HEADER_KEY, <<"traceparent">>).
-define(STATE_HEADER_KEY, <<"tracestate">>).

-define(KEY_MP, element(2, re:compile("^[a-z0-9][a-z0-9_*/-]{0,255}$|^([a-z0-9_*/-]{1,241})(@[a-z0-9_*/-]{1,14})$"))).
-define(VALUE_MP, element(2, re:compile("^[ -~]{0,256}$"))).

-define(MAX_TRACESTATE_PAIRS, 32).

fields(_) ->
    [?HEADER_KEY, ?STATE_HEADER_KEY].

-spec inject(Context, Carrier, CarrierSetFun, Options) -> Carrier
              when Context :: otel_ctx:t(),
                   Carrier :: otel_propagator:carrier(),
                   CarrierSetFun :: otel_propagator_text_map:carrier_set(),
                   Options :: otel_propagator_text_map:propagator_options().
inject(Ctx, Carrier, CarrierSet, _Options) ->
    case otel_tracer:current_span_ctx(Ctx) of
        SpanCtx=#span_ctx{trace_id=TraceId,
                          span_id=SpanId} when TraceId =/= 0 andalso SpanId =/= 0 ->
            {TraceParent, TraceState} = encode_span_ctx(SpanCtx),
            Carrier1 = CarrierSet(?HEADER_KEY, TraceParent, Carrier),
            case TraceState of
                <<>> ->
                    Carrier1;
                _ ->
                    CarrierSet(?STATE_HEADER_KEY, TraceState, Carrier1)
            end;
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
    MaybeSpanCtxString = CarrierGet(?HEADER_KEY, Carrier),
    case MaybeSpanCtxString of
        undefined ->
            Ctx;
        SpanCtxString ->
            case decode(string:trim(SpanCtxString)) of
                undefined ->
                    Ctx;
                SpanCtx ->
                    TraceStateString = CarrierGet(?STATE_HEADER_KEY, Carrier),
                    Tracestate = tracestate_decode(TraceStateString),
                    otel_tracer:set_current_span(Ctx, SpanCtx#span_ctx{tracestate=Tracestate})
            end
    end.

%%

-spec encode_span_ctx(opentelemetry:span_ctx()) -> {unicode:latin1_binary(), unicode:latin1_binary()}.
encode_span_ctx(#span_ctx{trace_id=TraceId,
                          span_id=SpanId,
                          trace_flags=TraceOptions,
                          tracestate=TraceState}) ->
    {encode_traceparent(TraceId, SpanId, TraceOptions), encode_tracestate(TraceState)}.

encode_traceparent(TraceId, SpanId, TraceOptions) ->
    Options = case TraceOptions band 1 of 1 -> <<"01">>; _ -> <<"00">> end,
    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),
    iolist_to_binary([?VERSION, "-", EncodedTraceId, "-", EncodedSpanId, "-", Options]).

encode_tracestate(Entries=[_|_]) ->
    StateHeaderValue = lists:join($,, [[Key, $=, Value] || {Key, Value} <- Entries]),
    unicode:characters_to_binary(StateHeaderValue);
encode_tracestate(_) ->
    <<>>.

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

tracestate_decode(undefined) ->
    [];
tracestate_decode(Value) ->
    parse_pairs(string:lexemes(Value, [$,])).

parse_pairs(Pairs) when length(Pairs) =< ?MAX_TRACESTATE_PAIRS ->
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
