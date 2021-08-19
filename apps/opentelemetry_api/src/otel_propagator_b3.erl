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
-module(otel_propagator_b3).

-behaviour(otel_propagator_text_map).

-export([fields/0,
         inject/3,
         extract/4]).

-include("opentelemetry.hrl").

-define(B3_TRACE_ID, <<"X-B3-TraceId">>).
-define(B3_SPAN_ID, <<"X-B3-SpanId">>).
-define(B3_SAMPLED, <<"X-B3-Sampled">>).

-define(B3_IS_SAMPLED(S), S =:= "1" orelse S =:= <<"1">> orelse S =:= "true" orelse S =:= <<"true">>).

fields() ->
    [?B3_TRACE_ID, ?B3_SPAN_ID, ?B3_SAMPLED].

inject(Ctx, Carrier, CarrierSet) ->
    case otel_tracer:current_span_ctx(Ctx) of
        #span_ctx{trace_id=TraceId,
                  span_id=SpanId,
                  trace_flags=TraceOptions} when TraceId =/= 0 andalso SpanId =/= 0 ->
            Options = case TraceOptions band 1 of 1 -> <<"1">>; _ -> <<"0">> end,
            EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
            EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),
            CarrierSet(?B3_TRACE_ID, iolist_to_binary(EncodedTraceId),
                       CarrierSet(?B3_SPAN_ID, iolist_to_binary(EncodedSpanId),
                                  CarrierSet(?B3_SAMPLED, Options, Carrier)));
        _ ->
            Carrier
    end.

extract(Ctx, Carrier, _CarrierKeysFun, CarrierGet) ->
    try
        TraceId = trace_id(Carrier, CarrierGet),
        SpanId = span_id(Carrier, CarrierGet),
        Sampled = CarrierGet(?B3_SAMPLED, Carrier),
        SpanCtx =
            otel_tracer:from_remote_span(string_to_integer(TraceId, 16),
                                         string_to_integer(SpanId, 16),
                                         case Sampled of True when ?B3_IS_SAMPLED(True) -> 1; _ -> 0 end),
        otel_tracer:set_current_span(Ctx, SpanCtx)
    catch
        throw:invalid ->
            Ctx;

        %% thrown if _to_integer fails
        error:badarg ->
            Ctx
    end.

trace_id(Carrier, CarrierGet) ->
    case CarrierGet(?B3_TRACE_ID, Carrier) of
        TraceId when is_list(TraceId) orelse is_binary(TraceId) ->
            case string:length(TraceId) =:= 32 orelse string:length(TraceId) =:= 16 of
                true ->
                    TraceId;
                _ ->
                    throw(invalid)
            end;
        _ ->
            throw(invalid)
    end.

span_id(Carrier, CarrierGet) ->
    case CarrierGet(?B3_SPAN_ID, Carrier) of
        SpanId when is_list(SpanId) orelse is_binary(SpanId) ->
            case string:length(SpanId) =:= 16 of
                true ->
                    SpanId;
                _ ->
                    throw(invalid)
            end;
        _ ->
            throw(invalid)
    end.

string_to_integer(S, Base) when is_binary(S) ->
    binary_to_integer(S, Base);
string_to_integer(S, Base) when is_list(S) ->
    list_to_integer(S, Base).
