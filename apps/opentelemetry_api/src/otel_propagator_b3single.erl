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
%% extracts trace context using the B3 single header format specification from
%% Zipkin.
%%
%% @see otel_propagator_b3
%% @end
%%%-----------------------------------------------------------------------
-module(otel_propagator_b3single).

-behaviour(otel_propagator_text_map).

-export([fields/1,
         inject/4,
         extract/5]).

-include("opentelemetry.hrl").

-define(B3_CONTEXT_KEY, <<"b3">>).

fields(_) ->
    [?B3_CONTEXT_KEY].

-spec inject(Context, Carrier, CarrierSetFun, Options) -> Carrier
              when Context :: otel_ctx:t(),
                   Carrier :: otel_propagator:carrier(),
                   CarrierSetFun :: otel_propagator_text_map:carrier_set(),
                   Options :: otel_propagator_text_map:propagator_options().
inject(Ctx, Carrier, CarrierSet, _Options) ->
    case otel_tracer:current_span_ctx(Ctx) of
        #span_ctx{trace_id=TraceId,
                  span_id=SpanId,
                  trace_flags=TraceOptions} when TraceId =/= 0, SpanId =/= 0 ->
            Options = case TraceOptions band 1 of 1 -> <<"1">>; _ -> <<"0">> end,
            EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
            EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),
            B3Context = iolist_to_binary([EncodedTraceId, "-", EncodedSpanId, "-", Options]),
            CarrierSet(?B3_CONTEXT_KEY, B3Context, Carrier);
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
    try
        [TraceId, SpanId, Sampled] = parse_b3_context(Carrier, CarrierGet),
        SpanCtx = otel_tracer:from_remote_span(TraceId, SpanId, Sampled),
        otel_tracer:set_current_span(Ctx, SpanCtx)
    catch
        throw:invalid ->
            undefined;

        %% thrown if _to_integer fails or an invalid string encoding is sent
        error:badarg ->
            undefined
    end.

% B3 maps propagation fields into a hyphen delimited string:
%   {TraceId}-{SpanId}-{SamplingState}-{ParentSpanId}, where the last two fields are optional.
%
% When only propagating a sampling decision, the header is still named b3, but
% only contains the sampling state:
%   {SamplingState}
parse_b3_context(Carrier, CarrierGet) ->
    case CarrierGet(?B3_CONTEXT_KEY, Carrier) of
        B3Context when is_binary(B3Context) ->
            decode_b3_context(string:split(B3Context, "-", all));
        _ ->
            throw(invalid)
    end.

decode_b3_context([TraceId, SpanId]) ->
    % Sampled flag is optional. If it's missing then the sampling decision is
    % deferred. We don't currently support it and just set the flag to 0
    % instead (similarly how some other OTEL implementations are doing).
    [parse_trace_id(TraceId), parse_span_id(SpanId), 0];
decode_b3_context([TraceId, SpanId, Sampled]) ->
    [parse_trace_id(TraceId), parse_span_id(SpanId), parse_is_sampled(Sampled)];
decode_b3_context([TraceId, SpanId, Sampled, _ParentSpanId]) ->
    [parse_trace_id(TraceId), parse_span_id(SpanId), parse_is_sampled(Sampled)];
decode_b3_context(_) ->
    throw(invalid).

% Trace ID is a 32 or 16 lower-hex character binary.
parse_trace_id(TraceId) when is_binary(TraceId) ->
     case string:length(TraceId) =:= 32 orelse string:length(TraceId) =:= 16 of
         true -> string_to_integer(TraceId, 16);
         _ -> throw(invalid)
     end;
parse_trace_id(_) ->
    throw(invalid).

% Span ID is a 16 lower-hex character binary.
parse_span_id(SpanId) when is_binary(SpanId) ->
     case string:length(SpanId) =:= 16 of
         true -> string_to_integer(SpanId, 16);
         _ -> throw(invalid)
     end;
parse_span_id(_) ->
    throw(invalid).

% Sampling State is encoded as a single hex character for all states except
% Defer. Defer is absence of the sampling field.
%
% Possible states:
%   1 - accept
%   0 - deny
%   d - debug (not supported at the moment, we instead used accept)
%
% Before the specification was written, some tracers propagated X-B3-Sampled as
% true or false.
parse_is_sampled(Sampled) when is_binary(Sampled) ->
    case Sampled of
        S when S =:= <<"1">> orelse S =:= <<"d">> orelse S =:= <<"true">> -> 1;
        S when S =:= <<"0">> orelse S =:= <<"false">> -> 0;
        _ -> throw(invalid)
    end;
parse_is_sampled(_) ->
    throw(invalid).

string_to_integer(S, Base) when is_binary(S) ->
    binary_to_integer(S, Base).
