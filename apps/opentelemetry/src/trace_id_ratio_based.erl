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
%% A sampler is a function run on each started span that returns whether to
%% record and propagate, only record or not record the span.
%% @end
%%%-------------------------------------------------------------------------
-module(trace_id_ratio_based).

-behavior(otel_sampler).

-export([description/1, setup/1, should_sample/7]).

-export_type([opts/0]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_sampler.hrl").

-type opts() :: #{attributes => opentelemetry:attributes(), probability := float()}.

%% 2^63 - 1
-define(MAX_VALUE, 9223372036854775807).

setup(Opts = #{probability := Probability}) ->
    IdUpperBound =
        case Probability of
            P when P =:= 0.0 ->
                0;
            P when P =:= 1.0 ->
                ?MAX_VALUE;
            P when P >= 0.0 andalso P =< 1.0 ->
                P * ?MAX_VALUE
        end,
    Attributes = maps:get(attributes, Opts, []),
    #{attributes => Attributes, probability => Probability, id_upper_bound => IdUpperBound}.

description(#{probability := Probability}) ->
    unicode:characters_to_binary(io_lib:format("TraceIdRatioBased{~.6f}", [Probability])).

should_sample(Ctx, TraceId, _Links, _SpanName, _SpanKind, _Attributes, #{
    attributes := DecisionAttributes, id_upper_bound := IdUpperBound
}) ->
    Decision = decide(TraceId, IdUpperBound),
    {Decision, DecisionAttributes, tracestate(Ctx)}.

decide(undefined, _IdUpperBound) ->
    ?DROP;
decide(0, _IdUpperBound) ->
    ?DROP;
decide(TraceId, IdUpperBound) ->
    Lower64Bits = TraceId band ?MAX_VALUE,
    case erlang:abs(Lower64Bits) < IdUpperBound of
        true -> ?RECORD_AND_SAMPLE;
        false -> ?DROP
    end.

tracestate(Ctx) ->
    tracestate_(otel_tracer:current_span_ctx(Ctx)).

tracestate_(#span_ctx{tracestate = undefined}) ->
    [];
tracestate_(#span_ctx{tracestate = TraceState}) ->
    TraceState;
tracestate_(undefined) ->
    [].