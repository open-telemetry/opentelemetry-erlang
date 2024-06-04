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
%% @doc An {@link otel_sampler} that samples a configured percentage of spans.
%%
%% This sampler samples a configured percentage of spans, where the sampling
%% decision is <i>deterministic</i> with respect to the span trace ID. That means
%% the sampler always makes the same decision for the same trace ID.
%%
%% This is one of the
%% <a href="https://opentelemetry.io/docs/specs/otel/trace/sdk/#built-in-samplers">built-in
%% samplers</a> provided by the OpenTelemetry SDK.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_sampler_trace_id_ratio_based).

-behavior(otel_sampler).

-export([description/1, setup/1, should_sample/7]).

-export_type([config/0, probability/0]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_sampler.hrl").

-type probability() :: float().
%% A probability on whether to sample a span, between `0.0' and `1.0'.

-type config() :: #{probability := probability(), id_upper_bound := integer()}.
%% The configuration for this sampler.

%% 2^63 - 1
-define(MAX_VALUE, 9223372036854775807).

%% @private
-spec setup(probability()) -> config().
setup(Probability) ->
    IdUpperBound =
        case Probability of
            P when P == 0.0 ->
                0;
            P when P =:= 1.0 ->
                ?MAX_VALUE;
            P when P >= 0.0 andalso P =< 1.0 ->
                P * ?MAX_VALUE
        end,
    #{probability => Probability, id_upper_bound => IdUpperBound}.

%% @private
description(#{probability := Probability}) ->
    unicode:characters_to_binary(io_lib:format("TraceIdRatioBased{~.6f}", [Probability])).

%% @private
should_sample(Ctx, TraceId, _Links, _SpanName, _SpanKind, _Attributes, #{
    id_upper_bound := IdUpperBound
}) ->
    SpanCtx = otel_tracer:current_span_ctx(Ctx),
    {decide(TraceId, IdUpperBound), [], otel_span:tracestate(SpanCtx)}.

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
