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
-module(always_off).

-behavior(otel_sampler).

-export([description/1, setup/1, should_sample/7]).

-export_type([opts/0]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_sampler.hrl").

-type opts() :: #{attributes => opentelemetry:attributes()}.

setup(#{attributes := Attributes}) -> Attributes;
setup(_) -> [].

description(_) -> <<"AlwaysOffSampler">>.

should_sample(Ctx, _TraceId, _Links, _SpanName, _SpanKind, _Attributes, DecisionAttributes) ->
    {?DROP, DecisionAttributes, tracestate(Ctx)}.

tracestate(Ctx) ->
    tracestate_(otel_tracer:current_span_ctx(Ctx)).

tracestate_(#span_ctx{tracestate = undefined}) ->
    [];
tracestate_(#span_ctx{tracestate = TraceState}) ->
    TraceState;
tracestate_(undefined) ->
    [].