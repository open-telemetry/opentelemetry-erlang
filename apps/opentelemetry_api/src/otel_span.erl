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
%% Span behaviour.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_span).

-export([get_ctx/2,
         trace_id/1,
         span_id/1,
         tracestate/1,
         is_recording/2]).

-include("opentelemetry.hrl").

-define(is_recording(SpanCtx), SpanCtx =/= undefined andalso SpanCtx#span_ctx.is_recording =:= true).

-type start_opts() :: #{attributes => opentelemetry:attributes(),
                        sampler => term(),
                        links => opentelemetry:links(),
                        is_recording => boolean(),
                        start_time => opentelemetry:timestamp(),
                        kind => opentelemetry:span_kind()}.

-export_type([start_opts/0]).

-spec get_ctx(Tracer, Span) -> SpanCtx when
      Tracer :: opentelemetry:tracer(),
      Span :: opentelemetry:span(),
      SpanCtx :: opentelemetry:span_ctx().
get_ctx(Tracer, Span) ->
    SpanModule = otel_tracer:span_module(Tracer),
    SpanModule:get_ctx(Span).

-spec is_recording(Tracer, SpanCtx) -> boolean() when
      Tracer :: opentelemetry:tracer(),
      SpanCtx :: opentelemetry:span_ctx().
is_recording(otel_span_noop, _) ->
    false;
is_recording(_Tracer, SpanCtx) ->
    ?is_recording(SpanCtx).

%% accessors
-spec trace_id(opentelemetry:span_ctx()) -> opentelemetry:trace_id().
trace_id(#span_ctx{trace_id=TraceId }) ->
    TraceId.

-spec span_id(opentelemetry:span_ctx()) -> opentelemetry:span_id().
span_id(#span_ctx{span_id=SpanId }) ->
    SpanId.

-spec tracestate(opentelemetry:span_ctx()) -> opentelemetry:tracestate().
tracestate(#span_ctx{tracestate=Tracestate}) ->
    Tracestate.
