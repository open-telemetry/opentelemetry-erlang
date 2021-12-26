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
%%%-------------------------------------------------------------------------
-module(otel_tracer_noop).

-behaviour(otel_tracer).

-export([start_span/4,
         with_span/5,
         end_span/2,
         noop_span_ctx/0]).

-include("opentelemetry.hrl").
-include("otel_tracer.hrl").

-define(NOOP_SPAN_CTX, #span_ctx{trace_id=0,
                                 span_id=0,
                                 trace_flags=0,
                                 tracestate=[],
                                 is_valid=false,
                                 is_recording=false,
                                 span_sdk=undefined}).
-define(NOOP_TRACER_CTX, []).

-spec start_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(),
                 otel_span:start_opts()) -> opentelemetry:span_ctx().
start_span(Ctx, _, _SpanName, _) ->
    %% Spec: Behavior of the API in the absence of an installed SDK
    case otel_tracer:current_span_ctx(Ctx) of
        Parent=#span_ctx{trace_id=TraceId} when TraceId =/= 0 ->
            Parent;
        _ ->
            %% If the parent Context contains no valid Span,
            %% an empty non-recording Span MUST be returned
            ?NOOP_SPAN_CTX
    end.

-spec with_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(),
                otel_span:start_opts(), otel_tracer:traced_fun(T)) -> T.
with_span(Ctx, Tracer, SpanName, Opts, Fun) ->
    SpanCtx = start_span(Ctx, Tracer, SpanName, Opts),
    Ctx1 = otel_tracer:set_current_span(Ctx, SpanCtx),
    otel_ctx:attach(Ctx1),
    try
        Fun(SpanCtx)
    after
        otel_ctx:attach(Ctx)
    end.

-spec end_span(opentelemetry:tracer(), opentelemetry:span_ctx())
              -> boolean() | {error, term()}.
end_span(_, _) ->
    true.

-spec noop_span_ctx() -> opentelemetry:span_ctx().
noop_span_ctx() ->
    ?NOOP_SPAN_CTX.
