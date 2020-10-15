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

-export([start_span/3,
         start_span/4,
         with_span/3,
         with_span/4,
         end_span/2,
         span_module/1]).

-include("opentelemetry.hrl").

-define(NOOP_SPAN_CTX, #span_ctx{trace_id=0,
                                 span_id=0,
                                 trace_flags=0,
                                 tracestate=[],
                                 is_valid=false,
                                 is_recording=false}).
-define(NOOP_TRACER_CTX, []).

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts()) -> opentelemetry:span_ctx().
start_span(_, _Name, _) ->
    ?NOOP_SPAN_CTX.

-spec start_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts())
                -> {opentelemetry:span_ctx(), otel_ctx:t()}.
start_span(Ctx, _, _Name, _) ->
    {?NOOP_SPAN_CTX, Ctx}.

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(), otel_tracer:traced_fun(T)) -> T.
with_span(Tracer, SpanName, Fun) ->
    with_span(Tracer, SpanName, #{}, Fun).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(),
                otel_span:start_opts(), otel_tracer:traced_fun(T)) -> T.
with_span(_, _SpanName, _Opts, Fun) ->
    Ctx = otel_ctx:get_current(),
    SpanCtx = ?NOOP_SPAN_CTX,
    Ctx1 = otel_tracer:set_current_span(Ctx, SpanCtx),
    otel_ctx:attach(Ctx1),
    try
        Fun(SpanCtx)
    after
        otel_ctx:attach(Ctx)
    end.

span_module(_) ->
    otel_span_noop.

-spec end_span(opentelemetry:tracer(), opentelemetry:span_ctx())
              -> boolean() | {error, term()}.
end_span(_, _) ->
    true.
