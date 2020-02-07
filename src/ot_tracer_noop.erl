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
-module(ot_tracer_noop).

-behaviour(ot_tracer).

-export([start_span/3,
         set_span/2,
         with_span/3,
         with_span/4,
         end_span/1,
         end_span/2,
         span_module/1,
         current_ctx/1,
         current_span_ctx/1]).

-include("opentelemetry.hrl").

-define(NOOP_SPAN_CTX, #span_ctx{trace_id=0,
                                 span_id=0,
                                 trace_flags=0,
                                 tracestate=[],
                                 is_valid=false,
                                 is_recorded=false}).
-define(NOOP_TRACER_CTX, []).

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(_, _Name, _) ->
    ?NOOP_SPAN_CTX.

-spec set_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
set_span(_, _SpanCtx) ->
    ok.

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_tracer:traced_fun(T)) -> T.
with_span(_, _SpanName, Fun) ->
    Fun(?NOOP_SPAN_CTX).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(),
                ot_span:start_opts(), ot_tracer:traced_fun(T)) -> T.
with_span(_, _SpanName, _Opts, Fun) ->
    Fun(?NOOP_SPAN_CTX).

-spec current_ctx(opentelemetry:tracer()) -> ot_tracer:tracer_ctx().
current_ctx(_Tracer) ->
    ?NOOP_TRACER_CTX.

-spec current_span_ctx(opentelemetry:tracer()) -> opentelemetry:span_ctx().
current_span_ctx(_) ->
    ?NOOP_SPAN_CTX.

span_module(_) ->
    ot_span_noop.

-spec end_span(opentelemetry:tracer()) -> boolean() | {error, term()}.
end_span(_) ->
    true.

-spec end_span(opentelemetry:tracer(), ot_tracer:tracer_ctx()) -> boolean() | {error, term()}.
end_span(_, _) ->
    true.
