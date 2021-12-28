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
-module(otel_tracer).

-export([start_span/3,
         start_span/4,
         with_span/4,
         with_span/5,
         non_recording_span/3,
         from_remote_span/3,
         set_current_span/1,
         set_current_span/2,
         current_span_ctx/0,
         current_span_ctx/1]).

-include("opentelemetry.hrl").

-define(CURRENT_SPAN_CTX, {?MODULE, span_ctx}).

-define(is_recording(SpanCtx), SpanCtx =/= undefined andalso SpanCtx#span_ctx.is_recording =:= true).

-type traced_fun(T) :: fun((opentelemetry:span_ctx()) -> T).

-export_type([traced_fun/1]).

-callback start_span(otel_ctx:t(),
                     opentelemetry:tracer(),
                     opentelemetry:span_name(),
                     otel_span:start_opts()) -> opentelemetry:span_ctx().
-callback with_span(otel_ctx:t(), opentelemetry:tracer(),
                    opentelemetry:span_name(), otel_span:start_opts(), traced_fun(T)) -> T.

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts())
                -> opentelemetry:span_ctx().
start_span(Tracer={Module, _}, SpanName, Opts) ->
    case otel_span:is_valid_name(SpanName) of
        true ->
            Module:start_span(otel_ctx:get_current(), Tracer, SpanName, otel_span:validate_start_opts(Opts));
        false ->
            otel_tracer_noop:noop_span_ctx()
    end.

-spec start_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts())
                -> opentelemetry:span_ctx().
start_span(Ctx, Tracer={Module, _}, SpanName, Opts) ->
    case otel_span:is_valid_name(SpanName) of
        true ->
            Module:start_span(Ctx, Tracer, SpanName, otel_span:validate_start_opts(Opts));
        false ->
            otel_tracer_noop:noop_span_ctx()
    end.

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts(), traced_fun(T)) -> T.
with_span(Tracer={Module, _}, SpanName, Opts, Fun) when is_atom(Module) ->
    case otel_span:is_valid_name(SpanName) of
        true ->
            Module:with_span(otel_ctx:get_current(), Tracer, SpanName, otel_span:validate_start_opts(Opts), Fun);
        false ->
            Fun(otel_tracer_noop:noop_span_ctx())
    end.

-spec with_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts(), traced_fun(T)) -> T.
with_span(Ctx, Tracer={Module, _}, SpanName, Opts, Fun) when is_atom(Module) ->
    case otel_span:is_valid_name(SpanName) of
        true ->
            Module:with_span(Ctx, Tracer, SpanName, otel_span:validate_start_opts(Opts), Fun);
        false ->
            Fun(otel_tracer_noop:noop_span_ctx())
    end.

%% @doc Returns a `span_ctx' record with `is_recording' set to `false'. This is mainly
%% for use in propagators when they extract a Span to be used as a parent.
-spec non_recording_span(opentelemetry:trace_id(), opentelemetry:span_id(), opentelemetry:trace_flags())
                        -> opentelemetry:span_ctx().
non_recording_span(TraceId, SpanId, Traceflags) ->
    #span_ctx{trace_id=TraceId,
              span_id=SpanId,
              is_recording=false,
              trace_flags=Traceflags}.

%% @doc Returns a `span_ctx' record with `is_recording' set to `false' and `is_remote' set to `true'.
%% This is mainly for use in propagators when they extract a Span to be used as a parent.
-spec from_remote_span(opentelemetry:trace_id(), opentelemetry:span_id(), opentelemetry:trace_flags())
                      -> opentelemetry:span_ctx().
from_remote_span(TraceId, SpanId, Traceflags) ->
    #span_ctx{trace_id=TraceId,
              span_id=SpanId,
              is_valid=true,
              is_recording=false,
              is_remote=true,
              trace_flags=Traceflags}.

-spec set_current_span(opentelemetry:span_ctx() | undefined) -> opentelemetry:span_ctx() | undefined.
set_current_span(SpanCtx) ->
    _ = otel_ctx:set_value(?CURRENT_SPAN_CTX, SpanCtx),
    SpanCtx.

-spec set_current_span(otel_ctx:t(), opentelemetry:span_ctx() | undefined) -> otel_ctx:t() | undefined.
set_current_span(Ctx, SpanCtx) ->
    otel_ctx:set_value(Ctx, ?CURRENT_SPAN_CTX, SpanCtx).

-spec current_span_ctx() -> opentelemetry:span_ctx() | undefined.
current_span_ctx() ->
    otel_ctx:get_value(?CURRENT_SPAN_CTX).

-spec current_span_ctx(otel_ctx:t()) -> opentelemetry:span_ctx() | undefined.
current_span_ctx(Ctx) ->
    otel_ctx:get_value(Ctx, ?CURRENT_SPAN_CTX, undefined).
