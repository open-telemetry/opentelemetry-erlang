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
-module(otel_tracer_default).

-behaviour(otel_tracer).

-export([start_span/4,
         with_span/5]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_tracer.hrl").
-include("otel_span_ets.hrl").

%% @doc Starts an inactive Span and returns its SpanCtx.
-spec start_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(),
                 otel_span:start_opts()) -> opentelemetry:span_ctx().
start_span(Ctx, {_, #tracer{on_start_processors=Processors,
                            on_end_processors=OnEndProcessors,
                            sampler=Sampler,
                            id_generator=IdGeneratorModule,
                            instrumentation_library=InstrumentationLibrary}}, Name, Opts) ->
    SpanCtx = otel_span_ets:start_span(Ctx, Name, Sampler, IdGeneratorModule, Opts, Processors, InstrumentationLibrary),
    SpanCtx#span_ctx{span_sdk={otel_span_ets, OnEndProcessors}}.

-spec with_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(),
                otel_span:start_opts(), otel_tracer:traced_fun(T)) -> T.
with_span(Ctx, Tracer, SpanName, Opts, Fun) ->
    SpanCtx = start_span(Ctx, Tracer, SpanName, Opts),
    Ctx1 = otel_tracer:set_current_span(Ctx, SpanCtx),
    otel_ctx:attach(Ctx1),
    try
        Fun(SpanCtx)
    after
        %% passing SpanCtx directly ensures that this `end_span' ends the span started
        %% in this function. If spans in `Fun()' were started and not finished properly
        %% they will be abandoned and it be up to the `otel_span_sweeper' to eventually remove them.
        _ = otel_span_ets:end_span(SpanCtx),
        otel_ctx:detach(Ctx)
    end.
