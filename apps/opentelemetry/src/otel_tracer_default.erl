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

-export([start_span/3,
         start_span/4,
         start_inactive_span/3,
         start_inactive_span/4,
         set_span/2,
         with_span/3,
         with_span/4,
         end_span/1,
         end_span/2,
         current_ctx/1,
         current_span_ctx/1,
         b3_propagators/0,
         w3c_propagators/0,
         text_map_propagators/1]).

%% tracer access functions
-export([span_module/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_tracer.hrl").

%% the context namespace key
%% -define(TRACER_KEY, '$__otel_tracer_ctx_key').
%% key under the namespace for the active tracer context
-define(TRACER_CTX, {otel_tracer_default, tracer_ctx}).
%% the span context extracted with a propagator
-define(EXTERNAL_SPAN_CTX, {otel_tracer_default, external_span_ctx}).

%% @doc Creates a Span and sets it to the current active Span in the process's Tracer Context.
-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts())
                -> opentelemetry:span_ctx().
start_span(Tracer, Name, Opts) ->
    PreviousTracerCtx = otel_ctx:get_value(?TRACER_CTX),
    SpanCtx = start_inactive_span(Tracer, Name, Opts),
    otel_ctx:set_value(?TRACER_CTX, #tracer_ctx{active=SpanCtx,
                                              previous=PreviousTracerCtx}),
    SpanCtx.

%% @doc Creates a Span and sets it to the current active Span in the process's Tracer Context.
-spec start_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts())
                -> {opentelemetry:span_ctx(), otel_ctx:t()}.
start_span(Ctx, Tracer, Name, Opts) ->
    PreviousTracerCtx = otel_ctx:get_value(?TRACER_CTX, undefined),
    {SpanCtx, _} = start_inactive_span(Ctx, Tracer, Name, Opts),
    {SpanCtx, otel_ctx:set_value(Ctx, ?TRACER_CTX, #tracer_ctx{active=SpanCtx,
                                                             previous=PreviousTracerCtx})}.

%% @doc Starts an inactive Span and returns its SpanCtx.
-spec start_inactive_span(opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts())
                         -> opentelemetry:span_ctx().
start_inactive_span(Tracer={_, #tracer{on_start_processors=Processors,
                                       instrumentation_library=InstrumentationLibrary}}, Name, Opts) ->
    Ctx = otel_ctx:get_current(),
    ParentSpanCtx = maybe_parent_span_ctx(Ctx),
    Opts1 = maybe_set_sampler(Tracer, Opts),
    otel_span_ets:start_span(Ctx, Name, ParentSpanCtx, Opts1, Processors, InstrumentationLibrary).

%% @doc Starts an inactive Span and returns its SpanCtx.
-spec start_inactive_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(),
                          otel_span:start_opts()) -> {opentelemetry:span_ctx(), otel_ctx:t()}.
start_inactive_span(Ctx, Tracer={_, #tracer{on_start_processors=Processors,
                                            instrumentation_library=InstrumentationLibrary}}, Name, _Opts) ->
    ParentSpanCtx = maybe_parent_span_ctx(Ctx),
    Opts1 = maybe_set_sampler(Tracer, Ctx),
    {otel_span_ets:start_span(Ctx, Name, ParentSpanCtx, Opts1, Processors, InstrumentationLibrary), Ctx}.

maybe_set_sampler(_Tracer, Opts) when is_map_key(sampler, Opts) ->
    Opts;
maybe_set_sampler({_, #tracer{sampler=Sampler}}, Opts) ->
    Opts#{sampler => Sampler}.

%% returns the span `start_opts' map with the parent span ctx set
%% based on the current tracer ctx, the ctx passed to `start_span'
%% or `start_inactive_span' and in the case none of those are defined it
%% checks the `EXTERNAL_SPAN_CTX' which can be set by a propagator extractor.
-spec maybe_parent_span_ctx(otel_ctx:t()) -> opentelemetry:span_ctx() | undefined.
maybe_parent_span_ctx(Ctx) ->
    case otel_ctx:get_value(Ctx, ?TRACER_CTX, undefined) of
        #tracer_ctx{active=ActiveSpanCtx} when ActiveSpanCtx =/= undefined ->
            ActiveSpanCtx;
        _ ->
            otel_ctx:get_value(?EXTERNAL_SPAN_CTX)
    end.

%% @doc Takes a SpanCtx and sets it to the current active Span in the process's Tracer Context.
%% The active Tracer Context of the process is made the previous Tracer Context and is restored
%% as the process's Tracer Context if `end_span/1' is called.
%% @end
-spec set_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
set_span(_Tracer, SpanCtx) ->
    ActiveTracerCtx = otel_ctx:get_value(?TRACER_CTX),
    otel_ctx:set_value(?TRACER_CTX, #tracer_ctx{active=SpanCtx,
                                              previous=ActiveTracerCtx}).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(), otel_tracer:traced_fun()) -> ok.
with_span(_Tracer, SpanName, Fun) ->
    with_span(_Tracer, SpanName, #{}, Fun).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(),
                otel_span:start_opts(), otel_tracer:traced_fun(T)) -> T.
with_span(Tracer, SpanName, Opts, Fun) ->
    %% starting the span makes it active in the `tracer_ctx'
    SpanCtx = start_inactive_span(Tracer, SpanName, Opts),
    PreviousTracerCtx = current_ctx(Tracer),
    set_span(Tracer, SpanCtx),
    try
        Fun(SpanCtx)
    after
        %% passing TracerCtx directly ensures that this `end_span' ends the span started
        %% in this function and sets the active span to the previous active tracer ctx.
        %% If spans in `Fun()' were started and not finished properly they will be
        %% abandoned and it be up to the `otel_span_sweeper' to eventually remove them.
        _ = end_span(Tracer, SpanCtx),
        otel_ctx:set_value(?TRACER_CTX, PreviousTracerCtx)
    end.

-spec current_span_ctx(opentelemetry:tracer()) -> opentelemetry:span_ctx().
current_span_ctx(_Tracer) ->
    case otel_ctx:get_value(?TRACER_CTX) of
        #tracer_ctx{active=SpanCtx,
                    previous=_PreviousTracerCtx} ->
            SpanCtx;
        _ ->
            undefined
    end.

%% Returns the current tracer context.
%% The pdict ctx stores both the current span ctx and the
%% previous trace context, which contains its previous and so on.
-spec current_ctx(opentelemetry:tracer()) -> otel_tracer:tracer_ctx().
current_ctx(_Tracer) ->
    otel_ctx:get_value(?TRACER_CTX).

-spec current_ctx(otel_ctx:t(), opentelemetry:tracer()) -> otel_tracer:tracer_ctx().
current_ctx(Ctx, _Tracer) ->
    otel_ctx:get_value(Ctx, ?TRACER_CTX, undefined).

span_module({_, #tracer{span_module=SpanModule}}) ->
    SpanModule.

-spec b3_propagators() -> {otel_propagator:text_map_extractor(), otel_propagator:text_map_injector()}.
b3_propagators() ->
    text_map_propagators(otel_propagator_http_b3).

-spec w3c_propagators() -> {otel_propagator:text_map_extractor(), otel_propagator:text_map_injector()}.
w3c_propagators() ->
    text_map_propagators(otel_propagator_http_w3c).

-spec text_map_propagators(module()) -> {otel_propagator:text_map_extractor(), otel_propagator:text_map_injector()}.
text_map_propagators(Module) ->
    ToText = fun Module:inject/1,
    FromText = fun Module:extract/2,
    Injector = otel_ctx:text_map_injector(?TRACER_CTX, ToText),
    Extractor = otel_ctx:text_map_extractor(?EXTERNAL_SPAN_CTX, FromText),
    {Extractor, Injector}.

%% @doc Ends the span in the current pdict context. And sets the previous
%% as the current active ctx or undefined if there is no local previous.
-spec end_span(opentelemetry:tracer()) -> boolean() | {error, term()}.
end_span(Tracer) ->
    case current_ctx(Tracer) of
        #tracer_ctx{active=SpanCtx,
                    previous=PreviousTracerCtx} ->
            Result = end_span(Tracer, SpanCtx),
            otel_ctx:set_value(?TRACER_CTX, PreviousTracerCtx),
            Result;
        _ ->
            false
    end.

-spec end_span(otel_ctx:t() | opentelemetry:tracer(), opentelemetry:tracer() | opentelemetry:span_ctx())
              -> boolean() | {error, term()}.
%% @doc Ends the Span by setting the the `end_time' and calling the `OnEnd' Span Processors.
end_span({_, #tracer{on_end_processors=Processors}}, SpanCtx) ->
    otel_span_ets:end_span(SpanCtx, Processors);
%% @doc Ends the Span in the context argument.
end_span(Ctx, Tracer) ->
    case current_ctx(Ctx, Tracer) of
        #tracer_ctx{active=SpanCtx,
                    previous=PreviousTracerCtx} ->
            Result = end_span(Tracer, SpanCtx),
            otel_ctx:set_value(?TRACER_CTX, PreviousTracerCtx),
            Result;
        _ ->
            false
    end.
