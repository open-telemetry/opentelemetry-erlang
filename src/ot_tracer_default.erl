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
-module(ot_tracer_default).

-behaviour(ot_tracer).

-export([start_span/3,
         set_span/2,
         with_span/3,
         with_span/4,
         end_span/1,
         end_span/2,
         current_ctx/1,
         current_span_ctx/1,
         b3_propagators/0,
         w3c_propagators/0]).

%% tracer access functions
-export([span_module/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_tracer.hrl").

%% the context namespace key
-define(TRACER_KEY, '$__ot_tracer_ctx_key').
%% key under the namespace for the active tracer context
-define(SPAN_CTX, {ot_tracer_default, span_ctx}).
%% the span context extracted with a propagator
-define(EXTERNAL_SPAN_CTX, {ot_tracer_default, external_span_ctx}).

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts())
                -> opentelemetry:span_ctx().
start_span(Tracer, Name, Opts) ->
    {ParentSpanCtx, ParentTracerCtx} = parent_span_ctx(Opts),
    Opts1 = maybe_set_sampler(Tracer, Opts),
    start_span(Tracer, Name, ParentTracerCtx, Opts1#{parent => ParentSpanCtx}).

maybe_set_sampler(_Tracer, Opts) when is_map_key(sampler, Opts) ->
    Opts;
maybe_set_sampler({_, #tracer{sampler=Sampler}}, Opts) ->
    Opts#{sampler => Sampler}.

start_span({_, #tracer{on_start_processors=Processors,
                       library_resource=LibraryResource}}, Name, ParentTracerCtx, Opts) ->
    SpanCtx1 = ot_span_ets:start_span(Name, Opts, Processors, LibraryResource),
    ot_ctx:set_value(?TRACER_KEY, ?SPAN_CTX, #tracer_ctx{active=SpanCtx1,
                                                         parent=ParentTracerCtx}),
    SpanCtx1.

-spec parent_span_ctx(ot_span:start_opts()) -> {opentelemetry:span_ctx(), tracer_ctx()}.
parent_span_ctx(#{parent := ParentSpanCtx=#span_ctx{}}) ->
    {ParentSpanCtx, ot_ctx:get_value(?TRACER_KEY, ?SPAN_CTX)};
parent_span_ctx(_) ->
    case ot_ctx:get_value(?TRACER_KEY, ?SPAN_CTX) of
        TracerCtx=#tracer_ctx{active=ActiveSpanCtx} when ActiveSpanCtx =/= undefined ->
            {ActiveSpanCtx, TracerCtx};
        _ ->
            {ot_ctx:get_value(?TRACER_KEY, ?EXTERNAL_SPAN_CTX), ot_ctx:get_value(?TRACER_KEY, ?SPAN_CTX)}
    end.


-spec set_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
set_span(_Tracer, SpanCtx) ->
    ot_ctx:set_value(?TRACER_KEY, ?SPAN_CTX, #tracer_ctx{active=SpanCtx,
                                                         parent=undefined}).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_tracer:traced_fun()) -> ok.
with_span(_Tracer, SpanName, Fun) ->
    with_span(_Tracer, SpanName, #{}, Fun).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(),
                ot_span:start_opts(), ot_tracer:traced_fun(T)) -> T.
with_span(Tracer, SpanName, Opts, Fun) ->
    %% starting the span makes it active in the `tracer_ctx'
    SpanCtx = start_span(Tracer, SpanName, Opts),
    TracerCtx = current_ctx(Tracer),
    try
        Fun(SpanCtx)
    after
        %% passing TracerCtx directly ensures that this `end_span' ends the span started
        %% in this function and sets the active span to its parent. If spans in `Fun()'
        %% were started and not finished properly they will be abandoned and it be up to
        %% the `ot_span_sweeper' to eventually remove them.
        _ = end_span(Tracer, TracerCtx)
    end.

-spec current_span_ctx(opentelemetry:tracer()) -> opentelemetry:span_ctx().
current_span_ctx(_Tracer) ->
    case ot_ctx:get_value(?TRACER_KEY, ?SPAN_CTX) of
        #tracer_ctx{active=SpanCtx,
                    parent=_ParentPdictSpanCtx} ->
            SpanCtx;
        _ ->
            undefined
    end.

%% Returns the current trace context.
%% The pdict ctx stores both the current span ctx and the
%% parent trace context, which contains its parent and so on.
-spec current_ctx(opentelemetry:tracer()) -> ot_tracer:tracer_ctx().
current_ctx(_Tracer) ->
    ot_ctx:get_value(?TRACER_KEY, ?SPAN_CTX).

span_module({_, #tracer{span_module=SpanModule}}) ->
    SpanModule.

-spec b3_propagators() -> {ot_propagation:http_extractor(), ot_propagation:http_injector()}.
b3_propagators() ->
    ToText = fun ot_propagation_http_b3:inject/2,
    FromText = fun ot_propagation_http_b3:extract/2,
    Injector = ot_ctx:http_injector(?TRACER_KEY, ?SPAN_CTX, ToText),
    Extractor = ot_ctx:http_extractor(?TRACER_KEY, ?EXTERNAL_SPAN_CTX, FromText),
    {Extractor, Injector}.

-spec w3c_propagators() -> {ot_propagation:http_extractor(), ot_propagation:http_injector()}.
w3c_propagators() ->
    ToText = fun ot_propagation_http_w3c:inject/2,
    FromText = fun ot_propagation_http_w3c:extract/2,
    Injector = ot_ctx:http_injector(?TRACER_KEY, ?SPAN_CTX, ToText),
    Extractor = ot_ctx:http_extractor(?TRACER_KEY, ?EXTERNAL_SPAN_CTX, FromText),
    {Extractor, Injector}.

%%--------------------------------------------------------------------
%% @doc
%% Ends the span in the current pdict context. And sets the parent
%% as the current span ctx or undefined if there is no local parent.
%% @end
%%--------------------------------------------------------------------
-spec end_span(opentelemetry:tracer()) -> boolean() | {error, term()}.
end_span(Tracer) ->
    end_span(Tracer, current_ctx(Tracer)).

-spec end_span(opentelemetry:tracer(), tracer_ctx()) -> boolean() | {error, term()}.
end_span({_, #tracer{on_end_processors=Processors}}, #tracer_ctx{active=SpanCtx,
                                                                 parent=ParentTracerCtx}) ->
    Result = ot_span_ets:end_span(SpanCtx, Processors),
    ot_ctx:set_value(?TRACER_KEY, ?SPAN_CTX, ParentTracerCtx),
    Result.
