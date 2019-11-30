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
         with_span/2,
         with_span/3,
         end_span/1,
         current_span_ctx/1,
         b3_propagators/0,
         w3c_propagators/0]).

%% tracer access functions
-export([span_module/1]).

-include("ot_tracer.hrl").

-define(TRACER_KEY, '$__ot_tracer_ctx_key').

-type pdict_trace_ctx() :: {opentelemetry:span_ctx(), pdict_trace_ctx() | undefined}.

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts())
                -> opentelemetry:span_ctx().
start_span(Tracer, Name, Opts) when is_map_key(sampler, Opts) ->
    start_span_(Tracer, Name, Opts);
start_span(Tracer={_, #tracer{sampler=Sampler}}, Name, Opts) ->
    start_span(Tracer, Name, Opts#{sampler => Sampler}).

start_span_({_, #tracer{on_start_processors=Processors}}, Name, Opts) when is_map_key(parent, Opts) ->
    case maps:get(parent, Opts) of
        {ParentSpanCtx, _}=Ctx ->
            SpanCtx1 = ot_span_ets:start_span(Name, Opts#{parent => ParentSpanCtx}, Processors),
            ot_ctx:set_value(?TRACER_KEY, ?SPAN_CTX, {SpanCtx1, Ctx}),
            SpanCtx1;
        ParentSpanCtx ->
            SpanCtx1 = ot_span_ets:start_span(Name, Opts, Processors),
            ot_ctx:set_value(?TRACER_KEY, ?SPAN_CTX, {SpanCtx1, {ParentSpanCtx, undefined}}),
            SpanCtx1
    end;
start_span_(Tracer, Name, Opts) ->
    case ot_ctx:get_value(?TRACER_KEY, ?SPAN_CTX) of
        {_SpanCtx, _}=ParentCtx ->
            start_span_(Tracer, Name, Opts#{parent => ParentCtx});
        _ ->
            start_span_(Tracer, Name, Opts#{parent => undefined})
    end.

-spec with_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
with_span(_Tracer, SpanCtx) ->
    ot_ctx:set_value(?TRACER_KEY, ?SPAN_CTX, {SpanCtx, undefined}).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_ctx(), fun()) -> ok.
with_span(_Tracer, SpanCtx, Fun) ->
    ot_ctx:with_value(?TRACER_KEY, ?SPAN_CTX, {SpanCtx, undefined}, Fun).

-spec current_span_ctx(opentelemetry:tracer()) -> opentelemetry:span_ctx().
current_span_ctx(_Tracer) ->
    case ot_ctx:get_value(?TRACER_KEY, ?SPAN_CTX) of
        {SpanCtx, _ParentPdictSpanCtx} ->
            SpanCtx;
        _ ->
            undefined
    end.

%% Internal function that returns the current trace context.
%% The pdict ctx stores both the current span ctx and the
%% parent trace context, which contains its parent and so on.
-spec current_ctx() -> pdict_trace_ctx().
current_ctx() ->
    ot_ctx:get_value(?TRACER_KEY, ?SPAN_CTX).

span_module({_, #tracer{span_module=SpanModule}}) ->
    SpanModule.

-spec b3_propagators() -> {ot_propagation:http_extractor(), ot_propagation:http_injector()}.
b3_propagators() ->
    ToText = fun ot_propagation_http_b3:inject/2,
    FromText = fun ot_propagation_http_b3:extract/2,
    Injector = ot_ctx:http_injector(?TRACER_KEY, ?SPAN_CTX, ToText),
    Extractor = ot_ctx:http_extractor(?TRACER_KEY, ?SPAN_CTX, FromText),
    {Extractor, Injector}.

-spec w3c_propagators() -> {ot_propagation:http_extractor(), ot_propagation:http_injector()}.
w3c_propagators() ->
    ToText = fun ot_propagation_http_w3c:inject/2,
    FromText = fun ot_propagation_http_w3c:extract/2,
    Injector = ot_ctx:http_injector(?TRACER_KEY, ?SPAN_CTX, ToText),
    Extractor = ot_ctx:http_extractor(?TRACER_KEY, ?SPAN_CTX, FromText),
    {Extractor, Injector}.

%%--------------------------------------------------------------------
%% @doc
%% Ends the span in the current pdict context. And sets the parent
%% as the current span ctx or undefined if there is no local parent.
%% @end
%%--------------------------------------------------------------------
-spec end_span(opentelemetry:tracer()) -> ok.
end_span({_, #tracer{on_end_processors=Processors}}) ->
    {SpanCtx, ParentCtx} = current_ctx(),
    ot_span_ets:end_span(SpanCtx, Processors),
    ot_ctx:set_value(?TRACER_KEY, ?SPAN_CTX, ParentCtx),
    ok.

