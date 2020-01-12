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
         end_span/2,
         current_span_ctx/1,
         b3_propagators/0,
         w3c_propagators/0]).

%% tracer access functions
-export([span_module/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_tracer.hrl").

-define(TRACER_KEY, '$__ot_tracer_ctx_key').

-type pdict_trace_ctx() :: {opentelemetry:span_ctx(), pdict_trace_ctx() | undefined}.

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts())
                -> opentelemetry:span_ctx().
start_span(Tracer, Name, Opts) when is_map_key(sampler, Opts) ->
    start_span_(Tracer, Name, Opts);
start_span(Tracer={_, #tracer{sampler=Sampler}}, Name, Opts) ->
    start_span(Tracer, Name, Opts#{sampler => Sampler}).

start_span_({_, #tracer{on_start_processors=Processors}}, Name, #{parent := _}=Opts) ->
    ot_span_ets:start_span(Name, Opts, Processors);
start_span_(Tracer, Name, Opts) ->
    ParentCtx = current_span_ctx(Tracer),
    start_span_(Tracer, Name, Opts#{parent => ParentCtx}).

-spec set_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
set_span(_Tracer, SpanCtx) ->
    ot_ctx:set_value(?TRACER_KEY, ?SPAN_CTX, SpanCtx),
    ok.

-spec current_span_ctx(opentelemetry:tracer()) -> opentelemetry:span_ctx().
current_span_ctx(_Tracer) ->
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
-spec end_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
end_span({_, #tracer{on_end_processors=Processors}}, SpanCtx) ->
    ot_span_ets:end_span(SpanCtx, Processors),
    ok.
