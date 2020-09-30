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
         with_span/3,
         with_span/4,
         set_current_span/1,
         set_current_span/2,
         current_span_ctx/0,
         current_span_ctx/1,
         current_external_span_ctx/0,
         text_map_propagators/1,
         end_span/1,
         end_span/2]).

%% tracer access functions
-export([span_module/1]).

-include("opentelemetry.hrl").

-define(CURRENT_SPAN_CTX, {?MODULE, span_ctx}).
%% the span context extracted with a propagator
-define(EXTERNAL_SPAN_CTX, {?MODULE, external_span_ctx}).

-type traced_fun(T) :: fun((opentelemetry:span_ctx()) -> T).
-type tracer_ctx() :: term().

-export_type([traced_fun/1, tracer_ctx/0]).

-callback start_span(opentelemetry:tracer(),
                     opentelemetry:span_name(),
                     otel_span:start_opts()) -> opentelemetry:span_ctx().
-callback start_span(otel_ctx:t(),
                     opentelemetry:tracer(),
                     opentelemetry:span_name(),
                     otel_span:start_opts()) -> {opentelemetry:span_ctx(), otel_ctx:t()}.
-callback with_span(opentelemetry:tracer(), opentelemetry:span_name(), traced_fun(T)) -> T.
-callback with_span(opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts(), traced_fun(T)) -> T.
-callback end_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> boolean() | {error, term()}.
-callback span_module(opentelemetry:tracer()) -> module().

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts())
                -> opentelemetry:span_ctx().
start_span(Tracer={Module, _}, Name, Opts) ->
    Module:start_span(Tracer, Name, Opts).

-spec start_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts())
                -> {opentelemetry:span_ctx(), otel_ctx:t()}.
start_span(Ctx, Tracer={Module, _}, Name, Opts) ->
    Module:start_span(Ctx, Tracer, Name, Opts).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(), traced_fun(T)) -> T.
with_span(Tracer={Module, _}, SpanName, Fun) when is_atom(Module) ->
    Module:with_span(Tracer, SpanName, Fun).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(), otel_span:start_opts(), traced_fun(T)) -> T.
with_span(Tracer={Module, _}, SpanName, Opts, Fun) when is_atom(Module) ->
    Module:with_span(Tracer, SpanName, Opts, Fun).

-spec end_span(opentelemetry:tracer()) -> boolean() | {error, term()}.
end_span(Tracer={Module, _}) ->
    Module:end_span(Tracer).

-spec end_span(opentelemetry:tracer(), opentelemetry:span_ctx())
              -> boolean() | {error, term()}.
end_span(Tracer={Module, _}, SpanCtx) ->
    Module:end_span(Tracer, SpanCtx).

-spec set_current_span(opentelemetry:span_ctx()) -> ok.
set_current_span(SpanCtx) ->
    otel_ctx:set_value(?CURRENT_SPAN_CTX, SpanCtx).

-spec set_current_span(otel_ctx:t(), opentelemetry:span_ctx()) -> otel_ctx:t() | undefined.
set_current_span(Ctx, SpanCtx) ->
    otel_ctx:set_value(Ctx, ?CURRENT_SPAN_CTX, SpanCtx).

-spec current_span_ctx() -> opentelemetry:span_ctx() | undefined.
current_span_ctx() ->
    otel_ctx:get_value(?CURRENT_SPAN_CTX).

-spec current_span_ctx(otel_ctx:t()) -> opentelemetry:span_ctx() | undefined.
current_span_ctx(Ctx) ->
    otel_ctx:get_value(Ctx, ?CURRENT_SPAN_CTX, undefined).

current_external_span_ctx() ->
    otel_ctx:get_value(?EXTERNAL_SPAN_CTX).

-spec text_map_propagators(module()) -> {otel_propagator:text_map_extractor(), otel_propagator:text_map_injector()}.
text_map_propagators(Module) ->
    ToText = fun Module:inject/1,
    FromText = fun Module:extract/2,
    Injector = otel_ctx:text_map_injector(?CURRENT_SPAN_CTX, ToText),
    Extractor = otel_ctx:text_map_extractor(?EXTERNAL_SPAN_CTX, FromText),
    {Extractor, Injector}.

%% tracer access functions

span_module(Tracer={Module, _}) ->
    Module:span_module(Tracer).
