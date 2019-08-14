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
-module(ot_tracer).

-export([start_span/1,
         start_span/2,
         start_span/3,
         with_span/1,
         with_span/2,
         current_span_ctx/0,
         finish/0,
         get_binary_format/0,
         get_http_text_format/0]).

-include("opentelemetry.hrl").

-define(tracer, (persistent_term:get({opentelemetry, tracer}, ot_tracer_sdk))).
-define(CURRENT_TRACER, {?MODULE, current_tracer}).

-callback start_span(opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
-callback with_span(opentelemetry:span_ctx()) -> ok.
-callback finish() -> ok.
-callback current_span_ctx() -> opentelemetry:span_ctx().
%% -callback get_current_span() -> opentelemetry:span().
-callback get_binary_format() -> binary().
-callback get_http_text_format() -> opentelemetry:http_headers().

-spec start_span(opentelemetry:span_name()) -> opentelemetry:span_ctx().
start_span(Name) ->
    start_span(Name, #{}).

-spec start_span(opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(Name, Opts) ->
    start_span(?tracer, Name, Opts).

-spec start_span(module(), opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(Tracer, Name, Opts) ->
    ot_ctx_pdict:with_value(?CURRENT_TRACER, Tracer),
    Tracer:start_span(Name, Opts).

-spec with_span(opentelemetry:span_ctx()) -> ok.
with_span(Span) ->
    with_span(?tracer, Span).

-spec with_span(module() | opentelemetry:span_ctx(), opentelemetry:span_ctx() | fun()) -> ok.
with_span(Span=#span_ctx{}, Fun) ->
    Tracer = ot_ctx_pdict:get(?CURRENT_TRACER, ?tracer),
    with_span(Tracer, Span, Fun);
with_span(Tracer, Span) ->
    ot_ctx_pdict:with_value(?CURRENT_TRACER, Tracer),
    Tracer:with_span(Span).

-spec with_span(module(), opentelemetry:span_ctx(), fun()) -> ok.
with_span(Tracer, SpanCtx, Fun) ->
    ot_ctx_pdict:with_value(?CURRENT_TRACER, Tracer, fun() -> Tracer:with_value(SpanCtx, Fun) end).

-spec finish() -> ok.
finish() ->
    Tracer = ot_ctx_pdict:get(?CURRENT_TRACER, ?tracer),
    Tracer:finish().

-spec current_span_ctx() -> opentelemetry:span_ctx().
current_span_ctx() ->
    Tracer = ot_ctx_pdict:get(?CURRENT_TRACER, ?tracer),
    Tracer:current_span_ctx().

-spec get_binary_format() -> binary().
get_binary_format() ->
    Tracer = ot_ctx_pdict:get(?CURRENT_TRACER, ?tracer),
    Tracer:get_binary_format().

-spec get_http_text_format() -> opentelemetry:http_headers().
get_http_text_format() ->
    Tracer = ot_ctx_pdict:get(?CURRENT_TRACER, ?tracer),
    Tracer:get_http_text_format().
