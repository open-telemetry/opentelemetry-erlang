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

-export([start_span/3,
         with_span/2,
         with_span/3,
         current_span_ctx/1,
         end_span/1]).

%% tracer access functions
-export([span_module/1]).

-include("opentelemetry.hrl").

-callback start_span(opentelemetry:tracer(),
                     opentelemetry:span_name(),
                     ot_span:start_opts()) -> opentelemetry:span_ctx().
-callback with_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
-callback with_span(opentelemetry:tracer(), opentelemetry:span_ctx(), fun()) -> ok.
-callback end_span(opentelemetry:tracer()) -> ok.
-callback current_span_ctx(opentelemetry:tracer()) -> opentelemetry:span_ctx().
-callback span_module(opentelemetry:tracer()) -> module().

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(Tracer={Module, _}, Name, Opts) ->
    Module:start_span(Tracer, Name, Opts).

with_span(Tracer={Module, _}, Span) when is_atom(Module) ->
    Module:with_span(Tracer, Span).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_ctx(), fun()) -> ok.
with_span(Tracer={Module, _}, SpanCtx, Fun) when is_atom(Module) ->
    Module:with_value(Tracer, SpanCtx, Fun).

-spec end_span(opentelemetry:tracer()) -> ok.
end_span(Tracer={Module, _}) ->
    Module:end_span(Tracer).

current_span_ctx(Tracer={Module, _}) ->
    Module:current_span_ctx(Tracer).

%% tracer access functions

span_module(Tracer={Module, _}) ->
    Module:span_module(Tracer).
