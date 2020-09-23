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
         start_span/4,
         start_inactive_span/3,
         start_inactive_span/4,
         set_span/2,
         with_span/3,
         with_span/4,
         current_ctx/1,
         current_span_ctx/1,
         end_span/1,
         end_span/2]).

%% tracer access functions
-export([span_module/1]).

-include("opentelemetry.hrl").

-type traced_fun(T) :: fun((opentelemetry:span_ctx()) -> T).
-type tracer_ctx() :: term().

-export_type([traced_fun/1]).

-callback start_span(opentelemetry:tracer(),
                     opentelemetry:span_name(),
                     ot_span:start_opts()) -> opentelemetry:span_ctx().
-callback start_span(ot_ctx:ctx(),
                     opentelemetry:tracer(),
                     opentelemetry:span_name(),
                     ot_span:start_opts()) -> {opentelemetry:span_ctx(), ot_ctx:ctx()}.
-callback start_inactive_span(opentelemetry:tracer(),
                              opentelemetry:span_name(),
                              ot_span:start_opts()) -> opentelemetry:span_ctx().
-callback start_inactive_span(ot_ctx:ctx(),
                              opentelemetry:tracer(),
                              opentelemetry:span_name(),
                              ot_span:start_opts()) -> {opentelemetry:span_ctx(), ot_ctx:ctx()}.
-callback set_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
-callback with_span(opentelemetry:tracer(), opentelemetry:span_name(), traced_fun(T)) -> T.
-callback with_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts(), traced_fun(T)) -> T.
-callback end_span(ot_ctx:ctx() | opentelemetry:tracer(), opentelemetry:tracer() | opentelemetry:span_ctx()) -> boolean() | {error, term()}.
-callback end_span(opentelemetry:tracer()) -> boolean() | {error, term()}.
-callback current_ctx(opentelemetry:tracer()) -> tracer_ctx().
-callback current_span_ctx(opentelemetry:tracer()) -> opentelemetry:span_ctx().
-callback span_module(opentelemetry:tracer()) -> module().

-spec start_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts())
                -> opentelemetry:span_ctx().
start_span(Tracer={Module, _}, Name, Opts) ->
    Module:start_span(Tracer, Name, Opts).

-spec start_span(ot_ctx:ctx(), opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts())
                -> {opentelemetry:span_ctx(), ot_ctx:ctx()}.
start_span(Ctx, Tracer={Module, _}, Name, Opts) ->
    Module:start_span(Ctx, Tracer, Name, Opts).

-spec start_inactive_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts())
                -> opentelemetry:span_ctx().
start_inactive_span(Tracer={Module, _}, Name, Opts) ->
    Module:start_inactive_span(Tracer, Name, Opts).

-spec start_inactive_span(ot_ctx:ctx(), opentelemetry:tracer(), opentelemetry:span_name(),
                          ot_span:start_opts()) -> {opentelemetry:span_ctx(), ot_ctx:ctx()}.
start_inactive_span(Ctx, Tracer={Module, _}, Name, Opts) ->
    Module:start_inactive_span(Ctx, Tracer, Name, Opts).

-spec set_span(opentelemetry:tracer(), opentelemetry:span_ctx()) -> ok.
set_span(Tracer={Module, _}, SpanCtx) when is_atom(Module) ->
    Module:set_span(Tracer, SpanCtx).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(), traced_fun(T)) -> T.
with_span(Tracer={Module, _}, SpanName, Fun) when is_atom(Module) ->
    Module:with_span(Tracer, SpanName, Fun).

-spec with_span(opentelemetry:tracer(), opentelemetry:span_name(), ot_span:start_opts(), traced_fun(T)) -> T.
with_span(Tracer={Module, _}, SpanName, Opts, Fun) when is_atom(Module) ->
    Module:with_span(Tracer, SpanName, Opts, Fun).

-spec end_span(opentelemetry:tracer()) -> boolean() | {error, term()}.
end_span(Tracer={Module, _}) ->
    Module:end_span(Tracer).

-spec end_span(ot_ctx:ctx() | opentelemetry:tracer(), opentelemetry:tracer() | opentelemetry:span_ctx())
              -> boolean() | {error, term()}.
end_span(Ctx, Tracer={Module, _}) ->
    Module:end_span(Ctx, Tracer);
end_span(Tracer={Module, _}, SpanCtx) ->
    Module:end_span(Tracer, SpanCtx).

-spec current_ctx(opentelemetry:tracer()) -> ot_tracer:tracer_ctx().
current_ctx(Tracer={Module, _}) ->
    Module:current_ctx(Tracer).

-spec current_span_ctx(opentelemetry:tracer()) -> opentelemetry:span_ctx().
current_span_ctx(Tracer={Module, _}) ->
    Module:current_span_ctx(Tracer).

%% tracer access functions

span_module(Tracer={Module, _}) ->
    Module:span_module(Tracer).
