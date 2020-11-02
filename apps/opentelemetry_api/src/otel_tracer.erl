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
         non_recording_span/3,
         set_current_span/1,
         set_current_span/2,
         current_span_ctx/0,
         current_span_ctx/1,
         text_map_propagators/1,
         end_span/0,
         set_attribute/2,
         set_attributes/1,
         add_event/2,
         add_events/1,
         set_status/1,
         update_name/1]).

-include("opentelemetry.hrl").

-define(CURRENT_SPAN_CTX, {?MODULE, span_ctx}).

-define(is_recording(SpanCtx), SpanCtx =/= undefined andalso SpanCtx#span_ctx.is_recording =:= true).

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

%% @doc Returns a `span_ctx' record with `is_recording' set to `false'. This is mainly
%% for use in propagators when they extract a Span to be used as a parent.
-spec non_recording_span(opentelemetry:trace_id(), opentelemetry:span_id(), opentelemetry:trace_flags())
                        -> opentelemetry:span_ctx().
non_recording_span(TraceId, SpanId, Traceflags) ->
    #span_ctx{trace_id=TraceId,
              span_id=SpanId,
              is_recording=false,
              trace_flags=Traceflags}.

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

-spec text_map_propagators(module()) -> {otel_propagator:text_map_extractor(), otel_propagator:text_map_injector()}.
text_map_propagators(Module) ->
    ToText = fun Module:inject/1,
    FromText = fun Module:extract/2,
    Injector = otel_ctx:text_map_injector(?CURRENT_SPAN_CTX, ToText),
    Extractor = otel_ctx:text_map_extractor(?CURRENT_SPAN_CTX, FromText),
    {Extractor, Injector}.

%% Span operations

-spec end_span() -> boolean() | {error, term()}.
end_span() ->
    otel_span:end_span(current_span_ctx()).

-spec set_attribute(Key, Value) -> boolean() when
      Key :: opentelemetry:attribute_key(),
      Value :: opentelemetry:attribute_value().
set_attribute(Key, Value) ->
    otel_span:set_attribute(current_span_ctx(), Key, Value).

-spec set_attributes(Attributes) -> boolean() when
      Attributes :: opentelemetry:attributes().
set_attributes(Attributes) when is_list(Attributes) ->
    otel_span:set_attributes(current_span_ctx(), Attributes);
set_attributes(_) ->
    false.

-spec add_event(Name, Attributes) -> boolean() when
      Name :: opentelemetry:event_name(),
      Attributes :: opentelemetry:attributes().
add_event(Name, Attributes) when is_list(Attributes) ->
    otel_span:add_event(current_span_ctx(), Name, Attributes);
add_event(_, _) ->
    false.

-spec add_events(Events) -> boolean() when
      Events :: opentelemetry:events().
add_events(Events) when is_list(Events)  ->
    otel_span:add_events(current_span_ctx(), Events);
add_events(_) ->
    false.

-spec set_status(Status) -> boolean() when
      Status :: opentelemetry:status().
set_status(Status) ->
    otel_span:set_status(current_span_ctx(), Status).

-spec update_name(Name) -> boolean() when
      Name :: opentelemetry:span_name().
update_name(SpanName) ->
    otel_span:update_name(current_span_ctx(), SpanName).
