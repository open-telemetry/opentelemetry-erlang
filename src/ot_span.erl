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
%% Span behaviour.
%% @end
%%%-------------------------------------------------------------------------
-module(ot_span).

-export([start_span/3,
         end_span/2,
         get_ctx/2,
         is_recording_events/2,
         set_attribute/4,
         set_attributes/3,
         add_event/4,
         add_events/3,
         set_status/3,
         update_name/3]).

-include("opentelemetry.hrl").

-type start_opts() :: #{parent => undefined | opentelemetry:span() | opentelemetry:span_ctx(),
                        attributes => opentelemetry:attributes(),
                        sampler => ot_sampler:sampler(),
                        sampling_hint => ot_sampler:sampling_decision(),
                        links => opentelemetry:links(),
                        is_recorded => boolean(),
                        start_time => wts:timestamp(),
                        kind => opentelemetry:span_kind()}.

-export_type([start_opts/0]).

-callback start_span(opentelemetry:span_name(), start_opts()) -> opentelemetry:span_ctx().
-callback end_span(opentelemetry:span_ctx()) -> boolean() | {error, term()}.
-callback get_ctx(opentelemetry:span()) -> opentelemetry:span_ctx().
-callback is_recording_events(opentelemetry:span_ctx()) -> boolean().
-callback set_attribute(opentelemetry:span_ctx(),
                        opentelemetry:attribute_key(),
                        opentelemetry:attribute_value()) -> boolean().
-callback set_attributes(opentelemetry:span_ctx(), opentelemetry:attributes()) -> boolean().
-callback add_event(opentelemetry:span_ctx(), unicode:unicode_binary(), opentelemetry:attributes()) -> boolean().
-callback add_events(opentelemetry:span_ctx(), opentelemetry:time_events()) -> boolean().
-callback set_status(opentelemetry:span_ctx(), opentelemetry:status()) -> boolean().
-callback update_name(opentelemetry:span_ctx(), opentelemetry:span_name()) -> boolean().

%% handy macros so we don't have function name typos
-define(DO(Tracer, SpanCtx, Args), do_span_function(?FUNCTION_NAME, Tracer, SpanCtx, Args)).

-spec start_span(Tracer, SpanName, Opts) -> SpanCtx when
      Tracer :: opentelemetry:tracer(),
      SpanName :: opentelemetry:span_name(),
      Opts :: start_opts(),
      SpanCtx :: opentelemetry:span_ctx().
start_span(Tracer, SpanName, Opts) ->
    SpanModule = ot_tracer:span_module(Tracer),
    SpanModule:start_span(SpanName, Opts).

-spec end_span(Tracer, SpanCtx) -> boolean() | {error, term()} when
      Tracer :: opentelemetry:tracer(),
      SpanCtx :: opentelemetry:span_ctx().
end_span(Tracer, SpanCtx) ->
    SpanModule = ot_tracer:span_module(Tracer),
    SpanModule:end_span(SpanCtx).

-spec get_ctx(Tracer, Span) -> SpanCtx when
      Tracer :: opentelemetry:tracer(),
      Span :: opentelemetry:span(),
      SpanCtx :: opentelemetry:span_ctx().
get_ctx(Tracer, Span) ->
    SpanModule = ot_tracer:span_module(Tracer),
    SpanModule:get_ctx(Span).

-spec is_recording_events(Tracer, SpanCtx) -> boolean() when
      Tracer :: opentelemetry:tracer(),
      SpanCtx :: opentelemetry:span_ctx().
is_recording_events(Tracer, SpanCtx) ->
    ?DO(Tracer, SpanCtx, []).

-spec set_attribute(Tracer, SpanCtx, Key, Value) -> boolean() when
      Tracer :: opentelemetry:tracer(),
      Key :: opentelemetry:attribute_key(),
      Value :: opentelemetry:attribute_value(),
      SpanCtx :: opentelemetry:span_ctx().
set_attribute(Tracer, SpanCtx, Key, Value) ->
    ?DO(Tracer, SpanCtx, [Key, Value]).

-spec set_attributes(Tracer, SpanCtx, Attributes) -> boolean() when
      Tracer :: opentelemetry:tracer(),
      Attributes :: opentelemetry:attributes(),
      SpanCtx :: opentelemetry:span_ctx().
set_attributes(Tracer, SpanCtx, Attributes) ->
    ?DO(Tracer, SpanCtx, [Attributes]).

-spec add_event(Tracer, SpanCtx, Name, Attributes) -> boolean() when
      Tracer :: opentelemetry:tracer(),
      Name :: unicode:unicode_binary(),
      Attributes :: opentelemetry:attributes(),
      SpanCtx :: opentelemetry:span_ctx().
add_event(Tracer, SpanCtx, Name, Attributes) ->
    ?DO(Tracer, SpanCtx, [Name, Attributes]).

-spec add_events(Tracer, SpanCtx, Events) -> boolean() when
      Tracer :: opentelemetry:tracer(),
      Events :: opentelemetry:events(),
      SpanCtx :: opentelemetry:span_ctx().
add_events(Tracer, SpanCtx, Events) ->
    ?DO(Tracer, SpanCtx, [Events]).

-spec set_status(Tracer, SpanCtx, Status) -> boolean() when
      Tracer :: opentelemetry:tracer(),
      Status :: opentelemetry:status(),
      SpanCtx :: opentelemetry:span_ctx().
set_status(Tracer, SpanCtx, Status) ->
    ?DO(Tracer, SpanCtx, [Status]).

-spec update_name(Tracer, SpanCtx, Name) -> boolean() when
      Tracer :: opentelemetry:tracer(),
      Name :: opentelemetry:span_name(),
      SpanCtx :: opentelemetry:span_ctx().
update_name(Tracer, SpanCtx, SpanName) ->
    ?DO(Tracer, SpanCtx, [SpanName]).

%% internal functions

do_span_function(Function, Tracer, SpanCtx, Args) ->
    SpanModule = ot_tracer:span_module(Tracer),
    apply_span_function(SpanModule, Function, [SpanCtx | Args]).

apply_span_function(ot_span_noop, _Function, _Args) ->
    ok;
apply_span_function(SpanModule, Function, Args) ->
    erlang:apply(SpanModule, Function, Args).
