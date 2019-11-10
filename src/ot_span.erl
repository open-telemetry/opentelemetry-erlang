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
         add_events/3,
         add_links/3,
         set_status/3,
         update_name/3]).

-include("opentelemetry.hrl").

-type start_opts() :: #{parent => undefined | opentelemetry:span() | opentelemetry:span_ctx(),
                        sampler => ot_sampler:sampler(),
                        links => opentelemetry:links(),
                        is_recorded => boolean(),
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
-callback add_events(opentelemetry:span_ctx(), opentelemetry:time_events()) -> boolean().
-callback set_status(opentelemetry:span_ctx(), opentelemetry:status()) -> boolean().
-callback update_name(opentelemetry:span_ctx(), opentelemetry:span_name()) -> boolean().

%% handy macros so we don't have function name typos
-define(DO(TracerOrSpanCtx, Args), do_span_function(?FUNCTION_NAME, TracerOrSpanCtx, Args)).
-define(DO(Tracer, SpanCtx, Args), do_span_function(?FUNCTION_NAME, Tracer, SpanCtx, Args)).

start_span(Tracer, Name, Opts) ->
    SpanModule = ot_tracer:span_module(Tracer),
    SpanModule:start_span(Name, Opts).

end_span(Tracer, SpanCtx) ->
    SpanModule = ot_tracer:span_module(Tracer),
    SpanModule:end_span(SpanCtx).

get_ctx(Tracer, Span) ->
    ?DO(Tracer, [Span]).

is_recording_events(Tracer, SpanCtx) ->
    ?DO(Tracer, SpanCtx, []).

set_attribute(Tracer, SpanCtx, Key, Value) ->
    ?DO(Tracer, SpanCtx, [Key, Value]).

set_attributes(Tracer, SpanCtx, Attributes) ->
    ?DO(Tracer, SpanCtx, [Attributes]).

add_events(Tracer, SpanCtx, Events) ->
    ?DO(Tracer, SpanCtx, [Events]).

add_links(Tracer, SpanCtx, Links) ->
    ?DO(Tracer, SpanCtx, [Links]).

set_status(Tracer, SpanCtx, Status) ->
    ?DO(Tracer, SpanCtx, [Status]).

update_name(Tracer, SpanCtx, Name) ->
    ?DO(Tracer, SpanCtx, [Name]).

%% internal functions

do_span_function(Function, Tracer, Args) ->
    SpanCtx = ot_tracer:current_span_ctx(Tracer),
    SpanModule = ot_tracer:span_module(Tracer),
    apply_span_function(SpanModule, Function, [SpanCtx | Args]).

do_span_function(Function, Tracer, SpanCtx, Args) ->
    SpanModule = ot_tracer:span_module(Tracer),
    apply_span_function(SpanModule, Function, [SpanCtx | Args]).

apply_span_function(ot_span_noop, _Function, _Args) ->
    ok;
apply_span_function(SpanModule, Function, Args) ->
    erlang:apply(SpanModule, Function, Args).
