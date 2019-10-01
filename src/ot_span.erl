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
         finish_span/2,
         get_ctx/2,
         is_recording_events/2,
         set_attributes/3,
         add_events/3,
         add_links/3,
         set_status/3,
         update_name/3]).

-type start_opts() :: #{parent => undefined | opentelemetry:span() | opentelemetry:span_ctx(),
                        sampler => ot_sampler:sampler(),
                        links => opentelemetry:links(),
                        is_recorded => boolean(),
                        kind => opentelemetry:span_kind()}.

-export_type([start_opts/0]).

-callback start_span(opentelemetry:span_name(), start_opts()) -> opentelemetry:span_ctx().
-callback finish_span(opentelemetry:span_ctx()) -> ok.
-callback get_ctx(opentelemetry:span()) -> opentelemetry:span_ctx().
-callback is_recording_events(opentelemetry:span_ctx()) -> boolean().
-callback set_attributes(opentelemetry:span_ctx(), opentelemetry:attributes()) -> ok.
-callback add_events(opentelemetry:span_ctx(), opentelemetry:time_events()) -> ok.
-callback set_status(opentelemetry:span_ctx(), opentelemetry:status()) -> ok.
-callback update_name(opentelemetry:span_ctx(), opentelemetry:span_name()) -> ok.

start_span(Module, Name, Opts) ->
    Module:start_span(Name, Opts).

finish_span(Module, Ctx) ->
    Module:finish_span(Ctx).

get_ctx(Module, Span) ->
    Module:get_ctx(Span).

is_recording_events(Module, SpanCtx) ->
    Module:is_recording_events(SpanCtx).

set_attributes(Module, SpanCtx, Attributes) ->
    Module:set_attributes(SpanCtx, Attributes).

add_events(Module, SpanCtx, Events) ->
    Module:add_events(SpanCtx, Events).

add_links(Module, SpanCtx, Links) ->
    Module:add_links(SpanCtx, Links).

set_status(Module, SpanCtx, Status) ->
    Module:set_status(SpanCtx, Status).

update_name(Module, SpanCtx, Name) ->
    Module:update_name(SpanCtx, Name).
