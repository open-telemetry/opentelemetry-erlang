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

-export([set_default_impl/1,
         start_span/1,
         start_span/2,
         finish_span/1,
         get_ctx/1,
         is_recording_events/1,
         set_attributes/2,
         add_events/2,
         add_links/2,
         set_status/2,
         update_name/2]).

-include("opentelemetry.hrl").

-type start_opts() :: #{parent => undefined | opentelemetry:span() | opentelemetry:span_ctx(),
                        sampler => module(),
                        links => opentelemetry:links(),
                        is_recorded => boolean(),
                        kind => opentelemetry:span_kind()}.

-export_type([start_opts/0]).

-callback setup() -> ok.
-callback start_span(opentelemetry:span_name(), start_opts()) -> opentelemetry:span_ctx().
-callback finish_span(opentelemetry:span_ctx()) -> ok.
-callback get_ctx(opentelemetry:span()) -> opentelemetry:span_ctx().
-callback is_recording_events(opentelemetry:span_ctx()) -> boolean().
-callback set_attributes(opentelemetry:span_ctx(), opentelemetry:attributes()) -> ok.
-callback add_events(opentelemetry:span_ctx(), opentelemetry:time_events()) -> ok.
-callback add_links(opentelemetry:span_ctx(), opentelemetry:links()) -> ok.
-callback set_status(opentelemetry:span_ctx(), opentelemetry:status()) -> ok.
-callback update_name(opentelemetry:span_ctx(), opentelemetry:span_name()) -> ok.

-define(span, (persistent_term:get({?MODULE, span}))).

-spec set_default_impl(module()) -> ok.
set_default_impl(Module) ->
    case erlang:function_exported(Module, setup, 0) of
        true ->
            ok = Module:setup();
        false ->
            ok
    end,
    persistent_term:put({?MODULE, span}, Module).

-spec start_span(opentelemetry:span_name()) -> opentelemetry:span_ctx().
start_span(SpanName) ->
    ?span:start_span(SpanName, #{}).

-spec start_span(opentelemetry:span_name(), start_opts()) -> opentelemetry:span_ctx().
start_span(SpanName, StartOpts) ->
    ?span:start_span(SpanName, StartOpts).

-spec finish_span(opentelemetry:span_ctx()) -> ok.
finish_span(SpanCtx) ->
    ?span:finish_span(SpanCtx).

-spec get_ctx(opentelemetry:span()) -> opentelemetry:span_ctx().
get_ctx(Span) ->
    ?span:is_recording_events(Span).

-spec is_recording_events(opentelemetry:span_ctx()) -> boolean().
is_recording_events(SpanCtx) ->
    ?span:is_recording_events(SpanCtx).

-spec set_attributes(opentelemetry:span_ctx(), opentelemetry:attributes()) -> ok.
set_attributes(SpanCtx, Attributes) ->
    ?span:set_attributes(SpanCtx, Attributes).

-spec add_events(opentelemetry:span_ctx(), opentelemetry:time_events()) -> ok.
add_events(SpanCtx, TimeEvents) ->
    ?span:add_events(SpanCtx, TimeEvents).

-spec add_links(opentelemetry:span_ctx(), opentelemetry:links()) -> ok.
add_links(SpanCtx, Links) ->
    ?span:add_links(SpanCtx, Links).

-spec set_status(opentelemetry:span_ctx(), opentelemetry:status()) -> ok.
set_status(SpanCtx, Status) ->
    ?span:set_status(SpanCtx, Status).

-spec update_name(opentelemetry:span_ctx(), opentelemetry:span_name()) -> ok.
update_name(SpanCtx, SpanName) ->
    ?span:update_name(SpanCtx, SpanName).
