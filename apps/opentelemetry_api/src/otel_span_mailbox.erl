%%%------------------------------------------------------------------------
%% Copyright 2024, OpenTelemetry Authors
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
%% This module implements the `otel_span` API, but sends the data to the current
%% process's mailbox rather than storing it anywhere, so that it can be inspected
%% in tests.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_span_mailbox).

-export([
         end_span/1,
         end_span/2,
         set_attribute/3,
         set_attributes/2,
         add_event/3,
         add_events/2,
         set_status/2,
         update_name/2]).

-include("opentelemetry.hrl").

-spec end_span(opentelemetry:span_ctx() | undefined) -> boolean().
end_span(SpanCtx) ->
    self() ! {?FUNCTION_NAME, SpanCtx},
    true.

-spec end_span(opentelemetry:span_ctx() | undefined,
               integer() | undefined) -> boolean().
end_span(SpanCtx, Timestamp) ->
    self() ! {?FUNCTION_NAME, SpanCtx, Timestamp},
    true.

-spec set_attribute(opentelemetry:span_ctx() | undefined,
                    opentelemetry:attribute_key(),
                    opentelemetry:attribute_value()) -> boolean().
set_attribute(SpanCtx, Key, Value) ->
    self() ! {?FUNCTION_NAME, SpanCtx, Key, Value},
    true.

-spec set_attributes(opentelemetry:span_ctx() | undefined, opentelemetry:attributes_map()) -> boolean().
set_attributes(SpanCtx, NewAttributes) ->
    self() ! {?FUNCTION_NAME, SpanCtx, NewAttributes},
    true.

-spec add_event(opentelemetry:span_ctx() | undefined, unicode:unicode_binary(), opentelemetry:attributes_map()) -> boolean().
add_event(SpanCtx, Name, Attributes) ->
    self() ! {?FUNCTION_NAME, SpanCtx, Name, Attributes},
    true.

-spec add_events(opentelemetry:span_ctx() | undefined, [opentelemetry:event()]) -> boolean().
add_events(SpanCtx, NewEvents) ->
    self() ! {?FUNCTION_NAME, SpanCtx, NewEvents},
    true.

-spec set_status(opentelemetry:span_ctx() | undefined, opentelemetry:status()) -> boolean().
set_status(SpanCtx, Status) ->
    self() ! {?FUNCTION_NAME, SpanCtx, Status},
    true.

-spec update_name(opentelemetry:span_ctx() | undefined, opentelemetry:span_name()) -> boolean().
update_name(SpanCtx, Name) ->
    self() ! {?FUNCTION_NAME, SpanCtx, Name},
    true.
