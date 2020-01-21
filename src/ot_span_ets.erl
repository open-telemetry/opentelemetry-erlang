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
%% ETS backed interface for working with spans.
%% @end
%%%-------------------------------------------------------------------------
-module(ot_span_ets).

-behaviour(ot_span).
-behaviour(gen_server).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2]).

-export([start_span/3,
         start_span/4,
         end_span/1,
         end_span/2,
         get_ctx/1,
         is_recording_events/1,
         set_attribute/3,
         set_attributes/2,
         add_event/3,
         add_events/2,
         set_status/2,
         update_name/2]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_tracer.hrl").
-include("ot_span.hrl").
-include("ot_span_ets.hrl").

-record(state, {}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

start_span(Name, Opts, LibraryResource) ->
    start_span(Name, Opts, fun(Span) -> Span end, LibraryResource).

%% @doc Start a span and insert into the active span ets table.
-spec start_span(opentelemetry:span_name(), ot_span:start_opts(), fun(), ot_tracer_server:library_resource())
                -> opentelemetry:span_ctx().
start_span(Name, Opts, Processors, LibraryResource) ->
    {SpanCtx, Span} = ot_span_utils:start_span(Name, Opts, LibraryResource),
    Span1 = Processors(Span),
    _ = storage_insert(Span1),
    SpanCtx.

end_span(SpanCtx) ->
    end_span(SpanCtx, fun(Span) -> Span end).

%% @doc End a span based on its context and send to reporter.
-spec end_span(opentelemetry:span_ctx(), fun()) -> boolean() | {error, term()}.
end_span(#span_ctx{span_id=SpanId,
                   tracestate=Tracestate,
                   trace_flags=TraceOptions}, Processors) when ?IS_SPAN_ENABLED(TraceOptions) ->
    case ets:take(?SPAN_TAB, SpanId) of
        [Span] ->
            Span1 = ot_span_utils:end_span(Span#span{tracestate=Tracestate}),
            Processors(Span1);
        _ ->
            false
    end;
end_span(_, _) ->
    ok.

-spec get_ctx(opentelemetry:span()) -> opentelemetry:span_ctx().
get_ctx(#span{trace_id=TraceId,
              span_id=SpanId,
              tracestate=TraceState,
              is_recorded=IsRecorded}) ->
    #span_ctx{trace_id=TraceId,
              span_id=SpanId,
              tracestate=TraceState,
              is_recorded=IsRecorded}.

-spec is_recording_events(opentelemetry:span_ctx()) -> boolean().
is_recording_events(#span_ctx{is_recorded=IsRecorded}) ->
    IsRecorded.

-spec set_attribute(opentelemetry:span_ctx(),
                    opentelemetry:attribute_key(),
                    opentelemetry:attribute_value()) -> boolean().
set_attribute(#span_ctx{span_id=SpanId}, Key, Value) ->
    set_attributes(#span_ctx{span_id=SpanId}, [{Key, Value}]).

-spec set_attributes(opentelemetry:span_ctx(), opentelemetry:attributes()) -> boolean().
set_attributes(#span_ctx{span_id=SpanId}, NewAttributes) ->
    case ets:lookup(?SPAN_TAB, SpanId) of
        [Span=#span{attributes=Attributes}] ->
            Span1 = Span#span{attributes=Attributes++NewAttributes},
            1 =:= ets:select_replace(?SPAN_TAB, [{Span, [], [{const, Span1}]}]);
        _ ->
            false
    end.

-spec add_event(opentelemetry:span_ctx(), unicode:unicode_binary(), opentelemetry:attributes()) -> boolean().
add_event(SpanCtx, Name, Attributes) ->
    TimedEvents = opentelemetry:timed_events([{opentelemetry:timestamp(),
                                               Name, Attributes}]),
    add_events(SpanCtx, TimedEvents).

-spec add_events(opentelemetry:span_ctx(), opentelemetry:timed_events()) -> boolean().
add_events(#span_ctx{span_id=SpanId}, NewTimedEvents) ->
    case ets:lookup(?SPAN_TAB, SpanId) of
        [Span=#span{timed_events=TimeEvents}] ->
            Span1 = Span#span{timed_events=TimeEvents++NewTimedEvents},
            1 =:= ets:select_replace(?SPAN_TAB, [{Span, [], [{const, Span1}]}]);
        _ ->
            false
    end.

-spec set_status(opentelemetry:span_ctx(), opentelemetry:status()) -> boolean().
set_status(#span_ctx{span_id=SpanId}, Status) ->
    ets:update_element(?SPAN_TAB, SpanId, {#span.status, Status}).

-spec update_name(opentelemetry:span_ctx(), opentelemetry:span_name()) -> boolean().
update_name(#span_ctx{span_id=SpanId}, Name) ->
    ets:update_element(?SPAN_TAB, SpanId, {#span.name, Name}).

%%

storage_insert(Span) ->
    ets:insert(?SPAN_TAB, Span).

init(_Opts) ->
    %% ets table is required for other parts to not crash so we create
    %% it in init and not in a handle_continue or whatever else
    case ets:info(?SPAN_TAB, name) of
        undefined ->
            ets:new(?SPAN_TAB, [named_table, public,
                                {write_concurrency, true},
                                {keypos, #span.span_id}]);
        _ ->
            ok
    end,

    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
