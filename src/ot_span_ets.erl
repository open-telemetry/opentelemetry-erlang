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

-export([start_span/2,
         finish_span/1,
         get_ctx/1,
         is_recording_events/1,
         set_attribute/3,
         set_attributes/2,
         add_events/2,
         set_status/2,
         update_name/2]).

-include("opentelemetry.hrl").

-record(state, {}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Start a span and insert into the active span ets table.
-spec start_span(opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(Name, Opts) ->
    {SpanCtx, Span} = ot_span_utils:start_span(Name, Opts),
    _ = storage_insert(Span),
    SpanCtx.

%% @doc Finish a span based on its context and send to reporter.
-spec finish_span(opentelemetry:span_ctx()) -> boolean() | {error, term()}.
finish_span(#span_ctx{span_id=SpanId,
                      tracestate=Tracestate,
                      trace_flags=TraceOptions}) when ?IS_SPAN_ENABLED(TraceOptions) ->
    case ets:take(?SPAN_TAB, SpanId) of
        [Span] ->
            Span1 = ot_span_utils:end_span(Span#span{tracestate=Tracestate}),
            ot_exporter:store_span(Span1);
        _ ->
            false
    end;
finish_span(_) ->
    ok.

-spec get_ctx(opentelemetry:span()) -> opentelemetry:span_ctx().
get_ctx(_Span) ->
    #span_ctx{}.

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

-spec add_events(opentelemetry:span_ctx(), opentelemetry:time_events()) -> boolean().
add_events(#span_ctx{span_id=SpanId}, NewTimeEvents) ->
    case ets:lookup(?SPAN_TAB, SpanId) of
        [Span=#span{time_events=TimeEvents}] ->
            Span1 = Span#span{time_events=TimeEvents++NewTimeEvents},
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
