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
-module(otel_span_ets).

-behaviour(gen_server).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2]).

-export([start_span/5,
         end_span/1,
         end_span/2,
         get_ctx/1,
         set_attribute/3,
         set_attributes/2,
         add_event/3,
         add_events/2,
         set_status/2,
         update_name/2]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_span.hrl").
-include("otel_span_ets.hrl").

-record(state, {}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Start a span and insert into the active span ets table.
-spec start_span(otel_ctx:t(), opentelemetry:span_name(), otel_span:start_opts(),
                 fun(), otel_tracer_server:instrumentation_library()) -> opentelemetry:span_ctx().
start_span(Ctx, Name, Opts, Processors, InstrumentationLibrary) ->
    case otel_span_utils:start_span(Ctx, Name, Opts) of
        {SpanCtx=#span_ctx{is_recording=true}, Span=#span{}} ->
            Span1 = Span#span{instrumentation_library=InstrumentationLibrary},
            Span2 = Processors(Ctx, Span1),
            io:format("ets_start_span: ~p ~p ~n", [Ctx, Span1, Span2]),
            _ = storage_insert(Span2),
            SpanCtx;
        {SpanCtx, #span{}} ->
            %% span isn't recorded so don't run processors or insert into ets table
            io:format("span is not recorded ~p~n", [SpanCtx]),
            SpanCtx
    end.

end_span(SpanCtx=#span_ctx{span_sdk={_, OnEndProcessors}}) ->
    end_span(SpanCtx, undefined, OnEndProcessors).

end_span(SpanCtx=#span_ctx{span_sdk={_, OnEndProcessors}}, Timestamp) ->
    end_span(SpanCtx, Timestamp, OnEndProcessors).

%% @doc End a span based on its context and send to exporter.
-spec end_span(opentelemetry:span_ctx(), integer() | undefined, fun()) -> boolean() | {error, term()}.
end_span(#span_ctx{span_id=SpanId,
                   is_recording=true,
                   tracestate=Tracestate}, Timestamp, Processors) ->
    case ets:take(?SPAN_TAB, SpanId) of
        [Span] ->
            io:format("otel_span_ets span ENDING ~p~n", [Span]),
            Span1 = otel_span_utils:end_span(Span#span{tracestate=Tracestate,
                                                       is_recording=false}, Timestamp),
            io:format("otel_span_ets span ENDED ~p~n", [Span1]),
            Processors(Span1);
        _ ->
            false
    end;
end_span(_, _, _) ->
    true.

-spec get_ctx(opentelemetry:span()) -> opentelemetry:span_ctx().
get_ctx(#span{trace_id=TraceId,
              span_id=SpanId,
              tracestate=TraceState,
              is_recording=IsRecording}) ->
    #span_ctx{trace_id=TraceId,
              span_id=SpanId,
              tracestate=TraceState,
              is_recording=IsRecording}.

-spec set_attribute(opentelemetry:span_ctx(),
                    opentelemetry:attribute_key(),
                    opentelemetry:attribute_value()) -> boolean().
set_attribute(#span_ctx{span_id=SpanId}, Key, Value) ->
    try ets:lookup_element(?SPAN_TAB, SpanId, #span.attributes) of
        Attributes ->
            ets:update_element(?SPAN_TAB, SpanId, {#span.attributes, [{Key, Value} | Attributes]})
    catch error:badarg ->
            false
    end;
set_attribute(_, _, _) ->
    false.

%% Note: Spans are referenced through the current active span context in a process
%% and thus modified only by a single process, so concurrent updates of the same field
%% are not a real concern. This allows `add_events' and `set_attributes' to lookup and
%% update only the specific element of the `span' without worrying about it having been
%% changed by another process between the lookup and update.
-spec set_attributes(opentelemetry:span_ctx(), opentelemetry:attributes()) -> boolean().
set_attributes(#span_ctx{span_id=SpanId}, NewAttributes) ->
    try ets:lookup_element(?SPAN_TAB, SpanId, #span.attributes) of
        Attributes ->
            ets:update_element(?SPAN_TAB, SpanId, {#span.attributes, Attributes++NewAttributes})
    catch error:badarg ->
            false
    end.

-spec add_event(opentelemetry:span_ctx(), unicode:unicode_binary(), opentelemetry:attributes()) -> boolean().
add_event(SpanCtx, Name, Attributes) ->
    Events = opentelemetry:events([{Name, Attributes}]),
    add_events(SpanCtx, Events).

-spec add_events(opentelemetry:span_ctx(), opentelemetry:events()) -> boolean().
add_events(#span_ctx{span_id=SpanId}, NewEvents) ->
    try ets:lookup_element(?SPAN_TAB, SpanId, #span.events) of
        Events ->
            ets:update_element(?SPAN_TAB, SpanId, {#span.events, Events++NewEvents})
    catch error:badarg ->
            false
    end.

-spec set_status(opentelemetry:span_ctx(), opentelemetry:status()) -> boolean().
set_status(#span_ctx{span_id=SpanId}, Status) ->
    ets:update_element(?SPAN_TAB, SpanId, {#span.status, Status}).

-spec update_name(opentelemetry:span_ctx(), opentelemetry:span_name()) -> boolean().
update_name(#span_ctx{span_id=SpanId}, Name) ->
    ets:update_element(?SPAN_TAB, SpanId, {#span.name, Name}).

%%

storage_insert(undefined) ->
    io:format("undefined span to be inserted"),
    ok;
storage_insert(Span) ->
    io:format("inserting into ets: ~p~n", [Span]),
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
