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

-behaviour(gen_server).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2]).

-export([start_span/1,
         start_span/2,
         finish_span/1]).

-include("opentelemetry.hrl").

-record(state, {}).

%% table to store active spans
-define(SPAN_TAB, otel_span_table).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @equiv start_span(Name, #{})
-spec start_span(opentelemetry:span_name()) -> opentelemetry:span_ctx().
start_span(Name) ->
    start_span(Name, #{}).


%% @doc Start a span and insert into the active span ets table.
-spec start_span(opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(Name, Opts) ->
    {SpanCtx, Span} = ot_span:start_span(Name, Opts),
    _ = storage_insert(Span),
    SpanCtx.

%% @doc Finish a span based on its context and send to reporter.
-spec finish_span(opentelemetry:span_ctx()) -> ok.
finish_span(#span_ctx{span_id=SpanId,
                      tracestate=Tracestate,
                      trace_options=TraceOptions}) when ?IS_SPAN_ENABLED(TraceOptions) ->
    case ets:take(?SPAN_TAB, SpanId) of
        [Span] ->
            _Span1 = ot_span:end_span(Span#span{tracestate=Tracestate}),
            %% oc_reporter:store_span(Span1),
            ok;
        _ ->
            ok
    end;
finish_span(_) ->
    ok.

%%

storage_insert(undefined) ->
    ok;
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
