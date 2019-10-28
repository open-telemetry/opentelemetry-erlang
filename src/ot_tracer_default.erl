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
-module(ot_tracer_default).

-behaviour(ot_tracer).

-export([setup/1,
         start_span/2,
         with_span/1,
         with_span/2,
         finish/0,
         current_span_ctx/0,
         get_binary_format/0,
         get_http_text_format/0]).

-define(SPAN_CTX, {?MODULE, span_ctx}).
-define(CTX_IMPL_KEY, {?MODULE, ctx}).
-define(SPAN_IMPL_KEY, {?MODULE, span}).

-define(ctx, (persistent_term:get(?CTX_IMPL_KEY))).
-define(span, (persistent_term:get(?SPAN_IMPL_KEY))).

-type pdict_trace_ctx() :: {opentelemetry:span_ctx(), pdict_trace_ctx() | undefined}.

-spec setup(map()) -> [supervisor:child_spec()].
setup(Opts) ->
    lists:filtermap(fun({ConfigKey, PersistentKey}) ->
                        {Module, Args} = maps:get(ConfigKey, Opts),
                        persistent_term:put(PersistentKey, Module),
                        case erlang:function_exported(Module, start_link, 1) of
                            true ->
                                {true, #{id => Module,
                                         start => {Module, start_link, [Args]}}};
                            false ->
                                false
                        end
                    end, [{ctx, ?CTX_IMPL_KEY},
                          {span, ?SPAN_IMPL_KEY}]).

-spec start_span(opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(Name, Opts) ->
    {ParentSpan, ParentCtx} = case ot_ctx:get(?ctx, ?SPAN_CTX) of
        {Span, _}=Ctx -> {Span, Ctx};
        _ -> {undefined, undefined}
    end,
    SpanCtx = ot_span:start_span(?span, Name, Opts#{parent => ParentSpan}),
    ot_ctx:with_value(?ctx, ?SPAN_CTX, {SpanCtx, ParentCtx}),
    SpanCtx.

-spec with_span(opentelemetry:span_ctx()) -> ok.
with_span(SpanCtx) ->
    ot_ctx:with_value(?ctx, ?SPAN_CTX, {SpanCtx, undefined}).

-spec with_span(opentelemetry:span_ctx(), fun()) -> ok.
with_span(SpanCtx, Fun) ->
    ot_ctx:with_value(?ctx, ?SPAN_CTX, {SpanCtx, undefined}, Fun).

-spec current_span_ctx() -> opentelemetry:span_ctx().
current_span_ctx() ->
    case ot_ctx:get(?ctx, ?SPAN_CTX) of
        {SpanCtx, _ParentPdictSpanCtx} ->
            SpanCtx;
        _ ->
            undefined
    end.

%% Internal function that returns the current trace context.
%% The pdict ctx stores both the current span ctx and the
%% parent trace context, which contains its parent and so on.
-spec current_ctx() -> pdict_trace_ctx().
current_ctx() ->
    ot_ctx:get(?ctx, ?SPAN_CTX).

%%--------------------------------------------------------------------
%% @doc
%% Finishes the span in the current pdict context. And sets the parent
%% as the current span ctx or undefined if there is no local parent.
%% @end
%%--------------------------------------------------------------------
-spec finish() -> ok.
finish() ->
    {SpanCtx, ParentCtx} = current_ctx(),
    ot_span:finish_span(?span, SpanCtx),
    ot_ctx:with_value(?ctx, ?SPAN_CTX, ParentCtx),
    ok.

-spec get_binary_format() -> binary().
get_binary_format() ->
    <<>>.

-spec get_http_text_format() -> opentelemetry:http_headers().
get_http_text_format() ->
    [].
