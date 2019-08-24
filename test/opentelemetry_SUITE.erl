-module(opentelemetry_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").

all() ->
    [{group, ot_ctx_pdict},
     {group, ot_ctx_seqtrace}].

all_testcases() ->
    [child_spans, non_default_tracer].

groups() ->
    [{ot_ctx_pdict, [parallel, shuffle], all_testcases()},
     {ot_ctx_seqtrace, [parallel, shuffle], all_testcases()}].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(CtxModule, Config) ->
    application:set_env(opentelemetry, tracer, {ot_tracer_default, #{span => {ot_span_ets, []},
                                                                     ctx => {CtxModule, []}}}),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config.

end_per_group(_, _Config) ->
    ok = application:stop(opentelemetry).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

child_spans(_Config) ->
    %% start a span and 2 children
    SpanCtx1 = ot_tracer:start_span(<<"span-1">>),
    SpanCtx2 = ot_tracer:start_span(<<"span-2">>),
    SpanCtx3 = ot_tracer:start_span(<<"span-3">>),

    %% finish the 3rd span
    ?assertMatch(SpanCtx3, ot_tracer:current_span_ctx()),
    ot_tracer:finish(),

    %% 2nd span should be the current span ctx now
    ?assertMatch(SpanCtx2, ot_tracer:current_span_ctx()),

    %% start another child of the 2nd span
    SpanCtx4 = ot_tracer:start_span(<<"span-4">>),
    ?assertMatch(SpanCtx4, ot_tracer:current_span_ctx()),

    %% finish 4th span and 2nd should be current
    ot_tracer:finish(),
    ?assertMatch(SpanCtx2, ot_tracer:current_span_ctx()),

    %% finish 2th span and 1st should be current
    ot_tracer:finish(),
    ?assertMatch(SpanCtx1, ot_tracer:current_span_ctx()),

    %% finish first and no span should be current ctx
    ot_tracer:finish(),
    ?assertMatch(undefined, ot_tracer:current_span_ctx()).

non_default_tracer(_Config) ->
    SpanCtx1 = ot_tracer:start_span(<<"span-1">>),
    ?assertNotMatch(#span_ctx{trace_id=0,
                              span_id=0}, SpanCtx1),
    ot_tracer:finish(),

    SpanCtx2 = ot_tracer:start_span(ot_tracer_noop, <<"span-2">>, #{}),
    ?assertMatch(#span_ctx{trace_id=0,
                           span_id=0}, SpanCtx2),
    ?assertMatch(SpanCtx2, ot_tracer:current_span_ctx()),
    ot_tracer:finish().
