-module(opentelemetry_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").
-include("ot_test_utils.hrl").

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
    application:set_env(opentelemetry, exporter, [{exporters, []},
                                                  {scheduled_delay_ms, 1}]),
    {ok, _} = application:ensure_all_started(opentelemetry),

    Config.

end_per_group(_, _Config) ->
    ok = application:stop(opentelemetry).

init_per_testcase(_, Config) ->
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    ot_exporter:register(ot_exporter_tab, Tid),
    [{tid, Tid} | Config].

end_per_testcase(_, _Config) ->
    ok.

child_spans(Config) ->
    Tid = ?config(tid, Config),

    %% start a span and 2 children
    SpanCtx1 = ot_tracer:start_span(<<"span-1">>),
    SpanCtx2 = ot_tracer:start_span(<<"span-2">>),
    SpanCtx3 = ot_tracer:start_span(<<"span-3">>),

    %% finish the 3rd span
    ?assertMatch(SpanCtx3, ot_tracer:current_span_ctx()),
    ot_tracer:finish(),

    assert_exported(Tid, SpanCtx3),

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
    ?assertMatch(undefined, ot_tracer:current_span_ctx()),

    assert_all_exported(Tid, [SpanCtx1, SpanCtx2, SpanCtx3, SpanCtx4]).

non_default_tracer(Config) ->
    Tid = ?config(tid, Config),

    SpanCtx1 = ot_tracer:start_span(<<"span-1">>),
    ?assertNotMatch(#span_ctx{trace_id=0,
                              span_id=0}, SpanCtx1),
    ot_tracer:finish(),

    SpanCtx2 = ot_tracer:start_span(ot_tracer_noop, <<"span-2">>, #{}),
    ?assertMatch(#span_ctx{trace_id=0,
                           span_id=0}, SpanCtx2),
    ?assertMatch(SpanCtx2, ot_tracer:current_span_ctx()),
    ot_tracer:finish(),

    assert_exported(Tid, SpanCtx1),
    assert_not_exported(Tid, SpanCtx2).

%%

assert_all_exported(Tid, SpanCtxs) ->
    [assert_exported(Tid, SpanCtx) || SpanCtx <- SpanCtxs].

assert_exported(Tid, #span_ctx{trace_id=TraceId,
                               span_id=SpanId}) ->
    ?UNTIL([] =/= ets:match(Tid, #span{trace_id=TraceId,
                                       span_id=SpanId,
                                       _='_'})).

assert_not_exported(Tid, #span_ctx{trace_id=TraceId,
                                   span_id=SpanId}) ->
    %% sleep so exporter has run before we check
    %% since we can't do like when checking it exists with UNTIL
    timer:sleep(100),
    ?assertMatch([], ets:match(Tid, #span{trace_id=TraceId,
                                          span_id=SpanId,
                                          _='_'})).
