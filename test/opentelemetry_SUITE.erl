-module(opentelemetry_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_test_utils.hrl").

all() ->
    [{group, ot_ctx_pdict},
     {group, ot_ctx_seqtrace}].

all_testcases() ->
    [child_spans, update_span_data, propagation].

groups() ->
    [{ot_ctx_pdict, [shuffle], all_testcases()},
     {ot_ctx_seqtrace, [shuffle], all_testcases()}].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_CtxModule, Config) ->
    application:set_env(opentelemetry, processors, [{ot_batch_processor, [{scheduled_delay_ms, 1}]}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config.

end_per_group(_, _Config) ->
    ok = application:stop(opentelemetry).

init_per_testcase(_, Config) ->
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    ot_batch_processor:set_exporter(ot_exporter_tab, Tid),
    [{tid, Tid} | Config].

end_per_testcase(_, _Config) ->
    ok.

child_spans(Config) ->
    Tid = ?config(tid, Config),

    %% start a span and 2 children
    SpanCtx1 = otel:start_span(<<"span-1">>),
    SpanCtx2 = otel:start_span(<<"span-2">>),
    SpanCtx3 = otel:start_span(<<"span-3">>),

    %% end the 3rd span
    ?assertMatch(SpanCtx3, otel:current_span_ctx()),
    otel:end_span(),

    assert_exported(Tid, SpanCtx3),

    %% 2nd span should be the current span ctx now
    ?assertMatch(SpanCtx2, otel:current_span_ctx()),

    %% start another child of the 2nd span
    SpanCtx4 = otel:start_span(<<"span-4">>),
    ?assertMatch(SpanCtx4, otel:current_span_ctx()),

    %% end 4th span and 2nd should be current
    otel:end_span(),
    ?assertMatch(SpanCtx2, otel:current_span_ctx()),

    %% end 2th span and 1st should be current
    otel:end_span(),
    ?assertMatch(SpanCtx1, otel:current_span_ctx()),

    %% end first and no span should be current ctx
    otel:end_span(),
    ?assertMatch(undefined, otel:current_span_ctx()),

    assert_all_exported(Tid, [SpanCtx1, SpanCtx2, SpanCtx3, SpanCtx4]).

update_span_data(Config) ->
    Tid = ?config(tid, Config),

    Links = [#link{trace_id=0,
                   span_id=0,
                   attributes=[],
                   tracestate=[]}],

    SpanCtx1=#span_ctx{trace_id=TraceId,
                       span_id=SpanId} = otel:start_span(<<"span-1">>, #{links => Links}),
    otel:set_attribute(<<"key-1">>, <<"value-1">>),

    TimedEvents = opentelemetry:timed_events([{opentelemetry:timestamp(),
                                               <<"timed-event-name">>, []}]),
    Status = opentelemetry:status(0, <<"status">>),

    %% with spanctx and tracer passed as an argument
    Tracer = opentelemetry:get_tracer(),
    ot_span:set_status(Tracer, SpanCtx1, Status),

    ot_span:add_events(Tracer, SpanCtx1, TimedEvents),

    ?assertMatch(SpanCtx1, otel:current_span_ctx()),
    otel:end_span(),

    ?UNTIL([] =/= ets:match(Tid, #span{trace_id=TraceId,
                                       span_id=SpanId,
                                       attributes=[{<<"key-1">>, <<"value-1">>}],
                                       links=Links,
                                       status=Status,
                                       timed_events=TimedEvents,
                                       _='_'})).

propagation(_Config) ->
    SpanCtx1=#span_ctx{trace_id=TraceId,
                       span_id=SpanId} = otel:start_span(<<"span-1">>),
    ot_baggage:set(<<"baggage-key-1">>, <<"baggage-value-1">>),
    Headers = ot_propagation:http_inject([{<<"existing-header">>, <<"I exist">>}]),

    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),

    ?assertListsMatch([{<<"X-B3-TraceId">>, EncodedTraceId},
                       {<<"X-B3-SpanId">>, EncodedSpanId},
                       {<<"X-B3-Sampled">>, "1"},
                       {<<"existing-header">>, <<"I exist">>},
                       {<<"Baggage-Context">>, [[<<"baggage-key-1">>, "=", <<"baggage-value-1">>]]}], Headers),

    otel:end_span(),

    ?assertEqual(undefined, otel:current_span_ctx()),

    ot_propagation:http_extract(Headers),
    ?assertSpanCtxsEqual(SpanCtx1, otel:current_span_ctx()),

    ok.

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
