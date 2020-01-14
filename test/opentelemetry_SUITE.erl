-module(opentelemetry_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_test_utils.hrl").

all() ->
    [{group, ot_ctx_pdict},
     {group, ot_ctx_seqtrace},
     {group, w3c},
     {group, b3}].

all_testcases() ->
    [child_spans, update_span_data].

groups() ->
    [{ot_ctx_pdict, [shuffle], all_testcases()},
     {ot_ctx_seqtrace, [shuffle], all_testcases()},
     {w3c, [propagation]},
     {b3, [propagation]}].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Propagator, Config) when Propagator =:= w3c ;
                                        Propagator =:= b3 ->
    application:set_env(opentelemetry, processors, [{ot_batch_processor, [{scheduled_delay_ms, 1}]}]),
    {ok, _} = application:ensure_all_started(opentelemetry),

    {CorrelationsHttpExtractor, CorrelationsHttpInjector} = ot_correlations:get_http_propagators(),
    {TraceHttpExtractor, TraceHttpInjector} = case Propagator of
                                                  w3c -> ot_tracer_default:w3c_propagators();
                                                  b3 -> ot_tracer_default:b3_propagators()
                                              end,
    opentelemetry:set_http_extractor([CorrelationsHttpExtractor,
                                      TraceHttpExtractor]),
    opentelemetry:set_http_injector([CorrelationsHttpInjector,
                                     TraceHttpInjector]),

    [{propagator, Propagator} | Config];
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

    EarlierTimestamp = wts:timestamp(),

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
    %% with a timestamp sent as part of the start opts
    SpanCtx4 = otel:start_span(<<"span-4">>, #{start_time => EarlierTimestamp}),
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

    assert_all_exported(Tid, [SpanCtx1, SpanCtx2, SpanCtx3, SpanCtx4]),

    [Span4] = ets:match_object(Tid, #span{trace_id=SpanCtx4#span_ctx.trace_id,
                                          span_id=SpanCtx4#span_ctx.span_id,
                                          _='_'}),

    ?assertEqual(EarlierTimestamp, Span4#span.start_time).

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

propagation(Config) ->
    Propagator = ?config(propagator, Config),
    SpanCtx1=#span_ctx{trace_id=TraceId,
                       span_id=SpanId} = otel:start_span(<<"span-1">>),
    Headers = ot_propagation:http_inject([{<<"existing-header">>, <<"I exist">>}]),

    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),

    ?assertListsMatch([{<<"existing-header">>, <<"I exist">>} |
                       trace_context(Propagator, EncodedTraceId, EncodedSpanId)], Headers),

    otel:end_span(),

    ?assertEqual(undefined, otel:current_span_ctx()),

    %% make header keys uppercase to validate the extractor is case insensitive
    BinaryHeaders = [{string:uppercase(Key), iolist_to_binary(Value)} || {Key, Value} <- Headers],
    ot_propagation:http_extract(BinaryHeaders),
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

trace_context(w3c, EncodedTraceId, EncodedSpanId) ->
    [{<<"traceparent">>,
     [<<"00">>, "-", EncodedTraceId,"-", EncodedSpanId, "-", <<"01">>]}];
trace_context(b3, EncodedTraceId, EncodedSpanId) ->
    [{<<"X-B3-Sampled">>, "1"},
     {<<"X-B3-SpanId">>, EncodedSpanId},
     {<<"X-B3-TraceId">>,EncodedTraceId}].
