-module(opentelemetry_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/tracer.hrl").
-include("ot_span.hrl").
-include("ot_test_utils.hrl").

all() ->
    [all_testcases(),
     {group, w3c},
     {group, b3}].

all_testcases() ->
    [with_span, macros, child_spans, update_span_data, tracer_instrumentation_library,
     tracer_previous_ctx, stop_temporary_app].

groups() ->
    [{w3c, [], [propagation]},
     {b3, [], [propagation]}].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Config.

end_per_suite(_Config) ->
    application:unload(opentelemetry),
    ok.

init_per_group(Propagator, Config) when Propagator =:= w3c ;
                                        Propagator =:= b3 ->
    application:set_env(opentelemetry, processors, [{ot_batch_processor, #{scheduled_delay_ms => 1}}]),
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

    [{propagator, Propagator} | Config].

end_per_group(_, _Config) ->
    _ = application:stop(opentelemetry).

init_per_testcase(_, Config) ->
    application:set_env(opentelemetry, processors, [{ot_batch_processor, #{scheduled_delay_ms => 1}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    ot_batch_processor:set_exporter(ot_exporter_tab, Tid),
    [{tid, Tid} | Config].

end_per_testcase(_, _Config) ->
    _ = application:stop(opentelemetry),
    ok.

macros(Config) ->
    Tid = ?config(tid, Config),

    SpanCtx1 = ?start_span(<<"span-1">>),
    SpanCtx2 = ?start_span(<<"span-2">>),

    ?assertMatch(SpanCtx2, ?current_span_ctx),
    ?end_span(),

    %% 1st span should be the current span ctx now
    ?assertMatch(SpanCtx1, ?current_span_ctx()),

    Attr1 = <<"attr-1">>,
    AttrValue1 = <<"attr-value-1">>,
    ?set_attribute(Attr1, AttrValue1),

    ?end_span(),

    [Span1] = assert_exported(Tid, SpanCtx1),

    ?assertEqual([{Attr1, AttrValue1}], Span1#span.attributes),

    ok.

with_span(Config) ->
    _Tid = ?config(tid, Config),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = ?start_span(<<"span-1">>),

    Result = some_result,
    ?assertMatch(Result, ot_tracer:with_span(Tracer, <<"with-span-2">>,
                                             fun(SpanCtx2) ->
                                                     ?assertNotEqual(SpanCtx1, SpanCtx2),
                                                     ?assertEqual(SpanCtx2, ?current_span_ctx()),
                                                     Result
                                             end)),

    ?assertMatch(SpanCtx1, ?current_span_ctx()),

    ok.

child_spans(Config) ->
    Tid = ?config(tid, Config),

    EarlierTimestamp = opentelemetry:timestamp(),

    %% start a span and 2 children
    SpanCtx1 = ?start_span(<<"span-1">>),
    SpanCtx2 = ?start_span(<<"span-2">>),
    SpanCtx3 = ?start_span(<<"span-3">>),

    %% end the 3rd span
    ?assertMatch(SpanCtx3, ?current_span_ctx()),
    ?end_span(),

    assert_exported(Tid, SpanCtx3),

    %% 2nd span should be the current span ctx now
    ?assertMatch(SpanCtx2, ?current_span_ctx()),

    %% start another child of the 2nd span
    %% with a timestamp sent as part of the start opts
    SpanCtx4 = ?start_span(<<"span-4">>, #{start_time => EarlierTimestamp}),
    ?assertMatch(SpanCtx4, ?current_span_ctx()),

    %% end 4th span and 2nd should be current
    ?end_span(),
    ?assertMatch(SpanCtx2, ?current_span_ctx()),

    %% end 2th span and 1st should be current
    ?end_span(),
    ?assertMatch(SpanCtx1, ?current_span_ctx()),

    %% end first and no span should be current ctx
    ?end_span(),
    ?assertMatch(undefined, ?current_span_ctx()),

    assert_all_exported(Tid, [SpanCtx1, SpanCtx2, SpanCtx3]),

    [Span4] = assert_exported(Tid, SpanCtx4),

    ?assertEqual(EarlierTimestamp, Span4#span.start_time).

update_span_data(Config) ->
    Tid = ?config(tid, Config),

    Links = [#link{trace_id=0,
                   span_id=0,
                   attributes=[],
                   tracestate=[]}],

    SpanCtx1=#span_ctx{trace_id=TraceId,
                       span_id=SpanId} = ?start_span(<<"span-1">>, #{links => Links}),
    ?set_attribute(<<"key-1">>, <<"value-1">>),

    Events = opentelemetry:events([{erlang:system_time(nanosecond),
                                    <<"event-name">>, []}]),
    Status = opentelemetry:status(0, <<"status">>),

    %% with spanctx and tracer passed as an argument
    Tracer = opentelemetry:get_tracer(),
    ot_span:set_status(Tracer, SpanCtx1, Status),

    ot_span:add_events(Tracer, SpanCtx1, Events),

    ?assertMatch(SpanCtx1, ?current_span_ctx()),
    ?end_span(),

    ?UNTIL_NOT_EQUAL([], ets:match(Tid, #span{trace_id=TraceId,
                                              span_id=SpanId,
                                              attributes=[{<<"key-1">>, <<"value-1">>}],
                                              links=Links,
                                              status=Status,
                                              events=Events,
                                              _='_'})).

propagation(_Config) ->
    #span_ctx{trace_id=TraceId} = ?start_span(<<"span-1">>),
    Headers = ot_propagation:http_inject([{<<"existing-header">>, <<"I exist">>}]),

    [?assert(is_binary(Value)) || {_Key, Value} <- Headers],

    ?end_span(),

    ?assertEqual(undefined, ?current_span_ctx()),

    %% make header keys uppercase to validate the extractor is case insensitive
    BinaryHeaders = [{string:uppercase(Key), iolist_to_binary(Value)} || {Key, Value} <- Headers],
    ot_propagation:http_extract(BinaryHeaders),

    %% extracted remote spans are not set to the active span
    %% instead they are stored under a special "external span"
    %% key and then used as the parent if current active span
    %% is undefined or invalid
    ?assertEqual(undefined, ?current_span_ctx()),

    #span_ctx{trace_id=TraceId2,
              span_id=_SpanId2} = ?start_span(<<"span-2">>),

    %% new span should be a child of the extracted span
    ?assertEqual(TraceId, TraceId2),

    ok.

tracer_instrumentation_library(Config) ->
    Tid = ?config(tid, Config),

    TracerName = tracer1,
    TracerVsn = <<"1.0.0">>,
    opentelemetry:register_tracer(TracerName, TracerVsn),

    Tracer = opentelemetry:get_tracer(TracerName),

    %% start a span and 2 children
    SpanCtx1 = ot_tracer:start_span(Tracer, <<"span-1">>, #{}),

    ot_tracer:end_span(Tracer),
    ?assertMatch(undefined, ?current_span_ctx()),

    [Span1] = assert_exported(Tid, SpanCtx1),

    ?assertEqual({instrumentation_library,<<"tracer1">>,<<"1.0.0">>}, Span1#span.instrumentation_library).

%% check that ending a span results in the tracer setting the previous tracer context
%% as the current active and not use the parent span ctx of the span being ended --
%% though at times those will be the same.
tracer_previous_ctx(Config) ->
    Tid = ?config(tid, Config),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = ot_tracer:start_span(Tracer, <<"span-1">>, #{}),
    ?assertMatch(SpanCtx1, ?current_span_ctx()),

    %% create a span that is not set to active and with no parent
    SpanCtx2 = ot_tracer:start_inactive_span(Tracer, <<"span-2">>, #{parent => undefined}),

    %% start a new active span with SpanCtx2 as the parent
    SpanCtx3 = ot_tracer:start_span(Tracer, <<"span-3">>, #{parent => SpanCtx2}),

    %% end SpanCtx3, even though it isn't the parent SpanCtx1
    %% should be the active context afterward
    ot_tracer:end_span(Tracer),

    ?assertMatch(SpanCtx1, ?current_span_ctx()),

    ot_tracer:end_span(Tracer),

    ot_tracer:set_span(Tracer, SpanCtx2),
    ot_tracer:end_span(Tracer),

    assert_all_exported(Tid, [SpanCtx3, SpanCtx1, SpanCtx2]),

    ok.

stop_temporary_app(_Config) ->
    SpanCtx1 = ?start_span(<<"span-1">>),
    ?assertNotMatch(#span_ctx{trace_id=0,
                              span_id=0}, SpanCtx1),

    ok = application:stop(opentelemetry),

    %% stopping opentelemetry resets the tracer to a noop
    SpanCtx2 = ?start_span(<<"span-2">>),
    ?assertMatch(#span_ctx{trace_id=0,
                           span_id=0}, SpanCtx2),
    ok.

%%

assert_all_exported(Tid, SpanCtxs) ->
    [assert_exported(Tid, SpanCtx) || SpanCtx <- SpanCtxs].

assert_exported(Tid, #span_ctx{trace_id=TraceId,
                               span_id=SpanId}) ->
    ?UNTIL_NOT_EQUAL([], ets:match_object(Tid, #span{trace_id=TraceId,
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
