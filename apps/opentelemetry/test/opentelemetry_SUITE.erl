-module(opentelemetry_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include("otel_tracer.hrl").
-include("otel_span.hrl").
-include("otel_test_utils.hrl").
-include("otel_sampler.hrl").

all() ->
    [all_testcases(),
     {group, w3c},
     {group, b3}].

all_testcases() ->
    [disable_auto_registration, registered_tracers, with_span, macros, child_spans,
     update_span_data, tracer_instrumentation_library, tracer_previous_ctx, stop_temporary_app,
     reset_after, attach_ctx, default_sampler, root_span_sampling_always_on,
     root_span_sampling_always_off,record_but_not_sample, record_exception_works,
     record_exception_with_message_works].

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
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),

    {BaggageTextMapExtractor, BaggageTextMapInjector} = otel_baggage:get_text_map_propagators(),
    {TraceTextMapExtractor, TraceTextMapInjector} = case Propagator of
                                                        w3c -> otel_tracer_default:w3c_propagators();
                                                        b3 -> otel_tracer_default:b3_propagators()
                                                    end,
    opentelemetry:set_text_map_extractors([BaggageTextMapExtractor,
                                           TraceTextMapExtractor]),
    opentelemetry:set_text_map_injectors([BaggageTextMapInjector,
                                          TraceTextMapInjector]),

    [{propagator, Propagator} | Config].

end_per_group(_, _Config) ->
    _ = application:stop(opentelemetry).

init_per_testcase(disable_auto_registration, Config) ->
    application:set_env(opentelemetry, register_loaded_applications, false),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config;
init_per_testcase(_, Config) ->
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    otel_batch_processor:set_exporter(otel_exporter_tab, Tid),
    [{tid, Tid} | Config].

end_per_testcase(disable_auto_registration, _Config) ->
    _ = application:stop(opentelemetry),
    _ = application:unload(opentelemetry),
    ok;
end_per_testcase(_, _Config) ->
    _ = application:stop(opentelemetry),
    ok.

disable_auto_registration(_Config) ->
    {_, #tracer{instrumentation_library=Library}} = opentelemetry:get_tracer(kernel),
    ?assertEqual(undefined, Library),
    ok.

registered_tracers(_Config) ->
    {_, #tracer{instrumentation_library=Library}} = opentelemetry:get_tracer(kernel),
    ?assertEqual(<<"kernel">>, Library#instrumentation_library.name),

    %% register a new tracer with the same name but different version
    opentelemetry:register_tracer(kernel, <<"fake-version">>),
    {_, #tracer{instrumentation_library=NewLibrary}} = opentelemetry:get_tracer(kernel),
    ?assertEqual(<<"kernel">>, NewLibrary#instrumentation_library.name),
    ?assertEqual(<<"fake-version">>, NewLibrary#instrumentation_library.version),
    ok.

macros(Config) ->
    Tid = ?config(tid, Config),

    SpanCtx1 = ?start_span(<<"span-1">>),

    %% start_span does not modify the context
    ?assertMatch(undefined, ?current_span_ctx),
    ?set_current_span(SpanCtx1),

    %% since SpanCtx1 was set to the current span it will be the parent
    SpanCtx2 = ?start_span(<<"span-2">>),
    ?set_current_span(SpanCtx2),

    ?assertMatch(SpanCtx2, ?current_span_ctx),
    ?end_span(SpanCtx2),

    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    Attr1 = <<"attr-1">>,
    AttrValue1 = <<"attr-value-1">>,
    ?set_attribute(Attr1, AttrValue1),

    ?end_span(SpanCtx1),

    [Span1] = assert_exported(Tid, SpanCtx1),

    ?assertEqual([{Attr1, AttrValue1}], Span1#span.attributes),

    ok.

with_span(Config) ->
    Tid = ?config(tid, Config),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = ?start_span(<<"span-1">>),
    otel_tracer:set_current_span(SpanCtx1),

    Result = some_result,
    ?assertMatch(Result, otel_tracer:with_span(Tracer, <<"with-span-2">>, #{},
                                               fun(SpanCtx2) ->
                                                       ?assertNotEqual(SpanCtx1, SpanCtx2),
                                                       ?assertEqual(SpanCtx2, ?current_span_ctx),
                                                       Result
                                               end)),

    ?assertMatch(SpanCtx1, ?current_span_ctx),

    otel_span:end_span(SpanCtx1),
    [_Span1] = assert_exported(Tid, SpanCtx1),

    ok.

child_spans(Config) ->
    Tid = ?config(tid, Config),

    EarlierTimestamp = opentelemetry:timestamp(),

    %% start a span and 2 children
    SpanCtx1 = ?start_span(<<"span-1">>),
    ?set_current_span(SpanCtx1),
    SpanCtx2 = ?start_span(<<"span-2">>),
    ?set_current_span(SpanCtx2),
    SpanCtx3 = ?start_span(<<"span-3">>),
    ?set_current_span(SpanCtx3),

    %% end the 3rd span
    ?assertMatch(SpanCtx3, ?current_span_ctx),
    ?end_span(SpanCtx3),

    assert_exported(Tid, SpanCtx3),

    %% 3rd span should still be the current span ctx
    %% even though it is ended and not able to be updated
    ?assertMatch(SpanCtx3, ?current_span_ctx),

    ?set_current_span(SpanCtx2),

    %% start another child of the 2nd span
    %% with a timestamp sent as part of the start opts
    SpanCtx4 = ?start_span(<<"span-4">>, #{start_time => EarlierTimestamp}),
    ?set_current_span(SpanCtx4),
    ?assertMatch(SpanCtx4, ?current_span_ctx),

    %% end 4th span and 2nd should be current
    ?end_span(SpanCtx4),

    ?set_current_span(SpanCtx2),
    ?assertMatch(SpanCtx2, ?current_span_ctx),

    %% end 2th span and 1st should be current
    ?end_span(SpanCtx2),
    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    %% end first and no span should be current ctx
    ?end_span(SpanCtx1),
    ?set_current_span(undefined),
    ?assertMatch(undefined, ?current_span_ctx),

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
    ?set_current_span(SpanCtx1),
    ?set_attribute(<<"key-1">>, <<"value-1">>),

    Events = opentelemetry:events([{erlang:system_time(nanosecond),
                                    <<"event-name">>, []}]),
    Status = opentelemetry:status(0, <<"status">>),

    otel_span:set_status(SpanCtx1, Status),

    otel_span:add_events(SpanCtx1, Events),

    ?assertMatch(SpanCtx1, ?current_span_ctx),
    ?end_span(SpanCtx1),

    ?UNTIL_NOT_EQUAL([], ets:match(Tid, #span{trace_id=TraceId,
                                              span_id=SpanId,
                                              attributes=[{<<"key-1">>, <<"value-1">>}],
                                              links=Links,
                                              status=Status,
                                              events=Events,
                                              _='_'})).

propagation(Config) ->
    Propagator = ?config(propagator, Config),
    SpanCtx=#span_ctx{trace_id=TraceId,
                      span_id=SpanId} = ?start_span(<<"span-1">>),
    ?set_current_span(SpanCtx),

    ?assertMatch(#span_ctx{trace_flags=1}, ?current_span_ctx),
    ?assertMatch(#span_ctx{is_recording=true}, ?current_span_ctx),


    otel_baggage:set("key-1", <<"value=1">>, []),
    %% TODO: should the whole baggage entry be dropped if metadata is bad?
    %% drop bad metadata (the `1').
    otel_baggage:set(<<"key-2">>, <<"value-2">>, [<<"metadata">>, 1, {<<"md-k-1">>, <<"md-v-1">>}]),
    %% drop baggage with bad value
    otel_baggage:set(<<"key-3">>, value3),

    Headers = otel_propagator:text_map_inject([{<<"existing-header">>, <<"I exist">>}]),

    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),

    ?assertListsEqual([{<<"baggage">>, <<"key-2=value-2;metadata;md-k-1=md-v-1,key-1=value%3D1">>},
                       {<<"existing-header">>, <<"I exist">>} |
                       trace_context(Propagator, EncodedTraceId, EncodedSpanId)], Headers),

    ?end_span(SpanCtx),

    ?assertEqual(#{<<"key-1">> => {<<"value=1">>, []},
                   <<"key-2">> => {<<"value-2">>, [<<"metadata">>, {<<"md-k-1">>, <<"md-v-1">>}]}},
                 otel_baggage:get_all()),

    %% ?end_span doesn't remove the span from the context
    ?assertEqual(SpanCtx, ?current_span_ctx),
    ?set_current_span(undefined),
    ?assertEqual(undefined, ?current_span_ctx),

    %% clear our baggage from the context to test extraction
    otel_baggage:clear(),
    ?assertEqual(#{}, otel_baggage:get_all()),

    %% make header keys uppercase to validate the extractor is case insensitive
    BinaryHeaders = [{string:uppercase(Key), iolist_to_binary(Value)} || {Key, Value} <- Headers],
    otel_propagator:text_map_extract(BinaryHeaders),

    ?assertEqual(#{<<"key-1">> => {<<"value=1">>, []},
                   <<"key-2">> => {<<"value-2">>, [<<"metadata">>, {<<"md-k-1">>, <<"md-v-1">>}]}},
                 otel_baggage:get_all()),

    %% extracted remote spans are set to the active span
    %% but with `is_recording' false
    ?assertMatch(#span_ctx{is_recording=false}, ?current_span_ctx),

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

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),

    otel_span:end_span(SpanCtx1),

    [Span1] = assert_exported(Tid, SpanCtx1),

    ?assertEqual({instrumentation_library,<<"tracer1">>,<<"1.0.0">>}, Span1#span.instrumentation_library).

%% check that ending a span results in the tracer setting the previous tracer context
%% as the current active and not use the parent span ctx of the span being ended --
%% though at times those will be the same.
tracer_previous_ctx(Config) ->
    Tid = ?config(tid, Config),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),
    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    %% create a span that is not on the current context and with no parent
    SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-2">>, #{}),
    Ctx = otel_tracer:set_current_span(otel_ctx:new(), SpanCtx2),
    %% start a new span with SpanCtx2 as the parent
    SpanCtx3 = otel_tracer:start_span(Ctx, Tracer, <<"span-3">>, #{}),

    %% end SpanCtx3, even though it isn't the parent SpanCtx1
    otel_span:end_span(SpanCtx3),

    ?assertEqual(SpanCtx1, ?current_span_ctx),

    otel_span:end_span(SpanCtx1),

    ?set_current_span(SpanCtx2),
    otel_span:end_span(SpanCtx2),

    assert_all_exported(Tid, [SpanCtx3, SpanCtx1, SpanCtx2]),

    ok.

attach_ctx(Config) ->
    Tid = ?config(tid, Config),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),
    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    %% create a span that is not set to active and with no parent
    SpanCtx2 = otel_tracer:start_span(otel_ctx:new(), Tracer, <<"span-2">>, #{}),
    Ctx = otel_ctx:get_current(),

    erlang:spawn(fun() ->
                         otel_ctx:attach(Ctx),
                         ?set_current_span(SpanCtx2),
                         otel_span:end_span(SpanCtx2)
                 end),

    otel_span:end_span(SpanCtx1),

    assert_all_exported(Tid, [SpanCtx1, SpanCtx2]),

    ok.

reset_after(Config) ->
    Tid = ?config(tid, Config),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),
    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    Ctx = otel_ctx:get_current(),

    try
        %% start but don't end a span
        _SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-2">>, #{})
    after
        otel_ctx:attach(Ctx)
    end,

    otel_span:end_span(SpanCtx1),

    assert_all_exported(Tid, [SpanCtx1]),

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

default_sampler(_Config) ->
    Tracer = opentelemetry:get_tracer(),

    %% root span should be sampled by default sampler
    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),
    ?assertMatch(true, SpanCtx1#span_ctx.is_recording),

    %% hack to set the created span as local not sampled
    ?set_current_span(SpanCtx1#span_ctx{is_remote=false,
                                        is_recording=false,
                                        trace_flags=0}),

    %% local not sampled should default to not sampled
    SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-2">>, #{}),
    ?assertMatch(false, SpanCtx2#span_ctx.is_recording),

    %% hack to set the created span as local not sampled
    ?set_current_span(SpanCtx1#span_ctx{is_remote=false,
                                        is_recording=true,
                                        trace_flags=1}),

    %% local not sampled should default to not sampled
    SpanCtx3 = otel_tracer:start_span(Tracer, <<"span-3">>, #{}),
    ?assertMatch(true, SpanCtx3#span_ctx.is_recording),

    %% hack to set the created span as remote not sampled
    ?set_current_span(SpanCtx1#span_ctx{is_remote=true,
                                        is_recording=false,
                                        trace_flags=0}),

    %% remote not sampled should default to not sampled
    SpanCtx4 = otel_tracer:start_span(Tracer, <<"span-4">>, #{}),
    ?assertMatch(false, SpanCtx4#span_ctx.is_recording),

    %% hack to set the created span as remote not sampled
    ?set_current_span(SpanCtx1#span_ctx{is_remote=true,
                                        is_recording=true,
                                        trace_flags=1}),

    %% remote not sampled should default to not sampled
    SpanCtx5 = otel_tracer:start_span(Tracer, <<"span-5">>, #{}),
    ?assertMatch(true, SpanCtx5#span_ctx.is_recording),

    %% hack to set the created span as local not sampled but recording
    ?set_current_span(SpanCtx1#span_ctx{is_remote=false,
                                        is_recording=true,
                                        trace_flags=0}),

    %% local not sampled but is recorded should default to sampled
    SpanCtx6 = otel_tracer:start_span(Tracer, <<"span-6">>, #{}),
    ?assertMatch(false, SpanCtx6#span_ctx.is_recording),

    ok.

root_span_sampling_always_off(_Config) ->
    Tracer = opentelemetry:get_tracer(),

    Sampler = otel_sampler:setup(always_off),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{sampler => Sampler}),
    ?assertMatch(false, SpanCtx1#span_ctx.is_recording),
    ?assertMatch(0, SpanCtx1#span_ctx.trace_flags),

    otel_tracer:set_current_span(SpanCtx1),
    SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-2">>, #{}),
    ?assertMatch(false, SpanCtx2#span_ctx.is_recording),
    ?assertMatch(0, SpanCtx2#span_ctx.trace_flags),

    ok.

root_span_sampling_always_on(_Config) ->
    Tracer = opentelemetry:get_tracer(),

    Sampler = otel_sampler:setup(always_on),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{sampler => Sampler}),
    ?assertMatch(true, SpanCtx1#span_ctx.is_recording),
    ?assertMatch(1, SpanCtx1#span_ctx.trace_flags),

    otel_tracer:set_current_span(SpanCtx1),
    SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-2">>, #{}),
    ?assertMatch(true, SpanCtx2#span_ctx.is_recording),
    ?assertMatch(1, SpanCtx1#span_ctx.trace_flags),

    ok.

record_but_not_sample(Config) ->
    ct:comment("Test that a Span that the sampler returns RECORD_ONLY for gets created"
               "as a valid recorded span but is not sent to the exporter."),
    Tid = ?config(tid, Config),

    Sampler = otel_sampler:new({static_sampler, #{<<"span-record-and-sample">> => ?RECORD_AND_SAMPLE,
                                                    <<"span-record">> => ?RECORD_ONLY}}),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-record-and-sample">>, #{sampler => Sampler}),
    ?assertEqual(true, SpanCtx1#span_ctx.is_recording),
    ?assertEqual(1, SpanCtx1#span_ctx.trace_flags),

    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-record">>, #{sampler => Sampler}),
    ?assertEqual(true, SpanCtx2#span_ctx.is_recording),
    ?assertEqual(0, SpanCtx2#span_ctx.trace_flags),

    ?set_current_span(SpanCtx2),
    otel_span:end_span(SpanCtx2),

    otel_span:end_span(SpanCtx1),

    assert_all_exported(Tid, [SpanCtx1]),

    %% span-2 is recorded but not sampled, so should not show up in the export table
    assert_not_exported(Tid, SpanCtx2),

    ok.

record_exception_works(Config) ->
    Tid = ?config(tid, Config),
    SpanCtx = ?start_span(<<"span-1">>),
    try throw(my_error) of
        _ ->
        ok
    catch
        Class:Term:Stacktrace ->
            otel_span:record_exception(SpanCtx, Class, Term, Stacktrace, [{"some-attribute", "value"}]),
            ?end_span(SpanCtx),
            [Span] = assert_exported(Tid, SpanCtx),
            [Event] = Span#span.events,
            ?assertEqual(<<"exception">>, Event#event.name),
            ?assertEqual([{<<"exception.type">>, <<"throw:my_error">>},
                          {<<"exception.stacktrace">>, list_to_binary(io_lib:format("~p", [Stacktrace], [{chars_limit, 50}]))},
                          {"some-attribute","value"}],
                         Event#event.attributes),
            ok
    end.

record_exception_with_message_works(Config) ->
    Tid = ?config(tid, Config),
    SpanCtx = ?start_span(<<"span-1">>),
    try throw(my_error) of
        _ ->
        ok
    catch
        Class:Term:Stacktrace ->
            otel_span:record_exception(SpanCtx, Class, Term, "My message", Stacktrace, [{"some-attribute", "value"}]),
            ?end_span(SpanCtx),
            [Span] = assert_exported(Tid, SpanCtx),
            [Event] = Span#span.events,
            ?assertEqual(<<"exception">>, Event#event.name),
            ?assertEqual([{<<"exception.type">>, <<"throw:my_error">>},
                          {<<"exception.stacktrace">>, list_to_binary(io_lib:format("~p", [Stacktrace], [{chars_limit, 50}]))},
                          {<<"exception.message">>, "My message"},
                          {"some-attribute","value"}],
                         Event#event.attributes),
            ok
    end.

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
     iolist_to_binary([<<"00">>, "-", EncodedTraceId,"-", EncodedSpanId, "-", <<"01">>])}];
trace_context(b3, EncodedTraceId, EncodedSpanId) ->
    [{<<"X-B3-Sampled">>, <<"1">>},
     {<<"X-B3-SpanId">>, iolist_to_binary(EncodedSpanId)},
     {<<"X-B3-TraceId">>, iolist_to_binary(EncodedTraceId)}].
