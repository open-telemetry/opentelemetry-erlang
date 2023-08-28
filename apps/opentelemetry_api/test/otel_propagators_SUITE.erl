-module(otel_propagators_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include("otel_tracer.hrl").
-include("opentelemetry.hrl").

-define(assertListsMatch(List1, List2), ?assertEqual(lists:sort(List1), lists:sort(List2))).

%% Headers for: #span_ctx{trace_id=21267647932558653966460912964485513216,
%%                        span_id=1152921504606846976,
%%                        trace_flags=0}
-define(EXPECTED_HEADERS, [{<<"traceparent">>,
                            <<"00-10000000000000000000000000000000-1000000000000000-00">>}]).

all() ->
    [tracestate, rewrite, {group, absence_of_an_installed_sdk}, custom_propagator].

groups() ->
    %% Tests of Behavior of the API in the absence of an installed SDK
    %% https://github.com/open-telemetry/opentelemetry-specification/blob/82865fae64e7b30ee59906d7c4d25a48fe446563/specification/trace/api.md#behavior-of-the-api-in-the-absence-of-an-installed-sdk
    [{absence_of_an_installed_sdk, [shuffle, parallel], [invalid_span_no_sdk_propagation,
                                                         nonrecording_no_sdk_propagation]}].

init_per_suite(Config) ->
    application:load(opentelemetry_api),

    %% this used to be done in the SDK `stop'
    %% need it here in case SDK tests were run before these
    opentelemetry:set_default_tracer({otel_tracer_noop, []}),
    CompositePropagator = otel_propagator_text_map_composite:create([custom_propagator,
                                                                     otel_propagator_trace_context]),
    opentelemetry:set_text_map_propagator(CompositePropagator),
    Config.

end_per_suite(_Config) ->
    ok.

tracestate(_Config) ->
    Tracestate = otel_tracestate:new(),
    Tracestate1 = otel_tracestate:add("a", "b", Tracestate),
    Tracestate2 = otel_tracestate:add("c", "d", Tracestate1),
    Tracestate3 = otel_tracestate:add(e, "f", Tracestate2),
    Tracestate4 = otel_tracestate:add("e", f, Tracestate3),

    ?assertEqual("d", otel_tracestate:get("c", Tracestate4)),

    Tracestate5 = otel_tracestate:update("c", "g", Tracestate4),
    ?assertEqual("g", otel_tracestate:get("c", Tracestate5)),

    ?assertEqual("", otel_tracestate:get("e", Tracestate5)),
    Tracestate6 = otel_tracestate:add("e", "f", Tracestate5),
    Tracestate7 = otel_tracestate:remove("e", Tracestate6),
    ?assertEqual("", otel_tracestate:get("e", Tracestate7)),

    TracestateEncoded = otel_tracestate:encode_header(Tracestate7),
    ?assertEqual(<<"c=g,a=b">>, TracestateEncoded),

    Tracestate8 = otel_tracestate:update("a", "h", Tracestate7),
    ?assertEqual("h", otel_tracestate:get("a", Tracestate8)),

    TracestateEncoded1 = otel_tracestate:encode_header(Tracestate8),
    ?assertEqual(<<"a=h,c=g">>, TracestateEncoded1),

    ok.

rewrite(_Config) ->
    otel_ctx:clear(),

    RecordingSpanCtx = #span_ctx{trace_id=21267647932558653966460912964485513216,
                                 span_id=1152921504606846976,
                                 is_valid=true,
                                 is_recording=true},
    otel_tracer:set_current_span(RecordingSpanCtx),

    Ctx = otel_ctx:get_current(),
    ?assertMatch([{<<"traceparent">>,
                   <<"00-10000000000000000000000000000000-1000000000000000-00">>}],
                 otel_propagator_trace_context:inject(Ctx, [],
                                                      fun otel_propagator_text_map:default_carrier_set/3, [])),

    ?assertMatch([{<<"traceparent">>,
                   <<"00-10000000000000000000000000000000-1000000000000000-00">>}],
                 otel_propagator_text_map:inject([])),

    ?assertMatch(<<"c=d,a=b">>,
                 otel_propagator_text_map:default_carrier_get(<<"tracestate">>,
                                                              [{<<"tracestate">>,<<"c=d">>},
                                                               {<<"traceparent">>,
                                                                <<"00-10000000000000000000000000000000-1000000000000000-00">>},
                                                               {<<"tracestate">>,<<"a=b">>}])),

    Tracestate = otel_tracestate:new([{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}]),
    TracestateEncoded = otel_tracestate:encode_header(Tracestate),

    otel_propagator_text_map:extract([{<<"tracestate">>,<<"a=b,c=d">>},
                                      {<<"traceparent">>,
                                       <<"00-10000000000000000000000000000000-1000000000000000-00">>},
                                      {<<"tracestate">>, TracestateEncoded}]),
    ?assertEqual(RecordingSpanCtx#span_ctx{is_recording=false,
                                           tracestate=Tracestate,
                                           is_remote=true}, otel_tracer:current_span_ctx()),

    %% should not fail on empty Carrier
    ?assertMatch(Ctx,
                 otel_propagator_trace_context:extract(Ctx, [],
                                                       fun otel_propagator_text_map:default_carrier_keys/1,
                                                       fun otel_propagator_text_map:default_carrier_get/2, [])),

    ok.

invalid_span_no_sdk_propagation(_Config) ->
    ct:comment("Test that a start_span called with an invalid span parent "
               "and no SDK results in the same invalid span as the child"),
    otel_ctx:clear(),

    InvalidSpanCtx = #span_ctx{trace_id=0,
                               span_id=0,
                               trace_flags=0,
                               is_valid=false,
                               is_recording=false},
    otel_tracer:set_current_span(InvalidSpanCtx),
    ?assertEqual(InvalidSpanCtx, otel_tracer:current_span_ctx()),
    ?with_span(<<"span-1">>, #{}, fun(_) ->
                                          %% parent is recording so a new span_id should be used
                                          ?assertEqual(InvalidSpanCtx, otel_tracer:current_span_ctx())
                                  end),
    ?assertEqual(InvalidSpanCtx, otel_tracer:current_span_ctx()),

    BinaryHeaders = otel_propagator_text_map:inject([]),
    %% invalid span contexts are skipped when injecting
    ?assertMatch([], BinaryHeaders),

    ok.

nonrecording_no_sdk_propagation(_Config) ->
    ct:comment("Test that a start_span called with an valid non-recording span parent "
               "and no SDK results in the same span_ctx as the child"),

    otel_ctx:clear(),

    NonRecordingSpanCtx = #span_ctx{trace_id=21267647932558653966460912964485513216,
                                    span_id=1152921504606846976,
                                    is_valid=true,
                                    is_recording=false},
    ?set_current_span(NonRecordingSpanCtx),
    BinaryHeaders = otel_propagator_text_map:inject([]),
    %% is_recording will always be false in extracted `span_ctx'
    otel_propagator_text_map:extract(BinaryHeaders),

    %% after being extracted `is_remote' will be set to `true'
    RemoteSpanCtx = NonRecordingSpanCtx#span_ctx{is_remote=true},

    ?assertEqual(RemoteSpanCtx, otel_tracer:current_span_ctx()),
    ?with_span(<<"span-1">>, #{}, fun(_) ->
                                          %% parent is non-recording so it should be returned
                                          %% as the "new" span
                                          ?assertEqual(RemoteSpanCtx, otel_tracer:current_span_ctx())
                                  end),
    ?assertEqual(RemoteSpanCtx, otel_tracer:current_span_ctx()),

    BinaryHeaders = otel_propagator_text_map:inject([]),
    ?assertMatch(?EXPECTED_HEADERS, BinaryHeaders),

    ok.

custom_propagator(_Config) ->
    Something = <<"hello">>,
    custom_propagator:add_to_context(Something),

    Headers = otel_propagator_text_map:inject([{<<"existing-header">>, <<"I exist">>}]),

    ?assertListsMatch([{<<"something-header-id">>, Something},
                       {<<"existing-header">>, <<"I exist">>}], Headers),

    %% clear context to test extraction
    otel_ctx:clear(),

    %% make header keys uppercase to validate the extractor is case insensitive
    BinaryHeaders = [{string:uppercase(Key), iolist_to_binary(Value)} || {Key, Value} <- Headers],
    otel_propagator_text_map:extract(BinaryHeaders),

    ?assertEqual(Something, custom_propagator:context_content()),

    ok.
