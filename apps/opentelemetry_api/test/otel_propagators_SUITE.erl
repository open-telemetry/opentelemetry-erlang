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
    [{group, absence_of_an_installed_sdk}, custom_propagator].

groups() ->
    %% Tests of Behavior of the API in the absence of an installed SDK
    %% https://github.com/open-telemetry/opentelemetry-specification/blob/82865fae64e7b30ee59906d7c4d25a48fe446563/specification/trace/api.md#behavior-of-the-api-in-the-absence-of-an-installed-sdk
    [{absence_of_an_installed_sdk, [shuffle, parallel], [invalid_span_no_sdk_propagation,
                                                         recording_no_sdk_propagation,
                                                         nonrecording_no_sdk_propagation]}].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    {Extractor, Injector} = custom_propagator:propagators(),
    {W3CExtractor, W3CInjector} = otel_tracer_default:w3c_propagators(),
    opentelemetry:set_text_map_extractors([Extractor, W3CExtractor]),
    opentelemetry:set_text_map_injectors([Injector, W3CInjector]),

    Config.

end_per_suite(_Config) ->
    ok.

invalid_span_no_sdk_propagation(_Config) ->
    ct:comment("Test that a start_span called with an invalid span parent "
               "and no SDK results in the same invalid span as the child"),
    otel_ctx:clear(),

    InvalidSpanCtx = #span_ctx{trace_id=0,
                               span_id=0,
                               trace_flags=0,
                               tracestate=[],
                               is_valid=false,
                               is_recording=false},
    otel_tracer:set_current_span(InvalidSpanCtx),
    ?assertEqual(InvalidSpanCtx, otel_tracer:current_span_ctx()),
    ?with_span(<<"span-1">>, #{}, fun(_) ->
                                          %% parent is recording so a new span_id should be used
                                          ?assertEqual(InvalidSpanCtx, otel_tracer:current_span_ctx())
                                  end),
    ?assertEqual(InvalidSpanCtx, otel_tracer:current_span_ctx()),

    BinaryHeaders = otel_propagator:text_map_inject([]),
    %% invalid span contexts are skipped when injecting
    ?assertMatch([], BinaryHeaders),

    ok.

recording_no_sdk_propagation(_Config) ->
    ct:comment("Test that a start_span called with an valid recording span parent "
               "and no SDK results in a new span_id for the child"),
    otel_ctx:clear(),

    RecordingSpanCtx = #span_ctx{trace_id=21267647932558653966460912964485513216,
                                 span_id=1152921504606846976,
                                 is_recording=true},
    otel_tracer:set_current_span(RecordingSpanCtx),
    ?assertEqual(RecordingSpanCtx, otel_tracer:current_span_ctx()),
    ?with_span(<<"span-1">>, #{}, fun(_) ->
                                          %% parent is recording so a new span_id should be used
                                          ?assertNotEqual(RecordingSpanCtx, otel_tracer:current_span_ctx())
                                  end),
    ?assertEqual(RecordingSpanCtx, otel_tracer:current_span_ctx()),
    BinaryHeaders = otel_propagator:text_map_inject([]),
    ?assertMatch(?EXPECTED_HEADERS, BinaryHeaders),

    ok.

nonrecording_no_sdk_propagation(_Config) ->
    ct:comment("Test that a start_span called with an valid non-recording span parent "
               "and no SDK results in the same span_ctx as the child"),

    otel_ctx:clear(),

    NonRecordingSpanCtx = #span_ctx{trace_id=21267647932558653966460912964485513216,
                                    span_id=1152921504606846976,
                                    is_recording=false},
    ?set_current_span(NonRecordingSpanCtx),
    BinaryHeaders = otel_propagator:text_map_inject([]),
    %% is_recording will always be false in extracted `span_ctx'
    otel_propagator:text_map_extract(BinaryHeaders),

    %% after being extracted `is_remote' will be set to `true'
    RemoteSpanCtx = NonRecordingSpanCtx#span_ctx{is_remote=true},

    ?assertEqual(RemoteSpanCtx, otel_tracer:current_span_ctx()),
    ?with_span(<<"span-1">>, #{}, fun(_) ->
                                          %% parent is non-recording so it should be returned
                                          %% as the "new" span
                                          ?assertEqual(RemoteSpanCtx, otel_tracer:current_span_ctx())
                                  end),
    ?assertEqual(RemoteSpanCtx, otel_tracer:current_span_ctx()),

    BinaryHeaders = otel_propagator:text_map_inject([]),
    ?assertMatch(?EXPECTED_HEADERS, BinaryHeaders),

    ok.

custom_propagator(_Config) ->
    Something = <<"hello">>,
    custom_propagator:add_to_context(Something),

    Headers = otel_propagator:text_map_inject([{<<"existing-header">>, <<"I exist">>}]),

    ?assertListsMatch([{<<"something-header-id">>, Something},
                       {<<"existing-header">>, <<"I exist">>}], Headers),

    %% clear context to test extraction
    otel_ctx:clear(),

    %% make header keys uppercase to validate the extractor is case insensitive
    BinaryHeaders = [{string:uppercase(Key), iolist_to_binary(Value)} || {Key, Value} <- Headers],
    otel_propagator:text_map_extract(BinaryHeaders),

    ?assertEqual(Something, custom_propagator:context_content()),

    ok.
