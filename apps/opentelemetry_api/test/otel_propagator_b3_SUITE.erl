-module(otel_propagator_b3_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include("otel_tracer.hrl").
-include("opentelemetry.hrl").

-define(assertListsEqual(List1, List2), ?assertEqual(lists:sort(List1), lists:sort(List2))).

all() ->
    [{group, b3},
     {group, b3multi}].

groups() ->
    ExtractTests = [extract, extract_sampling, extract_invalid_trace_id, extract_invalid_span_id],

    [{b3, [], [inject_single | ExtractTests]},
     {b3multi, [], [inject_multi | ExtractTests]}].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Propagator, Config) ->
    CompositePropagator = otel_propagator_text_map_composite:create([Propagator]),
    opentelemetry:set_text_map_propagator(CompositePropagator),
    [{propagator, CompositePropagator} | Config].

end_per_group(_, _Config) ->
    ok.

extract(_Config) ->
    % Single: Common B3 format
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ae38e-1">>}]),
    ?assertMatch(#span_ctx{trace_id = <<0,0,0,140,61,239,177,237,185,132,254,42,199,28,113,199>>,
                           span_id = <<0,7,229,25,110,42,227,142>>,
                           trace_flags=1}, otel_tracer:current_span_ctx()),

    % Single: B3 format with 16 character trace ID
    run_extract([{<<"b3">>, <<"0007e5196e2ae38e-0007e5196e2ae38e-1">>}]),
    ?assertMatch(#span_ctx{trace_id = <<0,7,229,25,110,42,227,142>>,
                           span_id = <<0,7,229,25,110,42,227,142>>,
                           trace_flags=1}, otel_tracer:current_span_ctx()),

    % Single: B3 format with parent span id
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ae38e-1-0101010101010101">>}]),
    ?assertMatch(#span_ctx{trace_id = <<0,0,0,140,61,239,177,237,185,132,254,42,199,28,113,199>>,
                           span_id = <<0,7,229,25,110,42,227,142>>,
                           trace_flags=1}, otel_tracer:current_span_ctx()),

    % Single: B3 format without trace flags
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ae38e">>}]),
    ?assertMatch(#span_ctx{trace_id = <<0,0,0,140,61,239,177,237,185,132,254,42,199,28,113,199>>,
                           span_id = <<0,7,229,25,110,42,227,142>>,
                           trace_flags=0}, otel_tracer:current_span_ctx()),

    % Multi: B3 format with 32 character trace ID
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>},
                 {<<"X-B3-Sampled">>, <<"1">>}]),
    ?assertMatch(#span_ctx{trace_id = <<0,0,0,140,61,239,177,237,185,132,254,42,199,28,113,199>>,
                           span_id = <<0,7,229,25,110,42,227,142>>,
                           trace_flags=1}, otel_tracer:current_span_ctx()),

    % Multi: B3 format with 16 character trace ID
    run_extract([{<<"X-B3-TraceId">>, <<"0007e5196e2ae38e">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>},
                 {<<"X-B3-Sampled">>, <<"1">>}]),
    ?assertMatch(#span_ctx{trace_id = <<0,7,229,25,110,42,227,142>>,
                           span_id = <<0,7,229,25,110,42,227,142>>,
                           trace_flags=1}, otel_tracer:current_span_ctx()),

    % Multi: B3 format without Sampled header
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>}]),
    ?assertMatch(#span_ctx{trace_id = <<0,0,0,140,61,239,177,237,185,132,254,42,199,28,113,199>>,
                           span_id = <<0,7,229,25,110,42,227,142>>,
                           trace_flags=0}, otel_tracer:current_span_ctx()),

    % Combined: Single header format is preferred when both are present
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ae38e-1">>},
                 {<<"X-B3-TraceId">>, <<"75694700a50b5df51a28412ea368a592">>},
                 {<<"X-B3-SpanId">>, <<"b6431ccfe6d2ea8f">>},
                 {<<"X-B3-Sampled">>, <<"0">>}]),
    ?assertMatch(#span_ctx{trace_id = <<0,0,0,140,61,239,177,237,185,132,254,42,199,28,113,199>>,
                           span_id = <<0,7,229,25,110,42,227,142>>,
                           trace_flags=1}, otel_tracer:current_span_ctx()),

    ok.

extract_sampling(_Config) ->
    % Single: Sampling is set to 1
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ae38e-1">>}]),
    ?assertMatch(#span_ctx{trace_flags=1}, otel_tracer:current_span_ctx()),

    % Single: Sampling is set to debug
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ae38e-d">>}]),
    ?assertMatch(#span_ctx{trace_flags=1}, otel_tracer:current_span_ctx()),

    % Single: Sampling is set to 0
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ae38e-0">>}]),
    ?assertMatch(#span_ctx{trace_flags=0}, otel_tracer:current_span_ctx()),

    % Single: Sampling is set to invalid value
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ae38e-x">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Single: B3 format with only sampling decision (even though this is
    % supported according to Zipkin docs, this is tricky to implement and other
    % OTEL tracers don't seem to support it either).
    run_extract([{<<"b3">>, <<"0">>}]),
    ?assertMatch(undefined, otel_tracer:current_span_ctx()),

    % Multi: Sampling is set to 1
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>},
                 {<<"X-B3-Sampled">>, <<"1">>}]),
    ?assertMatch(#span_ctx{trace_flags=1}, otel_tracer:current_span_ctx()),

    % Multi: Sampling is set to true
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>},
                 {<<"X-B3-Sampled">>, <<"true">>}]),
    ?assertMatch(#span_ctx{trace_flags=1}, otel_tracer:current_span_ctx()),

    % Multi: Sampling is set to 0
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>},
                 {<<"X-B3-Sampled">>, <<"0">>}]),
    ?assertMatch(#span_ctx{trace_flags=0}, otel_tracer:current_span_ctx()),

    % Multi: Sampling is set to false
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>},
                 {<<"X-B3-Sampled">>, <<"false">>}]),
    ?assertMatch(#span_ctx{trace_flags=0}, otel_tracer:current_span_ctx()),

    % Multi: Sampling is set to invalid value
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>},
                 {<<"X-B3-Sampled">>, <<"invalid">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Multi: B3 format with only sampling decision (even though this is
    % supported according to Zipkin docs, this is tricky to implement and other
    % OTEL tracers don't seem to support it either).
    run_extract([{<<"X-B3-Sampled">>, <<"0">>}]),
    ?assertMatch(undefined, otel_tracer:current_span_ctx()),

    ok.

extract_invalid_trace_id(_Config) ->
    % Single: Shorter trace ID than expected
    run_extract([{<<"b3">>, <<"01-0007e5196e2ae38e-1">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Single: Longer trace ID than expected
    run_extract([{<<"b3">>, <<"0123456789012345678901234567890123456789-0007e5196e2ae38e-1">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Single: Non-hex trace ID
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71cyyyy-0007e5196e2ae38e-1">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Multi: Shorter trace ID than expected
    run_extract([{<<"X-B3-TraceId">>, <<"01">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Multi: Longer trace ID than expected
    run_extract([{<<"X-B3-TraceId">>, <<"0123456789012345678901234567890123456789">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Multi: Non-hex trace ID
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71cyyyy">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    ok.

extract_invalid_span_id(_Config) ->
    % Single: Shorter span ID than expected
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007-1">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Single: Longer span ID than expected
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ae38eaa-1">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Single: Non-hex span ID
    run_extract([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ayyyy-1">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Multi: Shorter span ID than expected
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                 {<<"X-B3-SpanId">>, <<"0007">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Multi: Longer span ID than expected
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38eaa">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    % Multi: Non-hex span ID
    run_extract([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                 {<<"X-B3-SpanId">>, <<"0007e5196e2ae38eyyyy">>}]),
    ?assertEqual(undefined, otel_tracer:current_span_ctx()),

    ok.

inject_single(_Config) ->
    otel_tracer:set_current_span(#span_ctx{trace_id = <<0,0,0,140,61,239,177,237,185,132,254,42,199,28,113,199>>,
                                           span_id = <<0,7,229,25,110,42,227,142>>,
                                           trace_flags=1}),
    Headers = otel_propagator_text_map:inject([]),

    ?assertListsEqual([{<<"b3">>, <<"0000008c3defb1edb984fe2ac71c71c7-0007e5196e2ae38e-1">>}], Headers),

    ok.

inject_multi(_Config) ->
    % Span with all fields
    otel_tracer:set_current_span(#span_ctx{trace_id = <<0,0,0,140,61,239,177,237,185,132,254,42,199,28,113,199>>,
                                           span_id = <<0,7,229,25,110,42,227,142>>,
                                           trace_flags=1}),
    Headers = otel_propagator_text_map:inject([]),
    ?assertListsEqual([{<<"X-B3-TraceId">>, <<"0000008c3defb1edb984fe2ac71c71c7">>},
                       {<<"X-B3-SpanId">>, <<"0007e5196e2ae38e">>},
                       {<<"X-B3-Sampled">>, <<"1">>}], Headers),

    ok.

run_extract(HeaderKeyValuePairs) ->
    otel_ctx:clear(),
    otel_propagator_text_map:extract(HeaderKeyValuePairs).
