-module(otel_propagation_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include("otel_tracer.hrl").
-include("otel_span.hrl").
-include("otel_test_utils.hrl").
-include("otel_sampler.hrl").
-include("otel_span_ets.hrl").

all() ->
    [override_propagators,
     {group, w3c},
     {group, b3multi},
     {group, b3}].

groups() ->
    [{w3c, [], [propagation]},
     {b3multi, [], [propagation]},
     {b3, [], [propagation]}].

init_per_suite(Config) ->
    application:load(opentelemetry),
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config.

end_per_suite(_Config) ->
    application:unload(opentelemetry),
    ok.

init_per_group(Propagator, Config) when Propagator =:= w3c ;
                                        Propagator =:= b3multi ;
                                        Propagator =:= b3 ->
    %% start in group as well since we must stop it after each group run
    {ok, _} = application:ensure_all_started(opentelemetry),

    case Propagator of
        w3c ->
            CompositePropagator = otel_propagator_text_map_composite:create([baggage, trace_context]),
            opentelemetry:set_text_map_propagator(CompositePropagator);
        b3multi ->
            CompositePropagator = otel_propagator_text_map_composite:create([baggage, b3multi]),
            opentelemetry:set_text_map_propagator(CompositePropagator);
        b3 ->
            CompositePropagator = otel_propagator_text_map_composite:create([baggage, b3]),
            opentelemetry:set_text_map_propagator(CompositePropagator)
    end,

    [{propagator, Propagator} | Config].

end_per_group(_, _Config) ->
    _ = application:stop(opentelemetry).

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

    Headers = otel_propagator_text_map:inject([{<<"existing-header">>, <<"I exist">>}]),

    EncodedTraceId = io_lib:format("~32.16.0b", [TraceId]),
    EncodedSpanId = io_lib:format("~16.16.0b", [SpanId]),

    ?assertListsEqual([{<<"baggage">>, <<"key-2=value-2;metadata;md-k-1=md-v-1,key-1=value%3D1">>},
                       {<<"existing-header">>, <<"I exist">>} |
                       trace_context(Propagator, EncodedTraceId, EncodedSpanId)], Headers),

    otel_span:end_span(SpanCtx),

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
    otel_propagator_text_map:extract(BinaryHeaders),

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

override_propagators(_Config) ->
    SpanCtx=#span_ctx{} = ?start_span(<<"span-1">>),
    ?set_current_span(SpanCtx),

    ?assertMatch(#span_ctx{trace_flags=1}, ?current_span_ctx),
    ?assertMatch(#span_ctx{is_recording=true}, ?current_span_ctx),


    otel_baggage:set("key-1", <<"value=1">>, []),
    %% TODO: should the whole baggage entry be dropped if metadata is bad?
    %% drop bad metadata (the `1').
    otel_baggage:set(<<"key-2">>, <<"value-2">>, [<<"metadata">>, 1, {<<"md-k-1">>, <<"md-v-1">>}]),
    %% drop baggage with bad value
    otel_baggage:set(<<"key-3">>, value3),

    Headers = otel_propagator_text_map:inject({otel_propagator_baggage, []}, [{<<"existing-header">>, <<"I exist">>}]),

    %% the manually set propagators does not include trace_context or b3multi
    %% so header must only have the existing-header and the baggage
    ?assertListsEqual([{<<"baggage">>, <<"key-2=value-2;metadata;md-k-1=md-v-1,key-1=value%3D1">>},
                       {<<"existing-header">>, <<"I exist">>}], Headers),

    otel_span:end_span(SpanCtx),

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
    otel_propagator_text_map:extract({otel_propagator_baggage, []}, BinaryHeaders),

    ?assertEqual(#{<<"key-1">> => {<<"value=1">>, []},
                   <<"key-2">> => {<<"value-2">>, [<<"metadata">>, {<<"md-k-1">>, <<"md-v-1">>}]}},
                 otel_baggage:get_all()),

    %% no trace extractor used so current span ctx remains undefined
    ?assertEqual(undefined, ?current_span_ctx),

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
     iolist_to_binary([<<"00">>, "-", EncodedTraceId,"-", EncodedSpanId, "-", <<"01">>])}];
trace_context(b3multi, EncodedTraceId, EncodedSpanId) ->
    [{<<"X-B3-Sampled">>, <<"1">>},
     {<<"X-B3-SpanId">>, iolist_to_binary(EncodedSpanId)},
     {<<"X-B3-TraceId">>, iolist_to_binary(EncodedTraceId)}];
trace_context(b3, EncodedTraceId, EncodedSpanId) ->
    [{<<"b3">>,
      iolist_to_binary([EncodedTraceId, "-", EncodedSpanId, "-", <<"1">>])}].
