%% sets the API without an SDK installed
%% basic propagation must work without an SDK and this is not yet implemented
-module(opentelemetry_api_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").
-include("otel_tracer.hrl").

all() ->
    [noop_tracer, update_span_data, noop_with_span, can_create_link_from_span].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

can_create_link_from_span(_Config) ->
    %% start a span to create a link to
    SpanCtx = ?start_span(<<"span-1">>),

    %% extract individual values from span context
    TraceId = otel_span:trace_id(SpanCtx),
    SpanId = otel_span:span_id(SpanCtx),
    Tracestate = otel_span:tracestate(SpanCtx),

    %% end span, so there's no current span set
    ?end_span(),

    Attributes = [{<<"attr-1">>, <<"value-1">>}],

    ?assertMatch(undefined, opentelemetry:link(undefined)),
    ?assertMatch(undefined, opentelemetry:link(undefined, Attributes)),

    ?assertMatch(#link{trace_id=TraceId,
                       span_id=SpanId,
                       attributes=Attributes,
                       tracestate=Tracestate},
                 opentelemetry:link(TraceId, SpanId, Attributes, Tracestate)),

    ?assertMatch(#link{trace_id=TraceId,
                       span_id=SpanId,
                       attributes=[],
                       tracestate=Tracestate},
                 opentelemetry:link(SpanCtx)),

    ?assertMatch(#link{trace_id=TraceId,
                       span_id=SpanId,
                       attributes=Attributes,
                       tracestate=Tracestate},
                 opentelemetry:link(SpanCtx, Attributes)),

    ?assertMatch([#link{trace_id=TraceId,
                        span_id=SpanId,
                        attributes=Attributes,
                        tracestate=Tracestate},
                  #link{trace_id=TraceId,
                        span_id=SpanId,
                        attributes=[],
                        tracestate=Tracestate}],
                 opentelemetry:links([undefined, {SpanCtx, Attributes}, SpanCtx])).


noop_tracer(_Config) ->
    %% start a span and 2 children
    SpanCtx1 = ?start_span(<<"span-1">>),
    SpanCtx2 = ?start_span(<<"span-2">>),
    SpanCtx3 = ?start_span(<<"span-3">>),

    %% set to current and then end the 3rd span
    ?set_current_span(SpanCtx3),
    ?assertMatch(SpanCtx3, ?current_span_ctx),
    ?end_span(SpanCtx3),

    ?set_current_span(SpanCtx2),
    ?assertMatch(SpanCtx2, ?current_span_ctx),

    %% start another child of the 2nd span
    SpanCtx4 = ?start_span(<<"span-4">>),

    ?set_current_span(SpanCtx4),
    ?assertMatch(SpanCtx4, ?current_span_ctx),

    %% end 4th span
    ?end_span(SpanCtx4),

    ?set_current_span(SpanCtx2),
    ?assertMatch(SpanCtx2, ?current_span_ctx),

    %% end 2th span
    ?end_span(),

    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    %% end first and no span should be current ctx
    ?end_span(),

    %% 1st span is ended but still current
    ?assertMatch(SpanCtx1, ?current_span_ctx).

%% just shouldn't crash
update_span_data(_Config) ->
    Links = [#link{trace_id=0,
                   span_id=0,
                   attributes=[],
                   tracestate=[]}],

    SpanCtx1 = ?start_span(<<"span-1">>, #{links => Links}),
    ?set_current_span(SpanCtx1),

    ?set_attribute(<<"key-1">>, <<"value-1">>),

    Events = opentelemetry:events([{opentelemetry:timestamp(),
                                    <<"timed-event-name">>, []}]),
    Status = otel_http_status:to_status(200),
    ?assertMatch(#status{code = ?OTEL_STATUS_OK, message = <<"Ok">>}, Status),

    otel_span:set_status(SpanCtx1, Status),
    otel_span:add_events(SpanCtx1, Events),

    ?assertMatch(SpanCtx1, ?current_span_ctx),
    ?end_span(),

    ok.

noop_with_span(_Config) ->
    Tracer = opentelemetry:get_tracer(),
    ?assertMatch({otel_tracer_noop, _}, Tracer),

    Result = some_result,
    ?assertEqual(Result, otel_tracer:with_span(Tracer, <<"span1">>, fun(_) -> Result end)),
    ok.
