-module(opentelemetry_api_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").

all() ->
    [noop_tracer, update_span_data].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

noop_tracer(_Config) ->
    %% start a span and 2 children
    SpanCtx1 = otel:start_span(<<"span-1">>),
    SpanCtx2 = otel:start_span(<<"span-2">>),
    SpanCtx3 = otel:start_span(<<"span-3">>),

    %% end the 3rd span
    ?assertMatch(SpanCtx3, otel:current_span_ctx()),
    otel:end_span(),

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

    %% always returns a noop span
    ?assertMatch(SpanCtx1, otel:current_span_ctx()).

%% just shouldn't crash
update_span_data(_Config) ->
    Links = [#link{trace_id=0,
                   span_id=0,
                   attributes=[],
                   tracestate=[]}],

    SpanCtx1 = otel:start_span(<<"span-1">>, #{links => Links}),
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

    ok.
