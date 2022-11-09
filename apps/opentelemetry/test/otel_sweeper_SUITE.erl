%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(otel_sweeper_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("otel_test_utils.hrl").
-include("otel_span.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-include("../src/otel_span_ets.hrl").

all() ->
    [storage_size,
     drop,
     end_span,
     failed_attribute_and_end_span].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Config.

end_per_suite(_Config) ->
    application:unload(opentelemetry),
    application:stop(opentelemetry),
    ok.

init_per_testcase(storage_size, Config) ->
    application:set_env(opentelemetry, sweeper, #{interval => 250,
                                                  strategy => end_span,
                                                  span_ttl => 500,
                                                  storage_size => 100}),
    application:set_env(opentelemetry, tracer, otel_tracer_default),
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1,
                                                                            exporter => {otel_exporter_pid, self()}}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),

    Config;
init_per_testcase(Type, Config) ->
    application:set_env(opentelemetry, sweeper, #{interval => 250,
                                                  strategy => Type,
                                                  span_ttl => 500}),
    application:set_env(opentelemetry, tracer, otel_tracer_default),
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1,
                                                                             exporter => {otel_exporter_pid, self()}}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),

    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opentelemetry),
    ok.

storage_size(_Config) ->
    SpanName1 = <<"span-1">>,
    SpanCtx = ?start_span(SpanName1),
    ?set_current_span(SpanCtx),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpanCtx = ?start_span(ChildSpanName1),
    ?set_current_span(ChildSpanCtx),

    ?assertEqual(SpanCtx#span_ctx.trace_id, ChildSpanCtx#span_ctx.trace_id),

    [ChildSpanData] = ets:lookup(?SPAN_TAB, ChildSpanCtx#span_ctx.span_id),
    ?assertEqual(ChildSpanName1, ChildSpanData#span.name),
    ?assertEqual(SpanCtx#span_ctx.span_id, ChildSpanData#span.parent_span_id),

    %% wait until the sweeper sweeps away the parent span
    ?UNTIL(ets:tab2list(?SPAN_TAB) =:= []),

    %% sleep long enough that the exporter would have run again for sure
    timer:sleep(10),

    %% should be no exported spans
    ?assertEqual(no_span, receive
                              {span, #span{name=N}} when N =:= SpanName1 ->
                                  gotel_span
                          after
                              0 ->
                                  no_span
                          end).

drop(_Config) ->
    SpanName1 = <<"span-1">>,
    SpanCtx = ?start_span(SpanName1),
    otel_tracer:set_current_span(SpanCtx),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpanCtx = ?start_span(ChildSpanName1),
    otel_tracer:set_current_span(ChildSpanCtx),

    [ChildSpanData] = ets:lookup(?SPAN_TAB, ChildSpanCtx#span_ctx.span_id),
    ?assertEqual(ChildSpanName1, ChildSpanData#span.name),
    ?assertEqual(SpanCtx#span_ctx.span_id, ChildSpanData#span.parent_span_id),

    otel_span:end_span(ChildSpanCtx),

    %% wait until the sweeper sweeps away the parent span
    ?UNTIL(ets:tab2list(?SPAN_TAB) =:= []),

    otel_span:end_span(SpanCtx),

    receive
        {span, S=#span{name=Name}} when Name =:= ChildSpanName1 ->
            %% Verify the end time and duration are set when the span was finished
            ?assertMatch(ST when is_integer(ST), S#span.start_time),
            ?assertMatch(ST when is_integer(ST), S#span.end_time)
    after
      1000 -> ct:fail("Do not received any message after 1s")
    end,

    %% sleep long enough that the exporter would have run again for sure
    timer:sleep(10),

    %% should be no exported span for span-1
    ?assertEqual(no_span, receive
                              {span, #span{name=N}} when N =:= SpanName1 ->
                                  gotel_span
                          after
                              0 ->
                                  no_span
                          end).

end_span(_Config) ->
    SpanName1 = <<"span-1">>,
    SpanCtx = ?start_span(SpanName1),
    otel_tracer:set_current_span(SpanCtx),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpanCtx = ?start_span(ChildSpanName1),
    otel_tracer:set_current_span(ChildSpanCtx),

    otel_span:end_span(SpanCtx),

    %% wait until the sweeper sweeps away the parent span
    ?UNTIL(ets:tab2list(?SPAN_TAB) =:= []),

    lists:foreach(fun(Name) ->
                          receive
                              {span, S=#span{name=Name}} ->
                                  %% Verify the end time and duration are set when the span was finished
                                  ?assertMatch(ST when is_integer(ST), S#span.start_time),
                                  ?assertMatch(ST when is_integer(ST), S#span.end_time)
                          after
                            1000 -> ct:fail("Do not received any message after 1s")
                          end
                  end, [SpanName1, ChildSpanName1]).

failed_attribute_and_end_span(_Config) ->
    SpanName1 = <<"span-1">>,
    SpanCtx = ?start_span(SpanName1),
    otel_tracer:set_current_span(SpanCtx),

    ChildSpanName1 = <<"child-span-1">>,
    ChildSpanCtx = ?start_span(ChildSpanName1),
    otel_tracer:set_current_span(ChildSpanCtx),

    [ChildSpanData] = ets:lookup(?SPAN_TAB, ChildSpanCtx#span_ctx.span_id),
    ?assertEqual(ChildSpanName1, ChildSpanData#span.name),
    ?assertEqual(SpanCtx#span_ctx.span_id, ChildSpanData#span.parent_span_id),

    otel_span:end_span(ChildSpanData),

    %% wait until the sweeper sweeps away the parent span
    ?UNTIL(ets:tab2list(?SPAN_TAB) =:= []),

    receive
        {span, S=#span{name=Name,
                       attributes=Attributes}} when Name =:= SpanName1 ->
            %% should have attribute finished_by_sweeper
            ?assertMatch(#{<<"finished_by_sweeper">> := true}, otel_attributes:map(Attributes)),

            %% Verify the end time and duration are set when the span was finished
            ?assertMatch(ST when is_integer(ST), S#span.start_time),
            ?assertMatch(ST when is_integer(ST), S#span.end_time)
    after
      1000 -> ct:fail("Do not received any message after 1s")
    end.
