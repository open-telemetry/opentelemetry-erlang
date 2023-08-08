%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(otel_span_monitor_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("otel_test_utils.hrl").
-include("otel_span.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-include("otel_span_ets.hrl").

all() ->
    [{group, normal}, {group, abnormal}].

groups() ->
    [{normal, [], [monitor_pid]},
     {abnormal, [], [monitor_pid]}].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Config.

end_per_suite(_Config) ->
    application:unload(opentelemetry),
    ok.

init_per_testcase(ExitType, Config) ->
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{exporter => {otel_exporter_pid, self()},
                                                                             scheduled_delay_ms => 1}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    [{exit_type, ExitType} | Config].

end_per_testcase(_, _Config) ->
    _ = application:stop(opentelemetry),
    ok.

monitor_pid(Config) ->
    process_flag(trap_exit, true),
    ExitType = ?config(exit_type, Config),

    SpanName1 = <<"span-1">>,
    SpanName2 = <<"span-2">>,

    Attr1 = <<"attr-1">>,
    AttrValue1 = <<"attr-value-1">>,

    SpanCtx1 = ?start_span(SpanName1, #{monitor => true}),
    ?set_current_span(SpanCtx1),
    Ctx = otel_ctx:get_current(),
    ?add_event('some event on span 1', #{a => 1}),

    Pid1 = self(),
    Pid2 = erlang:spawn_link(fun() ->
                                    SpanCtx2 = ?start_span(Ctx, SpanName2, #{monitor => true}),
                                    ?set_current_span(SpanCtx2),
                                    ?assertMatch(SpanCtx2, ?current_span_ctx),

                                    ?add_event('some event on span 2', #{a => 2}),
                                    ?set_attribute(Attr1, AttrValue1),

                                    erlang:exit(ExitType)
                            end),

    otel_span:end_span(SpanCtx1),

    receive
        {'EXIT', Pid2, Reason} when Reason =:= ExitType ->
            receive
                {span, #span{name=SpanName2,
                             parent_span_id=ParentSpanId2,
                             pid=SpanPid2,
                             attributes=SpanAttributes2,
                             events=Events2}} ->
                    ?assertEqual(SpanCtx1#span_ctx.span_id, ParentSpanId2),
                    ?assertMatch([#event{name='process died'},
                                  #event{name='some event on span 2'}], otel_events:list(Events2)),
                    ?assertEqual(Pid2, SpanPid2),
                    ?assertEqual(#{Attr1 => AttrValue1,
                                   finished_by_monitor => true}, otel_attributes:map(SpanAttributes2)),
                    receive
                        {span, #span{name=SpanName1,
                                     pid=SpanPid1,
                                     attributes=SpanAttributes1,
                                     events=Events1}} ->
                            ?assertMatch([#event{name='some event on span 1'}], otel_events:list(Events1)),
                            ?assertEqual(Pid1, SpanPid1),
                            ?assertEqual(#{}, otel_attributes:map(SpanAttributes1))
                    after
                        1000 ->
                            ct:fail(span_1_timeout)
                    end
            after
                1000 ->
                    ct:fail(span_2_timeout)
            end,
            ok
    after
        1000 ->
            ct:fail(monitor_timeout)
    end.
