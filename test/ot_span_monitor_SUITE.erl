%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(ot_span_monitor_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("ot_test_utils.hrl").
-include("ot_span.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/tracer.hrl").

-include("../src/ot_span_ets.hrl").

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
    application:set_env(opentelemetry, processors, [{ot_batch_processor, #{scheduled_delay_ms => 1}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    ot_batch_processor:set_exporter(ot_exporter_tab, Tid),
    [{exit_type, ExitType}, {tid, Tid} | Config].

end_per_testcase(_, _Config) ->
    _ = application:stop(opentelemetry),
    ok.

monitor_pid(Config) ->
    process_flag(trap_exit, true),
    ExitType = ?config(exit_type, Config),
    Tid = ?config(tid, Config),

    Attr1 = <<"attr-1">>,
    AttrValue1 = <<"attr-value-1">>,

    Pid = erlang:spawn_link(fun() ->
                                    _SpanCtx1 = ?start_span(<<"span-1">>),
                                    SpanCtx2 = ?start_span(<<"span-2">>, #{monitor => true}),

                                    ?assertMatch(SpanCtx2, ?current_span_ctx),

                                    ?set_attribute(Attr1, AttrValue1),

                                    erlang:exit(ExitType)
                            end),

    receive
        {'EXIT', Pid, Reason} when Reason =:= ExitType ->
            %% process is down now check that there are 2 ended spans in the table
            %% even though only span-2 had monitor set to true span-1 is ended as
            %% well since it is based on the process of the span
            ?UNTIL(2 =:= ets:info(Tid, size)),

            Spans = ets:tab2list(Tid),

            Span2 = lists:keyfind(<<"span-2">>, #span.name, Spans),
            ?assertEqual([{Attr1, AttrValue1}], Span2#span.attributes),
            ok
    end.
