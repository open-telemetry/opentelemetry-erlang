-module(otel_log_handler_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-define(OTEL_LOG_HANDLER, otel_log_handler).
-define(LOG_MSG, "otel_log_hanlder_SUITE test, please ignore it").
-define(TRACEPARENT, "00-0226551413cd73a554184b324c82ad51-b7ad6b71432023a2-01").

all() ->
    [crud_test,
     exporting_runner_timeout_test,
     check_max_queue_test,
     export_max_queue_size_success_test,
     scheduled_export_success_test,
     flush_on_terminate_test,
     sanity_end_to_end_test,
     overload_protection_slow_exporter_test,
     overload_protection_fast_exporter_test,
     late_writes_test,
     late_writes_on_terminate_test,
     no_exporter_test,
     retry_exporter_init_test,
     default_opentelemetry_exporter_test,
     exporter_exit_test,
     disabled_exporter_test].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(exporting_runner_timeout_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterConf = {?MODULE, #{sleep => infinity}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                scheduled_delay_ms => 10,
                                exporting_timeout_ms => 1}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(check_max_queue_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterConf = {?MODULE, #{sleep => timer:seconds(30)}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => timer:minutes(1),
                                scheduled_delay_ms => timer:minutes(10),
                                max_queue_size => 10}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(export_max_queue_size_success_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterConf = {?MODULE, #{reply_to => self()}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => 100,
                                scheduled_delay_ms => timer:minutes(10),
                                max_queue_size => 10}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(scheduled_export_success_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterConf = {?MODULE, #{reply_to => self()}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => 10,
                                scheduled_delay_ms => 5}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(flush_on_terminate_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterConf = {?MODULE, #{reply_to => self()}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => 10,
                                %% high values to make sure nothing is exported
                                %% before terminating the handler
                                scheduled_delay_ms => timer:minutes(10),
                                max_queue_size => 10000}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(sanity_end_to_end_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterConf = {?MODULE, #{otlp => true, reply_to => self()}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => 1000,
                                %% setting scheduled delay high enough,
                                %% so that it doesn't occur during the test,
                                scheduled_delay_ms => timer:minutes(10),
                                %% the test will produce and expect 5 log events to be exporterd
                                max_queue_size => 5}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(overload_protection_slow_exporter_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterConf = {?MODULE, #{sleep => timer:seconds(10), clean_tab => true}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => timer:seconds(30),
                                max_queue_size => 500,
                                scheduled_delay_ms => 5}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(overload_protection_fast_exporter_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterConf = {?MODULE, #{clean_tab => true}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => timer:seconds(30),
                                max_queue_size => 500,
                                scheduled_delay_ms => 5}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(late_writes_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    Key = opentelemetry:instrumentation_scope(<<>>, <<>>, <<>>),
    LateWrites =  [{Key, log_event()} || _ <- lists:seq(1,5)],
    ExporterConf = {?MODULE, #{reply_to => self(), late_writes => LateWrites}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => timer:seconds(5),
                                %% high enough, as the test relies on dummy exporter replies order
                                scheduled_delay_ms => 50}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC}, {late_writes, LateWrites} | Config1];
init_per_testcase(late_writes_on_terminate_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    Key = opentelemetry:instrumentation_scope(<<>>, <<>>, <<>>),
    LateWrites =  [{Key, log_event()} || _ <- lists:seq(1,5)],
    ExporterConf = {?MODULE, #{reply_to => self(), late_writes => LateWrites}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => timer:seconds(5),
                                %% very high, so that export doesn't happen until termination
                                scheduled_delay_ms => 50,
                                %% this must be reached during the test to trigger export
                                %% and late writes insertion
                                max_queue_size => 10}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC}, {late_writes, LateWrites} | Config1];
init_per_testcase(no_exporter_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterConf = {?MODULE, #{undefined_exporter => true}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => timer:seconds(5),
                                max_queue_size => 500,
                                scheduled_delay_ms => 5}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(retry_exporter_init_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterConf = {?MODULE, #{retries => 5,
                               counter => atomics:new(1, []),
                               reply_to => self()}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => timer:seconds(5),
                                scheduled_delay_ms => 200}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(default_opentelemetry_exporter_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    %% no exporter, relying on the default opentelemtry GRPC exporter
    HandlerConf = #{config => #{exporting_timeout_ms => timer:seconds(5),
                                scheduled_delay_ms => 10}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(exporter_exit_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    ExporterSleep = timer:seconds(2),
    ExporterConf = {?MODULE, #{spawn_link => true, sleep => ExporterSleep, success => true}},
    HandlerConf = #{config => #{exporter => ExporterConf,
                                exporting_timeout_ms => timer:seconds(5),
                                scheduled_delay_ms => 10}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC}, {exporter_sleep, ExporterSleep} | Config1];
init_per_testcase(disabled_exporter_test = TC, Config) ->
    Config1 = common_testcase_init(Config),
    HandlerConf = #{config => #{exporter => ignore, scheduled_delay_ms => 1}},
    ok = logger:add_handler(TC, ?OTEL_LOG_HANDLER, HandlerConf),
    [{handler_id, TC} | Config1];
init_per_testcase(_TC, Config) ->
    common_testcase_init(Config).

end_per_testcase(_TC, Config) ->
    case ?config(handler_id, Config) of
        undefined -> ok;
        Id -> _ = logger:remove_handler(Id)
    end,
    _ = common_testcase_cleanup(Config),
    ok.

crud_test(_Config) ->
    ExporterConf = {?MODULE, #{success => true}},
    HandlerConf = #{config => #{exporter => ExporterConf}},
    ok = logger:add_handler(otel_handler_test, ?OTEL_LOG_HANDLER, HandlerConf),
    ok = logger:add_handler(otel_handler_test1, ?OTEL_LOG_HANDLER, HandlerConf),
    ok = logger:remove_handler(otel_handler_test1),
    {ok, #{config := #{reg_name := RegName}}} = logger:get_handler_config(otel_handler_test),
    true = erlang:is_process_alive(erlang:whereis(RegName)),

    %% Not allowed changes
    InvalidHandlerConf = #{config => #{reg_name => new_reg_name}},
    InvalidHandlerConf1 = #{config => #{exporter => {new_module, #{}}}},
    InvalidHandlerConf2 = #{config => #{atomic_ref => new_atomic_ref}},
    InvalidHandlerConf3 = #{config => #{tables => {new_tab1, new_tab2}}},
    InvalidHandlerConf4 = #{config => #{exporter => ExporterConf ,exporting_timeout_ms => 5555}},
    {error, {reg_name, _}} = logger:set_handler_config(otel_handler_test, InvalidHandlerConf),
    {error, {exporter, _}} = logger:set_handler_config(otel_handler_test, InvalidHandlerConf1),
    {error, {reg_name, _}} = logger:update_handler_config(otel_handler_test, InvalidHandlerConf),
    {error, {exporter, _}} = logger:update_handler_config(otel_handler_test, InvalidHandlerConf1),
    {error, {exporter, _}} = logger:update_handler_config(otel_handler_test, config,
                                                          #{exporter => {new_module, #{}}}),
    {error, {atomic_ref, _}} = logger:set_handler_config(otel_handler_test, InvalidHandlerConf2),
    {error, {tables, _}} = logger:set_handler_config(otel_handler_test, InvalidHandlerConf3),
    {error, {exporting_timeout_ms, _}} = logger:set_handler_config(otel_handler_test,
                                                                   InvalidHandlerConf4),
    {error, {atomic_ref, _}} = logger:update_handler_config(otel_handler_test, InvalidHandlerConf2),
    {error, {tables, _}} = logger:update_handler_config(otel_handler_test, InvalidHandlerConf3),
    {error, {exporting_timeout_ms, _}} = logger:update_handler_config(otel_handler_test,
                                                                      InvalidHandlerConf4),

    NewValidConf = #{max_queue_size => infinity,
                     scheduled_delay_ms => 3000},
    ok = logger:update_handler_config(otel_handler_test, #{config => NewValidConf}),
    ok = logger:update_handler_config(otel_handler_test, config, NewValidConf),
    ok = logger:set_handler_config(otel_handler_test, #{config => NewValidConf}),
    ok = logger:set_handler_config(otel_handler_test, level, debug),
    ok = logger:set_handler_config(otel_handler_test, level, critical),

    %% Allowed but invalid values
    NewInvalidConf = #{max_queue_size => -100, scheduled_delay_ms => "string"},
    {error, [_|_]} = logger:update_handler_config(otel_handler_test, #{config => NewInvalidConf}),
    {error, [_|_]} = logger:update_handler_config(otel_handler_test, config, NewInvalidConf),
    {error, [_|_]} = logger:set_handler_config(otel_handler_test, #{config => NewInvalidConf}),
    NewInvalidConf1 = #{unknown_opt => 100},
    {error, [_|_]} = logger:update_handler_config(otel_handler_test, #{config => NewInvalidConf1}),
    {error, [_|_]} = logger:update_handler_config(otel_handler_test, config, NewInvalidConf1),
    {error, [_|_]} = logger:set_handler_config(otel_handler_test, #{config => NewInvalidConf1}),

    ok = logger:remove_handler(otel_handler_test),

    {error, _} = logger:add_handler(otel_handler_test, ?OTEL_LOG_HANDLER, #{config => NewInvalidConf}),
    {error, _} = logger:add_handler(otel_handler_test, ?OTEL_LOG_HANDLER, #{config => NewInvalidConf1}).

exporting_runner_timeout_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config := #{scheduled_delay_ms := Delay,
                       reg_name := RegName}} = HandlerConf} = logger:get_handler_config(HandlerId),

    Mon = erlang:monitor(process, RegName),

    %% Insert a few log events to make sure runner process will be spawned and killed
    %% because it hangs forever (see `init_per_testcase/2`
    %% and exporter behaviour defined in this module
    true = ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf),
    true = ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf),

    %% Enough time to be sure export is triggered and runner killed
    Timeout = Delay * 10,
    receive
        {'DOWN', Mon, process, _Pid, _} ->
            %% test is to ensure we don't hit this
            ct:fail(otel_log_handler_crash)
    after Timeout ->
            ok
    end.

check_max_queue_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config := #{max_queue_size := MaxQueueSize}} = HandlerConf} =
        logger:get_handler_config(HandlerId),

    true = ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf),
    true = ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf),

    insert_events(HandlerConf, MaxQueueSize),

    %% Wait a little to give the handler time to transition to the export state
    timer:sleep(100),

    %% Insert the same number again, rgis time to the next table, as the previous is being exported,
    %% exporter is slow (see init_per_testcase), so we can be sure that we will go to the drop mode,
    %% with no chance to switch the table this time.
    insert_events(HandlerConf, MaxQueueSize),

    dropped = ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf).

export_max_queue_size_success_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config := #{max_queue_size := MaxSize}} = HandlerConf} =
        logger:get_handler_config(HandlerId),

    erlang:spawn(fun() -> insert_events(HandlerConf, MaxSize) end),
    %% Export must be triggered by reaching max_export_batch_size because
    %% scheduled_delay_ms is deliberately set to a high value
    receive
        {exported, Size} ->
            ?assertEqual(MaxSize, Size)
    after 5000 ->
            ct:fail(otel_log_handler_exporter_failed)
    end.

scheduled_export_success_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, HandlerConf} = logger:get_handler_config(HandlerId),

    LogsNum = 10,
    erlang:spawn(fun() -> insert_events(HandlerConf, LogsNum) end),

    receive
        {exported, Size} ->
            ?assertEqual(LogsNum, Size)
    after 5000 ->
            ct:fail(otel_log_handler_exporter_failed)
    end.

flush_on_terminate_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config := #{reg_name := RegName}} = HandlerConf} = logger:get_handler_config(HandlerId),

    LogsNum = 15,
    insert_events(HandlerConf, LogsNum),

    ?assert(erlang:is_pid(erlang:whereis(RegName))),
    ok = logger:remove_handler(HandlerId),
    receive
        {exported, Size} ->
            ?assertEqual(LogsNum, Size)
    after 5000 ->
            ct:fail(otel_log_handler_exporter_failed)
    end,
    ?assertEqual(undefined, erlang:whereis(RegName)).

sanity_end_to_end_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config := #{reg_name := RegName}}} = logger:get_handler_config(HandlerId),
    Mon = erlang:monitor(process, RegName),

    otel_propagator_text_map:extract(otel_propagator_trace_context, [{"traceparent",?TRACEPARENT}]),
    [_, TraceId, SpanId, _] = string:split(?TRACEPARENT, "-", all),
    LoggerMeta = logger:get_process_metadata(),
    ?assertMatch(#{otel_span_id := SpanId, otel_trace_id := TraceId}, LoggerMeta),
    %% the number of log events must be 5, since init_per_testcase set max_queue_size = 5
    %% Once 5 events are created, the handler will start exporting, so we can reliably expect 5 items
    logger:warning(?LOG_MSG),
    logger:error(?LOG_MSG),
    logger:warning(#{msg => ?LOG_MSG, foo => [bar, baz]}),
    logger:error(#{msg => ?LOG_MSG, foo => {bar, [baz]}}),
    logger:warning(?LOG_MSG ++ "test term: ~p", [#{foo => bar}]),
    receive
        {exported, Data} ->
            #{resource_logs := [#{scope_logs := [#{log_records := LogRecords}]}]} = Data,
            ?assertEqual(5, length(LogRecords)),
            {{Y, M, D}, _} = calendar:universal_time(),
            lists:foreach(
              fun(#{time_unix_nano := T, observed_time_unix_nano := T,
                    trace_id := LogTraceId, span_id := LogSpanId}) ->
                      %% this is to check that timestamps unit is actually nanosecond,
                      %% as otel_otlp_logs relies on OTP logger producing microsecond timestamps,
                      %% if it's not nanosecond, calendar:system_time_to_universal_time/2
                      %% is expected to produce some unrealistic dates
                      {{LogY, LogM, LogD}, _} = calendar:system_time_to_universal_time(T, nanosecond),
                      ?assertEqual({Y, M, D}, {LogY, LogM, LogD}),
                      ?assertEqual(hex_str_to_bin(TraceId, 128), LogTraceId),
                      ?assertEqual(hex_str_to_bin(SpanId, 64), LogSpanId)
              end,
              LogRecords)
    after 5000 ->
            ct:fail(otel_log_handler_exporter_failed)
    end,

    %% No crash is expected during the test
    receive
        {'DOWN', Mon, process, _Pid, _} ->
            %% test is to ensure we don't hit this
            ct:fail(otel_log_handler_crash)
    after 100 ->
            ok
    end.

overload_protection_slow_exporter_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config :=
               #{tables := {Tab1, Tab2},
                 max_queue_size := MaxSize,
                 reg_name := RegName}} = HandlerConf} = logger:get_handler_config(HandlerId),

    Pid1 = spawn(fun() -> insert_loop(HandlerConf) end),
    Pid2 = spawn(fun() -> insert_loop(HandlerConf) end),

    TimeSec = 5,
    ct:pal("Running ~p test case for ~p seconds...", [?FUNCTION_NAME, TimeSec]),
    timer:sleep(timer:seconds(TimeSec)),

    ?assert(ets:info(Tab1, size) =< MaxSize),
    ?assert(ets:info(Tab2, size) =< MaxSize),
    ct:pal("otel_log_handler status: ~p", [sys:get_status(RegName)]),

    exit(Pid1, kill),
    exit(Pid2, kill).

overload_protection_fast_exporter_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config :=
               #{tables := {Tab1, Tab2},
                 max_queue_size := MaxSize,
                 reg_name := RegName}} = HandlerConf} = logger:get_handler_config(HandlerId),

    Pid1 = spawn(fun() -> insert_loop(HandlerConf) end),
    Pid2 = spawn(fun() -> insert_loop(HandlerConf) end),

    TimeSec = 5,
    ct:pal("Running ~p test case for ~p seconds...", [?FUNCTION_NAME, TimeSec]),
    timer:sleep(timer:seconds(TimeSec)),

    %% TODO: it probably can be flaky under some race conditions
    ?assert(ets:info(Tab1, size) =< MaxSize),
    ?assert(ets:info(Tab2, size) =< MaxSize),
    ct:pal("otel_log_handler status: ~p", [sys:get_status(RegName)]),

    exit(Pid1, kill),
    exit(Pid2, kill).


%% this test mimics late inserts, that must be exported during the next exporter run
late_writes_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, HandlerConf} = logger:get_handler_config(HandlerId),
    LateWritesSize = length(?config(late_writes, Config)),
    NormalEventsSize = 7,
    insert_events(HandlerConf, NormalEventsSize),

    receive
        {exported, Size} ->
            ?assertEqual(NormalEventsSize, Size),
            %% There is no strict ordering guarantee, as exported replies are sent by
            %% different runner processes. The test relies on the fact that there is
            %% a delay between two exporter runs.
            receive
                {exported, Size1} ->
                    ?assertEqual(LateWritesSize, Size1)
            after 5000 ->
                    ct:fail(otel_log_handler_late_writes_exporter_failed)
            end
    after 5000 ->
            ct:fail(otel_log_handler_exporter_failed)
    end.

%% similar to late_writes_test but checks that the other table is checked and exported on termination
late_writes_on_terminate_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config := #{max_queue_size := MaxSize}} = HandlerConf} =
        logger:get_handler_config(HandlerId),
    LateWritesSize = length(?config(late_writes, Config)),
    insert_events(HandlerConf, MaxSize),

    receive
        {exported, Size} ->
            ?assertEqual(MaxSize, Size)
    after 5000 ->
            ct:fail(otel_log_handler_exporter_failed)
    end,

    ok = logger:remove_handler(HandlerId),
    receive
        {exported, Size1} ->
            ?assertEqual(LateWritesSize, Size1)
    after 5000 ->
            ct:fail(otel_log_handler_late_writes_exporter_failed)
    end.

no_exporter_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config := #{reg_name := RegName, scheduled_delay_ms := Delay}} = HandlerConf} =
        logger:get_handler_config(HandlerId),
    Mon = erlang:monitor(process, RegName),

    %% Enough time to be sure export is triggered and state transition happened
    Timeout = Delay * 10,
    receive
        {'DOWN', Mon, process, _Pid, _} ->
            %% test is to ensure we don't hit this
            ct:fail(otel_log_handler_crash)
    after Timeout ->
            ok
    end,
    %% Must be disabled
    ?assertEqual(dropped, ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf)).

retry_exporter_init_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, HandlerConf} = logger:get_handler_config(HandlerId),
    receive
        exporter_not_ready ->
            %% expect accepting events: exporter is not ready yet, but max_queue_size is not reached
            ?assertEqual(true, ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf)),
            receive
                exporter_ready ->
                    %% give some time to the handler to enable
                    timer:sleep(50),
                    ?assertEqual(true, ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf))
            after
                %% higher timeout, as we need to wait for several exporter init retries
                5000 ->
                    ct:fail(otel_log_handler_exporter_init_failed)
            end
    after 2000 ->
            ct:fail(otel_log_handler_exporter_failed)
    end.

%% opentelmetry_exporter is expected to successfully initialize and keep running
%% in disconected state (even though there is no collector to connect to).
%%  This test only checks that no crashes occur
default_opentelemetry_exporter_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config := #{scheduled_delay_ms := Delay, reg_name := RegName}} = HandlerConf} =
        logger:get_handler_config(HandlerId),
    Mon = erlang:monitor(process, RegName),

    insert_events(HandlerConf, 10),
    %% Give it some time to run and do state transition
    Timeout = Delay * 10,
    receive
        {'DOWN', Mon, process, _Pid, _} ->
            %% test is to ensure we don't hit this
            ct:fail(otel_log_handler_crash)
    after Timeout ->
            ok
    end.

exporter_exit_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    ExporterSleep = ?config(exporter_sleep, Config),
    {ok, #{config := #{reg_name := RegName}}} = logger:get_handler_config(HandlerId),
    Mon = erlang:monitor(process, RegName),

    Timeout = ExporterSleep * 2,
    receive
        {'DOWN', Mon, process, _Pid, test_exporter_crash} ->
            %% supervisor must restart the handler
            ?assert(wait_for_restart(RegName, 1000))
    after Timeout ->
            ct:fail(otel_log_handler_no_exit)
    end.

disabled_exporter_test(Config) ->
    HandlerId = ?config(handler_id, Config),
    {ok, #{config := #{scheduled_delay_ms := Delay}} = HandlerConf} =
        logger:get_handler_config(HandlerId),

    %% Enough time to be sure exporter init is triggered and handler is in permanent drop mode
    Timeout = Delay * 10,
    timer:sleep(Timeout),
    %% Must be disabled
    ?assertEqual(dropped, ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf)).

%% exporter behaviour
init(#{undefined_exporter := true}) ->
    ignore;
init(#{retries := N, counter := Ref, reply_to := Pid} = ExpConfig) ->
    case atomics:add_get(Ref, 1, 1) of
        N ->
            Pid ! exporter_ready,
            {ok, ExpConfig};
        _ ->
            Pid ! exporter_not_ready,
            {error, not_ready}
    end;
init(#{spawn_link := true, sleep := Time} = ExpConfig) ->
    F = fun () -> timer:sleep(Time),
                  exit(test_exporter_crash)
        end,
    Pid = erlang:spawn_link(F),
    {ok, ExpConfig#{exporter_pid => Pid}};
init(ExpConfig) ->
    {ok, ExpConfig}.

export(logs, {Tab, _LogHandlerConfig}, _Resource, #{sleep := Time} = State) ->
    timer:sleep(Time),
    case State of
        %% Mimic an exporter that must take records from the table
        %% (even if export failed, taken records are not inserted back to the table)
        #{clean_tab := true} ->
            {ok, _Size} = traverse_clean(Tab),
            ok;
        _ -> ok
    end,
    ok;
export(logs, {_Tab, _LogHandlerConfig}, _Resource, #{success := true} =_State) ->
    ok;
export(logs, {Tab, LogHandlerConfig}, Resource, #{otlp := true, reply_to := Pid} = _State) ->
    Res = otel_otlp_logs:to_proto(Tab, Resource, LogHandlerConfig),
    Pid ! {exported, Res};
export(logs, {Tab, _LogHandlerConfig}, _Resource, #{reply_to := Pid} = State) ->
    {ok, Size} = traverse_clean(Tab),
    Pid ! {exported, Size},
    case State of
        #{late_writes := Records} ->
            ets:insert(Tab, Records);
        _ -> ok
    end,
    ok;
export(logs, {Tab, _LogHandlerConfig}, _Resource, #{clean_tab := true} = _State) ->
    {ok, _Size} = traverse_clean(Tab),
    ok.

shutdown(_) ->
    ok.

%% helpers

common_testcase_init(Config) ->
    {ok, _} = application:ensure_all_started(opentelemetry_exporter),
    {ok, _} = application:ensure_all_started(opentelemetry_experimental),
    Config.

common_testcase_cleanup(_Config) ->
    _ = application:stop(opentelemetry_experimental),
    _ = application:stop(opentelemetry_exporter),
    ok.

log_event() ->
    #{level => warning,
      meta => #{gl => erlang:group_leader(), pid => erlang:self(), time => os:system_time(microsecond)},
      msg => {string, ?LOG_MSG}}.

hex_str_to_bin(Str, Size) ->
    B = iolist_to_binary(Str),
    <<(binary_to_integer(B, 16)):Size>>.

insert_events(HandlerConf, N) ->
    lists:foreach(fun(_) -> ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf) end,
                  lists:seq(1, N)).

insert_loop(HandlerConf) ->
    ?OTEL_LOG_HANDLER:log(log_event(), HandlerConf),
    insert_loop(HandlerConf).

traverse_clean(Tab) ->
    try
        ets:safe_fixtable(Tab, true),
        traverse_clean(Tab, ets:first(Tab), 0)
    after
        ets:safe_fixtable(Tab, false)
    end.

traverse_clean(Tab, '$end_of_table', Seq) ->
    ct:pal("Exported: ~p records from table: ~p", [Seq, Tab]),
    {ok, Seq};
traverse_clean(Tab, Key, Seq) ->
    %% Tab type is duplicate_bag, keys can be non unique
    Records = ets:take(Tab, Key),
    traverse_clean(Tab, ets:next(Tab, Key), Seq+length(Records)).

wait_for_restart(_, 0) -> false;
wait_for_restart(RegName, Retries) ->
    timer:sleep(1),
    is_pid(whereis(RegName)) orelse wait_for_restart(RegName, Retries-1).
