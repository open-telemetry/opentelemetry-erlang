-module(otel_log_handler_SUITE).

-compile(export_all).

%% Logger permits handler-specific config keys not represented in its closed type.
-eqwalizer({nowarn_function, exports_logs_after_idle_interval/1}).
-eqwalizer({nowarn_function, logged_and_observed_time/1}).
-eqwalizer({nowarn_function, caller_observed_time_is_kept/1}).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [exports_logs_after_idle_interval,
     logged_and_observed_time,
     caller_observed_time_is_kept].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(opentelemetry_experimental),
    OldPrimary = logger:get_primary_config(),
    ok = logger:update_primary_config(#{level => info}),
    [{old_primary, OldPrimary} | Config].

end_per_suite(Config) ->
    ok = logger:set_primary_config(?config(old_primary, Config)),
    _ = application:stop(opentelemetry_experimental),
    ok.

%% Regression test for the empty-batch wedge: an export interval that elapses
%% with no buffered logs must not stop the handler from exporting later logs.
%%
%% Before the fix, the first `export_logs` tick that fired with an empty batch
%% fell through to `handle_event/3` (`keep_state_and_data`) and left the
%% `gen_statem` parked in the `exporting` state. Because only the `idle` state
%% re-arms the export timer, the handler never exported again after the first
%% idle interval.
exports_logs_after_idle_interval(_Config) ->
    HandlerId = ?FUNCTION_NAME,
    ok = logger:add_handler(HandlerId, otel_log_handler,
                            #{level => info,
                              exporter => {?MODULE, {logs, self()}},
                              scheduled_delay_ms => 1}),

    %% Let several export intervals elapse with no logs — the condition that
    %% used to wedge the handler.
    timer:sleep(200),
    ok = flush(),

    %% A log emitted after the idle period must still be exported.
    logger:log(info, "post-idle log line", #{}),

    try
        receive
            {logs, _Batch} -> ok
        after
            5000 -> ct:fail(no_export_after_idle_interval)
        end
    after
        _ = logger:remove_handler(HandlerId)
    end.

%% The record carries the `time' set by the caller, and an observed time read
%% during the `logger:log' call, not at export.
logged_and_observed_time(_Config) ->
    HandlerId = ?FUNCTION_NAME,
    ok = logger:add_handler(HandlerId, otel_log_handler,
                            #{level => info,
                              exporter => {?MODULE, {logs, self()}},
                              scheduled_delay_ms => 500}),
    LoggedTime = 1700000000123456,
    Msg = <<"observed time log line">>,

    Before = os:system_time(nanosecond),
    logger:log(info, Msg, #{time => LoggedTime}),
    After = os:system_time(nanosecond),

    try
        receive
            {logs, {Batch, HandlerConfig}} ->
                #{resource_logs := [#{scope_logs := ScopeLogs}]} =
                    otel_otlp_logs:to_proto(Batch, otel_resource:create([]), HandlerConfig),
                [#{time_unix_nano := TimeNano,
                   observed_time_unix_nano := ObservedTime,
                   attributes := Attributes}] =
                    [R || #{log_records := Rs} <- ScopeLogs,
                          R=#{body := #{value := {string_value, B}}} <- Rs,
                          B =:= Msg],
                ?assertEqual(LoggedTime * 1000, TimeNano),
                ?assert(Before =< ObservedTime andalso ObservedTime =< After),
                ?assertEqual([], [A || A=#{key := <<"otel_observed_time">>} <- Attributes])
        after
            5000 -> ct:fail(no_export)
        end
    after
        _ = logger:remove_handler(HandlerId)
    end.

%% A caller that sets `otel_observed_time' keeps its own value.
caller_observed_time_is_kept(_Config) ->
    HandlerId = ?FUNCTION_NAME,
    ok = logger:add_handler(HandlerId, otel_log_handler,
                            #{level => info,
                              exporter => {?MODULE, {logs, self()}},
                              scheduled_delay_ms => 500}),
    ObservedTime = 1700000000123456789,
    Msg = <<"caller observed time log line">>,
    logger:log(info, Msg, #{otel_observed_time => ObservedTime}),
    try
        receive
            {logs, {Batch, HandlerConfig}} ->
                #{resource_logs := [#{scope_logs := ScopeLogs}]} =
                    otel_otlp_logs:to_proto(Batch, otel_resource:create([]), HandlerConfig),
                [#{observed_time_unix_nano := ObservedTime}] =
                    [R || #{log_records := Rs} <- ScopeLogs,
                          R=#{body := #{value := {string_value, B}}} <- Rs,
                          B =:= Msg]
        after
            5000 -> ct:fail(no_export)
        end
    after
        _ = logger:remove_handler(HandlerId)
    end.

flush() ->
    receive
        {logs, _} -> flush()
    after
        0 -> ok
    end.

%% --- otel_exporter test double (logs signal) ---
init({logs, Pid}) ->
    {ok, Pid}.

export(Logs, _Resource, Pid) ->
    Pid ! {logs, Logs},
    ok.

shutdown(_) ->
    ok.
