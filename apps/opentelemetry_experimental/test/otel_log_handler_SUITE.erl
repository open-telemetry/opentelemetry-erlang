-module(otel_log_handler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [exports_logs_after_idle_interval].

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
