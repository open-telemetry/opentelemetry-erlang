-module(ot_reporter_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").
-include("ot_sampler.hrl").

all() ->
    [reporting_timeout_test].

%% verifies that after the runner has to be killed for taking too long
%% that everything is still functional and the reporter does not crash
reporting_timeout_test(_Config) ->
    process_flag(trap_exit, true),

    {ok, Pid} = ot_reporter:start_link([{reporters, [fun(_, _) -> timer:sleep(timer:minutes(10)) end]},
                                        {reporting_timeout_ms, 1},
                                        {send_interval_ms, 1}]),

    receive
        {'EXIT', Pid, _} ->
            ct:fail(reporter_crash)
    after
        100 ->
            ok
    end.
