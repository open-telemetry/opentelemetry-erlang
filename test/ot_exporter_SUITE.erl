-module(ot_exporter_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").
-include("ot_sampler.hrl").

all() ->
    [exporting_timeout_test].

%% verifies that after the runner has to be killed for taking too long
%% that everything is still functional and the exporter does not crash
exporting_timeout_test(_Config) ->
    process_flag(trap_exit, true),

    {ok, Pid} = ot_exporter:start_link([{exporters, [fun(_, _) -> timer:sleep(timer:minutes(10)) end]},
                                        {exporting_timeout_ms, 1},
                                        {scheduled_delay_ms, 1}]),

    receive
        {'EXIT', Pid, _} ->
            ct:fail(exporter_crash)
    after
        100 ->
            ok
    end.
