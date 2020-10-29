-module(otel_batch_processor_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

all() ->
    [exporting_timeout_test].

%% verifies that after the runner has to be killed for taking too long
%% that everything is still functional and the exporter does not crash
exporting_timeout_test(_Config) ->
    process_flag(trap_exit, true),

    {ok, Pid} = otel_batch_processor:start_link(#{exporter => ?MODULE,
                                                  exporting_timeout_ms => 1,
                                                  scheduled_delay_ms => 1}),

    receive
        {'EXIT', Pid, _} ->
            %% test is to ensure we don't hit this
            ct:fail(batch_processor_crash)
    after
        100 ->
            ok
    end.

%% exporter behaviour

init(_) ->
    {ok, []}.

export(_, _) ->
    timer:sleep(timer:minutes(10)).

shutdown(_) ->
    ok.
