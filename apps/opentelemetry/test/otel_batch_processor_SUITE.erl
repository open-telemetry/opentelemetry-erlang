-module(otel_batch_processor_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("otel_span.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

all() ->
    [exporting_timeout_test,
     exporting_runner_timeout_test,
     check_table_size_test].

%% verifies that after the runner has to be killed for taking too long
%% that everything is still functional and the exporter does not crash
exporting_timeout_test(_Config) ->
    process_flag(trap_exit, true),

    {ok, Pid, _} = otel_batch_processor:start_link(#{name => test_processor,
                                                     resource => otel_resource:create([]),
                                                     exporter => ?MODULE,
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

exporting_runner_timeout_test(_Config) ->
    process_flag(trap_exit, true),

    {ok, Pid, State} = otel_batch_processor:start_link(
                                          #{name => test_processor,
                                            resource => otel_resource:create([]),
                                            exporter => ?MODULE,
                                            exporting_timeout_ms => 1,
                                            scheduled_delay_ms => 1}),

    %% Insert a few spans to make sure runner process will be spawned and killed
    %% because it hangs for 10 minutes (see export/4 below)
    true = otel_batch_processor:on_end(generate_span(), State),
    true = otel_batch_processor:on_end(generate_span(), State),

    receive
        {'EXIT', Pid, _} ->
            %% test is to ensure we don't hit this
            ct:fail(batch_processor_crash)
    after
        200 ->
            ok
    end.

check_table_size_test(_Config) ->
    MaxQueueSize = 10,
    {ok, _Pid, State} = otel_batch_processor:start_link(
                          #{name => test_processor_check_size_test,
                            resource => otel_resource:create([]),
                            exporter => ?MODULE,
                            exporting_timeout_ms => timer:minutes(10),
                            %% long enough, so that it never happens during the test
                            scheduled_delay_ms => timer:minutes(10),
                            max_queue_size => MaxQueueSize}
                         ),
    %% max_queue_size limit is not reached
    true = otel_batch_processor:on_end(generate_span(), State),

    insert_spans(State, MaxQueueSize),

    %% Wait a little to give the handler time to transition to the export state
    timer:sleep(30),

    %% Insert the same number again, rgis time to the next table, as the previous is being exported,
    %% exporter is slow (see init_per_testcase), so we can be sure that we will go to the drop mode,
    %% with no chance to switch the table this time.
    insert_spans(State, MaxQueueSize),

    dropped = otel_batch_processor:on_end(generate_span(), State).

%% exporter behaviour

init(_OtelSignal, _ExporterId, _) ->
    {ok, []}.

export(_, _, _, _) ->
    timer:sleep(timer:minutes(10)).

shutdown(_) ->
    ok.

%% helpers

insert_spans(State, N) ->
    lists:foreach(fun(_) -> otel_batch_processor:on_end(generate_span(), State) end,
                  lists:seq(1, N)).

generate_span() ->
    #span{trace_id = otel_id_generator:generate_trace_id(),
          span_id = otel_id_generator:generate_span_id(),
          name = "test_span",
          trace_flags = 1,
          is_recording = true,
          instrumentation_scope = #instrumentation_scope{name = "test"}}.
