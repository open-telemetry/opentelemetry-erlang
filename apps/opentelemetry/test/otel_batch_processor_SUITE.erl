-module(otel_batch_processor_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("otel_span.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

all() ->
    [exporting_timeout_test,
     check_table_size_test,
     exporting_runner_timeout_test].

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

check_table_size_test(_Config) ->
    MaxQueueSize = 10,
    CheckTableSizeMs = 1,
    {ok, _Pid, #{reg_name := RegName}} = otel_batch_processor:start_link(
                                           #{name => test_processor_check_size_test,
                                             resource => otel_resource:create([]),
                                             exporter => ?MODULE,
                                             exporting_timeout_ms => timer:minutes(10),
                                             %% long enough, so that it never happens during the test
                                             scheduled_delay_ms => timer:minutes(10),
                                             check_table_size_ms => CheckTableSizeMs,
                                             max_queue_size => MaxQueueSize}
                                          ),
    %% max_queue_size limit is not reached
    true = otel_batch_processor:on_end(generate_span(), #{reg_name => RegName}),
    lists:foreach(fun(_) ->
                          otel_batch_processor:on_end(generate_span(), #{reg_name => RegName})
                  end,
                  lists:seq(1, MaxQueueSize)),
    %% Wait for more than CheckTablesizeMS to be sure  check timeout occurred
    timer:sleep(CheckTableSizeMs * 5),
    dropped = otel_batch_processor:on_end(generate_span(), #{reg_name => RegName}),

    otel_batch_processor:force_flush(#{reg_name => RegName}),
    %% force_flush is async, have to wait for some long enough time again,
    timer:sleep(CheckTableSizeMs * 10),
    true = otel_batch_processor:on_end(generate_span(), #{reg_name => RegName}).

exporting_runner_timeout_test(_Config) ->
    process_flag(trap_exit, true),

    {ok, Pid, #{reg_name := RegName}} = otel_batch_processor:start_link(
                                          #{name => test_processor1,
                                            resource => otel_resource:create([]),
                                            exporter => ?MODULE,
                                            exporting_timeout_ms => 1,
                                            scheduled_delay_ms => 1}),

    %% Insert a few spans to make sure runner process will be spawned and killed
    %% because it hangs for 10 minutes (see export/4 below)
    true = otel_batch_processor:on_end(generate_span(), #{reg_name => RegName}),
    true = otel_batch_processor:on_end(generate_span(), #{reg_name => RegName}),

    receive
        {'EXIT', Pid, _} ->
            %% test is to ensure we don't hit this
            ct:fail(batch_processor_crash)
    after
        200 ->
            ok
    end.

%% exporter behaviour

init(_) ->
    {ok, []}.

export(_, _, _, _) ->
    timer:sleep(timer:minutes(10)).

shutdown(_) ->
    ok.

%% helpers

generate_span() ->
    StartTime = opentelemetry:timestamp(),
    EndTime = opentelemetry:timestamp(),
    #span{trace_id = otel_id_generator:generate_trace_id(),
          span_id = otel_id_generator:generate_span_id(),
          name = "test_span",
          start_time = StartTime,
          end_time = EndTime,
          trace_flags = 1,
          is_recording = true,
          parent_span_is_remote = undefined,
          instrumentation_scope = #instrumentation_scope{name = "test"}}.
