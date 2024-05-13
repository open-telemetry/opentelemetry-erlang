%%%------------------------------------------------------------------------
%% Copyright 2019, OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc The Batch Span Processor implements the {@link otel_span_processor}
%% behaviour.
%% 
%% It stores finished Spans in a ETS table buffer and exports
%% them on an interval or when the table reaches a maximum size.
%%
%% You can configure these timeouts:
%%
%% <ul>
%% <li>`exporting_timeout_ms': how long to let the exports run before killing.</li>
%% <li>`check_table_size_ms': timeout to check the size of the export table.</li>
%% <li>`scheduled_delay_ms': how often to trigger running the exporters.</li>
%% </ul>
%%
%% The size limit of the current table where finished spans are stored can
%% be configured with the `max_queue_size' option.
%% @end
%%%-----------------------------------------------------------------------
-module(otel_batch_processor).

-behaviour(gen_statem).
-behaviour(otel_span_processor).

-export([start_link/1,
         on_start/3,
         on_end/2,
         force_flush/1,
         report_cb/1,

         %% deprecated
         set_exporter/1,
         set_exporter/2,
         set_exporter/3]).

-export([init/1,
         callback_mode/0,
         idle/3,
         exporting/3,
         terminate/3]).

%% uncomment when OTP-23 becomes the minimum required version
%% -deprecated({set_exporter, 1, "set through the otel_tracer_provider instead"}).
%% -deprecated({set_exporter, 2, "set through the otel_tracer_provider instead"}).
%% -deprecated({set_exporter, 3, "set through the otel_tracer_provider instead"}).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("kernel/include/logger.hrl").
-include("otel_span.hrl").

-record(data, {exporter             :: {module(), term()} | undefined,
               exporter_config      :: {module(), term()} | undefined | none,
               resource             :: otel_resource:t() | undefined,
               handed_off_table     :: atom() | undefined,
               runner_pid           :: pid() | undefined,
               max_queue_size       :: integer() | infinity,
               exporting_timeout_ms :: integer(),
               check_table_size_ms  :: integer() | infinity,
               scheduled_delay_ms   :: integer(),
               table_1              :: atom(),
               table_2              :: atom(),
               reg_name             :: atom()}).

-define(CURRENT_TABLES_KEY(Name), {?MODULE, current_table, Name}).

%% create unique table names to support multiple batch processors at once
-define(TABLE_NAME(TN), lists:concat([TN, "_", erlang:pid_to_list(self())])).
-define(TABLE_1, ?REG_NAME(?TABLE_NAME(otel_export_table1))).
-define(TABLE_2, ?REG_NAME(?TABLE_NAME(otel_export_table2))).
-define(CURRENT_TABLE(RegName), persistent_term:get(?CURRENT_TABLES_KEY(RegName))).

-define(DEFAULT_MAX_QUEUE_SIZE, 2048).
-define(DEFAULT_SCHEDULED_DELAY_MS, timer:seconds(5)).
-define(DEFAULT_EXPORTER_TIMEOUT_MS, timer:minutes(5)).
-define(DEFAULT_CHECK_TABLE_SIZE_MS, timer:seconds(1)).

-define(ENABLED_KEY(RegName), {?MODULE, enabled_key, RegName}).

-ifdef(TEST).
-export([current_tab_to_list/1]).
current_tab_to_list(RegName) ->
    ets:tab2list(?CURRENT_TABLE(RegName)).
-endif.

%% require a unique name to distinguish multiple batch processors while
%% still having a single name, instead of a possibly changing pid, to
%% communicate with the processor
%% @doc Starts a Batch Span Processor.
%% @end
-spec start_link(#{name := atom() | list()}) -> {ok, pid(), map()}.
start_link(Config=#{name := Name}) ->
    RegisterName = ?REG_NAME(Name),
    Config1 = Config#{reg_name => RegisterName},
    {ok, Pid} = gen_statem:start_link({local, RegisterName}, ?MODULE, [Config1], []),
    {ok, Pid, Config1}.

%% @deprecated Please use {@link otel_tracer_provider}
set_exporter(Exporter) ->
    set_exporter(global, Exporter, []).

%% @deprecated Please use {@link otel_tracer_provider}
-spec set_exporter(module(), term()) -> ok.
set_exporter(Exporter, Options) ->
    gen_statem:call(?REG_NAME(global), {set_exporter, {Exporter, Options}}).

%% @deprecated Please use {@link otel_tracer_provider}
-spec set_exporter(atom(), module(), term()) -> ok.
set_exporter(Name, Exporter, Options) ->
    gen_statem:call(?REG_NAME(Name), {set_exporter, {Exporter, Options}}).

%% @private
-spec on_start(otel_ctx:t(), opentelemetry:span(), otel_span_processor:processor_config())
              -> opentelemetry:span().
on_start(_Ctx, Span, _) ->
    Span.

%% @private
-spec on_end(opentelemetry:span(), otel_span_processor:processor_config())
            -> true | dropped | {error, invalid_span} | {error, no_export_buffer}.
on_end(#span{trace_flags=TraceFlags}, _) when not(?IS_SAMPLED(TraceFlags)) ->
    dropped;
on_end(Span=#span{}, #{reg_name := RegName}) ->
    do_insert(RegName, Span);
on_end(_Span, _) ->
    {error, invalid_span}.

%% @private
-spec force_flush(#{reg_name := gen_statem:server_ref()}) -> ok.
force_flush(#{reg_name := RegName}) ->
    gen_statem:cast(RegName, force_flush).

%% @private
init([Args=#{reg_name := RegName}]) ->
    process_flag(trap_exit, true),

    SizeLimit = maps:get(max_queue_size, Args, ?DEFAULT_MAX_QUEUE_SIZE),
    ExportingTimeout = maps:get(exporting_timeout_ms, Args, ?DEFAULT_EXPORTER_TIMEOUT_MS),
    ScheduledDelay = maps:get(scheduled_delay_ms, Args, ?DEFAULT_SCHEDULED_DELAY_MS),
    CheckTableSize = maps:get(check_table_size_ms, Args, ?DEFAULT_CHECK_TABLE_SIZE_MS),

    %% TODO: this should be passed in from the tracer server
    Resource = case maps:find(resource, Args) of
                   {ok, R} ->
                       R;
                   error ->
                       otel_resource_detector:get_resource()
               end,
    %% Resource = otel_tracer_provider:resource(),

    Table1 = ?TABLE_1,
    Table2 = ?TABLE_2,

    _Tid1 = new_export_table(Table1),
    _Tid2 = new_export_table(Table2),
    persistent_term:put(?CURRENT_TABLES_KEY(RegName), Table1),

    %% only enable export table if there is going to be an exporter
    case maps:get(exporter, Args, none) of
        ExporterConfig when ExporterConfig =:= none ; ExporterConfig =:= undefined ->
            disable(RegName);
        ExporterConfig ->
            enable(RegName)
    end,

    {ok, idle, #data{exporter=undefined,
                     exporter_config=ExporterConfig,
                     resource = Resource,
                     handed_off_table=undefined,
                     max_queue_size=SizeLimit,
                     exporting_timeout_ms=ExportingTimeout,
                     check_table_size_ms=CheckTableSize,
                     scheduled_delay_ms=ScheduledDelay,
                     table_1=Table1,
                     table_2=Table2,
                     reg_name=RegName}}.

%% @private
callback_mode() ->
    [state_functions, state_enter].

%% @private
idle(enter, _OldState, Data=#data{exporter=undefined,
                                  exporter_config=ExporterConfig,
                                  scheduled_delay_ms=SendInterval,
                                  check_table_size_ms=CheckInterval,
                                  reg_name=RegName}) ->
    Exporter = init_exporter(RegName, ExporterConfig),
    {keep_state, Data#data{exporter=Exporter},
     [{{timeout, export_spans}, SendInterval, export_spans},
      {{timeout, check_table_size}, CheckInterval, check_table_size}]};
idle(enter, _OldState, #data{scheduled_delay_ms=SendInterval,
                             check_table_size_ms=CheckInterval}) ->
    {keep_state_and_data,
     [{{timeout, export_spans}, SendInterval, export_spans},
      {{timeout, check_table_size}, CheckInterval, check_table_size}]};
idle(_, export_spans, Data=#data{exporter=undefined,
                                 exporter_config=ExporterConfig,
                                 reg_name=RegName}) ->
    Exporter = init_exporter(RegName, ExporterConfig),
    {next_state, exporting, Data#data{exporter=Exporter}};
idle(_, export_spans, Data) ->
    {next_state, exporting, Data};
idle(EventType, Event, Data) ->
    handle_event_(idle, EventType, Event, Data).

%% receiving an `export_spans' timeout while exporting means the `ExportingTimeout'
%% is shorter than the `SendInterval'. Postponing the event will ensure we export
%% after
%% @private
exporting({timeout, export_spans}, export_spans, _) ->
    {keep_state_and_data, [postpone]};
exporting(enter, _OldState, #data{exporter=undefined,
                                  reg_name=RegName}) ->
    %% exporter still undefined, go back to idle
    %% first empty the table and disable the processor so no more spans are added
    %% we wait until the attempt to export to disable so we don't lose spans
    %% on startup but disable once it is clear an exporter isn't being set
    clear_table_and_disable(RegName),

    %% use state timeout to transition to `idle' since we can't set a
    %% new state in an `enter' handler
    {keep_state_and_data, [{state_timeout, 0, no_exporter}]};
exporting(enter, _OldState, Data=#data{exporting_timeout_ms=ExportingTimeout,
                                       scheduled_delay_ms=SendInterval}) ->
    case export_spans(Data) of
        ok ->
            %% in an `enter' handler we can't return a `next_state' or `next_event'
            %% so we rely on a timeout to trigger the transition to `idle'
            {keep_state, Data#data{runner_pid=undefined}, [{state_timeout, 0, empty_table}]};
        {OldTableName, RunnerPid} ->
            {keep_state, Data#data{runner_pid=RunnerPid,
                                   handed_off_table=OldTableName},
             [{state_timeout, ExportingTimeout, exporting_timeout},
              {{timeout, export_spans}, SendInterval, export_spans}]}
    end;

%% TODO: we need to just check if `exporter=undefined' right?
%% two hacks since we can't transition to a new state or send an action from `enter'
exporting(state_timeout, no_exporter, Data) ->
    {next_state, idle, Data};
exporting(state_timeout, empty_table, Data) ->
    {next_state, idle, Data};

exporting(state_timeout, exporting_timeout, Data=#data{handed_off_table=ExportingTable}) ->
    %% kill current exporting process because it is taking too long
    %% which deletes the exporting table, so create a new one and
    %% repeat the state to force another span exporting immediately
    Data1 = kill_runner(Data),
    new_export_table(ExportingTable),
    {next_state, idle, Data1};
%% important to verify runner_pid and FromPid are the same in case it was sent
%% after kill_runner was called but before it had done the unlink
exporting(info, {'EXIT', FromPid, _}, Data=#data{runner_pid=FromPid}) ->
    complete_exporting(Data);
%% important to verify runner_pid and FromPid are the same in case it was sent
%% after kill_runner was called but before it had done the unlink
exporting(info, {completed, FromPid}, Data=#data{runner_pid=FromPid}) ->
    complete_exporting(Data);
exporting(EventType, Event, Data) ->
    handle_event_(exporting, EventType, Event, Data).

%% transition to exporting on a force_flush unless we are already exporting
%% if exporting then postpone the event so the force flush happens after
%% this current exporting is complete
handle_event_(exporting, _, force_flush, _Data) ->
    {keep_state_and_data, [postpone]};
handle_event_(_State, _, force_flush, Data) ->
    {next_state, exporting, Data};

handle_event_(_State, {timeout, check_table_size}, check_table_size, #data{max_queue_size=infinity}) ->
    keep_state_and_data;
handle_event_(_State, {timeout, check_table_size}, check_table_size, #data{max_queue_size=MaxQueueSize,
                                                                           check_table_size_ms=CheckInterval,
                                                                           reg_name=RegName}) ->
    case ets:info(?CURRENT_TABLE(RegName), size) of
        M when M >= MaxQueueSize ->
            disable(RegName);
        _ ->
            enable(RegName)
    end,
    {keep_state_and_data, [{{timeout, check_table_size}, CheckInterval, check_table_size}]};
handle_event_(_, {call, From}, {set_exporter, ExporterConfig}, Data=#data{exporter=OldExporter,
                                                                          reg_name=RegName}) ->
    otel_exporter:shutdown(OldExporter),

    %% enable immediately or else spans will be dropped for a period even after this call returns
    enable(RegName),

    {keep_state, Data#data{exporter=undefined,
                           exporter_config=ExporterConfig}, [{reply, From, ok},
                                                             {next_event, internal, init_exporter}]};
handle_event_(_, internal, init_exporter, Data=#data{exporter=undefined,
                                                     exporter_config=ExporterConfig,
                                                     reg_name=RegName}) ->
    Exporter = init_exporter(RegName, ExporterConfig),
    {keep_state, Data#data{exporter=Exporter}};
handle_event_(_, _, _, _) ->
    keep_state_and_data.

%% @private
terminate(_Reason, _State, #data{exporter=Exporter,
                                 resource=Resource,
                                 reg_name=RegName}) ->
    CurrentTable = ?CURRENT_TABLE(RegName),

    %% `export' is used to perform a blocking export
    _ = export(Exporter, Resource, CurrentTable),

    ok.

%%

init_exporter(RegName, ExporterConfig) ->
    case otel_exporter:init(ExporterConfig) of
        Exporter when Exporter =/= undefined andalso Exporter =/= none ->
            enable(RegName),
            Exporter;
        _ ->
            %% exporter is undefined/none
            %% disable the insertion of new spans and delete the current table
            clear_table_and_disable(RegName),
            undefined
    end.

clear_table_and_disable(RegName) ->
    disable(RegName),
    ets:delete(?CURRENT_TABLE(RegName)),
    new_export_table(?CURRENT_TABLE(RegName)).

enable(RegName)->
    persistent_term:put(?ENABLED_KEY(RegName), true).

disable(RegName) ->
    persistent_term:put(?ENABLED_KEY(RegName), false).

is_enabled(RegName) ->
    persistent_term:get(?ENABLED_KEY(RegName), true).

do_insert(RegName, Span) ->
    try
        case is_enabled(RegName) of
            true ->
                ets:insert(?CURRENT_TABLE(RegName), Span);
            _ ->
                dropped
        end
    catch
        error:badarg ->
            {error, no_batch_span_processor};
        _:_ ->
            {error, other}
    end.

complete_exporting(Data=#data{handed_off_table=ExportingTable})
  when ExportingTable =/= undefined ->
    new_export_table(ExportingTable),
    {next_state, idle, Data#data{runner_pid=undefined,
                                 handed_off_table=undefined}};
complete_exporting(Data) ->
    {next_state, idle, Data#data{runner_pid=undefined,
                                 handed_off_table=undefined}}.

kill_runner(Data=#data{runner_pid=RunnerPid}) when RunnerPid =/= undefined ->
    Mon = erlang:monitor(process, RunnerPid),
    erlang:unlink(RunnerPid),
    erlang:exit(RunnerPid, kill),
    %% Wait for the runner process termination to be sure that
    %% the export table is destroyed and can be safely recreated
    receive
        {'DOWN', Mon, process, RunnerPid, _} ->
            Data#data{runner_pid=undefined, handed_off_table=undefined}
    end.

new_export_table(Name) ->
     ets:new(Name, [public,
                    named_table,
                    {write_concurrency, true},
                    duplicate_bag,
                    %% OpenTelemetry exporter protos group by the
                    %% instrumentation_scope. So using instrumentation_scope
                    %% as the key means we can easily lookup all spans for
                    %% for each instrumentation_scope and export together.
                    {keypos, #span.instrumentation_scope}]).

export_spans(#data{exporter=Exporter,
                   resource=Resource,
                   table_1=Table1,
                   table_2=Table2,
                   reg_name=RegName}) ->
    CurrentTable = ?CURRENT_TABLE(RegName),
    case ets:info(CurrentTable, size) of
        0 ->
            %% nothing to do if the table is empty
            ok;
        _ ->
            NewCurrentTable = case CurrentTable of
                                  Table1 ->
                                      Table2;
                                  Table2 ->
                                      Table1
                              end,

            %% an atom is a single word so this does not trigger a global GC
            persistent_term:put(?CURRENT_TABLES_KEY(RegName), NewCurrentTable),
            %% set the table to accept inserts
            enable(RegName),

            Self = self(),
            RunnerPid = erlang:spawn_link(fun() -> send_spans(Self, Resource, Exporter) end),
            ets:give_away(CurrentTable, RunnerPid, export),
            {CurrentTable, RunnerPid}
    end.

send_spans(FromPid, Resource, Exporter) ->
    receive
        {'ETS-TRANSFER', Table, FromPid, export} ->
            export(Exporter, Resource, Table),
            ets:delete(Table),
            completed(FromPid)
    end.

completed(FromPid) ->
    FromPid ! {completed, self()}.

export(undefined, _, _) ->
    true;
export(Exporter, Resource, SpansTid) ->
    %% don't let a exporter exception crash us
    %% and return true if exporter failed
    try
        otel_exporter_traces:export(Exporter, SpansTid, Resource) =:= failed_not_retryable
    catch
        Kind:Reason:StackTrace ->
            ?LOG_INFO(#{source => exporter,
                        during => export,
                        kind => Kind,
                        reason => Reason,
                        exporter => Exporter,
                        stacktrace => StackTrace}, #{report_cb => fun ?MODULE:report_cb/1}),
            true
    end.

%% logger format functions
%% @private
report_cb(#{source := exporter,
            during := export,
            kind := Kind,
            reason := Reason,
            exporter := {ExporterModule, _},
            stacktrace := StackTrace}) ->
    {"span exporter threw exception: exporter=~p ~ts",
     [ExporterModule, otel_utils:format_exception(Kind, Reason, StackTrace)]}.
