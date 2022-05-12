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
%% @doc The Batch Span Processor implements the `otel_span_processor'
%% behaviour. It stores finished Spans in a ETS table buffer and exports
%% them on an interval or when the table reaches a maximum size.
%%
%% Timeouts:
%%   exporting_timeout_ms: How long to let the exports run before killing.
%%   check_table_size_ms: Timeout to check the size of the export table.
%%   scheduled_delay_ms: How often to trigger running the exporters.
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
         set_exporter/1,
         set_exporter/2,
         report_cb/1]).

-export([init/1,
         callback_mode/0,
         idle/3,
         exporting/3,
         terminate/3]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("kernel/include/logger.hrl").
-include("otel_span.hrl").

-record(data, {exporter             :: {module(), term()} | undefined,
               exporter_config      :: {module(), term()} | undefined,
               resource             :: otel_resource:t(),
               handed_off_table     :: atom() | undefined,
               runner_pid           :: pid() | undefined,
               max_queue_size       :: integer() | infinity,
               exporting_timeout_ms :: integer(),
               check_table_size_ms  :: integer() | infinity,
               scheduled_delay_ms   :: integer()}).

-define(CURRENT_TABLES_KEY, {?MODULE, current_table}).
-define(TABLE_1, otel_export_table1).
-define(TABLE_2, otel_export_table2).
-define(CURRENT_TABLE, persistent_term:get(?CURRENT_TABLES_KEY)).

-define(DEFAULT_MAX_QUEUE_SIZE, 2048).
-define(DEFAULT_SCHEDULED_DELAY_MS, timer:seconds(5)).
-define(DEFAULT_EXPORTER_TIMEOUT_MS, timer:minutes(5)).
-define(DEFAULT_CHECK_TABLE_SIZE_MS, timer:seconds(1)).

-define(ENABLED_KEY, {?MODULE, enabled_key}).

start_link(Opts) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

%% @equiv set_exporter(Exporter, [])
set_exporter(Exporter) ->
    set_exporter(Exporter, []).

%% @doc Sets the batch exporter `Exporter'.
-spec set_exporter(module(), term()) -> ok.
set_exporter(Exporter, Options) ->
    gen_statem:call(?MODULE, {set_exporter, {Exporter, Options}}).

-spec on_start(otel_ctx:t(), opentelemetry:span(), otel_span_processor:processor_config())
              -> opentelemetry:span().
on_start(_Ctx, Span, _) ->
    Span.

-spec on_end(opentelemetry:span(), otel_span_processor:processor_config())
            -> true | dropped | {error, invalid_span} | {error, no_export_buffer}.
on_end(#span{trace_flags=TraceFlags}, _) when not(?IS_SAMPLED(TraceFlags)) ->
    dropped;
on_end(Span=#span{}, _) ->
    do_insert(Span);
on_end(_Span, _) ->
    {error, invalid_span}.

-spec force_flush(otel_span_processor:processor_config()) -> ok.
force_flush(_) ->
    gen_statem:cast(?MODULE, force_flush).

init([Args]) ->
    process_flag(trap_exit, true),

    SizeLimit = maps:get(max_queue_size, Args, ?DEFAULT_MAX_QUEUE_SIZE),
    ExportingTimeout = maps:get(exporting_timeout_ms, Args, ?DEFAULT_EXPORTER_TIMEOUT_MS),
    ScheduledDelay = maps:get(scheduled_delay_ms, Args, ?DEFAULT_SCHEDULED_DELAY_MS),
    CheckTableSize = maps:get(check_table_size_ms, Args, ?DEFAULT_CHECK_TABLE_SIZE_MS),

    Resource = otel_tracer_provider:resource(),

    _Tid1 = new_export_table(?TABLE_1),
    _Tid2 = new_export_table(?TABLE_2),
    persistent_term:put(?CURRENT_TABLES_KEY, ?TABLE_1),

    enable(),

    {ok, idle, #data{exporter=undefined,
                     exporter_config=maps:get(exporter, Args, none),
                     resource = Resource,
                     handed_off_table=undefined,
                     max_queue_size=case SizeLimit of
                                        infinity -> infinity;
                                        _ -> SizeLimit div erlang:system_info(wordsize)
                                    end,
                     exporting_timeout_ms=ExportingTimeout,
                     check_table_size_ms=CheckTableSize,
                     scheduled_delay_ms=ScheduledDelay}}.

callback_mode() ->
    [state_functions, state_enter].

idle(enter, _OldState, Data=#data{exporter=undefined,
                                  exporter_config=ExporterConfig,
                                  scheduled_delay_ms=SendInterval}) ->
    Exporter = otel_exporter:init(ExporterConfig),
    {keep_state, Data#data{exporter=Exporter}, [{{timeout, export_spans}, SendInterval, export_spans}]};
idle(enter, _OldState, #data{scheduled_delay_ms=SendInterval}) ->
    {keep_state_and_data, [{{timeout, export_spans}, SendInterval, export_spans}]};
idle(_, export_spans, Data=#data{exporter=undefined,
                                 exporter_config=ExporterConfig}) ->
    Exporter = otel_exporter:init(ExporterConfig),
    {next_state, exporting, Data#data{exporter=Exporter}};
idle(_, export_spans, Data) ->
    {next_state, exporting, Data};
idle(EventType, Event, Data) ->
    handle_event_(idle, EventType, Event, Data).

%% receiving an `export_spans' timeout while exporting means the `ExportingTimeout'
%% is shorter than the `SendInterval'. Postponing the event will ensure we export
%% after
exporting({timeout, export_spans}, export_spans, _) ->
    {keep_state_and_data, [postpone]};
exporting(enter, _OldState, #data{exporter=undefined}) ->
    %% exporter still undefined, go back to idle
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
handle_event_(_State, {timeout, check_table_size}, check_table_size, #data{max_queue_size=MaxQueueSize}) ->
    case ets:info(?CURRENT_TABLE, size) of
        M when M >= MaxQueueSize ->
            disable(),
            keep_state_and_data;
        _ ->
            enable(),
            keep_state_and_data
    end;
handle_event_(_, {call, From}, {set_exporter, ExporterConfig}, Data=#data{exporter=OldExporter}) ->
    otel_exporter:shutdown(OldExporter),
    {keep_state, Data#data{exporter=undefined,
                           exporter_config=ExporterConfig}, [{reply, From, ok},
                                                             {next_event, internal, init_exporter}]};
handle_event_(_, internal, init_exporter, Data=#data{exporter=undefined,
                                                     exporter_config=ExporterConfig}) ->
    Exporter = otel_exporter:init(ExporterConfig),
    {keep_state, Data#data{exporter=Exporter}};
handle_event_(_, _, _, _) ->
    keep_state_and_data.

terminate(_, _, _Data) ->
    %% TODO: flush buffers to exporter
    ok.

%%

enable()->
    persistent_term:put(?ENABLED_KEY, true).

disable() ->
    persistent_term:put(?ENABLED_KEY, false).

is_enabled() ->
    persistent_term:get(?ENABLED_KEY, true).

do_insert(Span) ->
    try
        case is_enabled() of
            true ->
                ets:insert(?CURRENT_TABLE, Span);
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

kill_runner(Data=#data{runner_pid=RunnerPid}) ->
    erlang:unlink(RunnerPid),
    erlang:exit(RunnerPid, kill),
    Data#data{runner_pid=undefined,
              handed_off_table=undefined}.

new_export_table(Name) ->
     ets:new(Name, [public,
                    named_table,
                    {write_concurrency, true},
                    duplicate_bag,
                    %% OpenTelemetry exporter protos group by the
                    %% instrumentation_library. So using instrumentation_library
                    %% as the key means we can easily lookup all spans for
                    %% for each instrumentation_library and export together.
                    {keypos, #span.instrumentation_library}]).

export_spans(#data{exporter=Exporter,
                   resource=Resource}) ->
    CurrentTable = ?CURRENT_TABLE,
    case ets:info(CurrentTable, size) of
        0 ->
            %% nothing to do if the table is empty
            ok;
        _ ->
            NewCurrentTable = case CurrentTable of
                                  ?TABLE_1 ->
                                      ?TABLE_2;
                                  ?TABLE_2 ->
                                      ?TABLE_1
                              end,

            %% an atom is a single word so this does not trigger a global GC
            persistent_term:put(?CURRENT_TABLES_KEY, NewCurrentTable),
            %% set the table to accept inserts
            enable(),

            Self = self(),
            RunnerPid = erlang:spawn_link(fun() -> send_spans(Self, Resource, Exporter) end),
            ets:give_away(CurrentTable, RunnerPid, export),
            {CurrentTable, RunnerPid}
    end.

%% Additional benefit of using a separate process is calls to `register` won't
%% timeout if the actual exporting takes longer than the call timeout
send_spans(FromPid, Resource, Exporter) ->
    receive
        {'ETS-TRANSFER', Table, FromPid, export} ->
            TableName = ets:rename(Table, current_send_table),
            export(Exporter, Resource, TableName),
            ets:delete(TableName),
            completed(FromPid)
    end.

completed(FromPid) ->
    FromPid ! {completed, self()}.

export(undefined, _, _) ->
    true;
export({ExporterModule, Config}, Resource, SpansTid) ->
    %% don't let a exporter exception crash us
    %% and return true if exporter failed
    try
        otel_exporter:export(ExporterModule, SpansTid, Resource, Config) =:= failed_not_retryable
    catch
        Kind:Reason:StackTrace ->
            ?LOG_INFO(#{source => exporter,
                        during => export,
                        kind => Kind,
                        reason => Reason,
                        exporter => ExporterModule,
                        stacktrace => StackTrace}, #{report_cb => fun ?MODULE:report_cb/1}),
            true
    end.

%% logger format functions
report_cb(#{source := exporter,
            during := export,
            kind := Kind,
            reason := Reason,
            exporter := ExporterModule,
            stacktrace := StackTrace}) ->
    {"exporter threw exception: exporter=~p ~ts",
     [ExporterModule, otel_utils:format_exception(Kind, Reason, StackTrace)]}.
