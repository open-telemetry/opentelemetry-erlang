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
%% @doc This module has the behaviour that each exporter must implement
%% and creates the buffer of trace spans to be exported.
%%
%% The exporter process can be configured to export the current finished
%% spans based on timeouts and the size of the finished spans table.
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
-module(ot_batch_processor).

-behaviour(gen_statem).
-behaviour(ot_span_processor).

-export([start_link/1,
         on_start/2,
         on_end/2,
         set_exporter/1,
         set_exporter/2]).

-export([init/1,
         callback_mode/0,
         idle/3,
         exporting/3,
         terminate/3]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("kernel/include/logger.hrl").

%% behaviour for exporters to implement
-type opts() :: term().

%% Do any initialization of the exporter here and return configuration
%% that will be passed along with a list of spans to the `export' function.
-callback init(term()) -> opts().

%% This function is called when the configured interval expires with any
%% spans that have been collected so far and the configuration returned in `init'.
%% Do whatever needs to be done to export each span here, the caller will block
%% until it returns.
-callback export(ets:tid(), opts()) -> ok | success | failed_not_retryable | failed_retryable.

-record(data, {exporter             :: {module(), term()} | undefined,
               handed_off_table     :: atom() | undefined,
               runner_pid           :: pid() | undefined,
               max_queue_size       :: integer() | infinity,
               exporting_timeout_ms :: integer(),
               check_table_size_ms  :: integer() | infinity,
               scheduled_delay_ms   :: integer()}).

-define(CURRENT_TABLES_KEY, {?MODULE, current_table}).
-define(TABLE_1, ot_export_table1).
-define(TABLE_2, ot_export_table2).
-define(CURRENT_TABLE, persistent_term:get(?CURRENT_TABLES_KEY)).

-define(DEFAULT_MAX_QUEUE_SIZE, 2048).
-define(DEFAULT_SCHEDULED_DELAY_MS, timer:seconds(5)).
-define(DEFAULT_EXPORTER_TIMEOUT_MS, timer:minutes(5)).
-define(DEFAULT_CHECK_TABLE_SIZE_MS, timer:seconds(1)).

-define(ENABLED_KEY, {?MODULE, enabled_key}).

start_link(Opts) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

%% @equiv set_exporter(Exporter, []).
set_exporter(Exporter) ->
    set_exporter(Exporter, []).

%% @doc Sets the batch exporter `Exporter'.
-spec set_exporter(module(), term()) -> ok.
set_exporter(Exporter, Options) ->
    gen_statem:call(?MODULE, {set_exporter, {Exporter, Options}}).

-spec on_start(opentelemetry:span(), ot_span_processor:processor_config()) -> opentelemetry:span().
on_start(Span, _) ->
    Span.

-spec on_end(opentelemetry:span(), ot_span_processor:processor_config())
            -> true | dropped | {error, invalid_span} | {error, no_export_buffer}.
on_end(Span=#span{}, _) ->
    do_insert(Span);
on_end(_Span, _) ->
    {error, invalid_span}.

init([Args]) ->
    process_flag(trap_exit, true),

    SizeLimit = proplists:get_value(max_queue_size, Args, ?DEFAULT_MAX_QUEUE_SIZE),
    ExportingTimeout = proplists:get_value(exporting_timeout_ms, Args, ?DEFAULT_EXPORTER_TIMEOUT_MS),
    ScheduledDelay = proplists:get_value(scheduled_delay_ms, Args, ?DEFAULT_SCHEDULED_DELAY_MS),
    CheckTableSize = proplists:get_value(check_table_size_ms, Args, ?DEFAULT_CHECK_TABLE_SIZE_MS),

    Exporter = init_exporter(proplists:get_value(exporter, Args, undefined)),

    _Tid1 = new_export_table(?TABLE_1),
    _Tid2 = new_export_table(?TABLE_2),
    persistent_term:put(?CURRENT_TABLES_KEY, ?TABLE_1),

    enable(),

    {ok, idle, #data{exporter=Exporter,
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

idle(enter, _OldState, #data{scheduled_delay_ms=SendInterval}) ->
    {keep_state_and_data, [{{timeout, export_spans}, SendInterval, export_spans}]};
idle(_, export_spans, Data) ->
    {next_state, exporting, Data};
idle(EventType, Event, Data) ->
    handle_event_(idle, EventType, Event, Data).

exporting({timeout, export_spans}, export_spans, _) ->
    {keep_state_and_data, [postpone]};
exporting(enter, _OldState, Data=#data{exporting_timeout_ms=ExportingTimeout,
                                       scheduled_delay_ms=SendInterval}) ->
    {OldTableName, RunnerPid} = export_spans(Data),
    {keep_state, Data#data{runner_pid=RunnerPid,
                           handed_off_table=OldTableName},
     [{state_timeout, ExportingTimeout, exporting_timeout},
      {{timeout, export_spans}, SendInterval, export_spans}]};
exporting(state_timeout, exporting_timeout, Data=#data{handed_off_table=ExportingTable}) ->
    %% kill current exporting process because it is taking too long
    %% which deletes the exporting table, so create a new one and
    %% repeat the state to force another span exporting immediately
    Data1 = kill_runner(Data),
    new_export_table(ExportingTable),
    {repeat_state, Data1};
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
handle_event_(_, {call, From}, {set_exporter, Exporter}, Data=#data{exporter=OldExporter}) ->
    shutdown_exporter(OldExporter),
    {keep_state, Data#data{exporter=init_exporter(Exporter)}, [{reply, From, ok}]};
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
                                 handed_off_table=undefined}}.

kill_runner(Data=#data{runner_pid=RunnerPid}) ->
    erlang:unlink(RunnerPid),
    erlang:exit(RunnerPid, kill),
    Data#data{runner_pid=undefined,
              handed_off_table=undefined}.

new_export_table(Name) ->
     ets:new(Name, [public, named_table, {write_concurrency, true}, duplicate_bag]).

init_exporter(undefined) ->
    undefined;
init_exporter({ExporterModule, Config}) when is_atom(ExporterModule) ->
    case ExporterModule:init(Config) of
        {ok, ExporterConfig} ->
            {ExporterModule, ExporterConfig};
        ignore ->
            undefined
    end;
init_exporter(ExporterModule) when is_atom(ExporterModule) ->
    init_exporter({ExporterModule, []}).

shutdown_exporter(undefined) ->
    ok;
shutdown_exporter({ExporterModule, Config}) ->
    ExporterModule:shutdown(Config).

export_spans(#data{exporter=Exporter}) ->
    CurrentTable = ?CURRENT_TABLE,
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
    RunnerPid = erlang:spawn_link(fun() -> send_spans(Self, Exporter) end),
    ets:give_away(CurrentTable, RunnerPid, export),
    {CurrentTable, RunnerPid}.

%% Additional benefit of using a separate process is calls to `register` won't
%% timeout if the actual exporting takes longer than the call timeout
send_spans(FromPid, Exporter) ->
    receive
        {'ETS-TRANSFER', Table, FromPid, export} ->
            TableName = ets:rename(Table, current_send_table),
            export(Exporter, TableName),
            ets:delete(TableName),
            completed(FromPid)
    end.

completed(FromPid) ->
    FromPid ! {completed, self()}.

export(undefined, _) ->
    true;
export({Exporter, Config}, SpansTid) ->
    %% don't let a exporter exception crash us
    %% and return true if exporter failed
    try
        Exporter:export(SpansTid, Config) =:= failed_not_retryable
    catch
        Class:Exception:StackTrace ->
            ?LOG_INFO("exporter threw exception: exporter=~p ~p:~p stacktrace=~p",
                      [Exporter, Class, Exception, StackTrace]),
            true
    end.
