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
-module(ot_exporter).

-behaviour(gen_statem).

-compile({no_auto_import, [register/2]}).

-export([start_link/1,
         store_span/1,
         register/1,
         register/2]).

-export([init/1,
         callback_mode/0,
         idle/3,
         exporting/3,
         terminate/3]).

-include("opentelemetry.hrl").
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

-record(data, {exporters            :: [{module(), term()}],
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
-define(DEFAULT_SCHEDULED_DEPLAY_MS, timer:seconds(5)).
-define(DEFAULT_EXPORTING_TIMEOUT, timer:minutes(5)).
-define(DEFAULT_CHECK_TABLE_SIZE_INTERVAL, infinity).

start_link(Opts) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

%% @equiv register(Exporter, []).
register(Exporter) ->
    register(Exporter, []).

%% @doc Register new traces exporter `Exporter' with `Config'.
-spec register(module(), term()) -> ok.
register(Exporter, Options) ->
    gen_statem:call(?MODULE, {register, init_exporter({Exporter, Options})}).

-spec store_span(opencensus:span()) -> true | {error, invalid_span} | {error, no_export_buffer}.
store_span(Span=#span{}) ->
    try
        ets:insert(?CURRENT_TABLE, Span)
    catch
        error:badarg ->
            {error, no_export_buffer}
    end;
store_span(_) ->
    {error, invalid_span}.

init([Args]) ->
    process_flag(trap_exit, true),

    SizeLimit = proplists:get_value(max_queue_size, Args, ?DEFAULT_MAX_QUEUE_SIZE),
    ExportingTimeout = proplists:get_value(exporting_timeout_ms, Args, ?DEFAULT_EXPORTING_TIMEOUT),
    ScheduledDelay = proplists:get_value(scheduled_delay_ms, Args, ?DEFAULT_SCHEDULED_DEPLAY_MS),
    CheckTableSize = proplists:get_value(check_table_size_ms, Args, ?DEFAULT_CHECK_TABLE_SIZE_INTERVAL),
    Exporters = [init_exporter(Config) || Config <- proplists:get_value(exporters, Args, [])],

    _Tid1 = new_export_table(?TABLE_1),
    _Tid2 = new_export_table(?TABLE_2),
    persistent_term:put(?CURRENT_TABLES_KEY, ?TABLE_1),

    {ok, idle, #data{exporters=Exporters,
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
    complete_exporting([], Data);
%% important to verify runner_pid and FromPid are the same in case it was sent
%% after kill_runner was called but before it had done the unlink
exporting(info, {completed, FromPid, FailedExporters}, Data=#data{runner_pid=FromPid}) ->
    complete_exporting(FailedExporters, Data);
exporting(EventType, Event, Data) ->
    handle_event_(exporting, EventType, Event, Data).

handle_event_(_State, {timeout, check_table_size}, check_table_size, #data{max_queue_size=infinity}) ->
    keep_state_and_data;
handle_event_(State, {timeout, check_table_size}, check_table_size, Data=#data{max_queue_size=SizeLimit}) ->
    case ets:info(?CURRENT_TABLE, memory) of
        M when M >= SizeLimit, State =:= idle ->
            Data1 = kill_runner(Data),
            {next_state, exporting, Data1};
        M when M >= SizeLimit, State =:= exporting ->
            Data1 = kill_runner(Data),
            {repeat_state, Data1};
        _ ->
            keep_state_and_data
    end;
handle_event_(_, {call, From}, {register, Exporter}, Data=#data{exporters=Exporters}) ->
    {keep_state, Data#data{exporters=[Exporter | Exporters]}, [{reply, From, ok}]};
handle_event_(_, _, _, _) ->
    keep_state_and_data.

terminate(_, _, _Data) ->
    ok.

%%

complete_exporting(FailedExporters, Data=#data{exporters=Exporters,
                                               handed_off_table=ExportingTable})
  when ExportingTable =/= undefined ->
    new_export_table(ExportingTable),
    {next_state, idle, Data#data{exporters=Exporters--FailedExporters,
                                 runner_pid=undefined,
                                 handed_off_table=undefined}}.

kill_runner(Data=#data{runner_pid=RunnerPid}) ->
    erlang:unlink(RunnerPid),
    erlang:exit(RunnerPid, kill),
    Data#data{runner_pid=undefined,
              handed_off_table=undefined}.

new_export_table(Name) ->
     ets:new(Name, [public, named_table, {write_concurrency, true}, duplicate_bag]).

init_exporter({Exporter, Config}) when is_atom(Exporter) ->
    {fun Exporter:export/2, Exporter:init(Config)};
init_exporter(Exporter) when is_atom(Exporter) ->
    {fun Exporter:export/2, Exporter:init([])};
init_exporter(Exporter) when is_function(Exporter) ->
    {Exporter, []};
init_exporter({Exporter, Config}) when is_function(Exporter) ->
    {Exporter, Config}.

export_spans(#data{exporters=Exporters}) ->
    CurrentTable = ?CURRENT_TABLE,
    NewCurrentTable = case CurrentTable of
                          ?TABLE_1 ->
                              ?TABLE_2;
                          ?TABLE_2 ->
                              ?TABLE_1
                      end,

    %% an atom is a single word so this does not trigger a global GC
    persistent_term:put(?CURRENT_TABLES_KEY, NewCurrentTable),

    Self = self(),
    RunnerPid = erlang:spawn_link(fun() -> send_spans(Self, Exporters) end),
    ets:give_away(CurrentTable, RunnerPid, export),
    {CurrentTable, RunnerPid}.

%% Additional benefit of using a separate process is calls to `register` won't
%% timeout if the actual exporting takes longer than the call timeout
send_spans(FromPid, Exporters) ->
    receive
        {'ETS-TRANSFER', Table, FromPid, export} ->
            TableName = ets:rename(Table, current_send_table),
            FailedExporters = lists:filtermap(fun({Exporter, Config}) ->
                                                      export(Exporter, TableName, Config)
                                              end, Exporters),
            ets:delete(TableName),
            completed(FromPid, FailedExporters)
    end.

completed(FromPid, FailedExporters) ->
    FromPid ! {completed, self(), FailedExporters}.

export(undefined, _, _) ->
    true;
export(Exporter, SpansTid, Config) ->
    %% don't let a exporter exception crash us
    %% and return true if exporter failed
    try
        Exporter(SpansTid, Config) =:= failed_not_retryable
    catch
        Class:Exception:StackTrace ->
            ?LOG_INFO("dropping exporter that threw exception: exporter=~p ~p:~p stacktrace=~p",
                      [Exporter, Class, Exception, StackTrace]),
            true
    end.
