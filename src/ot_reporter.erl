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
%% @doc This module has the behaviour that each reporter must implement
%% and creates the buffer of trace spans to be reported.
%%
%% The reporter process can be configured to report the current finished
%% spans based on timeouts and the size of the finished spans table.
%%
%% Timeouts:
%%   reporting_timeout_ms: How long to let the reports run before killing.
%%   check_table_size_ms: Timeout to check the size of the report table.
%%   send_interval_ms: How often to trigger running the reporters.
%%
%% The size limit of the current table where finished spans are stored can
%% be configured with the `size_limit_bytes' option.
%% @end
%%%-----------------------------------------------------------------------
-module(ot_reporter).

-behaviour(gen_statem).

-compile({no_auto_import, [register/2]}).

-export([start_link/1,
         store_span/1,
         register/1,
         register/2]).

-export([init/1,
         callback_mode/0,
         idle/3,
         reporting/3,
         terminate/3]).

-include("opentelemetry.hrl").
-include_lib("kernel/include/logger.hrl").

%% behaviour for reporters to implement
-type opts() :: term().

%% Do any initialization of the reporter here and return configuration
%% that will be passed along with a list of spans to the `report' function.
-callback init(term()) -> opts().

%% This function is called when the configured interval expires with any
%% spans that have been collected so far and the configuration returned in `init'.
%% Do whatever needs to be done to report each span here, the caller will block
%% until it returns.
-callback report(ets:tid(), opts()) -> ok.

-record(data, {reporters            :: [{module(), term()}],
               handed_off_table     :: atom() | undefined,
               runner_pid           :: pid() | undefined,
               size_limit           :: integer() | infinity,
               reporting_timeout_ms :: integer(),
               check_table_size_ms  :: integer() | infinity,
               send_interval_ms     :: integer()}).

-define(CURRENT_TABLES_KEY, {?MODULE, current_table}).
-define(TABLE_1, ot_report_table1).
-define(TABLE_2, ot_report_table2).
-define(CURRENT_TABLE, persistent_term:get(?CURRENT_TABLES_KEY)).

-define(DEFAULT_MAX_TABLE_SIZE, infinity).
-define(DEFAULT_REPORT_INTERVAL, timer:minutes(1)).
-define(DEFAULT_REPORTING_TIMEOUT, timer:minutes(30)).
-define(DEFAULT_CHECK_TABLE_SIZE_INTERVAL, infinity).

start_link(Opts) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

%% @equiv register(Reporter, []).
register(Reporter) ->
    register(Reporter, []).

%% @doc Register new traces reporter `Reporter' with `Config'.
-spec register(module(), term()) -> ok.
register(Reporter, Options) ->
    gen_statem:call(?MODULE, {register, init_reporter({Reporter, Options})}).

-spec store_span(opencensus:span()) -> true | {error, invalid_span} | {error, no_report_buffer}.
store_span(Span=#span{}) ->
    try
        ets:insert(?CURRENT_TABLE, Span)
    catch
        error:badarg ->
            {error, no_report_buffer}
    end;
store_span(_) ->
    {error, invalid_span}.

init([Args]) ->
    process_flag(trap_exit, true),

    SizeLimit = proplists:get_value(size_limit_bytes, Args, ?DEFAULT_MAX_TABLE_SIZE),
    ReportingTimeout = proplists:get_value(reporting_timeout_ms, Args, ?DEFAULT_REPORTING_TIMEOUT),
    SendInterval = proplists:get_value(send_interval_ms, Args, ?DEFAULT_REPORT_INTERVAL),
    CheckTableSize = proplists:get_value(check_table_size_ms, Args, ?DEFAULT_CHECK_TABLE_SIZE_INTERVAL),
    Reporters = [init_reporter(Config) || Config <- proplists:get_value(reporters, Args, [])],

    _Tid1 = new_report_table(?TABLE_1),
    _Tid2 = new_report_table(?TABLE_2),
    persistent_term:put(?CURRENT_TABLES_KEY, ?TABLE_1),

    {ok, idle, #data{reporters=Reporters,
                     handed_off_table=undefined,
                     size_limit=case SizeLimit of
                                    infinity -> infinity;
                                    _ -> SizeLimit div erlang:system_info(wordsize)
                                end,
                     reporting_timeout_ms=ReportingTimeout,
                     check_table_size_ms=CheckTableSize,
                     send_interval_ms=SendInterval}}.

callback_mode() ->
    [state_functions, state_enter].

idle(enter, _OldState, #data{send_interval_ms=SendInterval}) ->
    {keep_state_and_data, [{{timeout, report_spans}, SendInterval, report_spans}]};
idle(_, report_spans, Data) ->
    {next_state, reporting, Data};
idle(EventType, Event, Data) ->
    handle_event_(idle, EventType, Event, Data).

reporting({timeout, report_spans}, report_spans, _) ->
    {keep_state_and_data, [postpone]};
reporting(enter, _OldState, Data=#data{reporting_timeout_ms=ReportingTimeout,
                                       send_interval_ms=SendInterval}) ->
    {OldTableName, RunnerPid} = report_spans(Data),
    {keep_state, Data#data{runner_pid=RunnerPid,
                           handed_off_table=OldTableName},
     [{state_timeout, ReportingTimeout, reporting_timeout},
      {{timeout, report_spans}, SendInterval, report_spans}]};
reporting(state_timeout, reporting_timeout, Data=#data{handed_off_table=ReportingTable}) ->
    %% kill current reporting process because it is taking too long
    %% which deletes the reporting table, so create a new one and
    %% repeat the state to force another span reporting immediately
    Data1 = kill_runner(Data),
    new_report_table(ReportingTable),
    {repeat_state, Data1};
%% important to verify runner_pid and FromPid are the same in case it was sent
%% after kill_runner was called but before it had done the unlink
reporting(info, {'EXIT', FromPid, _}, Data=#data{runner_pid=FromPid}) ->
    complete_reporting([], Data);
%% important to verify runner_pid and FromPid are the same in case it was sent
%% after kill_runner was called but before it had done the unlink
reporting(info, {completed, FromPid, FailedReporters}, Data=#data{runner_pid=FromPid}) ->
    complete_reporting(FailedReporters, Data);
reporting(EventType, Event, Data) ->
    handle_event_(reporting, EventType, Event, Data).

handle_event_(_State, {timeout, check_table_size}, check_table_size, #data{size_limit=infinity}) ->
    keep_state_and_data;
handle_event_(State, {timeout, check_table_size}, check_table_size, Data=#data{size_limit=SizeLimit}) ->
    case ets:info(?CURRENT_TABLE, memory) of
        M when M >= SizeLimit, State =:= idle ->
            Data1 = kill_runner(Data),
            {next_state, reporting, Data1};
        M when M >= SizeLimit, State =:= reporting ->
            Data1 = kill_runner(Data),
            {repeat_state, Data1};
        _ ->
            keep_state_and_data
    end;
handle_event_(_, {call, From}, {register, Reporter}, Data=#data{reporters=Reporters}) ->
    {keep_state, Data#data{reporters=[Reporter | Reporters]}, [{reply, From, ok}]};
handle_event_(_, _, _, _) ->
    keep_state_and_data.

terminate(_, _, _Data) ->
    ok.

%%

complete_reporting(FailedReporters, Data=#data{reporters=Reporters,
                                               handed_off_table=ReportingTable})
  when ReportingTable =/= undefined ->
    new_report_table(ReportingTable),
    {next_state, idle, Data#data{reporters=Reporters--FailedReporters,
                                 runner_pid=undefined,
                                 handed_off_table=undefined}}.

kill_runner(Data=#data{runner_pid=RunnerPid}) ->
    erlang:unlink(RunnerPid),
    erlang:exit(RunnerPid, kill),
    Data#data{runner_pid=undefined,
              handed_off_table=undefined}.

new_report_table(Name) ->
     ets:new(Name, [public, named_table, {write_concurrency, true}, duplicate_bag]).

init_reporter({Reporter, Config}) ->
    {Reporter, Reporter:init(Config)};
init_reporter(Reporter) when is_atom(Reporter) ->
    {Reporter, Reporter:init([])}.

report_spans(#data{reporters=Reporters}) ->
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
    RunnerPid = erlang:spawn_link(fun() -> send_spans(Self, Reporters) end),
    ets:give_away(CurrentTable, RunnerPid, report),
    {CurrentTable, RunnerPid}.

%% Additional benefit of using a separate process is calls to `register` won't
%% timeout if the actual reporting takes longer than the call timeout
send_spans(FromPid, Reporters) ->
    receive
        {'ETS-TRANSFER', Table, FromPid, report} ->
            FailedReporters = lists:filtermap(fun({Reporter, Config}) ->
                                                      report(Reporter, Table, Config)
                                              end, Reporters),
            ets:delete(Table),
            completed(FromPid, FailedReporters)
    end.

completed(FromPid, FailedReporters) ->
    FromPid ! {completed, self(), FailedReporters}.

report(undefined, _, _) ->
    true;
report(Reporter, SpansTid, Config) ->
    %% don't let a reporter exception crash us
    %% and return true if reporter failed
    try
        Reporter:report(SpansTid, Config) =:= drop
    catch
        Class:Exception:StackTrace ->
            ?LOG_INFO("dropping reporter that threw exception: reporter=~p ~p:~p stacktrace=~p",
                      [Reporter, Class, Exception, StackTrace]),
            true
    end.
