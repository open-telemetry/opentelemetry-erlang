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

-record(data, {reporters :: [{module(), term()}],
               runner_pid :: pid(),
               tables :: {ets:tid(), ets:tid()},
               size_limit :: integer(),
               reporting_timeout_ms :: integer(),
               check_table_size_ms :: integer(),
               send_interval_ms :: integer()}).

-define(CURRENT_TABLES_KEY, {?MODULE, current_table}).
-define(TABLE_1, ot_report_table1).
-define(TABLE_2, ot_report_table2).
-define(CURRENT_TABLE, persistent_term:get(?CURRENT_TABLES_KEY)).

%% check size of current report table at heartbeat
%% if it gets too large kill the report process and
%% trigger a new report. Useful if reporting takes
%% longer than the report interval.
-define(DEFAULT_HEARTBEAT, 10000).
-define(DEFAULT_MAX_TABLE_SIZE, 0).
-define(DEFAULT_REPORT_INTERVAL, 30000).

start_link(Opts) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

%% @doc
%% @equiv register(Reporter, []).
%% @end
register(Reporter) ->
    register(Reporter, []).

%% @doc
%% Register new traces reporter `Reporter' with `Config'.
%% @end
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

    ReportingTimeout = proplists:get_value(reporting_timeout_ms, Args, 30000),
    SendInterval = proplists:get_value(send_interval_ms, Args, 30000),
    CheckTableSize = proplists:get_value(check_table_size_ms, Args, 30000),
    Reporters = [init_reporter(Config) || Config <- proplists:get_value(reporters, Args, [])],

    Tid1 = new_report_table(?TABLE_1),
    Tid2 = new_report_table(?TABLE_2),
    persistent_term:put(?CURRENT_TABLES_KEY, ?TABLE_1),

    {ok, idle, #data{reporters=Reporters,
                     tables={Tid1, Tid2},
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
    handle_event(idle, EventType, Event, Data).

reporting({timeout, report_spans}, report_spans, _) ->
    {keep_state_and_data, [postpone]};
reporting(enter, _OldState, Data=#data{reporting_timeout_ms=ReportingTimeout,
                                       send_interval_ms=SendInterval}) ->
    RunnerPid = report_spans(Data),
    {keep_state, Data#data{runner_pid=RunnerPid},
     [{state_timeout, ReportingTimeout, reporting_timeout},
      {{timeout, report_spans}, SendInterval, report_spans}]};
reporting(state_timeout, reporting_timeout, Data) ->
    %% kill current reporting process because it is taking too long
    Data1 = kill_runner(Data),
    {repeat_state, Data1};
reporting(info, {'EXIT', FromPid, _}, Data=#data{runner_pid=FromPid}) ->
    {next_state, idle, Data#data{runner_pid=undefined}};
reporting(EventType, Event, Data) ->
    handle_event(reporting, EventType, Event, Data).

handle_event(_, info, {'ETS-TRANSFER', Table, _FromPid, start}, Data) ->
    %% something happened that crashed the runner
    ets:delete_all_objects(Table),
    {next_state, idle, Data};
handle_event(_, info, {'ETS-TRANSFER', _Table, _FromPid, finish}, Data) ->
    {next_state, idle, Data};
handle_event(_, info, {completed, FailedReporters}, Data=#data{reporters=Reporters}) ->
    {next_state, idle, Data#data{reporters=Reporters--FailedReporters,
                                 runner_pid=undefined}};
handle_event(State, {timeout, check_table_size}, check_table_size, Data=#data{size_limit=SizeLimit}) ->
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
handle_event(_, {call, From}, {register, Reporter}, Data=#data{reporters=Reporters}) ->
    {keep_state, Data#data{reporters=[Reporter | Reporters]}, [{reply, From, ok}]};
handle_event(_, _, _, _) ->
    keep_state_and_data.

terminate(_, _, _Data) ->
    ok.

%%

kill_runner(Data=#data{runner_pid=RunnerPid}) ->
    erlang:unlink(RunnerPid),
    erlang:exit(RunnerPid, kill),
    Data#data{runner_pid=undefined}.

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
    RunnerPid.

%% Additional benefit of using a separate process is calls to `register` won't
%% timeout if the actual reporting takes longer than the call timeout
send_spans(FromPid, Reporters) ->
    receive
        {'ETS-TRANSFER', Table, FromPid, report} ->
            ets:setopts(Table, [{heir, FromPid, start}]),

            %% TODO would it simplify GC if we spawned a process and gave it CurrentTable?
            Reporters1 = lists:filtermap(fun({Reporter, Config}) ->
                                                 report(Reporter, Table, Config)
                                         end, Reporters),
            ets:delete_all_objects(Table),
            ets:give_away(Table, FromPid, finish),
            completed(FromPid, Reporters -- Reporters1)
    end.

completed(FromPid, FailedReporters) ->
    FromPid ! {completed, FailedReporters}.

report(undefined, _, _) ->
    true;
report(Reporter, SpansTid, Config) ->
    %% don't let a reporter exception crash us
    %% and remove a reporter that crashes from future calls to reporters
    try
        Reporter:report(SpansTid, Config) =/= drop
    catch
        Class:Exception:StackTrace ->
            ?LOG_INFO("dropping reporter that threw exception: reporter=~p ~p:~p stacktrace=~p",
                      [Reporter, Class, Exception, StackTrace]),
            false
    end.
