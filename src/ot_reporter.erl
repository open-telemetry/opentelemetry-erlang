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
%% @end
%%%-----------------------------------------------------------------------
-module(ot_reporter).

-behaviour(gen_server).

-compile({no_auto_import, [register/2]}).

-export([start_link/1,
         store_span/1,
         register/1,
         register/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

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

-record(state, {reporters :: [{module(), term()}],
                tables :: {ets:tid(), ets:tid()},
                send_interval_ms :: integer(),
                timer_ref :: reference()}).

-define(CURRENT_TABLES_KEY, {?MODULE, current_table}).
-define(TABLE_1, ot_report_table1).
-define(TABLE_2, ot_report_table2).
-define(CURRENT_TABLE, persistent_term:get(?CURRENT_TABLES_KEY)).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

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
    gen_server:call(?MODULE, {register, init_reporter({Reporter, Options})}).

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
    SendInterval = proplists:get_value(send_interval_ms, Args, 30000),
    Reporters = [init_reporter(Config) || Config <- proplists:get_value(reporters, Args, [])],

    Tid1 = new_report_table(?TABLE_1),
    Tid2 = new_report_table(?TABLE_2),
    persistent_term:put(?CURRENT_TABLES_KEY, ?TABLE_1),

    Ref = erlang:send_after(SendInterval, self(), report_spans),
    {ok, #state{reporters=Reporters,
                tables={Tid1, Tid2},
                send_interval_ms=SendInterval,
                timer_ref=Ref}}.

handle_call({register, Reporter}, _From, #state{reporters=Reporters} = State) ->
    {reply, ok, State#state{reporters=[Reporter | Reporters]}};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(report_spans, State=#state{reporters=Reporters,
                                       send_interval_ms=SendInterval,
                                       timer_ref=Ref}) ->
    erlang:cancel_timer(Ref),
    Ref1 = erlang:send_after(SendInterval, self(), report_spans),
    Reporters1 = send_spans(Reporters),
    {noreply, State#state{reporters=Reporters1,
                          timer_ref=Ref1}}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, #state{timer_ref=Ref}) ->
    erlang:cancel_timer(Ref),
    ok.

%%

new_report_table(Name) ->
     ets:new(Name, [public, named_table, {write_concurrency, true}, duplicate_bag]).

init_reporter({Reporter, Config}) ->
    {Reporter, Reporter:init(Config)};
init_reporter(Reporter) when is_atom(Reporter) ->
    {Reporter, Reporter:init([])}.

send_spans(Reporters) ->
    CurrentTable = ?CURRENT_TABLE,
    NewCurrentTable = case CurrentTable of
                          ?TABLE_1 ->
                              ?TABLE_2;
                          ?TABLE_2 ->
                              ?TABLE_1
                      end,

    %% an atom is a single word so this does not trigger a global GC
    persistent_term:put(?CURRENT_TABLES_KEY, NewCurrentTable),

    %% TODO would it simplify GC if we spawned a process and gave it CurrentTable?
    Reporters1 = lists:filtermap(fun({Reporter, Config}) ->
                                         report(Reporter, CurrentTable, Config)
                                 end, Reporters),
    ets:delete_all_objects(CurrentTable),

    Reporters1.

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
