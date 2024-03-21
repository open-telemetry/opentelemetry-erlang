%%%------------------------------------------------------------------------
%% Copyright 2023, OpenTelemetry Authors
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
%% @doc Common overload protection implementation
%% @end
%%%-------------------------------------------------------------------------

-module(otel_batch_olp).

-behaviour(gen_statem).

-export([init_conf/1,
         start_link/2,
         insert_signal/2,
         change_config/3,
         force_flush/1]).

%% gen_statem
-export([init/1,
         callback_mode/0,
         init_exporter/3,
         idle/3,
         exporting/3,
         terminate/3]).

-export_type([otel_batch_olp_config/0,
              otel_batch_olp_state/0,
              max_queue_size/0,
              otel_timeout_ms/0]).

-include_lib("kernel/include/logger.hrl").

%% Metrics are implmented separately
-type otel_signal() :: traces | logs.

-type otel_batch_olp_config() ::
        #{reg_name := atom(),
          cb_module := module(),
          otel_signal := otel_signal(),
          max_queue_size := max_queue_size(),
          exporting_timeout_ms := pos_integer(),
          scheduled_delay_ms := pos_integer(),
          exporter := otel_exporter:exporter_config(),
          shutdown_timeout_ms => pos_integer(),
          extra_ets_opts => [{keypos, pos_integer()}],
          resource => otel_resource:t()
      }.

%% External state returned to the caller, necessary to be able to communicate with
%% otel_batch_olp either via calls or inserts to ETS tables.
-type otel_batch_olp_state() ::
        #{reg_name := atom(),
          cb_module := module(),
          otel_signal := otel_signal(),
          tables := {ets:table(), ets:table()},
          atomic_ref := atomics:atomic_ref(),
          extra_ets_opts => [{keypos, pos_integer()}],
          max_queue_size => max_queue_size(),
          exporting_timeout_ms => otel_timeout_ms(),
          scheduled_delay_ms => otel_timeout_ms(),
          shutdown_timeout_ms => otel_timeout_ms(),
          exporter => otel_exporter:exporter_config()
        }.

-type max_queue_size() :: pos_integer() | infinity.
-type otel_timeout_ms() :: pos_integer().

-define(table_name(_RegName_, _TabName_), list_to_atom(lists:concat([_RegName_, "_", _TabName_]))).
-define(table_1(_RegName_), ?table_name(_RegName_, table1)).
-define(table_2(_RegName_), ?table_name(_RegName_, table2)).

%% Use of atomics provides much better overload protection comparing to periodic ETS table size check.
%% It allows to enter drop mode as soon as max_queue_size is reached, while periodic table check
%% can overlook a large and fast burst of writes that can result in inserting a much larger amount of
%% log events than the configured max_queue_size.
%% Performance-wise, the cost of `atomics:get/2`, `atomics:sub_get/3` is comparable with
%% `persistent_term:get/2,3`
-define(current_tab(_AtomicRef_), atomics:get(_AtomicRef_, ?CURRENT_TAB_IX)).
-define(tab_name(_TabIx_, _Tabs_), element(_TabIx_, _Tabs_)).
-define(next_tab(_CurrentTab_), case _CurrentTab_ of
                                    ?TAB_1_IX -> ?TAB_2_IX;
                                    ?TAB_2_IX -> ?TAB_1_IX
                                end).

-define(set_current_tab(_AtomicRef_, _TabIx_), atomics:put(_AtomicRef_, ?CURRENT_TAB_IX, _TabIx_)).
-define(set_available(_AtomicRef_, _TabIx_, _Size_), atomics:put(_AtomicRef_, _TabIx_, _Size_)).
-define(get_available(_AtomicRef_, _TabIx_), atomics:get(_AtomicRef_, _TabIx_)).
-define(sub_get_available(_AtomicRef_, _TabIx_), atomics:sub_get(_AtomicRef_, _TabIx_, 1)).
-define(disable(_AtomicRef_), atomics:put(_AtomicRef_, ?CURRENT_TAB_IX, 0)).

-define(MAX_SIGNED_INT, (1 bsl 63)-1).
-define(TAB_1_IX, 1).
-define(TAB_2_IX, 2).
%% signifies which table is currently enabled (0 - disabled, 1 - table_1, 2 - table_2)
-define(CURRENT_TAB_IX, 3).

-define(DEFAULT_SHUTDOWN_MS, 5000).
-define(DEFAULT_EXPORTER_MODULE, opentelemetry_exporter).

-define(time_ms, erlang:monotonic_time(millisecond)).
-define(rem_time(_Timeout_, _T0_, _T1_), max(0, _Timeout_ - (_T1_ - _T0_))).

-define(private_field_err(_FieldName_), {error, {_FieldName_, "private_field_change_not_allowed"}}).
-define(change_not_allowed_err(_FieldName_), {error, {_FieldName_, "field_change_not_allowed"}}).

-record(data, {exporter              :: {module(), State :: term()} | ignore | undefined,
               exporter_config       :: otel_exporter:exporter_config(),
               resource              :: otel_resource:t(),
               handed_off_table      :: ets:table() | undefined,
               runner                :: {pid(), reference()} | undefined,
               tables                :: {ets:table(), ets:table()},
               reg_name              :: atom(),
               max_queue_size        :: max_queue_size(),
               exporting_timeout_ms  :: otel_timeout_ms(),
               scheduled_delay_ms    :: otel_timeout_ms(),
               shutdown_ms           :: otel_timeout_ms(),
               atomic_ref            :: atomics:atomic_ref(),
               exporter_timer        :: undefined | reference(),
               otel_signal           :: otel_signal(),
               %% for future extensions
               cb_module             :: module(),
               %% Arbitrary config of the callback module
               external_config       :: term(),
               extra                 = [] %% Unused, for future extensions
              }).

%%--------------------------------------------------------------------
%% Test utils
%%--------------------------------------------------------------------

-ifdef(TEST).
-export([current_tab_to_list/1]).
current_tab_to_list(RegName) ->
    {_, #data{tables=Tabs, atomic_ref=AtomicRef}} = sys:get_state(RegName),
    case ?current_tab(AtomicRef) of
        0 -> [];
        TabIx -> ets:tab2list(?tab_name(TabIx, Tabs))
    end.
-endif.

%%--------------------------------------------------------------------
%% otel_batch_olp API
%%--------------------------------------------------------------------

-spec init_conf(otel_batch_olp_config() | otel_batch_olp_state()) ->
          {ok, otel_batch_olp_state()} | {error, term()}.
init_conf(#{reg_name := RegName, cb_module := _Module, otel_signal := _, exporter := _,
            max_queue_size := _, exporting_timeout_ms := _, scheduled_delay_ms := _} = Config) ->
    case validate_config(without_state_fields(Config)) of
        ok ->
            AtomicRef = atomics:new(3, [{signed, true}]),
            {ok, Config#{reg_name => RegName,
                          tables => {?table_1(RegName), ?table_2(RegName)},
                          atomic_ref => AtomicRef}};
        Err ->
            Err
    end.
-spec start_link(otel_batch_olp_state(), term()) -> gen_statem:start_ret().
start_link(#{reg_name := RegName} = OlpState, ExternalConfig) ->
    gen_statem:start_link({local, RegName}, ?MODULE, [OlpState, ExternalConfig], []).

-spec insert_signal(tuple(), otel_batch_olp_state()) -> true | dropped | {error, term()}.
insert_signal(Record, #{atomic_ref := AtomicRef, tables := Tabs} = State) ->
    try
        case ?current_tab(AtomicRef) of
            0 -> dropped;
            CurrentTab ->
                case ?sub_get_available(AtomicRef, CurrentTab) of
                    Seq when Seq > 0 ->
                        ets:insert(?tab_name(CurrentTab, Tabs), Record);
                    0 ->
                        %% max_queue_size is reached
                        Res = ets:insert(?tab_name(CurrentTab, Tabs), Record),
                        _ = force_flush(State),
                        Res;
                    _ ->
                        dropped
                end
        end
    catch
        error:badarg ->
            {error, {no_otel_batch_olp, maps:get(otel_signal, State, undefined)}};
        Err:Reason ->
            {error, {Err, Reason}}
    end.

-spec force_flush(otel_batch_olp_state()) -> ok.
force_flush(#{reg_name := RegName}) ->
    gen_statem:cast(RegName, force_flush).

-spec change_config(OldConfigState, NewConfigOrState, NewExtConfig) ->
          {ok, NewConfigState} | {error, Reason} when
      OldConfigState :: otel_batch_olp_state(),
      NewConfigOrState :: otel_batch_olp_config() | otel_batch_olp_state(),
      NewExtConfig :: term(),
      NewConfigState :: otel_batch_olp_state(),
      Reason :: term().
change_config(#{reg_name := RegName}, #{reg_name := RegName1}, _) when RegName =/= RegName1 ->
    ?private_field_err(reg_name);
change_config(#{atomic_ref := Ref}, #{atomic_ref := Ref1}, _) when Ref =/= Ref1 ->
    ?private_field_err(atomic_ref);
change_config(#{tables := Tabs}, #{tables := Tabs1}, _) when Tabs =/= Tabs1 ->
    ?private_field_err(tables);
%% Changing timeout or exporter config requires restart/re-initialiazation of exporter,
%% which is not supported now. If timeout or exporter needs to be changed,
%% the handler should be stopped and started with the new config
change_config(#{exporter := Exporter}, #{exporter := Exporter1}, _) when Exporter =/= Exporter1 ->
    ?change_not_allowed_err(exporter);
change_config(#{exporting_timeout_ms := T}, #{exporting_timeout_ms := T1}, _) when T =/= T1 ->
    ?change_not_allowed_err(exporting_timeout_ms);
change_config(#{reg_name := RegName} = OldConfig, NewConfigOrState, NewExtConfig) ->
    NewConfig = without_state_fields(NewConfigOrState),
    case validate_config(NewConfig) of
        ok ->
            %% This is necessary, so that the config returned to the caller
            %% contains all the immutable keys required to communicate with
            %% otel_batch_olp
            NewConfig1 = copy_required_fields(OldConfig, NewConfig),
            gen_statem:call(RegName, {change_config, NewConfig1, NewExtConfig});
        Err ->
            Err
    end.

%%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------

init([OlpState, ExtConfig]) ->
    #{atomic_ref := AtomicRef,
      reg_name := RegName,
      tables := {Tab1, Tab2},
      cb_module := CbModule,
      otel_signal := OtelSignal,
      max_queue_size := MaxQueueSize,
      scheduled_delay_ms := ScheduledDelay,
      exporting_timeout_ms := ExportTimeoutMs,
      exporter := ExporterConfig
     } = OlpState,
    process_flag(trap_exit, true),
    Resource = maps:get(resource, OlpState, otel_resource_detector:get_resource()),
    ExporterConfig1 = exporter_conf_with_timeout(ExporterConfig, ExportTimeoutMs),

    %% assert table names match
    Tab1 = ?table_1(RegName),
    Tab2 = ?table_2(RegName),

    ExtraEtsOpts = maps:get(extra_ets_opts, OlpState, []),
    _Tid1 = new_export_table(Tab1, ExtraEtsOpts),
    _Tid2 = new_export_table(Tab2, ExtraEtsOpts),

    %% This is sligthly increased, to give the exporter runner a chance to  garcefully time-out
    %% before being killed by the handler.
    ExportTimeoutMs1 = ExportTimeoutMs + 1000,

    Data = #data{atomic_ref=AtomicRef,
                 exporter=undefined,
                 exporter_config=ExporterConfig1,
                 otel_signal = OtelSignal,
                 resource=Resource,
                 tables={Tab1, Tab2},
                 reg_name=RegName,
                 cb_module = CbModule,
                 exporting_timeout_ms=ExportTimeoutMs1,
                 scheduled_delay_ms=ScheduledDelay,
                 shutdown_ms = maps:get(shutdown_timeout_ms, OlpState, ?DEFAULT_SHUTDOWN_MS),
                 max_queue_size=size_limit(MaxQueueSize),
                 external_config=ExtConfig},
    %% Also used in change_config API, thus mutable
    Data1 = add_mutable_config_to_data(OlpState, ExtConfig, Data),

    ?set_current_tab(AtomicRef, ?TAB_1_IX),
    ?set_available(AtomicRef, ?TAB_1_IX, Data1#data.max_queue_size),
    ?set_available(AtomicRef, ?TAB_2_IX, Data1#data.max_queue_size),

    {ok, init_exporter, Data1}.

callback_mode() ->
    [state_functions, state_enter].

%% TODO: handle exporter crashes and re-init it.
%% This is not expected to happen with the default grpc opentelemetry_exporter,
%% as it keeps running and retrying by itself in case of network failures.
init_exporter(enter, _OldState, _Data) ->
    {keep_state_and_data, [{state_timeout, 0, do_init_exporter}]};
init_exporter(_, do_init_exporter, Data=#data{exporter_config=ExporterConfig,
                                              atomic_ref=AtomicRef,
                                              tables=Tabs,
                                              scheduled_delay_ms=SendInterval,
                                              reg_name=RegName,
                                              otel_signal=Signal}) ->
    case do_init_exporter(Signal, RegName, ExporterConfig) of
        %% error should be retried
        error ->
            {keep_state_and_data, [{state_timeout, SendInterval, do_init_exporter}]};
        ignore ->
            %% no exporter: disable the insertion of new log events and delete the current table
            clear_table_and_disable(AtomicRef, Tabs),
            {next_state, idle, Data#data{exporter=ignore}};
        Exporter ->
            TimerRef = start_exporting_timer(SendInterval),
            {next_state, idle, Data#data{exporter=Exporter, exporter_timer=TimerRef}}
    end;
init_exporter(_, _, _) ->
    %% Ignore any other, e.g, external events like force_flush in this state
    keep_state_and_data.

idle(enter, _OldState, _Data) ->
    keep_state_and_data;
idle(info, {timeout, Ref, export_signals}, Data=#data{exporter_timer=Ref}) ->
    {next_state, exporting, Data};
idle(cast, force_flush, #data{exporter=Exporter}=Data) when Exporter =/= ignore ->
    {next_state, exporting, Data};
idle(EventType, EventContent, Data) ->
    handle_event_(idle, EventType, EventContent, Data).

exporting(info, {timeout, Ref, export_signals}, #data{exporter_timer=Ref}) ->
    {keep_state_and_data, [postpone]};
exporting(enter, _OldState, Data=#data{atomic_ref=AtomicRef,
                                       tables=Tabs,
                                       max_queue_size=MaxSize,
                                       exporting_timeout_ms=ExportingTimeout,
                                       scheduled_delay_ms=SendInterval}) ->
    CurrentTab = ?current_tab(AtomicRef),
    {Data1, Actions} =
        case ?get_available(AtomicRef, CurrentTab) of
            %% No events yet, maximum available capacity, nothing to export
            MaxSize ->
                %% The other table may contain residual (late) writes not exported
                %% during the previous run. If current table is not empty, we don't
                %% need to check the size of the previous (currently disabled) table,
                %% since we will switch to it after this exporter run.
                %% However, if current table remains empty for a long time,
                %% neither export nor table switch will be triggered, and any
                %% residual late log events in the previous table would be left
                %% dangling. To avoid such cases, we check other table size
                %% and export it if it's not empty.
                maybe_export_other_table(CurrentTab, Data);
            _ ->
                RunnerPidRef = export_signals(CurrentTab, Data),
                {Data#data{runner=RunnerPidRef,
                           handed_off_table=?tab_name(CurrentTab, Tabs)},
                 [{state_timeout, ExportingTimeout, exporting_timeout}]}
        end,
    {keep_state, Data1#data{exporter_timer = start_exporting_timer(SendInterval)}, Actions};
exporting(state_timeout, empty_table, Data) ->
    {next_state, idle, Data};
exporting(state_timeout, exporting_timeout, Data) ->
    %% kill current exporting process because it is taking too long
    Data1 = kill_runner(Data),
    {next_state, idle, Data1};
%% Exit reason is ignored, since we don't handle exporter failures specifically for now
exporting(info, {'DOWN', Ref, process, Pid, _Info}, Data=#data{runner={Pid, Ref}}) ->
    complete_exporting(Data);
exporting(EventType, Event, Data) ->
    handle_event_(exporting, EventType, Event, Data).

terminate(_Reason, _State, #data{exporter=ignore}) ->
    ok;
terminate(_Reason, State, Data=#data{exporter=Exporter,
                                     resource=Resource,
                                     external_config=ExtConfig,
                                     atomic_ref=AtomicRef,
                                     tables={Tab1, Tab2},
                                     shutdown_ms=ShutdownMs,
                                     otel_signal=Signal
                                    }) ->
    ?disable(AtomicRef),
    T0 = ?time_ms,
    _ = maybe_wait_for_current_runner(State, Data, ShutdownMs),
    T1 = ?time_ms,

    %% Check both tables as each one may have some late unexported signals data.
    %% NOTE: exports are attempted sequentially to follow the specification restriction:
    %% "Export will never be called concurrently for the same exporter instance"
    %% (see: https://opentelemetry.io/docs/specs/otel/logs/sdk/#export).
    RemTime = ?rem_time(ShutdownMs, T0, T1),
    ets:info(Tab1, size) > 0
        andalso export_and_wait(Exporter, Resource, Tab1, ExtConfig, RemTime, Signal),
    T2 = ?time_ms,
    RemTime1 = ?rem_time(RemTime, T1, T2),
    ets:info(Tab2, size) > 0
        andalso export_and_wait(Exporter, Resource, Tab2, ExtConfig, RemTime1, Signal),

    _ = otel_exporter:shutdown(Exporter),
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

handle_event_(_State, {call, From}, {change_config, NewConfig, NewExtConfig}, Data) ->
    {keep_state,
     add_mutable_config_to_data(NewConfig, NewExtConfig, Data),
     [{reply, From, {ok, NewConfig}}]};
handle_event_(_State, info, {'EXIT', _Pid, Reason}, _Data)  ->
    %% This can be a linked exporter process, unless someone linked to the handler process,
    %% or explicitly called exit(HandlerPid, Reason)
    %% This will call terminate/3 and may try to export current log events,
    %% even if the linked exporter process is down.
    %% This is safe, though, as all errors of otel_exporter:export/5 are caught.
    {stop, Reason};
handle_event_(_State, _, _, _) ->
    keep_state_and_data.

do_init_exporter(_Signal, _RegName, ExporterConfig) ->
    otel_exporter:init(ExporterConfig).

start_exporting_timer(SendInterval) ->
    erlang:start_timer(SendInterval, self(), export_signals).

maybe_export_other_table(CurrentTab, Data=#data{tables=Tabs,
                                                exporting_timeout_ms=ExportingTimeout}) ->
    NextTab = ?next_tab(CurrentTab),
    %% Check ETS size instead of the counter, as late writes can't be detected with the atomic counter
    case ets:info(?tab_name(NextTab, Tabs), size) of
        0 ->
            %% in an `enter' handler we can't return a `next_state' or `next_event'
            %% so we rely on a timeout to trigger the transition to `idle'
            {Data#data{runner=undefined}, [{state_timeout, 0, empty_table}]};
        _ ->
            RunnerPid = export_signals(NextTab, Data),
            {Data#data{runner=RunnerPid, handed_off_table=?tab_name(CurrentTab, Tabs)},
             [{state_timeout, ExportingTimeout, exporting_timeout}]}
    end.

export_signals(CurrentTab, #data{exporter=Exporter,
                                 max_queue_size=MaxSize,
                                 resource=Resource,
                                 atomic_ref=AtomicRef,
                                 tables=Tabs,
                                 external_config=ExtConfig,
                                 otel_signal=Signal}) ->

    NewCurrentTab = ?next_tab(CurrentTab),
    %% the new table is expected to be empty or hold a few late writes from the previous export,
    %% so it safe to set available max size
    ?set_available(AtomicRef, NewCurrentTab, MaxSize),
    ?set_current_tab(AtomicRef, NewCurrentTab),
    export_async(Exporter, Resource, ?tab_name(CurrentTab, Tabs), ExtConfig, Signal).

export_async(Exporter, Resource, CurrentTab, ExtConfig, Signal) ->
    erlang:spawn_monitor(fun() -> export(Exporter, Resource, CurrentTab, ExtConfig, Signal) end).

export(undefined, _, _, _, _) ->
    true;
export({ExporterModule, ExporterState}, Resource, Tab, ExtConfig, Signal) ->
    try
        %% TODO: API for both signals should probably be aligned in otel_exporter
        TabOrTabConfig = case Signal of
                             traces -> Tab;
                             logs -> {Tab, ExtConfig}
                         end,
        %% we ignore values, as no retries mechanism, is implemented
        otel_exporter:export(Signal, ExporterModule, TabOrTabConfig, Resource, ExporterState)
    catch
        Class:Reason:St ->
            ?LOG_ERROR("~p exporter ~p failed with exception: ~p:~p, stacktrace: ~p",
                       [Signal, Class, Reason, otel_utils:stack_without_args(St)]),
            error
    end.

new_export_table(Name, ExtraEtsOpts) ->
    %% log event timestamps used as keys are not guaranteed to always be unique,
    %% so we use duplicate_bag
    %% Using timestamps as keys instead of instrumentation scopes is expected
    %% to have higher entropy which should improve write concurrency
    Opts = lists:usort([public,
                        named_table,
                        {write_concurrency, true},
                        duplicate_bag] ++ ExtraEtsOpts),
    ets:new(Name, Opts).

clear_table_and_disable(AtomicRef, Tabs) ->
    case ?current_tab(AtomicRef) of
        0 ->
            %% already disabled
            ok;
        CurrentTab ->
            ?disable(AtomicRef),
            CurrentTabName = ?tab_name(CurrentTab, Tabs),
            ets:delete_all_objects(CurrentTabName),
            ok
    end.

complete_exporting(Data) ->
    {next_state, idle, Data#data{runner=undefined,
                                 handed_off_table=undefined}}.

kill_runner(Data=#data{runner={RunnerPid, Ref}, handed_off_table=Tab}) ->
    _ = erlang:demonitor(Ref),
    %% NOTE: `exit/2` is async, but as we don't delete/recreate export tables,
    %% we don't need to wait for runner termination
    erlang:exit(RunnerPid, kill),
    _ = ets:delete_all_objects(Tab),
    Data#data{runner=undefined, handed_off_table=undefined};
kill_runner(Data=#data{runner=undefined}) ->
    Data.

exporter_conf_with_timeout({?DEFAULT_EXPORTER_MODULE, Conf}, TimeoutMs) ->
    {?DEFAULT_EXPORTER_MODULE, Conf#{timeout_ms => TimeoutMs}};
exporter_conf_with_timeout(OtherExporter, _Timeout) ->
    OtherExporter.

%% terminate/3 helpers

export_and_wait(Exporter, Resource, Tab, Config, Timeout, Signal) ->
    RunnerPidRef = export_async(Exporter, Resource, Tab, Config, Signal),
    wait_for_runner(RunnerPidRef, Timeout).

wait_for_runner({RunnerPid, RunnerRef}, Timeout) ->
    receive
        {'DOWN', RunnerRef, process, RunnerPid, _Info} -> ok
    after Timeout ->
            erlang:demonitor(RunnerRef),
            erlang:exit(RunnerPid, kill),
            ok
    end.

maybe_wait_for_current_runner(exporting, #data{runner={Pid, Ref}}, Timeout) ->
    wait_for_runner({Pid, Ref}, Timeout);
maybe_wait_for_current_runner(_State, _Date, _Timeout) -> ok.

%% Config helpers

validate_config(Config) ->
    Errs = maps:fold(fun(K, Val, Acc) ->
                             case validate_opt(K, Val, Config) of
                                 ok -> Acc;
                                 Err -> [Err | Acc]
                             end
              end,
                     [], Config),
    case Errs of
        [] -> ok;
        _ -> {error, Errs}
    end.

validate_opt(max_queue_size, infinity, _Config) ->
    ok;
validate_opt(K, Val, _Config) when is_integer(Val), Val > 0,
                                   K =:= max_queue_size;
                                   K =:= exporting_timeout_ms;
                                   K =:= scheduled_delay_ms;
                                   K =:= shutdown_timeout_ms ->
    ok;
validate_opt(exporter, {Module, _}, _Config) when is_atom(Module) ->
    ok;
validate_opt(exporter, Module, _Config) when is_atom(Module) ->
    ok;
validate_opt(otel_signal, Signal, _Config) when Signal =:= traces; Signal =:= logs ->
    ok;
validate_opt(cb_module, Module, _Config) ->
    Module:module_info(),
    ok;
validate_opt(extra_ets_opts, [{keypos, Pos}], _Config) when Pos >= 1 ->
    ok;
validate_opt(extra_ets_opts, [], _Config) ->
    ok;
validate_opt(reg_name, RegName, _Config) when is_atom(RegName) ->
    ok;
validate_opt(resource, Resource, _Config) when is_tuple(Resource) ->
    ok;
validate_opt(K, Val, _Config) ->
    {invalid_config, K, Val}.

add_mutable_config_to_data(Config, ExtConfig, Data) ->
    #{max_queue_size:=SizeLimit,
      scheduled_delay_ms:=ScheduledDelay
     } = Config,
    Data#data{max_queue_size=size_limit(SizeLimit),
              scheduled_delay_ms=ScheduledDelay,
              external_config=ExtConfig}.

%% high enough, must be infeasible to reach
size_limit(infinity) ->
    ?MAX_SIGNED_INT;
size_limit(Int) ->
    Int.

copy_required_fields(OldConf, NewConf) ->
    #{reg_name := RegName,
      otel_signal := Signal,
      tables := Tabs,
      atomic_ref := AtomicRef} = OldConf,
    NewConf#{reg_name => RegName,
             otel_signal => Signal,
             tables => Tabs,
             atomic_ref => AtomicRef}.

without_state_fields(ConfigOrState) ->
    maps:without([tables, atomic_ref], ConfigOrState).
