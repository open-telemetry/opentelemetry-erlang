%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(otel_log_handler).

-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-export([start_link/2]).

-export([log/2,
         adding_handler/1,
         removing_handler/1,
         changing_config/3,
         filter_config/1,
         report_cb/1]).

-export([init/1,
         callback_mode/0,
         idle/3,
         exporting/3,
         handle_event/3]).

-type config() :: #{id => logger:handler_id(),
                    regname := atom(),
                    config => term(),
                    level => logger:level() | all | none,
                    module => module(),
                    filter_default => log | stop,
                    filters => [{logger:filter_id(), logger:filter()}],
                    formatter => {module(), logger:formatter_config()}}.

-define(DEFAULT_CALL_TIMEOUT, 5000).
-define(DEFAULT_MAX_QUEUE_SIZE, 2048).
-define(DEFAULT_SCHEDULED_DELAY_MS, timer:seconds(5)).
-define(DEFAULT_EXPORTER_TIMEOUT_MS, timer:minutes(5)).

-define(name_to_reg_name(Module, Id),
        list_to_atom(lists:concat([Module, "_", Id]))).

-record(data, {exporter             :: {module(), term()} | undefined,
               exporter_config      :: {module(), term()} | undefined,
               resource             :: otel_resource:t(),

               runner_pid           :: pid() | undefined,
               max_queue_size       :: integer() | infinity,
               exporting_timeout_ms :: integer(),
               scheduled_delay_ms   :: integer(),

               config :: #{},
               batch  :: #{opentelemetry:instrumentation_scope() => [logger:log_event()]}}).

start_link(RegName, Config) ->
    gen_statem:start_link({local, RegName}, ?MODULE, [RegName, Config], []).

-spec adding_handler(Config) -> {ok, Config} | {error, Reason} when
      Config :: config(),
      Reason :: term().
adding_handler(#{id := Id,
                 module := Module}=Config) ->
    RegName = ?name_to_reg_name(Module, Id),
    ChildSpec =
        #{id       => Id,
          start    => {?MODULE, start_link, [RegName, Config]},
          restart  => temporary,
          shutdown => 2000,
          type     => worker,
          modules  => [?MODULE]},
    case supervisor:start_child(opentelemetry_experimental_sup, ChildSpec) of
        {ok, _Pid} ->
            %% ok = logger_handler_watcher:register_handler(Name,Pid),
            %% OlpOpts = logger_olp:get_opts(Olp),
            {ok, Config#{regname => RegName}};
        {error, {Reason, Ch}} when is_tuple(Ch), element(1, Ch) == child ->
            {error, Reason};
        {error, _Reason}=Error ->
            Error
    end.

%%%-----------------------------------------------------------------
%%% Updating handler config
-spec changing_config(SetOrUpdate, OldConfig, NewConfig) ->
          {ok,Config} | {error,Reason} when
      SetOrUpdate :: set | update,
      OldConfig :: config(),
      NewConfig :: config(),
      Config :: config(),
      Reason :: term().
changing_config(SetOrUpdate, OldConfig, NewConfig=#{regname := Id}) ->
    gen_statem:call(Id, {changing_config, SetOrUpdate, OldConfig, NewConfig}).

%%%-----------------------------------------------------------------
%%% Handler being removed
-spec removing_handler(Config) -> ok when
      Config :: config().
removing_handler(Config=#{regname := Id}) ->
    gen_statem:call(Id, {removing_handler, Config}).

%%%-----------------------------------------------------------------
%%% Log a string or report
-spec log(LogEvent, Config) -> ok when
      LogEvent :: logger:log_event(),
      Config :: config().
log(LogEvent, _Config=#{regname := Id}) ->
    Scope = case LogEvent of
                #{meta := #{otel_scope := Scope0=#instrumentation_scope{}}} ->
                    Scope0;
                #{meta := #{mfa := {Module, _, _}}} ->
                    opentelemetry:get_application_scope(Module);
                _ ->
                    opentelemetry:instrumentation_scope(<<>>, <<>>, <<>>)
            end,

    gen_statem:cast(Id, {log, Scope, LogEvent}).

%%%-----------------------------------------------------------------
%%% Remove internal fields from configuration
-spec filter_config(Config) -> Config when
      Config :: config().
filter_config(Config=#{regname := Id}) ->
    gen_statem:call(Id, {filter_config, Config}).

init([_RegName, Config]) ->
    process_flag(trap_exit, true),

    Resource = otel_resource_detector:get_resource(),

    SizeLimit = maps:get(max_queue_size, Config, ?DEFAULT_MAX_QUEUE_SIZE),
    ExportingTimeout = maps:get(exporting_timeout_ms, Config, ?DEFAULT_EXPORTER_TIMEOUT_MS),
    ScheduledDelay = maps:get(scheduled_delay_ms, Config, ?DEFAULT_SCHEDULED_DELAY_MS),

    ExporterConfig = maps:get(exporter, Config, {opentelemetry_exporter, #{protocol => grpc}}),

    {ok, idle, #data{exporter=undefined,
                     exporter_config=ExporterConfig,
                     resource=Resource,
                     config=Config,
                     max_queue_size=case SizeLimit of
                                        infinity -> infinity;
                                        _ -> SizeLimit div erlang:system_info(wordsize)
                                    end,
                     exporting_timeout_ms=ExportingTimeout,
                     scheduled_delay_ms=ScheduledDelay,
                     batch=#{}}}.

callback_mode() ->
    [state_functions, state_enter].

idle(enter, _OldState, Data=#data{exporter=undefined,
                                  exporter_config=ExporterConfig,
                                  scheduled_delay_ms=SendInterval}) ->
    Exporter = init_exporter(ExporterConfig),
    {keep_state, Data#data{exporter=Exporter},
     [{{timeout, export_logs}, SendInterval, export_logs}]};
idle(enter, _OldState, #data{scheduled_delay_ms=SendInterval}) ->
    {keep_state_and_data, [{{timeout, export_logs}, SendInterval, export_logs}]};
idle(_, export_logs, Data=#data{exporter=undefined,
                                 exporter_config=ExporterConfig}) ->
    Exporter = init_exporter(ExporterConfig),
    {next_state, exporting, Data#data{exporter=Exporter}, [{next_event, internal, export}]};
idle(_, export_logs, Data) ->
    {next_state, exporting, Data, [{next_event, internal, export}]};
idle(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

exporting({timeout, export_logs}, export_logs, _) ->
    {keep_state_and_data, [postpone]};
exporting(enter, _OldState, _Data) ->
    keep_state_and_data;
exporting(internal, export, Data=#data{exporter=Exporter,
                                       resource=Resource,
                                       config=Config,
                                       batch=Batch}) when map_size(Batch) =/= 0 ->
    _ = export(Exporter, Resource, Batch, Config),
    {next_state, idle, Data#data{batch=#{}}};
exporting(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

handle_event({call, From}, {changing_config, _SetOrUpdate, _OldConfig, NewConfig}, Data) ->
    {keep_state, Data#data{config=NewConfig}, [{reply, From, NewConfig}]};
handle_event({call, From}, {removing_handler, Config}, _Data) ->
    %% TODO: flush
    {keep_state_and_data, [{reply, From, Config}]};
handle_event({call, From}, {filter_handler, Config}, Data) ->
    {keep_state, Data, [{reply, From, Config}]};
handle_event({call, From}, {filter_config, Config}, Data) ->
    {keep_state, Data, [{reply, From, Config}]};
handle_event({call, _From}, _Msg, _Data) ->
    keep_state_and_data;
handle_event(cast, {log, Scope, LogEvent}, Data=#data{batch=Logs}) ->
    {keep_state, Data#data{batch=maps:update_with(Scope, fun(V) ->
                                                                  [LogEvent | V]
                                                          end, [LogEvent], Logs)}};
handle_event(_, _, _) ->
    keep_state_and_data.

%%

init_exporter(ExporterConfig) ->
    case otel_exporter:init(ExporterConfig) of
        Exporter when Exporter =/= undefined andalso Exporter =/= none ->
            Exporter;
        _ ->
            undefined
    end.

export(undefined, _, _, _) ->
    true;
export({ExporterModule, ExporterConfig}, Resource, Batch, Config) ->
    %% don't let a exporter exception crash us
    %% and return true if exporter failed
    try
        otel_exporter:export_logs(ExporterModule, {Batch, Config}, Resource, ExporterConfig)
            =:= failed_not_retryable
    catch
        Kind:Reason:StackTrace ->
            ?LOG_WARNING(#{source => exporter,
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
    {"log exporter threw exception: exporter=~p ~ts",
     [ExporterModule, otel_utils:format_exception(Kind, Reason, StackTrace)]}.
