%%%------------------------------------------------------------------------
%% Copyright 2022-2023, OpenTelemetry Authors
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
%% @doc Specification: https://opentelemetry.io/docs/specs/otel/logs/sdk
%% @end
%%%-------------------------------------------------------------------------
-module(otel_log_handler).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

%% Logger handler
-export([log/2,
         adding_handler/1,
         removing_handler/1,
         changing_config/3]).

%% OpenTelemetry specific
-export([force_flush/1]).

-export_type([config/0,
              otel_log_handler_config/0]).

-type config() :: #{id => logger:handler_id(),
                    level => logger:level() | all | none,
                    module => module(),
                    filter_default => log | stop,
                    filters => [{logger:filter_id(), logger:filter()}],
                    formatter => {module(), logger:formatter_config()},
                    config => otel_log_handler_config()
                   }.

-type config_state() :: #{id => logger:handler_id(),
                          level => logger:level() | all | none,
                          module => module(),
                          filter_default => log | stop,
                          filters => [{logger:filter_id(), logger:filter()}],
                          formatter => {module(), logger:formatter_config()},
                          config := otel_batch_olp:otel_batch_olp_state()
                         }.

-type otel_log_handler_config() ::
        #{max_queue_size => otel_batch_olp:max_queue_size(),
          exporting_timeout_ms => otel_batch_olp:otel_timeout_ms(),
          scheduled_delay_ms => otel_batch_olp:otel_timeout_ms(),
          exporter => otel_exporter:exporter_config()}.

-define(SUP, opentelemetry_experimental_sup).

-define(DEFAULT_MAX_QUEUE_SIZE, 2048).
-define(DEFAULT_SCHEDULED_DELAY_MS, timer:seconds(1)).
-define(DEFAULT_EXPORTER_TIMEOUT_MS, timer:seconds(30)).
-define(DEFAULT_EXPORTER_MODULE, opentelemetry_exporter).
-define(DEFAULT_EXPORTER, {?DEFAULT_EXPORTER_MODULE, #{protocol => grpc}}).

%% Slightly higher than GRACE_SHUTDOWN_MS, so that the supervisor doesn't kill the handler too early
-define(SUP_SHUTDOWN_MS, 5500).
-define(GRACE_SHUTDOWN_MS, 5000).

%%--------------------------------------------------------------------
%% Logger handler callbacks
%%--------------------------------------------------------------------

-spec adding_handler(config()) -> {ok, config_state()} | {error, term()}.
adding_handler(#{id := Id}=Config) ->
    HandlerConfig = maps:merge(default_config(), maps:get(config, Config, #{})),
    RegName = ?REG_NAME(Id),
    OtelBatchOlpConfig = HandlerConfig#{reg_name => RegName,
                                        cb_module => ?MODULE,
                                        otel_signal => logs},

    case otel_batch_olp:init_conf(OtelBatchOlpConfig) of
        {ok, OlpState} ->
            %% logger conf must keep all the fields of otel_batch_olp conf,
            %% as it includes table names, reg_name and atomic ref.
            Config1 = Config#{config => OlpState},
            start(Id, OlpState, Config1);
        Err ->
            Err
    end.

-spec changing_config(SetOrUpdate, OldConfigState, NewConfig) ->
          {ok, NewConfigState} | {error, Reason} when
      SetOrUpdate :: set | update,
      OldConfigState :: config_state(),
      NewConfig :: config(),
      NewConfigState :: config_state(),
      Reason :: term().
changing_config(SetOrUpdate, #{config := #{reg_name := _} = OlpState}, NewConfig) ->
    NewOlpConfig = maps:get(config, NewConfig, #{}),
    Default = case SetOrUpdate of
                  update -> OlpState;
                  set -> default_config()
              end,
    NewOlpConfig1 = maps:merge(with_changeable_fields(Default), NewOlpConfig),
    %% NewConfig which is `logger:handler_config()` is already merged with either old config or default,
    %% depending on `SetOrUpdate` value
    case otel_batch_olp:change_config(OlpState, NewOlpConfig1, NewConfig) of
        {ok, NewOlpState} ->
            {ok, NewConfig#{config => NewOlpState}};
        Err ->
            Err
    end.

-spec removing_handler(config_state()) -> ok | {error, term()}.
removing_handler(_Config=#{id := Id}) ->
    Res = supervisor:terminate_child(?SUP, Id),
    _ = supervisor:delete_child(?SUP, Id),
    Res.

-spec log(LogEvent, Config) -> true | dropped | {error, term()} when
      LogEvent :: logger:log_event(),
      Config :: config_state().
log(LogEvent, #{config := OlpConfig}) ->
    Ts = case LogEvent of
                #{meta := #{time := Time}} -> Time;
                _ -> logger:timestamp()
            end,
    otel_batch_olp:insert_signal({Ts, LogEvent}, OlpConfig).

-spec force_flush(config_state()) -> ok.
force_flush(#{config := OlpConfig}) ->
    otel_batch_olp:force_flush(OlpConfig).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

start(Id, OtelBatchOlpState, Config) ->
    ChildSpec =
        #{id       => Id,
          start    => {otel_batch_olp, start_link, [OtelBatchOlpState, Config]},
          %% The handler must be stopped gracefully by calling `logger:remove_handler/1`,
          %% which calls `supervisor:terminate_child/2` (in `removing_handler/2` cb).
          %% Any other termination is abnormal and deserves a restart.
          restart  => permanent,
          shutdown => ?SUP_SHUTDOWN_MS,
          type     => worker,
          modules  => [?MODULE]},
    case supervisor:start_child(?SUP, ChildSpec) of
        {ok, _Pid} ->
            {ok, Config};
        {error, {Reason, Ch}} when is_tuple(Ch), element(1, Ch) == child ->
            {error, Reason};
        {error, _Reason}=Error ->
            Error
    end.

default_config() ->
    #{max_queue_size => ?DEFAULT_MAX_QUEUE_SIZE,
      exporting_timeout_ms => ?DEFAULT_EXPORTER_TIMEOUT_MS,
      scheduled_delay_ms => ?DEFAULT_SCHEDULED_DELAY_MS,
      shutdown_timeout_ms => ?GRACE_SHUTDOWN_MS,
      exporter => ?DEFAULT_EXPORTER}.

%% Select fields that are allowed to be changed
with_changeable_fields(Config) ->
    maps:with([max_queue_size, scheduled_delay_ms], Config).
