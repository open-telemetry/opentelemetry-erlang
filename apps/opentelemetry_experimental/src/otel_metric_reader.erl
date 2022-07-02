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
%% @doc MetricReader is an SDK module that provides the
%% common configurable aspects of the OpenTelemetry Metrics SDK and
%% determines the following capabilities:
%%
%% * Collecting metrics from the SDK on demand.
%% * Handling the ForceFlush and Shutdown signals from the SDK.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metric_reader).

-export([start_monitor/2,
         collect/1,
         shutdown/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include_lib("kernel/include/logger.hrl").
-include("otel_view.hrl").
-include("otel_metrics.hrl").

-type temporality() :: cumulative | delta. %% default is cumulative

-export_type([temporality/0]).

-record(state,
        {
         exporter,
         default_aggregation_mapping :: #{},
         temporality :: temporality(),
         export_interval_ms :: integer() | undefined,
         tref :: reference()
        }).

-define(ACTIVE_VIEWS_TAB, otel_active_views).

-spec start_monitor(module(), otel_configuration:t()) -> {ok, pid()} | ignore | {error, term()}.
start_monitor(ReaderModule, Config) ->
    gen_server:start_monitor(ReaderModule, Config, []).

collect(ReaderPid) ->
    ReaderPid ! collect.

shutdown(ReaderPid) ->
    gen_server:call(ReaderPid, shutdown).

init(Config) ->
    ExporterModuleConfig = maps:get(exporter, Config, undefined),
    Exporter = otel_exporter:init(ExporterModuleConfig),

    DefaultAggregationMapping = maps:get(default_aggregation_mapping, Config, #{}),
    Temporality = maps:get(temporality, Config, cumulative),

    %% if a periodic reader is needed then this value is set
    %% ExporterIntervalMs = maps:get(export_interval_ms, Config, undefined), %% somehow do default of 10000 millis
    ExporterIntervalMs = 10000,

    %% ets tables are required for other parts to not crash so we create
    %% it in init and not in a handle_continue or whatever else
    create_tables(),

    TRef = erlang:send_after(ExporterIntervalMs, self(), collect),

    {ok, #state{exporter=Exporter,
                default_aggregation_mapping=DefaultAggregationMapping,
                temporality=Temporality,
                export_interval_ms=ExporterIntervalMs,
                tref=TRef}}.

handle_call(shutdown, _From, State) ->
    {reply, ok, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(collect, State=#state{exporter={ExporterModule, Config},
                                  export_interval_ms=ExporterIntervalMs,
                                  tref=TRef}) ->
    erlang:cancel_timer(TRef, []), %% {async, true} ?
    NewTRef = erlang:send_after(ExporterIntervalMs, self(), collect),

    ExporterModule:export(otel_aggregations, Config),

    {noreply, State#state{tref=NewTRef}}.

code_change(State) ->
    {ok, State}.

%%

create_tables() ->
    case ets:info(?ACTIVE_VIEWS_TAB, name) of
        undefined ->
            ets:new(?ACTIVE_VIEWS_TAB, [named_table,
                                        set,
                                        protected,
                                        {read_concurrency, true},
                                        {keypos, 2}
                                       ]);
        _ ->
            ok
    end.
