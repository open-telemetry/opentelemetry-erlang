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

-record(state,
        {
         exporter,
         default_aggregation_mapping :: #{otel_instrument:kind() => module()},
         temporality_mapping :: #{otel_instrument:kind() => otel_aggregation:temporality()},
         export_interval_ms :: integer() | undefined,
         tref :: reference(),
         view_aggregation_tab :: ets:tid(),
         metrics_tab :: ets:tid()
        }).

-spec start_monitor(module(), otel_configuration:t()) -> {ok, {pid(), reference()}} | ignore | {error, term()}.
start_monitor(ReaderModule, Config) ->
    gen_server:start_monitor(ReaderModule, Config, []).

collect(ReaderPid) ->
    ReaderPid ! collect.

shutdown(ReaderPid) ->
    gen_server:call(ReaderPid, shutdown).

init(Config=#{view_aggregation_tab := ViewAggregationTab,
              metrics_tab := MetricsTab}) ->
    ExporterModuleConfig = maps:get(exporter, Config, undefined),
    Exporter = otel_exporter:init(ExporterModuleConfig),

    DefaultAggregationMapping = maps:get(default_aggregation_mapping, Config, #{}),
    Temporality = maps:get(temporality_mapping, Config, #{}),

    %% if a periodic reader is needed then this value is set
    %% somehow need to do a default of 10000 millis, but only if this is a periodic reader
    ExporterIntervalMs = maps:get(export_interval_ms, Config, undefined),

    TRef = case ExporterIntervalMs of
               undefined ->
                   undefined;
               _ ->
                   erlang:send_after(ExporterIntervalMs, self(), collect)
           end,

    {ok, #state{exporter=Exporter,
                default_aggregation_mapping=DefaultAggregationMapping,
                temporality_mapping=Temporality,
                export_interval_ms=ExporterIntervalMs,
                tref=TRef,
                view_aggregation_tab=ViewAggregationTab,
                metrics_tab=MetricsTab}}.

handle_call(shutdown, _From, State) ->
    {reply, ok, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(collect, State=#state{exporter=undefined,
                                  export_interval_ms=ExporterIntervalMs,
                                  tref=TRef}) ->
    erlang:cancel_timer(TRef, []), %% {async, true} ?
    NewTRef = erlang:send_after(ExporterIntervalMs, self(), collect),
    {noreply, State#state{tref=NewTRef}};
handle_info(collect, State=#state{exporter={ExporterModule, Config},
                                  export_interval_ms=undefined,
                                  tref=undefined,
                                  view_aggregation_tab=ViewAggregationTable,
                                  metrics_tab=MetricsTable}) ->
    %% collect from view aggregations table and then export
    Metrics = collect_(ViewAggregationTable, MetricsTable),

    ExporterModule:export(Metrics, Config),

    {noreply, State};
handle_info(collect, State=#state{exporter={ExporterModule, Config},
                                  export_interval_ms=ExporterIntervalMs,
                                  tref=TRef,
                                  view_aggregation_tab=ViewAggregationTable,
                                  metrics_tab=MetricsTable}) ->
    erlang:cancel_timer(TRef, []), %% {async, true} ?
    NewTRef = erlang:send_after(ExporterIntervalMs, self(), collect),

    %% collect from view aggregations table and then export
    Metrics = collect_(ViewAggregationTable, MetricsTable),

    ExporterModule:export(Metrics, Config),

    {noreply, State#state{tref=NewTRef}}.

code_change(State) ->
    {ok, State}.

%%

collect_(ViewAggregationTab, MetricsTab) ->
    CollectionStartTime = erlang:system_time(nanosecond),

    %% Need to be able to efficiently get all from VIEW_AGGREGATIONS_TAB that apply to this reader

    %% for each VIEW_AGGGREGATIONS_TAB entry look up metrics from METRICS_TAB using the name
    %% to select for key `{Name, '_'}'. This gives the current value for each set of attributes
    %% for an aggregation.

    %% use the information (temporality) from the VIEW_AGGREGATIONS_TAB entry to reset the
    %% METRICS_TAB entry value (like setting value back to 0 for DELTA)

    ets:foldl(fun({#instrument{value_type=ValueType,
                               unit=Unit}, ViewAggregations}, MetricsAcc) ->
                      lists:foldl(fun(#view_aggregation{aggregation_module=otel_aggregation_drop}, Acc) ->
                                          Acc;
                                     (#view_aggregation{name=Name,
                                                        aggregation_module=AggregationModule,
                                                        description=Description,
                                                        temporality=Temporality,
                                                        is_monotonic=IsMonotonic
                                                        }, Acc) ->
                                          AggregationModule:checkpoint(MetricsTab, Name, Temporality,
                                                                       ValueType, CollectionStartTime),
                                          Data = data(AggregationModule, Name, Temporality, IsMonotonic,
                                                      CollectionStartTime, MetricsTab),

                                          [metric(Name, Description, Unit, Data) | Acc]
                                  end, MetricsAcc, ViewAggregations)
              end, [], ViewAggregationTab).

%%

metric(Name, Description, Unit, Data) ->
    #metric{name=Name,
            description=Description,
            unit=Unit,
            data=Data}.

data(otel_aggregation_sum, Name, Temporality, IsMonotonic, CollectionStartTime, MetricTab) ->
    Datapoints = otel_aggregation_sum:collect(MetricTab, Name, Temporality, CollectionStartTime),
    #sum{
       aggregation_temporality=Temporality,
       is_monotonic=IsMonotonic,
       datapoints=Datapoints};
data(otel_aggregation_last_value, Name, Temporality, _IsMonotonic, CollectionStartTime, MetricTab) ->
    Datapoints = otel_aggregation_last_value:collect(MetricTab, Name, Temporality, CollectionStartTime),
    #gauge{datapoints=Datapoints};
data(otel_aggregation_histogram_explicit, Name, Temporality, _IsMonotonic, CollectionStartTime, MetricTab) ->
    Datapoints = otel_aggregation_histogram_explicit:collect(MetricTab, Name, Temporality, CollectionStartTime),
    #histogram{datapoints=Datapoints,
               aggregation_temporality=Temporality
              }.

