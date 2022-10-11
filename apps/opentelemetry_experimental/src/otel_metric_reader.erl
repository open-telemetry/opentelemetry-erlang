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

-export([start_link/2,
         collect/1,
         set_exporter/3,
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
         tref :: reference() | undefined,
         callbacks_tab :: atom(),
         view_aggregation_tab :: atom(),
         metrics_tab :: atom(),
         config :: #{}
        }).

-spec start_link(atom(), map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(ChildId, Config) ->
    gen_server:start_link({local, ChildId}, ?MODULE, [ChildId, Config], []).

collect(ReaderPid) ->
    ReaderPid ! collect.

set_exporter(ReaderPid, Exporter, Options) ->
    gen_server:call(ReaderPid, {set_exporter, Exporter, Options}).

shutdown(ReaderPid) ->
    gen_server:call(ReaderPid, shutdown).

init([ChildId, Config]) ->
    CallbacksTable = otel_meter_server:callbacks_table_name(ChildId),
    ViewAggregationTable = otel_meter_server:view_aggregation_table_name(ChildId),
    MetricsTable = otel_meter_server:metrics_table_name(ChildId),

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
                view_aggregation_tab=ViewAggregationTable,
                callbacks_tab=CallbacksTable,
                metrics_tab=MetricsTable,
                tref=TRef,
                config=Config}}.

handle_call({set_exporter, Module, Config}, _From, State=#state{exporter=OldExporter}) ->
    otel_exporter:shutdown(OldExporter),
    Exporter = otel_exporter:init({Module, Config}),
    {reply, ok, State#state{exporter=Exporter}};
handle_call(shutdown, _From, State) ->
    {reply, ok, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(collect, State=#state{exporter=undefined,
                                  export_interval_ms=ExporterIntervalMs,
                                  tref=TRef}) when TRef =/= undefined ->
    erlang:cancel_timer(TRef, [{async, true}]),
    NewTRef = erlang:send_after(ExporterIntervalMs, self(), collect),
    {noreply, State#state{tref=NewTRef}};
handle_info(collect, State=#state{exporter={ExporterModule, Config},
                                  export_interval_ms=undefined,
                                  tref=undefined,
                                  callbacks_tab=CallbacksTab,
                                  view_aggregation_tab=ViewAggregationTable,
                                  metrics_tab=MetricsTable}) ->
    Resource = [],
    %% collect from view aggregations table and then export
    Metrics = collect_(CallbacksTab, ViewAggregationTable, MetricsTable),

    otel_exporter:export_metrics(ExporterModule, Metrics, Resource, Config),

    {noreply, State};
handle_info(collect, State=#state{exporter={ExporterModule, Config},
                                  export_interval_ms=ExporterIntervalMs,
                                  tref=TRef,
                                  callbacks_tab=CallbacksTab,
                                  view_aggregation_tab=ViewAggregationTable,
                                  metrics_tab=MetricsTable}) ->
    Resource = [],
    erlang:cancel_timer(TRef, [{async, true}]),
    NewTRef = erlang:send_after(ExporterIntervalMs, self(), collect),

    %% collect from view aggregations table and then export
    Metrics = collect_(CallbacksTab, ViewAggregationTable, MetricsTable),

    otel_exporter:export_metrics(ExporterModule, Metrics, Resource, Config),

    {noreply, State#state{tref=NewTRef}};
%% no tref or exporter, do nothing at all
handle_info(_, State) ->
    {noreply, State}.

code_change(State) ->
    {ok, State}.

%%

-spec collect_(any(), any(), any()) -> [any()].
collect_(CallbacksTab, ViewAggregationTab, MetricsTab) ->
    CollectionStartTime = erlang:system_time(nanosecond),

    Results = run_callbacks(CallbacksTab),
    %% take the results of running each callback and aggregate the values
    _ = handle_callback_results(Results, ViewAggregationTab, MetricsTab),

    %% Need to be able to efficiently get all from VIEW_AGGREGATIONS_TAB that apply to this reader

    %% for each VIEW_AGGREGATIONS_TAB entry look up metrics from METRICS_TAB using the name
    %% to select for key `{Name, '_'}'. This gives the current value for each set of attributes
    %% for an aggregation.

    %% use the information (temporality) from the VIEW_AGGREGATIONS_TAB entry to reset the
    %% METRICS_TAB entry value (like setting value back to 0 for DELTA)

    ets:foldl(fun({#instrument{value_type=ValueType,
                               unit=Unit}, ViewAggregations}, MetricsAcc) ->
                      checkpoint_metrics(MetricsTab, ValueType, Unit, CollectionStartTime, ViewAggregations)
                          ++ MetricsAcc
              end, [], ViewAggregationTab).

%% call each callback and associate the result with the Instruments it observes
run_callbacks(CallbacksTab) ->
    ets:foldl(fun({Callback, CallbackArgs, Instruments}, Acc) ->
                      [{Instruments, Callback(CallbackArgs)} | Acc]
              end, [], CallbacksTab).

handle_callback_results(Results, ViewAggregationTab, MetricsTab) ->
    lists:foldl(fun({Instruments, Result}, Acc) when is_tuple(Result) ->
                        handle_instrument_observation(Result,
                                                      Instruments,
                                                      ViewAggregationTab, MetricsTab) ++ Acc;
                   ({Instruments, Result}, Acc) when is_list(Result) ->
                        [handle_instrument_observation(R,
                                                        Instruments,
                                                        ViewAggregationTab, MetricsTab) || R <- Result] ++ Acc;
                   (_, Acc) ->
                        ?LOG_DEBUG("Return of metric callback must be a tuple or a list"),
                        Acc
                end, [], Results).

%% single instrument callbacks return a 2-tuple of {number(), opentelemetry:attributes_map()}
handle_instrument_observation({Number, Attributes}, [Instrument],
                              ViewAggregationTab, MetricsTab)
  when is_number(Number) ->
    try ets:lookup_element(ViewAggregationTab, Instrument, 2) of
        ViewAggregations ->
            [AggregationModule:aggregate(MetricsTab, {Name, Attributes}, Number) || #view_aggregation{name=Name, aggregation_module=AggregationModule} <- ViewAggregations]
    catch
        exit:badarg ->
            %% no Views for this Instrument, so nothing to do
            []
    end;
%% multi-instrument callbacks return a 3-tuple of {otel_instrument:name(), number(), opentelemetry:attributes_map()}
handle_instrument_observation({InstrumentName, Number, Attributes}, Instruments, ViewAggregationTab, MetricsTab)
  when is_atom(InstrumentName), is_number(Number) ->
    case lists:keyfind(InstrumentName, #instrument.name, Instruments) of
        false ->
            ?LOG_DEBUG("Unknown Instrument ~p used in metric callback", [InstrumentName]),
            [];
        Instrument ->
            try ets:lookup_element(ViewAggregationTab, Instrument, 2) of
                ViewAggregations ->
                    [AggregationModule:aggregate(MetricsTab, {Name, Attributes}, Number) ||
                        #view_aggregation{name=Name, aggregation_module=AggregationModule} <- ViewAggregations]
            catch
                exit:badarg ->
                    %% no Views for this Instrument, so nothing to do
                    []
            end
    end;
handle_instrument_observation(_, _, _, _) ->
    ?LOG_DEBUG("Metric callback return must be of type {number(), map()} or {atom(), number(), map()}", []),
    [].

checkpoint_metrics(MetricsTab, ValueType, Unit, CollectionStartTime, ViewAggregations) ->
    lists:foldl(fun(#view_aggregation{aggregation_module=otel_aggregation_drop}, Acc) ->
                        Acc;
                   (#view_aggregation{name=Name,
                                      instrument=Instrument,
                                      aggregation_module=AggregationModule,
                                      description=Description,
                                      temporality=Temporality,
                                      is_monotonic=IsMonotonic
                                     }, Acc) ->
                        AggregationModule:checkpoint(MetricsTab, Name, Temporality,
                                                     ValueType, CollectionStartTime),
                        Data = data(AggregationModule, Name, Temporality, IsMonotonic,
                                    CollectionStartTime, MetricsTab),

                        [metric(Instrument, Name, Description, Unit, Data) | Acc]
                end, [], ViewAggregations).

metric(#instrument{meter=Meter}, Name, Description, Unit, Data) ->
    #metric{scope=otel_meter_default:scope(Meter),
            name=Name,
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

