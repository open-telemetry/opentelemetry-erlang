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

-behaviour(gen_server).

-export([start_link/3,
         collect/1,
         shutdown/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         code_change/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include_lib("kernel/include/logger.hrl").
-include("otel_view.hrl").
-include("otel_metrics.hrl").

-record(state,
        {
         exporter,
         provider_sup :: supervisor:sup_ref(),
         id :: reference(),
         default_aggregation_mapping :: #{otel_instrument:kind() => module()},
         temporality_mapping :: #{otel_instrument:kind() => otel_aggregation:temporality()},
         export_interval_ms :: integer() | undefined,
         tref :: reference() | undefined,
         callbacks_tab :: ets:table(),
         view_aggregation_tab :: ets:table(),
         metrics_tab :: ets:table(),
         config :: #{}
        }).

%% -spec start_link(atom(), map()) -> {ok, pid()} | ignore | {error, term()}.
%% start_link(ChildId, CallbacksTable, ViewAggregationTable, MetricsTable, Config) ->
%%     gen_server:start_link({local, ChildId}, ?MODULE, [ChildId, CallbacksTable, ViewAggregationTable, MetricsTable, Config], []).
start_link(ReaderId, ProviderSup, Config) ->
    gen_server:start_link(?MODULE, [ReaderId, ProviderSup, Config], []).

collect(ReaderPid) ->
    ReaderPid ! collect.

shutdown(ReaderPid) ->
    gen_server:call(ReaderPid, shutdown).

init([ReaderId, ProviderSup, Config]) ->
    ExporterModuleConfig = maps:get(exporter, Config, undefined),
    Exporter = otel_exporter:init(ExporterModuleConfig),

    DefaultAggregationMapping = maps:get(default_aggregation_mapping, Config, otel_aggregation:default_mapping()),
    Temporality = maps:get(default_temporality_mapping, Config, #{}),

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
                provider_sup=ProviderSup,
                id=ReaderId,
                default_aggregation_mapping=DefaultAggregationMapping,
                temporality_mapping=Temporality,
                export_interval_ms=ExporterIntervalMs,
                tref=TRef,
                config=Config}, {continue, register_with_server}}.

handle_continue(register_with_server, State=#state{provider_sup=ProviderSup,
                                                   id=ReaderId,
                                                   default_aggregation_mapping=DefaultAggregationMapping,
                                                   temporality_mapping=Temporality}) ->
    ServerPid = otel_meter_server_sup:provider_pid(ProviderSup),
    {CallbacksTab, ViewAggregationTab, MetricsTab} =
        otel_meter_server:add_metric_reader(ServerPid, ReaderId, self(),
                                            DefaultAggregationMapping,
                                            Temporality),
    {noreply, State#state{callbacks_tab=CallbacksTab,
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
                                  tref=TRef}) when TRef =/= undefined andalso
                                                   ExporterIntervalMs =/= undefined ->
    erlang:cancel_timer(TRef, [{async, true}]),
    NewTRef = erlang:send_after(ExporterIntervalMs, self(), collect),
    {noreply, State#state{tref=NewTRef}};
handle_info(collect, State=#state{id=ReaderId,
                                  exporter={ExporterModule, Config},
                                  export_interval_ms=undefined,
                                  tref=undefined,
                                  callbacks_tab=CallbacksTab,
                                  view_aggregation_tab=ViewAggregationTab,
                                  metrics_tab=MetricsTab
                                 }) ->
    Resource = [],

    %% collect from view aggregations table and then export
    Metrics = collect_(CallbacksTab, ViewAggregationTab, MetricsTab, ReaderId),

    otel_exporter:export_metrics(ExporterModule, Metrics, Resource, Config),

    {noreply, State};
handle_info(collect, State=#state{id=ReaderId,
                                  exporter={ExporterModule, Config},
                                  export_interval_ms=ExporterIntervalMs,
                                  tref=TRef,
                                  callbacks_tab=CallbacksTab,
                                  view_aggregation_tab=ViewAggregationTab,
                                  metrics_tab=MetricsTab
                                 }) when TRef =/= undefined andalso
                                         ExporterIntervalMs =/= undefined  ->
    Resource = [],
    erlang:cancel_timer(TRef, [{async, true}]),
    NewTRef = erlang:send_after(ExporterIntervalMs, self(), collect),

    %% collect from view aggregations table and then export
    Metrics = collect_(CallbacksTab, ViewAggregationTab, MetricsTab, ReaderId),


    otel_exporter:export_metrics(ExporterModule, Metrics, Resource, Config),

    {noreply, State#state{tref=NewTRef}};
%% no tref or exporter, do nothing at all
handle_info(_, State) ->
    {noreply, State}.

code_change(State) ->
    {ok, State}.

%%

-spec collect_(any(), any(), any(), reference()) -> [any()].
collect_(CallbacksTab, ViewAggregationTab, MetricsTab, ReaderId) ->
    CollectionStartTime = erlang:system_time(nanosecond),

    Results = run_callbacks(ReaderId, CallbacksTab),
    %% take the results of running each callback and aggregate the values
    _ = handle_callback_results(Results, ViewAggregationTab, MetricsTab, ReaderId),

    %% Need to be able to efficiently get all from VIEW_AGGREGATIONS_TAB that apply to this reader

    %% for each VIEW_AGGREGATIONS_TAB entry look up metrics from METRICS_TAB using the name
    %% to select for key `{Name, '_'}'. This gives the current value for each set of attributes
    %% for an aggregation.

    %% use the information (temporality) from the VIEW_AGGREGATIONS_TAB entry to reset the
    %% METRICS_TAB entry value (like setting value back to 0 for DELTA)

    %% ViewAggregationTab is a `bag' so to iterate over every ViewAggregation for
    %% each Instrument we use `first'/`next' and lookup the list of ViewAggregations
    %% by the key (Instrument)
    Key = ets:first(ViewAggregationTab),
    collect_(CallbacksTab, ViewAggregationTab, MetricsTab, CollectionStartTime, ReaderId, [], Key).

collect_(_CallbacksTab, _ViewAggregationTab, _MetricsTab, _CollectionStartTime, _ReaderId, MetricsAcc, '$end_of_table') ->
    MetricsAcc;
collect_(CallbacksTab, ViewAggregationTab, MetricsTab, CollectionStartTime, ReaderId, MetricsAcc, Key) ->
    ViewAggregations = ets:lookup_element(ViewAggregationTab, Key, 2),
    collect_(CallbacksTab, ViewAggregationTab, MetricsTab, CollectionStartTime, ReaderId,
             checkpoint_metrics(MetricsTab,
                                CollectionStartTime,
                                ReaderId,
                                ViewAggregations) ++ MetricsAcc,
             ets:next(ViewAggregationTab, Key)).

%% call each callback and associate the result with the Instruments it observes
run_callbacks(ReaderId, CallbacksTab) ->
    try ets:lookup_element(CallbacksTab, ReaderId, 2) of
        Callbacks ->
            lists:map(fun({Callback, CallbackArgs, Instruments}) ->
                              {Instruments, Callback(CallbackArgs)}
                      end, Callbacks)
    catch
        error:badarg ->
            []
    end.

handle_callback_results(Results, ViewAggregationTab, MetricsTab, ReaderId) ->
    lists:foldl(fun({Instruments, Result}, Acc) when is_tuple(Result) ->
                        handle_instrument_observation(Result,
                                                      Instruments,
                                                      ViewAggregationTab,
                                                      MetricsTab,
                                                      ReaderId)
                            ++ Acc;
                   ({Instruments, Result}, Acc) when is_list(Result) ->
                        [handle_instrument_observation(R,
                                                       Instruments,
                                                       ViewAggregationTab,
                                                       MetricsTab,
                                                       ReaderId)
                         || R <- Result] ++ Acc;
                   (_, Acc) ->
                        ?LOG_DEBUG("Return of metric callback must be a tuple or a list"),
                        Acc
                end, [], Results).

%% single instrument callbacks return a 2-tuple of {number(), opentelemetry:attributes_map()}
handle_instrument_observation({Number, Attributes}, [#instrument{meter=Meter,
                                                                 name=Name}],
                              ViewAggregationTab, MetricsTab, _ReaderId)
  when is_number(Number) ->
    try ets:lookup_element(ViewAggregationTab, {Meter, Name}, 2) of
        ViewAggregations ->
            [otel_aggregation:maybe_init_aggregate(MetricsTab, ViewAggregation, Number, Attributes) || #view_aggregation{}=ViewAggregation <- ViewAggregations]
    catch
        error:badarg ->
            %% no Views for this Instrument, so nothing to do
            []
    end;
%% multi-instrument callbacks return a 3-tuple of {otel_instrument:name(), number(), opentelemetry:attributes_map()}
handle_instrument_observation({InstrumentName, Number, Attributes}, Instruments, ViewAggregationTab, MetricsTab, ReaderId)
  when is_atom(InstrumentName), is_number(Number) ->
    case lists:keyfind(InstrumentName, #instrument.name, Instruments) of
        false ->
            ?LOG_DEBUG("Unknown Instrument ~p used in metric callback", [InstrumentName]),
            [];
        #instrument{meter=Meter} ->
            try ets:lookup_element(ViewAggregationTab, {Meter, InstrumentName}, 2) of
                ViewAggregations ->
                    [otel_aggregation:maybe_init_aggregate(MetricsTab, ViewAggregation, Number, Attributes) ||
                        #view_aggregation{reader=Id}=ViewAggregation <- ViewAggregations, Id =:= ReaderId]
            catch
                error:badarg ->
                    %% no Views for this Instrument, so nothing to do
                    []
            end
    end;
handle_instrument_observation(_, _, _, _, _) ->
    ?LOG_DEBUG("Metric callback return must be of type {number(), map()} or {atom(), number(), map()}", []),
    [].

checkpoint_metrics(MetricsTab, CollectionStartTime, Id, ViewAggregations) ->
    lists:foldl(fun(#view_aggregation{aggregation_module=otel_aggregation_drop}, Acc) ->
                        Acc;
                   (ViewAggregation=#view_aggregation{name=Name,
                                                      reader=ReaderId,
                                                      instrument=Instrument=#instrument{unit=Unit},
                                                      aggregation_module=AggregationModule,
                                                      description=Description
                                                     }, Acc) when Id =:= ReaderId ->
                        AggregationModule:checkpoint(MetricsTab,
                                                     ViewAggregation,
                                                     CollectionStartTime),
                        Data = AggregationModule:collect(MetricsTab,
                                                         ViewAggregation,
                                                         CollectionStartTime),
                        [metric(Instrument, Name, Description, Unit, Data) | Acc];
                   (_, Acc) ->
                        Acc
                end, [], ViewAggregations).

metric(#instrument{meter=Meter}, Name, Description, Unit, Data) ->
    #metric{scope=otel_meter_default:scope(Meter),
            name=Name,
            description=Description,
            unit=Unit,
            data=Data}.
