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
         call_collect/1,
         checkpoint_generation/1,
         shutdown/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         code_change/1]).

-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").
-include("otel_metrics.hrl").

-record(state,
        {
         exporter,
         provider_sup :: supervisor:sup_ref(),
         id :: reference(),
         default_aggregation_mapping :: #{otel_instrument:kind() => module()},
         temporality_mapping :: #{otel_instrument:kind() => otel_instrument:temporality()},
         export_interval_ms :: integer() | undefined,
         tref :: reference() | undefined,
         callbacks_tab :: ets:table(),
         streams_tab :: ets:table(),
         metrics_tab :: ets:table(),
         exemplars_tab :: ets:table(),
         config :: #{},
         resource :: otel_resource:t() | undefined,
         generation_ref :: atomics:atomics_ref(),
         producers :: [otel_metric_producer:t()]
        }).

%% -spec start_link(atom(), map()) -> {ok, pid()} | ignore | {error, term()}.
%% start_link(ChildId, CallbacksTable, StreamsTable, MetricsTable, Config) ->
%%     gen_server:start_link({local, ChildId}, ?MODULE, [ChildId, CallbacksTable, StreamsTable, MetricsTable, Config], []).
start_link(ReaderId, ProviderSup, Config) ->
    gen_server:start_link(?MODULE, [ReaderId, ProviderSup, Config], []).

collect(ReaderPid) ->
    ReaderPid ! collect.

call_collect(ReaderPid) ->
    gen_server:call(ReaderPid, collect).

shutdown(ReaderPid) ->
    gen_server:call(ReaderPid, shutdown).

%% Get the current checkpoint generation.
checkpoint_generation(ReaderId) ->
    GenerationRef = persistent_term:get({?MODULE, ReaderId}),
    atomics:get(GenerationRef, 1).

%% Increment and return the previous checkpoint generation.
inc_checkpoint_generation(ReaderId) ->
    GenerationRef = persistent_term:get({?MODULE, ReaderId}),
    atomics:add_get(GenerationRef, 1, 1) - 1.

init([ReaderId, ProviderSup, Config]) ->
    ExporterModuleConfig = maps:get(exporter, Config, undefined),
    Exporter = otel_exporter_metrics:init(ExporterModuleConfig),

    DefaultAggregationMapping = maps:get(default_aggregation_mapping, Config, otel_aggregation:default_mapping()),
    Temporality = maps:get(default_temporality_mapping, Config, otel_aggregation:default_temporality_mapping()),

    %% if a periodic reader is needed then this value is set
    %% somehow need to do a default of 10000 MILLIS, but only if this is a periodic reader
    ExporterIntervalMs = maps:get(export_interval_ms, Config, undefined),

    TRef = case ExporterIntervalMs of
               undefined ->
                   undefined;
               _ ->
                   erlang:send_after(ExporterIntervalMs, self(), collect)
           end,

    GenerationRef =
        try persistent_term:get({?MODULE, ReaderId})
        catch
            error:badarg ->
                GenerationRef0 = atomics:new(1, []),
                persistent_term:put({?MODULE, ReaderId}, GenerationRef0),
                GenerationRef0
        end,

    {ok, #state{exporter=Exporter,
                provider_sup=ProviderSup,
                id=ReaderId,
                default_aggregation_mapping=DefaultAggregationMapping,
                temporality_mapping=Temporality,
                export_interval_ms=ExporterIntervalMs,
                tref=TRef,
                generation_ref=GenerationRef,
                producers=[],
                config=Config}, {continue, register_with_server}}.

handle_continue(register_with_server, State=#state{provider_sup=ProviderSup,
                                                   id=ReaderId,
                                                   default_aggregation_mapping=DefaultAggregationMapping,
                                                   temporality_mapping=Temporality}) ->
    ServerPid = otel_meter_server_sup:provider_pid(ProviderSup),
    {CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab, Resource, Producers} =
        otel_meter_server:add_metric_reader(ServerPid, ReaderId, self(),
                                            DefaultAggregationMapping,
                                            Temporality),
    {noreply, State#state{callbacks_tab=CallbacksTab,
                          streams_tab=StreamsTab,
                          metrics_tab=MetricsTab,
                          exemplars_tab=ExemplarsTab,
                          resource=Resource,
                          producers=Producers}}.

handle_call(collect, _From, State) ->
    State1 = collect_(State),
    {reply, ok, State1};
handle_call(shutdown, _From, State) ->
    {reply, ok, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(collect, State) ->
    State1 = collect_(State),
    {noreply, State1};
%% no tref or exporter, do nothing at all
handle_info(_, State) ->
    {noreply, State}.

code_change(State) ->
    {ok, State}.

%%

collect_(State=#state{exporter=undefined,
                      export_interval_ms=ExporterIntervalMs,
                      tref=TRef}) when TRef =/= undefined andalso
                                       ExporterIntervalMs =/= undefined ->
    erlang:cancel_timer(TRef, [{async, true}]),
    NewTRef = erlang:send_after(ExporterIntervalMs, self(), collect),
    State#state{tref=NewTRef};
collect_(State=#state{id=ReaderId,
                      exporter={_ExporterModule, _Config}=Exporter,
                      export_interval_ms=undefined,
                      tref=undefined,
                      callbacks_tab=CallbacksTab,
                      streams_tab=StreamsTab,
                      metrics_tab=MetricsTab,
                      exemplars_tab=ExemplarsTab,
                      resource=Resource,
                      producers=Producers
                     }) ->
    Metrics = run_collection(CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab, ReaderId, Producers),

    otel_exporter_metrics:export(Exporter, Metrics, Resource),

    State;
collect_(State=#state{id=ReaderId,
                      exporter={_ExporterModule, _Config}=Exporter,
                      export_interval_ms=ExporterIntervalMs,
                      tref=TRef,
                      callbacks_tab=CallbacksTab,
                      streams_tab=StreamsTab,
                      metrics_tab=MetricsTab,
                      exemplars_tab=ExemplarsTab,
                      resource=Resource,
                      producers=Producers
                     }) when TRef =/= undefined andalso
                             ExporterIntervalMs =/= undefined  ->
    erlang:cancel_timer(TRef, [{async, true}]),
    NewTRef = erlang:send_after(ExporterIntervalMs, self(), collect),

    Metrics = run_collection(CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab, ReaderId, Producers),

    otel_exporter_metrics:export(Exporter, Metrics, Resource),

    State#state{tref=NewTRef}.

run_collection(CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab, ReaderId, Producers) ->
    %% collect from view aggregations table and then export
    Metrics = collect_(CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab, ReaderId),

    ExternalMetrics = run_producers(Producers),

    Metrics ++ ExternalMetrics.

run_producers(Producers) ->
    run_producers(Producers, []).

run_producers([], ExternalMetrics) ->
    ExternalMetrics;
run_producers([Producer | Rest], ExternalMetrics) ->
    run_producers(Rest, run_producer(Producer) ++ ExternalMetrics).

run_producer(Producer) ->
    otel_metric_producer:produce_batch(Producer).

-spec collect_(any(), ets:table(), any(), ets:table(), reference()) -> [any()].
collect_(CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab, ReaderId) ->
    _ = run_callbacks(ReaderId, CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab),

    %% Need to be able to efficiently get all from VIEW_AGGREGATIONS_TAB that apply to this reader

    %% for each VIEW_AGGREGATIONS_TAB entry look up metrics from METRICS_TAB using the name
    %% to select for key `{Name, '_'}'. This gives the current value for each set of attributes
    %% for an aggregation.

    %% use the information (temporality) from the VIEW_AGGREGATIONS_TAB entry to reset the
    %% METRICS_TAB entry value (like setting value back to 0 for DELTA)

    %% StreamsTab is a `bag' so to iterate over every Stream for
    %% each Instrument we use `first'/`next' and lookup the list of Streams
    %% by the key (Instrument)
    Key = ets:first(StreamsTab),

    Generation = inc_checkpoint_generation(ReaderId),
    collect_(CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab, Generation, ReaderId, [], Key).

run_callbacks(ReaderId, CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab) ->
    try ets:lookup_element(CallbacksTab, ReaderId, 2) of
        Callbacks ->
            otel_observables:run_callbacks(Callbacks, ReaderId, StreamsTab, MetricsTab, ExemplarsTab)
    catch
        error:badarg ->
            []
    end.

collect_(_CallbacksTab, _StreamsTab, _MetricsTab, _ExemplarsTab, _Generation, _ReaderId, MetricsAcc, '$end_of_table') ->
    MetricsAcc;
collect_(CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab, Generation, ReaderId, MetricsAcc, Key) ->
    Streams = ets:lookup_element(StreamsTab, Key, 2),
    collect_(CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab, Generation, ReaderId,
             checkpoint_metrics(MetricsTab,
                                ExemplarsTab,
                                Generation,
                                ReaderId,
                                Streams) ++ MetricsAcc,
             ets:next(StreamsTab, Key)).

checkpoint_metrics(MetricsTab, ExemplarsTab, Generation, Id, Streams) ->
    lists:foldl(fun(#stream{aggregation_module=otel_aggregation_drop}, Acc) ->
                        Acc;
                   (Stream=#stream{name=Name,
                                   reader=ReaderId,
                                   instrument=Instrument=#instrument{unit=Unit},
                                   aggregation_module=AggregationModule,
                                   description=Description
                                  }, Acc) when Id =:= ReaderId ->
                        Data = AggregationModule:collect(MetricsTab,
                                                         ExemplarsTab,
                                                         Stream,
                                                         Generation),
                        [metric(Instrument, Name, Description, Unit, Data) | Acc];
                   (_, Acc) ->
                        Acc
                end, [], Streams).

metric(#instrument{meter={_, Meter=#meter{}}}, Name, Description, Unit, Data) ->
    #metric{scope=otel_meter_default:scope(Meter),
            name=Name,
            description=Description,
            unit=Unit,
            data=Data}.
