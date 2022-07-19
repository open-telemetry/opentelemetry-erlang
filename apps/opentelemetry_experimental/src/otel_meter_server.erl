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
%% @doc This module is the SDK's implementation of the MeterProvider. The
%% calls to the server are done from the API module `otel_meter_provider'.
%% This `gen_server' is started as part of the SDK's supervision tree and
%% registers itself as the default MeterProvider by using the atom
%% `otel_meter_provider' as its name.
%%
%% The MeterProvider is where Meter's are created and Views are registered.
%%
%% Each MeterProvider has an associated MetricReader.
%%
%% The MeterProvider "owns" any Instrument created with a Meter from that
%% MeterProvider.
%%
%% For Measumrents on an Instrument the MeterProvider's Views are checked
%% for a match. If no match is found the default aggregation and temporality
%% is used.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_meter_server).

-behaviour(gen_server).

-export([start_link/1,
         add_metric_reader/3,
         add_view/3,
         add_view/4,
         record/4,
         force_flush/1,
         report_cb/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include_lib("kernel/include/logger.hrl").
-include("otel_metrics.hrl").
-include("otel_view.hrl").

-type meter() :: #meter{}.

-export_type([meter/0]).

-record(reader,
        {
         pid                         :: pid(),
         monitor_ref                 :: reference(),
         module                      :: module(),
         config                      :: term(),
         view_aggregation_tab        :: ets:tid(),
         metrics_tab                 :: ets:tid(),
         default_aggregation_mapping :: #{},
         default_temporality_mapping :: #{}
        }).

-type reader() :: #reader{}.

-record(state,
        {
         shared_meter,

         views :: [otel_view:t()],
         readers :: [#reader{}],

         %% the meter configuration shared between all named
         %% meters created by this provider
         telemetry_library :: #telemetry_library{} | undefined,
         resource :: otel_resource:t()
        }).

-spec start_link(otel_configuration:t()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Config) ->
    start_link(?DEFAULT_METER_PROVIDER, Config).

-spec start_link(atom(), otel_configuration:t()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Provider, Config) ->
    gen_server:start_link({local, Provider}, ?MODULE, [Provider, Config], []).

-spec add_metric_reader(atom(), module(), term()) -> boolean().
add_metric_reader(Provider, ReaderModule, ReaderOptions) ->
    gen_server:call(Provider, {add_metric_reader, ReaderModule, ReaderOptions}).

-spec add_view(atom(), otel_view:criteria(), otel_view:config()) -> boolean().
add_view(Provider, Criteria, Config) ->
    add_view(Provider, undefined, Criteria, Config).

-spec add_view(atom(), otel_view:name(), otel_view:criteria(), otel_view:config()) -> boolean().
add_view(Provider, Name, Criteria, Config) ->
    gen_server:call(Provider, {add_view, Name, Criteria, Config}).

-spec record(atom(), otel_instrument:t(), number(), otel_attributes:t()) -> ok.
record(Provider, Instrument, Number, Attributes) ->
    gen_server:cast(Provider, {record,  #measurement{instrument=Instrument,
                                                     value=Number,
                                                     attributes=Attributes}}).

force_flush(Provider) ->
    gen_server:call(Provider, force_flush).

init([Name, _Config]) ->
    Resource = otel_resource_detector:get_resource(),

    Meter = #meter{module=otel_meter_default,
                   provider=Name},

    opentelemetry_experimental:set_default_meter({otel_meter_default, Meter}),

    {ok, LibraryVsn} = application:get_key(opentelemetry_experimental, vsn),
    LibraryName = <<"opentelemetry">>,
    LibraryLanguage = <<"erlang">>,
    TelemetryLibrary = #telemetry_library{name=LibraryName,
                                          language=LibraryLanguage,
                                          version=list_to_binary(LibraryVsn)},
    {ok, #state{shared_meter=Meter,
                views=[],
                readers=[],
                telemetry_library=TelemetryLibrary,
                resource=Resource}}.

handle_call(resource, _From, State=#state{resource=Resource}) ->
    {reply, Resource, State};
handle_call({get_meter, Name, Vsn, SchemaUrl}, _From, State=#state{shared_meter=Meter}) ->
    InstrumentationLibrary = opentelemetry:instrumentation_library(Name, Vsn, SchemaUrl),
    MeterTuple = {Meter#meter.module,
                  Meter#meter{instrumentation_library=InstrumentationLibrary}},
    {reply, MeterTuple, State};
handle_call({get_meter, InstrumentationLibrary}, _From, State=#state{shared_meter=Meter}) ->
    {reply, {Meter#meter.module,
             Meter#meter{instrumentation_library=InstrumentationLibrary}}, State};
handle_call({add_metric_reader, ReaderModule, ReaderConfig}, _From, State=#state{readers=Readers}) ->
    Reader = start_reader(ReaderModule, ReaderConfig),
    {reply, ok, State#state{readers=[Reader | Readers]}};
handle_call({add_view, Name, Criteria, Config}, _From, State=#state{views=Views}) ->
    %% TODO: drop View if Criteria is a wildcard instrument name and View name is not undefined
    View = otel_view:new(Name, Criteria, Config),
    {reply, true, State#state{views=[View | Views]}};
handle_call(force_flush, _From, State=#state{readers=Readers}) ->
    [otel_metric_reader:collect(Pid) || #reader{pid=Pid} <- Readers],
    {reply, ok, State}.

handle_cast({record, Measurement}, State=#state{readers=Readers,
                                                views=Views}) ->
    handle_measurement(Measurement, Readers, Views),
    {noreply, State}.

handle_info({'DOWN', Ref, process, ReaderPid, _Reason}, State=#state{readers=Readers}) ->
    %% TODO: update all viewaggregations to reference the new pid of the reader
    case lists:search(fun(#reader{pid=Pid,
                                  monitor_ref=MonRef}) ->
                              Pid =:= ReaderPid andalso MonRef =:= Ref
                      end, Readers) of
        {value, R=#reader{module=Module,
                          config=Config}} ->
            Reader = start_reader(Module, Config),
            {noreply, State#state{readers=[Reader | lists:delete(R, Readers)]}};
        false ->
            {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

code_change(State) ->
    {ok, State}.

%%

create_tables() ->
    %% elements are {Instrument, [ViewAggregation]}
    ViewAggregationTab = ets:new(view_aggregation_tab, [set,
                                                        protected,
                                                        {keypos, 1}]),
    MetricsTab = ets:new(metrics_tab, [set,
                                       public,
                                       {keypos, 2}]),
    {ViewAggregationTab, MetricsTab}.

%% a Measurement's Instrument is matched against Views
%% each matched View+Reader becomes a ViewAggregation
%% for each ViewAggregation a Measurement updates a Metric (`#metric')
%% active metrics are indexed by the ViewAggregation name + the Measurement's Attributes

handle_measurement(Measurement=#measurement{instrument=Instrument},
                   Readers,
                   Views) ->
    ViewMatches = otel_view:match_instrument_to_views(Instrument, Views),
    lists:map(fun(Reader=#reader{view_aggregation_tab=ViewAggregationTab}) ->
                      case ets:lookup(ViewAggregationTab, Instrument) of
                          [] ->
                              %% this instrument hasn't been seen before
                              Matches = per_reader_aggregations(Reader, Instrument, ViewMatches),
                              true = ets:insert(ViewAggregationTab, {Instrument, Matches}),
                              update_aggregations(Measurement, Reader, Matches);
                          [{_, Matches}] ->
                              %% TODO: matches need to be  updated when a new view is added
                              update_aggregations(Measurement, Reader, Matches)
                      end
              end, Readers).

update_aggregations(#measurement{attributes=Attributes,
                                 value=Value}, #reader{metrics_tab=MetricsTab}, ViewAggregations) ->
    lists:map(fun(#view_aggregation{name=Name,
                                    aggregation_module=AggregationModule,
                                    aggregation_options=Options}) ->
                          case AggregationModule:aggregate(MetricsTab, {Name, Attributes}, Value) of
                              true ->
                                  ok;
                              false ->
                                  %% entry doesn't exist, create it and rerun the aggregate function
                                  Metric = AggregationModule:init({Name, Attributes}, Options),
                                  _ = ets:insert(MetricsTab, Metric),
                                  AggregationModule:aggregate(MetricsTab, {Name, Attributes}, Value)
                          end
              end, ViewAggregations).

start_reader(ReaderModule, ReaderConfig) ->
    {ViewAggregationTab, MetricsTab} = create_tables(),

    %% TODO: using a monitor bc the original idea was readers could continue operating without
    %% the meter server this isn't the case right now since the ets tables are owned by the meter server
    %% unless this is changed this needs to be switched to a link
    %% also need to be able to recover who the readers are when the meter server restarts
    %% discovery by looking at a supervision tree could be a good idea as it would allow for added
    %% readers to be returned to the server after a crash
    {ok, {Pid, Mon}} = otel_metric_reader:start_monitor(otel_metric_reader,
                                                        ReaderConfig#{view_aggregation_tab => ViewAggregationTab,
                                                                      metrics_tab => MetricsTab}),
    ReaderAggregationMapping = maps:merge(otel_aggregation:default_mapping(),
                                          maps:get(default_aggregation_mapping, ReaderConfig, #{})),
    ReaderTemporalityMapping = maps:merge(otel_aggregation:temporality_mapping(),
                                          maps:get(default_temporality_mapping, ReaderConfig, #{})),
    #reader{pid=Pid,
            monitor_ref=Mon,
            module=ReaderModule,
            config=ReaderConfig,
            view_aggregation_tab=ViewAggregationTab,
            metrics_tab=MetricsTab,
            default_aggregation_mapping=ReaderAggregationMapping,
            default_temporality_mapping=ReaderTemporalityMapping}.

%% create an aggregation for each Reader and its possibly unique aggregation/temporality
per_reader_aggregations(Reader, Instrument, ViewAggregations) ->
    [view_aggregation_for_reader(Instrument, ViewAggregation, View, Reader)
     || {View, ViewAggregation} <- ViewAggregations].

view_aggregation_for_reader(Instrument=#instrument{kind=Kind}, ViewAggregation, View,
                            Reader=#reader{default_temporality_mapping=ReaderTemporalityMapping}) ->
    AggregationModule = aggregation_module(Instrument, View, Reader),
    Temporality = maps:get(Kind, ReaderTemporalityMapping),

    ViewAggregation#view_aggregation{
      aggregation_module=AggregationModule,
      temporality=Temporality}.

%% no aggregation defined for the View, so get the aggregation from the Reader
%% the Reader's mapping of Instrument Kind to Aggregation was merged with the
%% global default, so any missing Kind entries are filled in from the global
%% mapping in `otel_aggregation'
-spec aggregation_module(otel_instrument:t(), otel_view:t(), reader()) -> module().
aggregation_module(#instrument{kind=Kind}, #view{aggregation_module=undefined},
                   #reader{default_aggregation_mapping=ReaderAggregationMapping}) ->
    maps:get(Kind, ReaderAggregationMapping);
aggregation_module(_Instrument, #view{aggregation_module=Module}, _Reader) ->
    Module.

report_cb(#{instrument_name := Name,
            class := Class,
            exception := Exception,
            stacktrace := StackTrace}) ->
    {"failed to create instrument: name=~ts exception=~ts",
     [Name, otel_utils:format_exception(Class, Exception, StackTrace)]};
report_cb(#{view_name := Name,
            class := Class,
            exception := Exception,
            stacktrace := StackTrace}) ->
    {"failed to create view: name=~ts exception=~ts",
     [Name, otel_utils:format_exception(Class, Exception, StackTrace)]}.
