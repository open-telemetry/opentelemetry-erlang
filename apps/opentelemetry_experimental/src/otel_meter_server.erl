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

-export([start_link/3,
         add_metric_reader/7,
         add_instrument/2,
         register_callback/4,
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
%% need to move shared records out of otel_span.hrl
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include_lib("kernel/include/logger.hrl").
-include("otel_metrics.hrl").
-include("otel_view.hrl").

-type meter() :: #meter{}.

-record(reader,
        {
         child_id :: atom(),
         pid                         :: pid(),
         monitor_ref                 :: reference(),
         module                      :: module(),
         config                      :: term(),
         callbacks_tab               :: ets:tid() | atom(),
         view_aggregation_tab        :: ets:tid() | atom(),
         metrics_tab                 :: ets:tid() | atom(),
         default_aggregation_mapping :: map(),
         default_temporality_mapping :: map()
        }).

-type reader() :: #reader{}.

-type view_config() :: #{name => otel_instrument:name() | undefined,
                         description => unicode:unicode_binary() | undefined,
                         selector => otel_view:criteria(),
                         attribute_keys => [opentelemetry:attribute_key()] | undefined,
                         aggregation_module => module() | undefined,
                         aggregation_options => map()}.

-export_type([meter/0,
              view_config/0]).

-record(state,
        {
         shared_meter,

         views :: [otel_view:t()],
         readers :: [#reader{}],

         resource :: otel_resource:t()
        }).

-spec start_link(atom(), atom(), otel_configuration:t()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Name, RegName, Config) ->
    gen_server:start_link({local, RegName}, ?MODULE, [Name, RegName, Config], []).

-spec add_instrument(atom(), otel_instrument:t()) -> boolean().
add_instrument(Provider, Instrument) ->
    gen_server:call(Provider, {add_instrument, Instrument}).

add_metric_reader(Provider, ReaderPid, DefaultAggregationMapping, Temporality,
                  ViewAggregationTable, CallbacksTable, MetricsTable) ->
    gen_server:call(Provider, {add_metric_reader, ReaderPid, DefaultAggregationMapping, Temporality,
                               ViewAggregationTable, CallbacksTable, MetricsTable}).

-spec register_callback(atom(), [otel_instrument:t()], otel_instrument:callback(), term()) -> boolean().
register_callback(Provider, Instruments, Callback, CallbackArgs) ->
    gen_server:call(Provider, {register_callback, Instruments, Callback, CallbackArgs}).

-spec add_view(atom(), otel_view:criteria(), otel_view:config()) -> boolean().
add_view(Provider, Criteria, Config) ->
    add_view(Provider, undefined, Criteria, Config).

-spec add_view(atom(), otel_view:name(), otel_view:criteria(), otel_view:config()) -> boolean().
add_view(Provider, Name, Criteria, Config) ->
    gen_server:call(Provider, {add_view, Name, Criteria, Config}).

-spec record(atom(), otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok.
record(Provider, Instrument, Number, Attributes) ->
    gen_server:cast(Provider, {record,  #measurement{instrument=Instrument,
                                                     value=Number,
                                                     attributes=Attributes}}).

force_flush(Provider) ->
    gen_server:call(Provider, force_flush).

init([Name, RegName, Config]) ->
    Resource = otel_resource_detector:get_resource(),

    Meter = #meter{module=otel_meter_default,
                   provider=RegName},

    opentelemetry_experimental:set_default_meter(Name, {otel_meter_default, Meter}),

    %% TODO: drop View if Criteria is a wildcard instrument name and View
    %% name is not undefined
    Views = [new_view(V) || V <- maps:get(views, Config, [])],

    {ok, #state{shared_meter=Meter,
                views=Views,
                readers=[],
                resource=Resource}}.

new_view(ViewConfig) ->
    Name = maps:get(name, ViewConfig, undefined),
    Description = maps:get(description, ViewConfig, undefined),
    Selector = maps:get(selector, ViewConfig, undefined),
    AttributeKeys = maps:get(attribute_keys, ViewConfig, undefined),
    AggregationModule = maps:get(aggregation_module, ViewConfig, undefined),
    AggregationOptions = maps:get(aggregation_options, ViewConfig, undefined),
    otel_view:new(Name, Selector, #{description => Description,
                                    attribute_keys => AttributeKeys,
                                    aggregation_module => AggregationModule,
                                    aggregation_options => AggregationOptions
                                   }).

handle_call({add_metric_reader, ReaderPid, DefaultAggregationMapping, Temporality,
             ViewAggregationTable, CallbacksTable, MetricsTable}, _From, State=#state{readers=Readers}) ->

    {reply, ok, State#state{readers=[metric_reader(ReaderPid, DefaultAggregationMapping, Temporality,
                                                   ViewAggregationTable, CallbacksTable, MetricsTable) | Readers]}};
handle_call({record, Measurement}, _From, State=#state{readers=Readers,
                                                       views=Views}) ->
    handle_measurement(Measurement, Readers, Views),
    {reply, ok, State};
handle_call(resource, _From, State=#state{resource=Resource}) ->
    {reply, Resource, State};
handle_call({add_instrument, Instrument}, _From, State=#state{readers=Readers,
                                                              views=Views}) ->
    _ = add_instrument_(Instrument, Views, Readers),
    {reply, ok, State};
handle_call({register_callback, Instruments, Callback, CallbackArgs}, _From, State=#state{readers=Readers,
                                                                            views=Views}) ->
    _ = register_callback_(Instruments, Callback, CallbackArgs, Views, Readers),
    {reply, ok, State};
handle_call({get_meter, Name, Vsn, SchemaUrl}, _From, State=#state{shared_meter=Meter}) ->
    Scope = opentelemetry:instrumentation_scope(Name, Vsn, SchemaUrl),
    MeterTuple = {Meter#meter.module,
                  Meter#meter{instrumentation_scope=Scope}},
    {reply, MeterTuple, State};
handle_call({get_meter, Scope}, _From, State=#state{shared_meter=Meter}) ->
    {reply, {Meter#meter.module,
             Meter#meter{instrumentation_scope=Scope}}, State};
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

%% TODO: Uncomment when we can drop OTP-23 support
%% handle_info({'DOWN_READER', Ref, process, _Pid, _} , State=#state{readers=Readers}) ->
handle_info({'DOWN', Ref, process, _Pid, _} , State=#state{readers=Readers}) ->
    {noreply, State#state{readers=lists:keydelete(Ref, #reader.monitor_ref, Readers)}};
handle_info(_, State) ->
    {noreply, State}.

code_change(State) ->
    {ok, State}.

%%

%% Match the Instrument to views and then store a per-Reader aggregation for the View
add_instrument_(Instrument, Views, Readers) ->
    ViewMatches = otel_view:match_instrument_to_views(Instrument, Views),
    lists:map(fun(Reader=#reader{callbacks_tab=CallbackTab,
                                 view_aggregation_tab=ViewAggregationTab}) ->
                      Matches = per_reader_aggregations(Reader, Instrument, ViewMatches),
                      _ = ets:insert(ViewAggregationTab, {Instrument, Matches}),
                      case {Instrument#instrument.callback, Instrument#instrument.callback_args} of
                          {undefined, _} ->
                              ok;
                          {Callback, CallbackArgs} ->
                              ets:insert(CallbackTab, {Callback, CallbackArgs, [Instrument]})
                      end
              end, Readers).

%% Match the Instrument to views and then store a per-Reader aggregation for the View
register_callback_(Instruments, Callback, CallbackArgs, Views, Readers) ->
    lists:map(fun(Instrument) ->
                      ViewMatches = otel_view:match_instrument_to_views(Instrument, Views),
                      lists:map(fun(Reader=#reader{callbacks_tab=CallbackTab,
                                                   view_aggregation_tab=ViewAggregationTab}) ->
                                        Matches = per_reader_aggregations(Reader, Instrument, ViewMatches),
                                        _ = ets:insert(ViewAggregationTab, {Instrument, Matches}),
                                        ets:insert(CallbackTab, {Callback, CallbackArgs, Instruments})
                                end, Readers)
              end, Instruments).

metric_reader(ReaderPid, DefaultAggregationMapping, Temporality,
              ViewAggregationTable, CallbacksTable, MetricsTable) ->
    %% TODO: Uncomment when we can drop OTP-23 support
    %% Ref = erlang:monitor(process, ReaderPid, [{tag, 'DOWN_READER'}]),
    Ref = erlang:monitor(process, ReaderPid),

    ReaderAggregationMapping = maps:merge(otel_aggregation:default_mapping(),
                                          DefaultAggregationMapping),
    ReaderTemporalityMapping = maps:merge(otel_aggregation:temporality_mapping(),
                                          Temporality),

    #reader{pid=ReaderPid,
            monitor_ref=Ref,
            callbacks_tab=CallbacksTable,
            view_aggregation_tab=ViewAggregationTable,
            metrics_tab=MetricsTable,
            default_aggregation_mapping=ReaderAggregationMapping,
            default_temporality_mapping=ReaderTemporalityMapping}.


%% a Measurement's Instrument is matched against Views
%% each matched View+Reader becomes a ViewAggregation
%% for each ViewAggregation a Measurement updates a Metric (`#metric')
%% active metrics are indexed by the ViewAggregation name + the Measurement's Attributes

handle_measurement(Measurement=#measurement{instrument=Instrument},
                   Readers,
                   Views) ->
    ViewMatches = otel_view:match_instrument_to_views(Instrument, Views),
    lists:map(fun(Reader=#reader{view_aggregation_tab=ViewAggregationTab}) ->
                      try ets:lookup(ViewAggregationTab, Instrument) of
                          [] ->
                              %% this instrument hasn't been seen before
                              Matches = per_reader_aggregations(Reader, Instrument, ViewMatches),
                              true = ets:insert(ViewAggregationTab, {Instrument, Matches}),
                              update_aggregations(Measurement, Reader, Matches);
                          [{_, Matches}] ->
                              %% TODO: matches need to be  updated when a new view is added
                              update_aggregations(Measurement, Reader, Matches)
                      catch
                          %% table doesn't exist, Reader may have crashed and be still
                          %% waiting on the DOWN message from the monitor
                          exit:badarg ->
                              Reader
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

%% create an aggregation for each Reader and its possibly unique aggregation/temporality
per_reader_aggregations(Reader, Instrument, ViewAggregations) ->
    [view_aggregation_for_reader(Instrument, ViewAggregation, View, Reader)
     || {View, ViewAggregation} <- ViewAggregations].

view_aggregation_for_reader(Instrument=#instrument{kind=Kind}, ViewAggregation, View,
                            Reader=#reader{default_temporality_mapping=ReaderTemporalityMapping}) ->
    AggregationModule = aggregation_module(Instrument, View, Reader),
    Temporality = maps:get(Kind, ReaderTemporalityMapping, ?AGGREGATION_TEMPORALITY_UNSPECIFIED),

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
