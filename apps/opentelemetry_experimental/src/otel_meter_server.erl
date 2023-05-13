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

-export([start_link/4,
         add_metric_reader/4,
         add_metric_reader/5,
         add_instrument/1,
         add_instrument/2,
         register_callback/3,
         register_callback/4,
         add_view/2,
         add_view/3,
         add_view/4,
         record/5,
         record/6,
         force_flush/0,
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
-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").
-include_lib("kernel/include/logger.hrl").
-include("otel_metrics.hrl").
-include("otel_view.hrl").

-type meter() :: #meter{}.

-record(reader,
        {
         id                          :: reference(),
         pid                         :: pid(),
         monitor_ref                 :: reference(),
         module                      :: module(),
         config                      :: term(),
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

         instruments_tab :: ets:table(),
         callbacks_tab :: ets:table(),
         view_aggregations_tab :: ets:table(),
         metrics_tab :: ets:table(),

         views :: [otel_view:t()],
         readers :: [#reader{}],

         resource :: otel_resource:t()
        }).

-spec start_link(atom(), atom(), otel_resource:t(), otel_configuration:t()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Name, RegName, Resource, Config) ->
    gen_server:start_link({local, RegName}, ?MODULE, [Name, RegName, Resource, Config], []).

-spec add_instrument(otel_instrument:t()) -> boolean().
add_instrument(Instrument) ->
    add_instrument(?GLOBAL_METER_PROVIDER_REG_NAME, Instrument).

-spec add_instrument(atom(), otel_instrument:t()) -> boolean().
add_instrument(Provider, Instrument) ->
    gen_server:call(Provider, {add_instrument, Instrument}).

add_metric_reader(ReaderId, ReaderPid, DefaultAggregationMapping, Temporality) ->
    add_metric_reader(?GLOBAL_METER_PROVIDER_REG_NAME, ReaderId, ReaderPid,
                      DefaultAggregationMapping, Temporality).

add_metric_reader(Provider, ReaderId, ReaderPid, DefaultAggregationMapping, Temporality) ->
    gen_server:call(Provider, {add_metric_reader, ReaderId, ReaderPid, DefaultAggregationMapping, Temporality}).

-spec register_callback([otel_instrument:t()], otel_instrument:callback(), term()) -> boolean().
register_callback(Instruments, Callback, CallbackArgs) ->
    register_callback(?GLOBAL_METER_PROVIDER_REG_NAME, Instruments, Callback, CallbackArgs).

-spec register_callback(atom(), [otel_instrument:t()], otel_instrument:callback(), term()) -> boolean().
register_callback(Provider, Instruments, Callback, CallbackArgs) ->
    gen_server:call(Provider, {register_callback, Instruments, Callback, CallbackArgs}).

-spec add_view(otel_view:criteria(), otel_view:config()) -> boolean().
add_view(Criteria, Config) ->
    add_view(?GLOBAL_METER_PROVIDER_REG_NAME, undefined, Criteria, Config).

-spec add_view(otel_view:name(), otel_view:criteria(), otel_view:config()) -> boolean().
add_view(Name, Criteria, Config) ->
    add_view(?GLOBAL_METER_PROVIDER_REG_NAME, Name, Criteria, Config).

-spec add_view(atom(), otel_view:name(), otel_view:criteria(), otel_view:config()) -> boolean().
add_view(Provider, Name, Criteria, Config) ->
    gen_server:call(Provider, {add_view, Name, Criteria, Config}).

-spec record(ets:table(), ets:table(), otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok.
record(ViewAggregationsTab, MetricsTab, Instrument, Number, Attributes) ->
    handle_measurement(#measurement{instrument=Instrument,
                                    value=Number,
                                    attributes=Attributes}, ViewAggregationsTab, MetricsTab).

-spec record(otel_meter:t(), ets:table(), ets:table(), otel_instrument:t() | otel_instrument:name(), number(), opentelemetry:attributes_map()) -> ok.
record(Meter, ViewAggregationTab, MetricsTab, Name, Number, Attributes) ->
    handle_measurement(Meter, Name, Number, Attributes, ViewAggregationTab, MetricsTab).

-spec force_flush() -> ok.
force_flush() ->
    force_flush(?GLOBAL_METER_PROVIDER_REG_NAME).

-spec force_flush(gen_server:server_ref()) -> ok.
force_flush(Provider) ->
    gen_server:call(Provider, force_flush).

init([Name, RegName, Resource, Config]) ->
    InstrumentsTab = instruments_tab(RegName),
    CallbacksTab = callbacks_tab(RegName),
    ViewAggregationsTab = view_aggregations_tab(RegName),
    MetricsTab = metrics_tab(RegName),

    Meter = #meter{module=otel_meter_default,
                   instruments_tab=InstrumentsTab,
                   provider=RegName,
                   view_aggregations_tab=ViewAggregationsTab,
                   metrics_tab=MetricsTab},

    %% TODO: don't do this if its already set?
    opentelemetry_experimental:set_default_meter(Name, {otel_meter_default, Meter}),

    %% TODO: drop View if Criteria is a wildcard instrument name and View
    %% name is not undefined
    Views = [new_view(V) || V <- maps:get(views, Config, [])],

    {ok, #state{shared_meter=Meter,
                instruments_tab=InstrumentsTab,
                callbacks_tab=CallbacksTab,
                view_aggregations_tab=ViewAggregationsTab,
                metrics_tab=MetricsTab,
                views=Views,
                readers=[],
                resource=Resource}}.

handle_call({add_metric_reader, ReaderId, ReaderPid, DefaultAggregationMapping, Temporality},
            _From, State=#state{readers=Readers,
                                views=Views,
                                instruments_tab=InstrumentsTab,
                                callbacks_tab=CallbacksTab,
                                view_aggregations_tab=ViewAggregationsTab,
                                metrics_tab=MetricsTab,
                                resource=Resource}) ->
    Reader = metric_reader(ReaderId,
                           ReaderPid,
                           DefaultAggregationMapping,
                           Temporality),
    Readers1 = [Reader | Readers],

    %% create ViewAggregations entries for existing View/Instrument
    %% matches for the new Reader
    _ = update_view_aggregations(InstrumentsTab, CallbacksTab, ViewAggregationsTab, Views, Readers1),

    {reply, {CallbacksTab, ViewAggregationsTab, MetricsTab, Resource}, State#state{readers=Readers1}};
handle_call(resource, _From, State=#state{resource=Resource}) ->
    {reply, Resource, State};
handle_call({add_instrument, Instrument}, _From, State=#state{readers=Readers,
                                                              views=Views,
                                                              instruments_tab=InstrumentsTab,
                                                              callbacks_tab=CallbacksTab,
                                                              view_aggregations_tab=ViewAggregationsTab}) ->
    _ = add_instrument_(InstrumentsTab, CallbacksTab, ViewAggregationsTab, Instrument, Views, Readers),
    {reply, ok, State};
handle_call({register_callback, Instruments, Callback, CallbackArgs}, _From, State=#state{readers=Readers,
                                                                                          callbacks_tab=CallbacksTab}) ->
    _ = register_callback_(CallbacksTab, Instruments, Callback, CallbackArgs, Readers),
    {reply, ok, State};
handle_call({get_meter, Name, Vsn, SchemaUrl}, _From, State=#state{shared_meter=Meter}) ->
    Scope = opentelemetry:instrumentation_scope(Name, Vsn, SchemaUrl),
    MeterTuple = {Meter#meter.module,
                  Meter#meter{instrumentation_scope=Scope}},
    {reply, MeterTuple, State};
handle_call({get_meter, Scope}, _From, State=#state{shared_meter=Meter}) ->
    {reply, {Meter#meter.module,
             Meter#meter{instrumentation_scope=Scope}}, State};
handle_call({add_view, Name, Criteria, Config}, _From, State=#state{views=Views,
                                                                    instruments_tab=InstrumentsTab,
                                                                    callbacks_tab=CallbacksTab,
                                                                    view_aggregations_tab=ViewAggregationsTab,
                                                                    readers=Readers}) ->
    %% TODO: drop View if Criteria is a wildcard instrument name and View name is not undefined
    NewView = otel_view:new(Name, Criteria, Config),
    _ = update_view_aggregations(InstrumentsTab, CallbacksTab, ViewAggregationsTab, [NewView], Readers),
    {reply, true, State#state{views=[NewView | Views]}};
handle_call(force_flush, _From, State=#state{readers=Readers}) ->
    [otel_metric_reader:collect(Pid) || #reader{pid=Pid} <- Readers],
    {reply, ok, State}.

handle_cast(_, State) ->
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

instruments_tab(Name) ->
    ets:new(list_to_atom(lists:concat([instruments, "_", Name])), [set,
                                                                   named_table,
                                                                   {keypos, 1},
                                                                   protected]).

callbacks_tab(Name) ->
    ets:new(list_to_atom(lists:concat([callbacks, "_", Name])), [bag,
                                                                 named_table,
                                                                 {keypos, 1},
                                                                 protected]).

view_aggregations_tab(Name) ->
    ets:new(list_to_atom(lists:concat([view_aggregations, "_", Name])), [bag,
                                                                         named_table,
                                                                         {keypos, 1},
                                                                         public]).

metrics_tab(Name) ->
    ets:new(list_to_atom(lists:concat([metrics, "_", Name])), [set,
                                                               named_table,
                                                               {keypos, 2},
                                                               public]).

-dialyzer({nowarn_function,new_view/1}).
new_view(ViewConfig) ->
    Name = maps:get(name, ViewConfig, undefined),
    Description = maps:get(description, ViewConfig, undefined),
    Selector = maps:get(selector, ViewConfig, undefined),
    AttributeKeys = maps:get(attribute_keys, ViewConfig, undefined),
    AggregationModule = maps:get(aggregation_module, ViewConfig, undefined),
    AggregationOptions = maps:get(aggregation_options, ViewConfig, #{}),
    otel_view:new(Name, Selector, #{description => Description,
                                    attribute_keys => AttributeKeys,
                                    aggregation_module => AggregationModule,
                                    aggregation_options => AggregationOptions
                                   }).

%% Match the Instrument to views and then store a per-Reader aggregation for the View
add_instrument_(InstrumentsTab, CallbacksTab, ViewAggregationsTab, Instrument=#instrument{meter=Meter,
                                                                                          name=Name}, Views, Readers) ->
    Key = {Meter, Name},
    case ets:insert_new(InstrumentsTab, {Key, Instrument}) of
        true ->
            ViewMatches = otel_view:match_instrument_to_views(Instrument, Views),
            lists:foreach(fun(Reader=#reader{id=ReaderId}) ->
                                  Matches = per_reader_aggregations(Reader, Instrument, ViewMatches),
                                  [_ = ets:insert(ViewAggregationsTab, {Key, M}) || M <- Matches],
                                  case {Instrument#instrument.callback, Instrument#instrument.callback_args} of
                                      {undefined, _} ->
                                          ok;
                                      {Callback, CallbackArgs} ->
                                          ets:insert(CallbacksTab, {ReaderId, {Callback, CallbackArgs, Instrument}})
                                  end
                          end, Readers);
        false ->
            ?LOG_INFO("Instrument ~p already created. Ignoring attempt to create Instrument with the same name in the same Meter.", [Name]),
            ok
    end.

%% used when a new View is added and the Views must be re-matched with each Instrument
update_view_aggregations(InstrumentsTab, CallbacksTab, ViewAggregationsTab, Views, Readers) ->
    ets:foldl(fun({_, Instrument}, Acc) ->
                      update_view_aggregations_(Instrument, CallbacksTab, ViewAggregationsTab, Views, Readers),
                      Acc
              end, ok, InstrumentsTab).

update_view_aggregations_(Instrument=#instrument{meter=Meter,
                                                 name=Name}, CallbacksTab, ViewAggregationsTab, Views, Readers) ->
    Key = {Meter, Name},
    ViewMatches = otel_view:match_instrument_to_views(Instrument, Views),
    lists:foreach(fun(Reader=#reader{id=ReaderId}) ->
                          Matches = per_reader_aggregations(Reader, Instrument, ViewMatches),
                          [true = ets:insert(ViewAggregationsTab, {Key, M}) || M <- Matches],
                          case {Instrument#instrument.callback, Instrument#instrument.callback_args} of
                              {undefined, _} ->
                                  ok;
                              {Callback, CallbackArgs} ->
                                  ets:insert(CallbacksTab, {ReaderId, {Callback, CallbackArgs, Instrument}})
                          end
                  end, Readers).

%% Match the Instrument to views and then store a per-Reader aggregation for the View
register_callback_(CallbacksTab, Instruments, Callback, CallbackArgs, Readers) ->
    lists:map(fun(#reader{id=ReaderId}) ->
                      ets:insert(CallbacksTab, {ReaderId, {Callback, CallbackArgs, Instruments}})
              end, Readers).

metric_reader(ReaderId, ReaderPid, DefaultAggregationMapping, Temporality) ->
    %% TODO: Uncomment when we can drop OTP-23 support
    %% Ref = erlang:monitor(process, ReaderPid, [{tag, 'DOWN_READER'}]),
    Ref = erlang:monitor(process, ReaderPid),

    ReaderAggregationMapping = maps:merge(otel_aggregation:default_mapping(),
                                          DefaultAggregationMapping),
    ReaderTemporalityMapping = maps:merge(otel_aggregation:temporality_mapping(),
                                          Temporality),

    #reader{id=ReaderId,
            pid=ReaderPid,
            monitor_ref=Ref,
            default_aggregation_mapping=ReaderAggregationMapping,
            default_temporality_mapping=ReaderTemporalityMapping}.


%% a Measurement's Instrument is matched against Views
%% each matched View+Reader becomes a ViewAggregation
%% for each ViewAggregation a Measurement updates a Metric (`#metric')
%% active metrics are indexed by the ViewAggregation name + the Measurement's Attributes

handle_measurement(#measurement{instrument=#instrument{meter=Meter,
                                                       name=Name},
                                value=Value,
                                attributes=Attributes},
                   ViewAggregationsTab, MetricsTab) ->
    Matches = ets:match(ViewAggregationsTab, {{Meter, Name}, '$1'}),
    update_aggregations(Value, Attributes, Matches, MetricsTab).

handle_measurement(Meter, Name, Number, Attributes, ViewAggregationsTab, MetricsTab) ->
    Matches = ets:match(ViewAggregationsTab, {{Meter, Name}, '$1'}),
    update_aggregations(Number, Attributes, Matches, MetricsTab).

update_aggregations(Value, Attributes, ViewAggregations, MetricsTab) ->
    lists:foreach(fun([ViewAggregation=#view_aggregation{}]) ->
                          otel_aggregation:maybe_init_aggregate(MetricsTab,
                                                                ViewAggregation,
                                                                Value,
                                                                Attributes);
                     (_) ->
                          ok
                  end, ViewAggregations).

%% create an aggregation for each Reader and its possibly unique aggregation/temporality
per_reader_aggregations(Reader, Instrument, ViewAggregations) ->
    [view_aggregation_for_reader(Instrument, ViewAggregation, View, Reader)
     || {View, ViewAggregation} <- ViewAggregations].

view_aggregation_for_reader(Instrument=#instrument{kind=Kind}, ViewAggregation, View=#view{attribute_keys=AttributeKeys},
                            Reader=#reader{id=Id,
                                           default_temporality_mapping=ReaderTemporalityMapping}) ->
    AggregationModule = aggregation_module(Instrument, View, Reader),
    Temporality = maps:get(Kind, ReaderTemporalityMapping, ?TEMPORALITY_UNSPECIFIED),

    ViewAggregation#view_aggregation{
      reader=Id,
      attribute_keys=AttributeKeys,
      aggregation_module=AggregationModule,
      aggregation_options=#{},
      temporality=Temporality};
view_aggregation_for_reader(Instrument=#instrument{kind=Kind}, ViewAggregation, View,
                            Reader=#reader{id=Id,
                                           default_temporality_mapping=ReaderTemporalityMapping}) ->
    AggregationModule = aggregation_module(Instrument, View, Reader),
    Temporality = maps:get(Kind, ReaderTemporalityMapping, ?TEMPORALITY_UNSPECIFIED),

    ViewAggregation#view_aggregation{
      reader=Id,
      attribute_keys=undefined,
      aggregation_module=AggregationModule,
      aggregation_options=#{},
      temporality=Temporality}.


%% no aggregation defined for the View, so get the aggregation from the Reader
%% the Reader's mapping of Instrument Kind to Aggregation was merged with the
%% global default, so any missing Kind entries are filled in from the global
%% mapping in `otel_aggregation'
-spec aggregation_module(otel_instrument:t(), otel_view:t(), reader()) -> module().
aggregation_module(#instrument{kind=Kind}, undefined,
                   #reader{default_aggregation_mapping=ReaderAggregationMapping}) ->
    maps:get(Kind, ReaderAggregationMapping);
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
