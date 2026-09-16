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
         get_readers/0,
         get_readers/1,
         add_instrument/1,
         add_instrument/2,
         register_callback/3,
         register_callback/4,
         add_view/2,
         add_view/3,
         add_view/4,
         record/4,
         force_flush/0,
         force_flush/1,
         report_cb/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/1]).

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
         streams_tab :: ets:table(),
         metrics_tab :: ets:table(),
         exemplars_tab :: ets:table(),

         exemplars_enabled :: boolean(),
         exemplar_filter :: always_on | always_off | trace_based,

         views :: [otel_view:t()],
         readers :: [#reader{}],

         resource :: otel_resource:t(),

         producers :: [otel_metric_producer:t()]
        }).

%% I think these have warnings because the new view function is ignored
%% which is because it calls functions that use matchspecs in record defs
-dialyzer({nowarn_function, add_view_/9}).
-dialyzer({nowarn_function, new_view/1}).

-spec start_link(atom(), atom(), otel_resource:t(), otel_configuration:t()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Name, RegName, Resource, Config) ->
    gen_server:start_link({local, RegName}, ?MODULE, [Name, RegName, Resource, Config], []).

-spec add_instrument(otel_instrument:t()) -> otel_instrument:t().
add_instrument(Instrument) ->
    add_instrument(?GLOBAL_METER_PROVIDER_REG_NAME, Instrument).

-spec add_instrument(atom(), otel_instrument:t()) -> otel_instrument:t().
add_instrument(Provider, Instrument) ->
    gen_server:call(Provider, {add_instrument, Instrument}).

add_metric_reader(ReaderId, ReaderPid, DefaultAggregationMapping, Temporality) ->
    add_metric_reader(?GLOBAL_METER_PROVIDER_REG_NAME, ReaderId, ReaderPid,
                      DefaultAggregationMapping, Temporality).

add_metric_reader(Provider, ReaderId, ReaderPid, DefaultAggregationMapping, Temporality) ->
    gen_server:call(Provider, {add_metric_reader, ReaderId, ReaderPid, DefaultAggregationMapping, Temporality}).

get_readers() ->
    get_readers(?GLOBAL_METER_PROVIDER_REG_NAME).

get_readers(Provider) ->
    gen_server:call(Provider, get_readers).

-spec register_callback([otel_instrument:t()], otel_instrument:callback(), otel_instrument:callback_args()) -> boolean().
register_callback(Instruments, Callback, CallbackArgs) ->
    register_callback(?GLOBAL_METER_PROVIDER_REG_NAME, Instruments, Callback, CallbackArgs).

-spec register_callback(atom(), otel_instrument:t() | [otel_instrument:t()],
                        otel_instrument:callback(), otel_instrument:callback_args()) -> boolean().
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

-spec record(otel_ctx:t(), otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok | false.
record(Ctx, Instrument=#instrument{}, Number, Attributes) ->
    handle_measurement(Ctx, Instrument, Number, Attributes);
record(_, _, _, _) ->
    false.

-spec force_flush() -> ok.
force_flush() ->
    force_flush(?GLOBAL_METER_PROVIDER_REG_NAME).

-spec force_flush(gen_server:server_ref()) -> ok.
force_flush(Provider) ->
    gen_server:call(Provider, force_flush).

init([Name, RegName, Resource, Config]) ->
    InstrumentsTab = otel_metrics_tables:instruments_tab(RegName),
    CallbacksTab = otel_metrics_tables:callbacks_tab(RegName),
    StreamsTab = otel_metrics_tables:streams_tab(RegName),
    MetricsTab = otel_metrics_tables:metrics_tab(RegName),
    ExemplarsTab = otel_metrics_tables:exemplars_tab(RegName),

    Meter = #meter{module=otel_meter_default,
                   instrumentation_scope=opentelemetry:instrumentation_scope(<<>>, <<>>, <<>>),
                   instruments_tab=InstrumentsTab,
                   provider=RegName,
                   streams_tab=StreamsTab,
                   metrics_tab=MetricsTab,
                   exemplars_tab=ExemplarsTab},

    %% TODO: don't do this if its already set?
    opentelemetry_experimental:set_default_meter(Name, {otel_meter_default, Meter}),

    Views = lists:filtermap(fun new_view/1, maps:get(views, Config, [])),
    ExemplarsEnabled = maps:get(exemplars_enabled, Config, false),
    ExemplarFilter = maps:get(exemplar_filter, Config, trace_based),
    Producers = init_producers(maps:get(metric_producers, Config, [])),

    {ok, #state{shared_meter=Meter,
                instruments_tab=InstrumentsTab,
                callbacks_tab=CallbacksTab,
                streams_tab=StreamsTab,
                metrics_tab=MetricsTab,
                exemplars_tab=ExemplarsTab,
                exemplars_enabled=ExemplarsEnabled,
                exemplar_filter=ExemplarFilter,
                views=Views,
                readers=[],
                resource=Resource,
                producers=Producers}}.

init_producers(ProducerConfigs) ->
    lists:filtermap(fun({ProducerModule, ProducerConfig}) ->
                            case otel_metric_producer:init(ProducerModule, ProducerConfig) of
                                false ->
                                    false;
                                Producer ->
                                    {true, Producer}
                            end
                    end, ProducerConfigs).

handle_call(get_readers, _From, State=#state{readers=Readers}) ->

    {reply, Readers, State};
handle_call({add_metric_reader, ReaderId, ReaderPid, DefaultAggregationMapping, Temporality},
            _From, State=#state{readers=Readers,
                                views=Views,
                                instruments_tab=InstrumentsTab,
                                callbacks_tab=CallbacksTab,
                                streams_tab=StreamsTab,
                                metrics_tab=MetricsTab,
                                exemplars_tab=ExemplarsTab,
                                exemplars_enabled=ExemplarsEnabled,
                                exemplar_filter=ExemplarFilter,
                                resource=Resource,
                                producers=Producers}) ->
    Reader = metric_reader(ReaderId,
                           ReaderPid,
                           DefaultAggregationMapping,
                           Temporality),
    Readers1 = [Reader | Readers],

    %% create Streams entries for existing View/Instrument
    %% matches for the new Reader
    NewStreams = update_streams(InstrumentsTab, CallbacksTab, StreamsTab, Views, [Reader], ExemplarsEnabled, ExemplarFilter),
    maybe_log_stream_conflicts(StreamsTab, NewStreams),

    {reply, {CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab, Resource, Producers}, State#state{readers=Readers1}};
handle_call(resource, _From, State=#state{resource=Resource}) ->
    {reply, Resource, State};
handle_call({add_instrument, Instrument}, _From, State=#state{readers=Readers,
                                                              views=Views,
                                                              instruments_tab=InstrumentsTab,
                                                              callbacks_tab=CallbacksTab,
                                                              streams_tab=StreamsTab,
                                                              exemplars_enabled=ExemplarsEnabled,
                                                              exemplar_filter=ExemplarFilter}) ->
    CanonicalInstrument = add_instrument_(InstrumentsTab, CallbacksTab, StreamsTab, Instrument, Views, Readers, ExemplarsEnabled, ExemplarFilter),
    {reply, CanonicalInstrument, State};
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
                                                                    streams_tab=StreamsTab,
                                                                    readers=Readers}) ->
    add_view_(Name, Criteria, Config, InstrumentsTab, CallbacksTab, StreamsTab, Readers, Views, State);
handle_call(force_flush, _From, State=#state{readers=Readers}) ->
    %% for force_flush do a sync collection of each reader so it blocks until complete
    [otel_metric_reader:call_collect(Pid) || #reader{pid=Pid} <- Readers],
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

add_view_(Name, Criteria, Config, InstrumentsTab, CallbacksTab, StreamsTab, Readers, Views, State=#state{exemplars_enabled=ExemplarsEnabled,
                                                                                                        exemplar_filter=ExemplarFilter}) ->
    case otel_view:new(Name, Criteria, Config) of
        {ok, NewView} ->
            Views1 = [NewView | Views],
            %% Re-evaluate the complete view set. Passing only NewView would
            %% create a fallback/default stream for every instrument it does
            %% not match, potentially replacing a stream from an older view.
            UpdatedStreams = update_streams(InstrumentsTab, CallbacksTab, StreamsTab, Views1, Readers, ExemplarsEnabled, ExemplarFilter),
            maybe_log_stream_conflicts(StreamsTab, UpdatedStreams),
            {reply, true, State#state{views=Views1}};
        {error, named_wildcard_view} ->
            {reply, false, State}
    end.

new_view(ViewConfig) ->
    Name = maps:get(name, ViewConfig, undefined),
    Description = maps:get(description, ViewConfig, undefined),
    Selector = maps:get(selector, ViewConfig, undefined),
    AttributeKeys = maps:get(attribute_keys, ViewConfig, undefined),
    AggregationModule = maps:get(aggregation_module, ViewConfig, undefined),
    AggregationOptions = maps:get(aggregation_options, ViewConfig, #{}),
    case otel_view:new(Name, Selector, #{description => Description,
                                         attribute_keys => AttributeKeys,
                                         aggregation_module => AggregationModule,
                                         aggregation_options => AggregationOptions
                                        }) of
        {ok, View} -> {true, View};
        {error, named_wildcard_view} -> false
    end.

%% Match the Instrument to views and then store a per-Reader aggregation for the View
add_instrument_(InstrumentsTab, CallbacksTab, StreamsTab,
                Instrument=#instrument{meter={_, Meter=#meter{}},
                                       name=Name}, Views, Readers, ExemplarsEnabled, ExemplarFilter) ->
    Identity = otel_instrument:identity(Instrument),
    case otel_metrics_tables:insert_instrument(InstrumentsTab, Meter, Instrument) of
        true ->
            NewStreams = update_streams_(Instrument, CallbacksTab, StreamsTab, Views, Readers, ExemplarsEnabled, ExemplarFilter),
            maybe_log_stream_conflicts(StreamsTab, NewStreams),
            Instrument;
        false ->
            Existing = otel_metrics_tables:lookup_instrument_by_identity(InstrumentsTab,
                                                                         Meter,
                                                                         Identity),
            maybe_log_name_conflict(Existing, Instrument),
            maybe_log_advisory_conflict(Existing, Instrument),
            ?LOG_DEBUG("Instrument ~p already created; returning the canonical Instrument.", [Name]),
            Existing
    end.

maybe_log_name_conflict(#instrument{name=Name}, #instrument{name=Name}) ->
    ok;
maybe_log_name_conflict(#instrument{name=ExistingName}, #instrument{name=Name}) ->
    ?LOG_WARNING(
       "Instrument name ~p conflicts case-insensitively with previously registered name ~p; "
       "returning the first-created Instrument.",
       [Name, ExistingName]).

maybe_log_advisory_conflict(#instrument{advisory_params=AdvisoryParams},
                            #instrument{advisory_params=AdvisoryParams}) ->
    ok;
maybe_log_advisory_conflict(#instrument{name=Name}, _) ->
    ?LOG_WARNING("Identical Instrument ~p was registered with different advisory parameters; using the first-seen parameters.", [Name]).

maybe_log_stream_conflicts(StreamsTab, UpdatedStreams) ->
    case unique_active_streams(UpdatedStreams) of
        [] ->
            ok;
        Candidates ->
            StreamsByKey = index_streams(otel_metrics_tables:list_streams(StreamsTab)),
            Conflicts = lists:usort(
                          lists:flatmap(
                            fun(Stream) ->
                                    conflicts_with(
                                      Stream,
                                      maps:get(stream_key(Stream), StreamsByKey, []))
                            end,
                            Candidates)),
            log_stream_conflicts(Conflicts)
    end.

log_stream_conflicts(Conflicts) ->
    lists:foreach(
      fun({Name, Scope, DefinitionA, DefinitionB}) ->
              ?LOG_WARNING(
                 "Conflicting metric streams remain after applying Views; both streams remain active. "
                 "Configure a View to rename one stream or align its identifying fields. "
                 "name=~p scope=~p stream_definitions=~p",
                 [Name, Scope, [DefinitionA, DefinitionB]])
      end,
      Conflicts).

index_streams(Streams) ->
    lists:foldl(
      fun(#stream{aggregation_module=otel_aggregation_drop}, Acc) ->
              Acc;
         (Stream, Acc) ->
              Key = stream_key(Stream),
              maps:update_with(Key, fun(KeyStreams) -> [Stream | KeyStreams] end,
                               [Stream], Acc)
      end,
      #{},
      Streams).

unique_active_streams(Streams) ->
    maps:values(
      lists:foldl(
        fun(#stream{aggregation_module=otel_aggregation_drop}, Acc) ->
                Acc;
           (Stream=#stream{id=Id}, Acc) ->
                maps:put(Id, Stream, Acc)
        end,
        #{},
        Streams)).

stream_key(#stream{reader=Reader, scope=Scope, name=Name}) ->
    {Reader, Scope, normalize_stream_name(Name)}.

conflicts_with(StreamA=#stream{id=Id,
                               scope=Scope,
                               name=NameA},
               Streams) ->
    NormalizedName = normalize_stream_name(NameA),
    DefinitionA = stream_definition(StreamA),
    [begin
         DefinitionB = stream_definition(StreamB),
         {NormalizedDefinitionA, NormalizedDefinitionB} =
             ordered_pair(DefinitionA, DefinitionB),
         {NormalizedName, Scope, NormalizedDefinitionA, NormalizedDefinitionB}
     end || StreamB=#stream{id=OtherId} <- Streams,
            OtherId =/= Id].

stream_definition(#stream{aggregation_module=otel_aggregation_sum,
                          instrument=#instrument{unit=Unit},
                          description=Description,
                          temporality=Temporality,
                          is_monotonic=IsMonotonic}) ->
    #{point_type => sum,
      unit => normalize_stream_optional(Unit),
      description => normalize_stream_optional(Description),
      temporality => Temporality,
      monotonic => IsMonotonic};
stream_definition(#stream{aggregation_module=otel_aggregation_last_value,
                          instrument=#instrument{unit=Unit},
                          description=Description}) ->
    #{point_type => gauge,
      unit => normalize_stream_optional(Unit),
      description => normalize_stream_optional(Description)};
stream_definition(#stream{aggregation_module=otel_aggregation_histogram_explicit,
                          instrument=#instrument{unit=Unit},
                          description=Description,
                          temporality=Temporality}) ->
    #{point_type => histogram,
      unit => normalize_stream_optional(Unit),
      description => normalize_stream_optional(Description),
      temporality => Temporality};
stream_definition(#stream{aggregation_module=AggregationModule,
                          instrument=#instrument{unit=Unit},
                          description=Description,
                          temporality=Temporality,
                          is_monotonic=IsMonotonic}) ->
    #{point_type => AggregationModule,
      unit => normalize_stream_optional(Unit),
      description => normalize_stream_optional(Description),
      temporality => Temporality,
      monotonic => IsMonotonic}.

ordered_pair(A, B) when A =< B ->
    {A, B};
ordered_pair(A, B) ->
    {B, A}.

normalize_stream_name(Value) ->
    otel_instrument:normalized_name(Value).

normalize_stream_optional(undefined) ->
    <<>>;
normalize_stream_optional(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
normalize_stream_optional(Value) ->
    Value.

%% used when a new View is added and the Views must be re-matched with each Instrument
update_streams(InstrumentsTab, CallbacksTab, StreamsTab, Views, Readers, ExemplarsEnabled, ExemplarFilter) ->
    otel_metrics_tables:fold_instruments(
      InstrumentsTab,
      fun(Instrument, Streams) ->
              update_streams_(Instrument,
                              CallbacksTab,
                              StreamsTab,
                              Views,
                              Readers,
                              ExemplarsEnabled,
                              ExemplarFilter) ++ Streams
      end,
      []).

update_streams_(Instrument=#instrument{meter={_, #meter{}},
                                       name=_Name}, CallbacksTab, StreamsTab, Views, Readers, ExemplarsEnabled, ExemplarFilter) ->
    ViewMatches = otel_view:match_instrument_to_views(Instrument, Views, ExemplarsEnabled, ExemplarFilter),
    lists:flatmap(fun(Reader=#reader{id=ReaderId}) ->
                          Matches = per_reader_aggregations(Reader, Instrument, ViewMatches),
                          NewStreams = otel_metrics_tables:replace_streams(StreamsTab,
                                                                          Instrument,
                                                                          ReaderId,
                                                                          Matches),
                          case {Instrument#instrument.callback, Instrument#instrument.callback_args} of
                              {undefined, _} ->
                                  ok;
                              {Callback, CallbackArgs} ->
                                  otel_metrics_tables:insert_callback(CallbacksTab, ReaderId, Callback, CallbackArgs, Instrument)
                          end,
                          NewStreams
                  end, Readers).

%% Match the Instrument to views and then store a per-Reader aggregation for the View
register_callback_(CallbacksTab, Instruments, Callback, CallbackArgs, Readers) ->
    lists:map(fun(#reader{id=ReaderId}) ->
                      otel_metrics_tables:insert_callback(CallbacksTab, ReaderId, Callback, CallbackArgs, Instruments)
              end, Readers).

metric_reader(ReaderId, ReaderPid, DefaultAggregationMapping, Temporality) ->
    %% TODO: Uncomment when we can drop OTP-23 support
    %% Ref = erlang:monitor(process, ReaderPid, [{tag, 'DOWN_READER'}]),
    Ref = erlang:monitor(process, ReaderPid),

    ReaderAggregationMapping = maps:merge(otel_aggregation:default_mapping(),
                                          DefaultAggregationMapping),

    #reader{id=ReaderId,
            pid=ReaderPid,
            monitor_ref=Ref,
            default_aggregation_mapping=ReaderAggregationMapping,
            default_temporality_mapping=Temporality}.


%% a Measurement's Instrument is matched against Views
%% each matched View+Reader becomes a Stream
%% for each Stream a Measurement updates a Metric (`#metric')
%% active metrics are indexed by the Stream name + the Measurement's Attributes

handle_measurement(Ctx, Instrument=#instrument{meter={_, Meter=#meter{streams_tab=StreamsTab}}}, Number, Attributes) ->
    Streams = otel_metrics_tables:match_streams(StreamsTab, Instrument),
    update_aggregations(Ctx, Meter, Number, Attributes, Streams).

update_aggregations(Ctx, Meter, Value, Attributes, Streams) ->
    lists:foreach(fun(Stream=#stream{instrument=Instrument}) ->
                        maybe_init_aggregate(Ctx, Meter, Value, Instrument, Stream, Attributes);
                     (_) ->
                          ok
                  end, Streams).

maybe_init_aggregate(_, _Meter, Value, #instrument{kind=Kind} = Instrument, _Stream, _Attributes)
        when Value < 0, Kind == ?KIND_COUNTER orelse Kind == ?KIND_HISTOGRAM ->
    ?LOG_INFO("Discarding negative value for instrument ~s of type ~s", [Instrument#instrument.name, Kind]),
    ok;

maybe_init_aggregate(Ctx, #meter{metrics_tab=MetricsTab,
                                 exemplars_tab=ExemplarsTab}, Value, _Instrument, Stream, Attributes) ->
    otel_aggregation:maybe_init_aggregate(Ctx, MetricsTab, ExemplarsTab, Stream, Value, Attributes).

%% create an aggregation for each Reader and its possibly unique aggregation/temporality
per_reader_aggregations(Reader, Instrument, Streams) ->
    [stream_for_reader(Instrument, Stream, View, Reader)
     || {View, Stream} <- Streams].

stream_for_reader(Instrument=#instrument{kind=Kind}, Stream, View=#view{attribute_keys=AttributeKeys},
                  Reader=#reader{id=Id,
                                 default_temporality_mapping=ReaderTemporalityMapping}) ->
    AggregationModule = aggregation_module(Instrument, View, Reader),
    Temporality = maps:get(Kind, ReaderTemporalityMapping, ?TEMPORALITY_CUMULATIVE),

    Forget = do_forget(Kind, Temporality),

    Stream#stream{
      id=make_ref(),
      reader=Id,
      attribute_keys=AttributeKeys,
      aggregation_module=AggregationModule,
      forget=Forget,
      temporality=Temporality};
stream_for_reader(Instrument=#instrument{kind=Kind}, Stream, View,
                            Reader=#reader{id=Id,
                                           default_temporality_mapping=ReaderTemporalityMapping}) ->
    AggregationModule = aggregation_module(Instrument, View, Reader),
    Temporality = maps:get(Kind, ReaderTemporalityMapping, ?TEMPORALITY_CUMULATIVE),

    Forget = do_forget(Kind, Temporality),

    Stream#stream{
      id=make_ref(),
      reader=Id,
      attribute_keys=undefined,
      aggregation_module=AggregationModule,
      forget=Forget,
      temporality=Temporality}.


%% no aggregation defined for the View, so get the aggregation from the Reader
%% the Reader's mapping of Instrument Kind to Aggregation was merged with the
%% global default, so any missing Kind entries are filled in from the global
%% mapping in `otel_aggregation'
-spec aggregation_module(otel_instrument:t(), otel_view:t() | undefined, reader()) -> module().
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

do_forget(_, ?TEMPORALITY_DELTA) ->
    true;
do_forget(?KIND_OBSERVABLE_COUNTER, _) ->
    true;
do_forget(?KIND_OBSERVABLE_GAUGE, _) ->
    true;
do_forget(?KIND_OBSERVABLE_UPDOWNCOUNTER, _) ->
    true;
do_forget(_, _) ->
    false.
