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
%% @doc
%% @end
%%%-------------------------------------------------------------------------
-module(otel_view).

-export([new/2,
         new/3,
         match_instrument_to_views/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_metrics.hrl").
-include("otel_view.hrl").

-type name() :: atom().

-type criteria() :: #{instrument_name => otel_instrument:name(),
                      instrument_kind => otel_instrument:kind(),
                      instrument_unit => otel_instrument:unit(),
                      meter_name => unicode:unicode_binary() | undefined,
                      meter_version => unicode:unicode_binary() | undefined,
                      meter_schema_url => unicode:unicode_binary() | undefined}.
-type config() :: #{description => unicode:unicode_binary() | undefined,
                    attribute_keys => [atom()] | undefined,
                    aggregation_module => module() | default | undefined,
                    aggregation_options => map()
                    %% exemplar_reservoir
                   }.

-type t() :: #view{}.

-export_type([t/0,
              name/0,
              criteria/0,
              config/0]).

-include_lib("opentelemetry_api/include/gradualizer.hrl").

%% ignore dialyzer warnings in functions using matchspecs or related to those that do
-dialyzer({nowarn_function, do_new/2}).
-dialyzer({nowarn_function, criteria_to_instrument_matchspec/1}).
-dialyzer({nowarn_function, maybe_init_meter/1}).
-dialyzer({nowarn_function, update_meter_name/2}).
-dialyzer({nowarn_function, update_meter_version/2}).
-dialyzer({nowarn_function, update_meter_schema_url/2}).

-spec new(criteria() | undefined, config()) -> {ok, t()} | {error, named_wildcard_view}.
new(Criteria, Config) ->
    new(undefined, Criteria, Config).

-spec new(name(), criteria() | undefined, config()) -> {ok, t()} | {error, named_wildcard_view}.
new(undefined, Criteria, Config) ->
    {ok, do_new(Criteria, Config)};
new(Name, #{instrument_name := '*'}, _Config) ->
    ?LOG_INFO("Wildacrd Views can not have a name, discarding view ~s", [Name]),
    {error, named_wildcard_view};
new(Name, Criteria, Config) ->
    View = do_new(Criteria, Config),
    {ok, View#view{name=Name}}.

%% no name means Instrument name is used
%% must reject wildcard Criteria  in this case
do_new(Criteria, Config) ->
    CriteriaInstrumentName = view_name_from_criteria(Criteria),
    Matchspec = criteria_to_instrument_matchspec(Criteria),
    %% no name given so use the name of the instrument in the selection
    %% if no instrument name is given then it'll stay `undefined'
    #view{name=CriteriaInstrumentName,
          instrument_matchspec=Matchspec,
          description=maps:get(description, Config, undefined),
          attribute_keys=maps:get(attribute_keys, Config, undefined),
          aggregation_module=maps:get(aggregation_module, Config, undefined),
          aggregation_options=maps:get(aggregation_options, Config, #{})}.

-spec match_instrument_to_views(otel_instrument:t(), [t()]) -> [{t() | undefined, #stream{}}].
match_instrument_to_views(Instrument=#instrument{name=InstrumentName,
                                                 meter=Meter,
                                                 kind=Kind,
                                                 description=Description,
                                                 advisory_params=AdvisoryParams}, Views) ->
    IsMonotonic = otel_instrument:is_monotonic(Instrument),
    Temporality = otel_instrument:temporality(Instrument),
    Scope = otel_meter:scope(Meter),
    {ExemplarReservoirModule, ExemplarReservoirConfig} = exemplar_reservoir(Kind),
    ExemplarReservoir = otel_metric_exemplar_reservoir:new(ExemplarReservoirModule, ExemplarReservoirConfig),
    case lists:filtermap(fun(View=#view{name=ViewName,
                                        description=ViewDescription,
                                        attribute_keys=AttributeKeys,
                                        aggregation_options=AggregationOptions,
                                        instrument_matchspec=Matchspec}) ->
                                 case ets:match_spec_run([Instrument], Matchspec) of
                                     [] ->
                                         false;
                                     _ ->
                                         AggregationOptions1 = aggregation_options(AggregationOptions, AdvisoryParams),
                                         {true, {View, #stream{name=value_or(ViewName,
                                                                             InstrumentName),
                                                               scope=Scope,
                                                               instrument=Instrument,
                                                               temporality=Temporality,
                                                               is_monotonic=IsMonotonic,
                                                               attribute_keys=AttributeKeys,
                                                               aggregation_options=AggregationOptions1,
                                                               description=value_or(ViewDescription,
                                                                                    Description),
                                                               exemplar_resevoir=ExemplarReservoir
                                                              }}}
                                 end
                         end, Views) of
        [] ->
            AggregationOptions1 = aggregation_options(#{}, AdvisoryParams),
            [{undefined, #stream{name=InstrumentName,
                                 scope=Scope,
                                 instrument=Instrument,
                                 temporality=Temporality,
                                 is_monotonic=IsMonotonic,
                                 attribute_keys=undefined,
                                 aggregation_options=AggregationOptions1,
                                 description=Description,
                                 exemplar_resevoir=ExemplarReservoir}}];
        Aggs ->
            Aggs
    end.

%%

aggregation_options(#{explicit_bucket_boundaries := _} = AggregationOptions, _AdvisoryParams) ->
    AggregationOptions;
aggregation_options(AggregationOptions, #{explicit_bucket_boundaries := Boundaries}) ->
    maps:put(explicit_bucket_boundaries, Boundaries, AggregationOptions);
aggregation_options(AggregationOptions, _AdvisoryParams) ->
    AggregationOptions.

value_or(undefined, Other) ->
    Other;
value_or(Value, _Other) ->
    Value.

-spec criteria_to_instrument_matchspec(map() | undefined) -> ets:comp_match_spec().
criteria_to_instrument_matchspec(Criteria) when is_map(Criteria) ->
    Instrument =
        maps:fold(fun(instrument_name, '*', InstrumentAcc) ->
                          InstrumentAcc;
                     (instrument_name, InstrumentName, InstrumentAcc) ->
                          InstrumentAcc#instrument{name=InstrumentName};
                     (instrument_kind, Kind, InstrumentAcc) ->
                          InstrumentAcc#instrument{kind=Kind};
                     (instrument_unit, Unit, InstrumentAcc) ->
                          InstrumentAcc#instrument{unit=Unit};
                     (meter_name, MeterName, InstrumentAcc) ->
                          Meter = maybe_init_meter(InstrumentAcc),
                          Meter1 = update_meter_name(MeterName, Meter),
                          InstrumentAcc#instrument{meter=Meter1};
                     (meter_version, MeterVersion, InstrumentAcc) ->
                          Meter = maybe_init_meter(InstrumentAcc),
                          Meter1 = update_meter_version(MeterVersion, Meter),
                          InstrumentAcc#instrument{meter=Meter1};
                     (meter_schema_url, SchemaUrl, InstrumentAcc) ->
                          Meter = maybe_init_meter(InstrumentAcc),
                          Meter1 = update_meter_schema_url(SchemaUrl, Meter),
                          InstrumentAcc#instrument{meter=Meter1}
                          %% eqwalizer:ignore using ignore as an ets matchspec workaround
                  end, #instrument{_='_'}, Criteria),
    ets:match_spec_compile([{Instrument, [], [true]}]);
criteria_to_instrument_matchspec(_) ->
    %% eqwalizer:ignore using ignore as an ets matchspec workaround
    ets:match_spec_compile([{#instrument{_='_'}, [], [true]}]).

maybe_init_meter(#instrument{meter='_'}) ->
    {'_', #meter{instrumentation_scope=#instrumentation_scope{_='_'},
                 _='_'}}.

update_meter_name(MeterName, {_, Meter=#meter{instrumentation_scope=Scope}}) ->
    {'_', Meter#meter{instrumentation_scope=Scope#instrumentation_scope{name=MeterName}}}.

update_meter_version(MeterVersion, {_, Meter=#meter{instrumentation_scope=Scope}}) ->
    {'_', Meter#meter{instrumentation_scope=Scope#instrumentation_scope{version=MeterVersion}}}.

update_meter_schema_url(SchemaUrl, {_, Meter=#meter{instrumentation_scope=Scope}}) ->
    {'_', Meter#meter{instrumentation_scope=Scope#instrumentation_scope{schema_url=SchemaUrl}}}.

view_name_from_criteria(Criteria) when is_map(Criteria) ->
    case maps:get(instrument_name, Criteria, undefined) of
        '*' -> undefined;
        Name -> Name
    end;
view_name_from_criteria(_) ->
    undefined.

exemplar_reservoir(Kind) when Kind =:= ?KIND_COUNTER
                              ; Kind =:= ?KIND_OBSERVABLE_COUNTER
                              ; Kind =:= ?KIND_UPDOWN_COUNTER
                              ; Kind =:= ?KIND_OBSERVABLE_UPDOWNCOUNTER
                              ; Kind =:= ?KIND_OBSERVABLE_GAUGE ->
    {otel_metric_exemplar_reservoir_simple, #{}};
exemplar_reservoir(_Kind) ->
    {otel_metric_exemplar_reservoir_drop, #{}}.
