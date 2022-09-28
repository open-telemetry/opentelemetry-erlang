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

-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").

-type name() :: atom().

-type criteria() :: #{instrument_name => otel_instrument:name(),
                      instrument_kind => otel_instrument:kind(),
                      meter_name => otel_meter:name(),
                      meter_version => otel_meter:version(),
                      meter_schema_url => otel_meter:schema_url()}.
-type config() :: #{description => unicode:unicode_binary(),
                    attribute_keys => [atom()],
                    aggregation => module() | default
                    %% exemplar_reservoir
                   }.

-type t() :: #view{}.
-type selection() :: #selection{}.

-export_type([t/0,
              name/0,
              selection/0,
              criteria/0,
              config/0]).

%% no name means Instrument name is used
%% must reject wildcard Criteria  in this case
-spec new(criteria(), config()) -> t().
new(Criteria, Config) ->
    Selection = criteria_to_selection(Criteria),
    %% no name given so use the name of the instrument in the selection
    %% if no instrument name is given then it'll stay `undefined'
    #view{name=Selection#selection.instrument_name,
          selection=Selection,
          description=maps:get(description, Config, undefined),
          attribute_keys=maps:get(attribute_keys, Config, undefined),
          aggregation_module=maps:get(aggregation, Config, undefined),
          aggregation_options=#{}}.

-spec new(name(), criteria(), config()) -> t().
new(Name, Criteria, Config) ->
    View = new(Criteria, Config),
    View#view{name=Name}.

-spec match_instrument_to_views(otel_instrument:t(), opentelemetry:attributes_map()) ->
          [{otel_view:t(), #view_aggregation{}}].
match_instrument_to_views(Instrument=#instrument{name=Name,
                                                 meter=Meter,
                                                 description=Description}, Views) ->
    IsMonotonic = otel_instrument:is_monotonic(Instrument),
    Temporality = otel_aggregation:instrument_temporality(Instrument),
    Scope = otel_meter:scope(Meter),
    case lists:filtermap(fun(View=#view{name=ViewName,
                                        description=ViewDescription,
                                        aggregation_options=AggregationOptions,
                                        selection=#selection{instrument_name=InstrumentName}})
                               when Name =:= InstrumentName ->
                                 {true, {View, #view_aggregation{name=value_or(ViewName,
                                                                               InstrumentName),
                                                                 scope=Scope,
                                                                 instrument=Instrument,
                                                                 temporality=Temporality,
                                                                 is_monotonic=IsMonotonic,
                                                                 aggregation_options=AggregationOptions,
                                                                 description=value_or(ViewDescription,
                                                                                      Description)
                                                                }}};
                            (_) ->
                                 false
                         end, Views) of
        [] ->
            [{#view{}, #view_aggregation{name=Name,
                                         scope=Scope,
                                         instrument=Instrument,
                                         temporality=Temporality,
                                         is_monotonic=IsMonotonic,
                                         aggregation_options=[],
                                         description=Description}}];
        Aggs ->
            Aggs
    end.

%%

value_or(undefined, Other) ->
    Other;
value_or(Value, _Other) ->
    Value.

criteria_to_selection(Criteria) ->
    maps:fold(fun(instrument_name, InstrumentName, SelectionAcc) ->
                      SelectionAcc#selection{instrument_name=InstrumentName};
                 (instrument_kind, Kind, SelectionAcc) ->
                      SelectionAcc#selection{instrument_kind=Kind};
                 (meter_name, MeterName, SelectionAcc) ->
                      SelectionAcc#selection{meter_name=MeterName};
                 (meter_version, MeterVersion, SelectionAcc) ->
                      SelectionAcc#selection{meter_version=MeterVersion};
                 (meter_schema_url, SchemaUrl, SelectionAcc) ->
                      SelectionAcc#selection{schema_url=SchemaUrl}
              end, #selection{}, Criteria).
