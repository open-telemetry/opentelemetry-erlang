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
         match_instrument_to_views/4]).

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
          aggregation_module=maps:get(aggregation, Config, undefined)}.

-spec new(name(), criteria(), config()) -> t().
new(Name, Criteria, Config) ->
    View = new(Criteria, Config),
    View#view{name=Name}.

-spec match_instrument_to_views(otel_instrument:t(), opentelemetry:attributes_map(), [t()], #{}) -> [#view_aggregation{}].
match_instrument_to_views(Instrument=#instrument{name=Name,
                                                 description=Description}, Attributes, Views, DefaultAggregationMappings) ->
    Aggs =
        lists:filtermap(fun(View=#view{name=ViewName,
                                       description=ViewDescription,
                                       selection=#selection{instrument_name=InstrumentName}})
                              when Name =:= InstrumentName ->
                                %% create an aggregation for each Reader mapping of aggregation if
                                %% the view does not define an aggregation
                                AggregationModules = aggregation_modules(Instrument, View, DefaultAggregationMappings),
                                {true, [#view_aggregation{name=case ViewName of
                                                                   undefined ->
                                                                       InstrumentName;
                                                                   _ ->
                                                                       ViewName
                                                               end,
                                                          view=View#view{aggregation_module=AggregationModule},
                                                          instrument=Instrument,
                                                          description=case ViewDescription of
                                                                          undefined ->
                                                                              Description;
                                                                          _ ->
                                                                              ViewDescription
                                                                      end,
                                                          attributes_aggregation=#{Attributes => AggregationModule:new(Instrument, Attributes, erlang:system_time(nanosecond), #{})}
                                                         } || AggregationModule <- AggregationModules]};
                           (_) ->
                                false
                        end, Views),
    lists:flatten(Aggs).

%%

aggregation_modules(#instrument{kind=Kind}, #view{aggregation_module=undefined}, DefaultAggregationMappings) ->
    %% will crash if the Reader's aggregation mapping doesn't have a match for the Instrument Kind
    [maps:get(Kind, Mapping) || Mapping <- DefaultAggregationMappings];
aggregation_modules(_Instrument, #view{aggregation_module=Module}, _DefaultAggregationMapping) ->
    [Module].

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
