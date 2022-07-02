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

-module(otel_metric_exporter_pid).

-export([init/1,
         export/2,
         force_flush/0,
         shutdown/0]).

-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").
-include("otel_metrics.hrl").

init(Pid) ->
    {ok, Pid}.

export(Table, Pid) ->
    ets:foldl(fun({Instrument, ViewAggregations}, Acc) ->
                  send_metrics(Pid, Instrument, ViewAggregations),
                  Acc
              end, ok, Table),
    ok.

force_flush() ->
    ok.

shutdown() ->
    ok.

%%

send_metrics(Pid, #instrument{value_type=_ValueType,
                              unit=Unit}, ViewAggregations) ->
    lists:foreach(fun(#view_aggregation{name=Name,
                                        view=#view{aggregation_module=AggregationModule},
                                        description=Description,
                                        attributes_aggregation=AttributesAggregation}) ->
                          Data = data(AggregationModule, AttributesAggregation),

                          Pid ! metric(Name, Description, Unit, Data)
                  end, ViewAggregations).

metric(Name, Description, Unit, Data) ->
    #metric{name=Name,
            description=Description,
            unit=Unit,
            data=Data}.

data(otel_aggregation_sum, AttributesAggregation) ->
    Datapoints = maps:fold(fun(Attributes, Aggregation, Acc) ->
                                   [datapoint(Aggregation, Attributes) | Acc]
                           end, [], AttributesAggregation),
    Temporality=temporality,
    IsMonotonic=is_monotonic,
    #sum{
       aggregation_temporality=Temporality,
       is_monotonic=IsMonotonic,
       datapoints=Datapoints};
data(otel_aggregation_histogram_explicit, AttributesAggregation) ->
    Datapoints = maps:fold(fun(Attributes, Aggregation, Acc) ->
                                   [datapoint(Aggregation, Attributes) | Acc]
                           end, [], AttributesAggregation),
    Temporality=temporality,
    #histogram{datapoints=Datapoints,
               aggregation_temporality=Temporality
              }.

datapoint(#sum_aggregation{value=Value,
                      %% instrument_is_monotonic=IsMonotonic,
                      %% instrument_temporality=Temporality,
                      start_time_unix_nano=StartTimeUnixNano}, Attributes) ->
    Flags = 0,
    Exemplars = [],
    #datapoint{
       value=Value,
       attributes=Attributes,
       start_time_unix_nano=StartTimeUnixNano,
       %% time_unix_nano=Time,
       exemplars=Exemplars,
       flags=Flags};
datapoint(#last_value_aggregation{attributes=Attributes,
                             start_time_unix_nano=StartTimeUnixNano,
                             value=Value}, Attributes) ->
    Flags = 0,
    Exemplars = [],
    #gauge{datapoints=[#datapoint{
                          value=Value,
                          attributes=Attributes,
                          start_time_unix_nano=StartTimeUnixNano,
                          %% time_unix_nano=Time,
                          exemplars=Exemplars,
                          flags=Flags}]};
datapoint(#explicit_histogram_aggregation
        {
         attributes=Attributes,
         start_time_unix_nano=StartTimeUnixNano,
         boundaries=Boundaries,
         bucket_counts=Buckets,
         record_min_max=RecordMinMax,
         min=Min,
         max=Max,
         sum=Sum,
         instrument_temporality=Temporality
        }, Attributes) ->
    Flags = 0,
    Exemplars = [],
    #histogram_datapoint
                   {
                     attributes=Attributes,
                     start_time_unix_nano=StartTimeUnixNano,
                     time_unix_nano=0,
                     count=0,
                     sum=Sum,
                     bucket_counts=Buckets,
                     explicit_bounds=Boundaries,
                     exemplars=Exemplars,
                     flags=Flags,
                     min=Min,
                     max=Max
                   }.
