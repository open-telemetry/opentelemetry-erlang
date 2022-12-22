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
-module(otel_otlp_metrics).

-export([to_proto/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry_experimental/include/otel_metrics.hrl").

to_proto(Metrics, Resource) ->
    InstrumentationScopeMetrics = to_proto_by_scope(Metrics),
    Attributes = otel_resource:attributes(Resource),
    ResourceMetrics = #{resource => #{attributes => otel_otlp_common:to_attributes(Attributes),
                                      dropped_attributes_count => otel_attributes:dropped(Attributes)},
                        scope_metrics => InstrumentationScopeMetrics},
    case otel_resource:schema_url(Resource) of
        undefined ->
            #{resource_metrics => [ResourceMetrics]};
        SchemaUrl ->
            #{resource_metrics => [ResourceMetrics#{schema_url => SchemaUrl}]}
    end.

to_proto_by_scope(Metrics) ->
    ByScopes = lists:foldl(fun(Metric=#metric{scope=Scope}, Acc) ->
                                   Proto = to_proto(Metric),
                                   maps:update_with(Scope, fun(List) -> [Proto | List] end, [Proto], Acc)
                           end, #{}, Metrics),
    maps:fold(fun(Scope, MetricsProto, Acc) ->
                      [case Scope#instrumentation_scope.schema_url of
                           undefined ->
                               #{scope => otel_otlp_common:to_instrumentation_scope_proto(Scope),
                                 metrics => MetricsProto};
                           SchemaUrl ->
                               #{scope => otel_otlp_common:to_instrumentation_scope_proto(Scope),
                                 metrics => MetricsProto,
                                 schema_url => SchemaUrl}
                       end | Acc]
              end, [], ByScopes).

to_proto(#metric{name=Name,
                 description=Description,
                 unit=Unit,
                 data=Data}) ->
    #{name => Name,
      description => Description,
      unit => otel_otlp_common:to_binary(Unit),
      data => to_data(Data)}.

to_data(#sum{aggregation_temporality=Temporality,
             is_monotonic=IsMonotonic,
             datapoints=Datapoints}) ->
    {sum, #{data_points => [to_data_points(Datapoint) || Datapoint <- Datapoints],
            aggregation_temporality => Temporality,
            is_monotonic => IsMonotonic}};
to_data(#gauge{datapoints=Datapoints}) ->
    {gauge, #{data_points => [to_data_points(Datapoint) || Datapoint <- Datapoints]}};
to_data(#histogram{datapoints=Datapoints,
                   aggregation_temporality=Temporality
                  }) ->
    {histogram, #{data_points => [to_histogram_data_points(Datapoint) || Datapoint <- Datapoints],
                  aggregation_temporality => Temporality}}.

to_data_points(#datapoint{attributes=Attributes,
                          start_time_unix_nano=StartTimeUnixNano,
                          time_unix_nano=CollectionStartNano,
                          value=Value,
                          exemplars=Exemplars,
                          flags=Flags
                         }) ->
    #{attributes => otel_otlp_common:to_attributes(Attributes),
      start_time_unix_nano => opentelemetry:timestamp_to_nano(StartTimeUnixNano),
      time_unix_nano => opentelemetry:timestamp_to_nano(CollectionStartNano),
      value => to_datapoint_value(Value),
      exemplars => Exemplars,
      flags => Flags
     }.

to_histogram_data_points(#histogram_datapoint{
                            attributes=Attributes,
                            start_time_unix_nano=StartTimeUnixNano,
                            time_unix_nano=CollectionStartNano,
                            count=Count,
                            sum=Sum,
                            bucket_counts=Buckets,
                            explicit_bounds=Boundaries,
                            exemplars=Exemplars,
                            flags=Flags,
                            min=Min,
                            max=Max
                           }) ->
    #{attributes => otel_otlp_common:to_attributes(Attributes),
      start_time_unix_nano => StartTimeUnixNano,
      time_unix_nano => CollectionStartNano,
      count => Count,
      sum => Sum,
      bucket_counts => Buckets,
      explicit_bounds => Boundaries,
      exemplars => Exemplars,
      flags => Flags,
      min => Min,
      max => Max
     }.

%%

to_datapoint_value(Value) when is_integer(Value) ->
    {as_int, Value};
to_datapoint_value(Value) when is_float(Value) ->
    {as_double, Value}.
