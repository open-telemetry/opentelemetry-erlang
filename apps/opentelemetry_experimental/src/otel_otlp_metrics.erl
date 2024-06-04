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

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_metric_exemplar.hrl").
-include("otel_metrics.hrl").

-spec to_proto([#metric{}], otel_resource:t()) -> opentelemetry_exporter_metrics_service_pb:export_metrics_service_request() | empty.
to_proto([], _) ->
    empty;
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
    Metric =
        #{name => otel_otlp_common:to_binary(Name),
          description => Description,
          data => to_data(Data)},
    case Unit of
        undefined -> Metric;
        _ -> Metric#{unit => otel_otlp_common:to_binary(Unit)}
    end.

to_data(#sum{aggregation_temporality=Temporality,
             is_monotonic=IsMonotonic,
             datapoints=Datapoints}) ->
    {sum, #{data_points => [to_data_points(Datapoint) || Datapoint <- Datapoints],
            aggregation_temporality => to_otlp_temporality(Temporality),
            is_monotonic => IsMonotonic}};
to_data(#gauge{datapoints=Datapoints}) ->
    {gauge, #{data_points => [to_data_points(Datapoint) || Datapoint <- Datapoints]}};
to_data(#histogram{datapoints=Datapoints,
                   aggregation_temporality=Temporality
                  }) ->
    {histogram, #{data_points => [to_histogram_data_points(Datapoint) || Datapoint <- Datapoints],
                  aggregation_temporality => to_otlp_temporality(Temporality)}}.

to_data_points(#datapoint{attributes=Attributes,
                          start_time=StartTime,
                          time=CollectionStartTime,
                          value=Value,
                          exemplars=Exemplars,
                          flags=Flags
                         }) ->

    #{attributes => otel_otlp_common:to_attributes(Attributes),
      start_time_unix_nano => opentelemetry:timestamp_to_nano(StartTime),
      time_unix_nano => opentelemetry:timestamp_to_nano(CollectionStartTime),
      value => to_datapoint_value(Value),
      exemplars => to_exemplars(Exemplars),
      flags => Flags
     }.

to_histogram_data_points(#histogram_datapoint{
                            attributes=Attributes,
                            start_time=StartTime,
                            time=CollectionStartTime,
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
      %% eqwalizer:ignore start_time in histogram_datapoint has to support matchspec entries
      start_time_unix_nano => opentelemetry:timestamp_to_nano(StartTime),
      time_unix_nano => opentelemetry:timestamp_to_nano(CollectionStartTime),
      count => Count,
      sum => Sum,
      bucket_counts => Buckets,
      explicit_bounds => Boundaries,
      exemplars => to_exemplars(Exemplars),
      flags => Flags,
      min => Min,
      max => Max
     }.

%%

to_datapoint_value(Value) when is_integer(Value) ->
    {as_int, Value};
to_datapoint_value(Value) when is_float(Value) ->
    {as_double, Value}.

to_otlp_temporality(?TEMPORALITY_DELTA) ->
    'AGGREGATION_TEMPORALITY_DELTA';
to_otlp_temporality(?TEMPORALITY_CUMULATIVE) ->
    'AGGREGATION_TEMPORALITY_CUMULATIVE'.

to_exemplars(Exemplars) ->
    [to_exemplar(Exemplar) || Exemplar <- Exemplars].

to_exemplar(#exemplar{value=Value,
                      time=Time,
                      filtered_attributes=FilteredAttributes,
                      span_id=undefined,
                      trace_id=undefined}) ->
    #{filtered_attributes => otel_otlp_common:to_attributes(FilteredAttributes),
      time_unix_nano      => opentelemetry:timestamp_to_nano(Time),
      value               => value_to_int_or_double(Value)
     };
to_exemplar(#exemplar{value=Value,
                      time=Time,
                      filtered_attributes=FilteredAttributes,
                      span_id=SpanId,
                      trace_id=TraceId}) when SpanId =/= undefined andalso
                                              TraceId =/= undefined  ->
    #{filtered_attributes => otel_otlp_common:to_attributes(FilteredAttributes),
      time_unix_nano      => opentelemetry:timestamp_to_nano(Time),
      value               => value_to_int_or_double(Value),
      span_id             => <<SpanId:64>>,
      trace_id            => <<TraceId:128>>
     }.

value_to_int_or_double(Value) when is_float(Value) ->
    {as_double, Value};
value_to_int_or_double(Value) when is_integer(Value) ->
    {as_int, Value}.
