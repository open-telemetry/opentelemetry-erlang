%%%------------------------------------------------------------------------
%% Copyright 2024, OpenTelemetry Authors
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
-module(otel_metric_exemplar).

-export([new/5,
         reservoir/3]).

-include("otel_metric_exemplar.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").

-type exemplar() :: #exemplar{}.

-export_type([exemplar/0]).

-spec new(number(), opentelemetry:timestamp(), opentelemetry:attributes_map(), opentelemetry:trace_id() | undefined, opentelemetry:span_id() | undefined) -> exemplar().
new(Value, Time, FilteredAttributes, TraceId, SpanId) ->
    #exemplar{value=Value,
              time=Time,
              filtered_attributes=FilteredAttributes,
              span_id=SpanId,
              trace_id=TraceId}.


reservoir(Kind, ExemplarsEnabled, ExemplarFilter) when ExemplarFilter =:= always_off ;
                                                    ExemplarsEnabled =:= false ->
    reservoir(Kind, fun otel_metric_exemplar_filter:always_off/6);
reservoir(Kind, _, always_on) ->
    reservoir(Kind, fun otel_metric_exemplar_filter:always_on/6);
reservoir(Kind, _, trace_based) ->
    Filter = fun otel_metric_exemplar_filter:sampled/6,
    reservoir(Kind, Filter).

reservoir(Kind, Filter) when Kind =:= ?KIND_COUNTER
                             ; Kind =:= ?KIND_OBSERVABLE_COUNTER
                             ; Kind =:= ?KIND_UPDOWN_COUNTER
                             ; Kind =:= ?KIND_OBSERVABLE_UPDOWNCOUNTER
                             ; Kind =:= ?KIND_OBSERVABLE_GAUGE ->
    otel_metric_exemplar_reservoir:new(otel_metric_exemplar_reservoir_simple, #{}, Filter);
reservoir(Kind, Filter) when Kind =:= ?KIND_HISTOGRAM ->
    otel_metric_exemplar_reservoir:new(otel_metric_exemplar_reservoir_aligned_histogram, #{}, Filter);
reservoir(_Kind, Filter) ->
    otel_metric_exemplar_reservoir:new(otel_metric_exemplar_reservoir_simple, #{}, Filter).
