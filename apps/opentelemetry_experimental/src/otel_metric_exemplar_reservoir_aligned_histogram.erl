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
%% @doc Exemplar Reservoir used by the Explicit Histogram Aggregation. The
%% same bucket boundaries as the aggregation. Simple algorithm for adding
%% each new measurement as an exemplar:
%%
%% ```
%%   bucket = find_histogram_bucket(measurement)
%%   if bucket < num_buckets then
%%     reservoir[bucket] = measurement
%%   end

%%   def find_histogram_bucket(measurement):
%%     for boundary, idx in bucket_boundaries do
%%       if value <= boundary then
%%         return idx
%%       end
%%     end
%%     return boundaries.length
%%'''
%%
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metric_exemplar_reservoir_aligned_histogram).

-export([new/1,
         offer/6,
         collect/3]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-record(state, {bucket_boundaries :: [float()]}).

-type config() :: #{explicit_bucket_boundaries => [float()]}.

-spec new(config()) -> #state{}.
new(#{explicit_bucket_boundaries := BucketBoundaries}) ->
    #state{bucket_boundaries=BucketBoundaries};
new(_) ->
    #state{bucket_boundaries=otel_aggregation_histogram_explicit:default_buckets()}.

offer(Ctx, ExemplarsTab, Key, Value, FilteredAttributes, #state{bucket_boundaries=BucketBoundaries}) ->
    Bucket = find_bucket(BucketBoundaries, Value),
    add_exemplar(Ctx, ExemplarsTab, Key, Bucket, Value, FilteredAttributes).

%% @doc Return all exemplars for a `Key' and then delete them.
-spec collect(ets:table(), term(), #state{}) -> [otel_metric_exemplar:exemplar()].
collect(ExemplarsTab, Key, _State) ->
    Exemplars = ets:match(ExemplarsTab, {{Key, '_'}, '$1'}),

    _ = ets:match_delete(ExemplarsTab, {{Key, '_'}, '_'}),

    Exemplars.

%%

add_exemplar(Ctx, ExemplarsTab, Key, Bucket, Value, FilteredAttributes) ->
    Time = opentelemetry:timestamp(),
    {TraceId, SpanId} = case otel_tracer:current_span_ctx(Ctx) of
                            #span_ctx{trace_id=TraceId0,
                                      span_id=SpanId0} ->
                                {TraceId0, SpanId0};
                            _ ->
                                {undefined, undefined}
                        end,
    Exemplar = otel_metric_exemplar:new(Value, Time, FilteredAttributes, TraceId, SpanId),

    _ = ets:insert(ExemplarsTab, {{Key, Bucket}, Exemplar}),

    ok.

find_bucket(Boundaries, Value) ->
    find_bucket(Boundaries, Value, 1).

find_bucket([X | _Rest], Value, Pos) when Value =< X ->
    Pos;
find_bucket([_X | Rest], Value, Pos) ->
    find_bucket(Rest, Value, Pos+1);
find_bucket(_, _, Pos) ->
    Pos.
