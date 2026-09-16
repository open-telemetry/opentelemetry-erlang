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
%% @doc Simple Exemplar Reservoir. Exemplars are stored in an ETS table
%% keyed on `{StreamName, Attributes, ReaderId, Bucket}' where `Bucket' is
%% calculated as:
%%
%% ```
%%   if num_measurements_seen < num_buckets then
%%     bucket = num_measurements_seen
%%   else
%%     bucket = random_integer(0, num_measurements_seen)
%%   end
%%   if bucket < num_buckets then
%%     reservoir[bucket] = measurement
%%   end
%% '''
%%
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metric_exemplar_reservoir_simple).

-export([new/1,
         offer/6,
         collect/3]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

%% measurements seen count is kept in the table as `{Key, NumSeen}'
-define(MEASUREMENTS_SEEN_POS, 2).

-record(state, {num_buckets :: integer()}).

-type config() :: #{num_buckets => integer()}.

-spec new(config()) -> #state{}.
new(#{num_buckets := NumBuckets}) when is_integer(NumBuckets) ->
    #state{num_buckets=NumBuckets};
new(_) ->
    #state{num_buckets=erlang:system_info(schedulers_online)}.

offer(Ctx, ExemplarsTab, Key, Value, FilteredAttributes, #state{num_buckets=NumBuckets}) ->
    Seen = ets:update_counter(ExemplarsTab, Key, {?MEASUREMENTS_SEEN_POS, 1}, {Key, 0}),
    case Seen =< NumBuckets of
        true ->
            add_exemplar(Ctx, ExemplarsTab, Key, Seen, Value, FilteredAttributes);
        false ->
            case rand:uniform(Seen) of
                X when X =< NumBuckets ->
                    add_exemplar(Ctx, ExemplarsTab, Key, X, Value, FilteredAttributes);
                _ ->
                    ok
            end
    end.

%% @doc Return all exemplars for a `Key' and then delete them.
-spec collect(ets:table(), term(), #state{}) -> [otel_metric_exemplar:exemplar()].
collect(ExemplarsTab, Key, _State) ->
    Exemplars = ets:select(ExemplarsTab, [{{{'$1', '_'}, '$2'},
                                           [{'==', '$1', {const, Key}}],
                                           ['$2']}]),

    _ = ets:select_delete(ExemplarsTab, [{{'$1', '_'}, [{'==', '$1', {const, Key}}], [true]},
                                         {{{'$1', '_'}, '_'}, [{'==', '$1', {const, Key}}], [true]}]),

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
