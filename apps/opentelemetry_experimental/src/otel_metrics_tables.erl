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
%% @doc This module wraps all usage of the `ets' module so that the calls
%% can be type checked.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metrics_tables).

-export([instruments_tab/1,
         callbacks_tab/1,
         streams_tab/1,
         metrics_tab/1,
         exemplars_tab/1,

         insert_instrument/3,
         replace_streams/4,
         insert_callback/5,
         match_streams/2,
         list_streams/1,
         fold_instruments/3,
         lookup_instrument_by_identity/3,
         lookup_explicit_histogram_bucket_counts/5,
         lookup_sum_checkpoint/5]).

-include("otel_metrics.hrl").
-include("otel_view.hrl").

instruments_tab(Name) ->
    ets:new(list_to_atom(lists:concat([instruments, "_", Name])), [set,
                                                                   named_table,
                                                                   {keypos, 1},
                                                                   protected]).

callbacks_tab(Name) ->
    ets:new(list_to_atom(lists:concat([callbacks, "_", Name])), [bag,
                                                                 named_table,
                                                                 {keypos, 1},
                                                                 protected]).

streams_tab(Name) ->
    %% Store the complete Stream list for an Instrument in a single `set' object
    %% so replacements are atomic and recorders never observe a delete/insert gap.
    ets:new(list_to_atom(lists:concat([streams, "_", Name])), [set,
                                                               named_table,
                                                               {keypos, 1},
                                                               public]).

metrics_tab(Name) ->
    ets:new(list_to_atom(lists:concat([metrics, "_", Name])), [set,
                                                               named_table,
                                                               {keypos, 2},
                                                               public]).

exemplars_tab(Name) ->
    ets:new(list_to_atom(lists:concat([exemplars, "_", Name])), [set,
                                                                 named_table,
                                                                 {keypos, 1},
                                                                 public]).

-spec insert_instrument(ets:table(), #meter{}, otel_instrument:t())
                       -> boolean().
insert_instrument(InstrumentsTab, Meter, Instrument) ->
    Identity = otel_instrument:identity(Instrument),
    ets:insert_new(InstrumentsTab, {{Meter, Identity}, Instrument}).

-spec replace_streams(ets:table(), otel_instrument:t(), reference(), [#stream{}]) ->
          [#stream{}].
replace_streams(StreamsTab, Instrument, Reader, Streams) ->
    InstrumentId = otel_instrument:id(Instrument),
    {Existing, Retained} =
        lists:partition(fun(#stream{reader=ExistingReader}) ->
                                ExistingReader =:= Reader
                        end,
                        match_streams(StreamsTab, Instrument)),

    %% Re-evaluating Views replaces the complete stream set for this reader.
    %% Reuse ids one-to-one so existing aggregation state remains reachable,
    %% including when multiple independent Views have the same exported name.
    TaggedStreams = [{Stream, false} || Stream <- Streams],
    {Streams1, Remaining1} = reuse_stream_ids(TaggedStreams, Existing,
                                              fun same_stream_configuration/2),
    {Streams2, Remaining2} = reuse_stream_ids(Streams1, Remaining1,
                                              fun same_named_aggregation/2),
    {Streams3, _Remaining3} = reuse_stream_ids(Streams2, Remaining2,
                                               fun same_aggregation/2),

    FinalStreams = [Stream || {Stream, _Reused} <- Streams3],
    true = ets:insert(StreamsTab, {InstrumentId, Retained ++ FinalStreams}),
    FinalStreams.

reuse_stream_ids(Streams, Existing, Match) ->
    lists:mapfoldl(
      fun({Stream, true}, Remaining) ->
              {{Stream, true}, Remaining};
         ({Stream, false}, Remaining) ->
              case take_matching_stream(Stream, Remaining, Match) of
                  {ExistingStream, Remaining1} ->
                      {{Stream#stream{id=ExistingStream#stream.id}, true}, Remaining1};
                  false ->
                      {{Stream, false}, Remaining}
              end
      end,
      Existing,
      Streams).

take_matching_stream(Stream, [Existing | Rest], Match) ->
    case Match(Stream, Existing) of
        true ->
            {Existing, Rest};
        false ->
            case take_matching_stream(Stream, Rest, Match) of
                {Found, Rest1} -> {Found, [Existing | Rest1]};
                false -> false
            end
    end;
take_matching_stream(_Stream, [], _Match) ->
    false.

same_stream_configuration(StreamA, StreamB) ->
    stream_configuration(StreamA) =:= stream_configuration(StreamB).

stream_configuration(#stream{name=Name,
                             scope=Scope,
                             instrument=Instrument,
                             reader=Reader,
                             attribute_keys=AttributeKeys,
                             aggregation_module=AggregationModule,
                             aggregation_options=AggregationOptions,
                             temporality=Temporality,
                             is_monotonic=IsMonotonic,
                             description=Description,
                             forget=Forget,
                             exemplar_reservoir=ExemplarReservoir}) ->
    {Name, Scope, Instrument, Reader, AttributeKeys, AggregationModule,
     AggregationOptions, Temporality, IsMonotonic, Description, Forget,
     ExemplarReservoir}.

same_named_aggregation(StreamA=#stream{name=Name},
                       StreamB=#stream{name=Name}) ->
    same_aggregation(StreamA, StreamB);
same_named_aggregation(_, _) ->
    false.

same_aggregation(#stream{aggregation_module=AggregationModule,
                         aggregation_options=AggregationOptions},
                 #stream{aggregation_module=AggregationModule,
                         aggregation_options=AggregationOptions}) ->
    true;
same_aggregation(_, _) ->
    false.

-spec insert_callback(ets:table(), reference(), otel_instrument:callback(),
                      otel_instrument:callback_args(),
                      otel_instrument:t() | [otel_instrument:t()]) -> true.
insert_callback(CallbacksTab, ReaderId, Callback, CallbackArgs, Instrument)->
    ets:insert(CallbacksTab, {ReaderId, {Callback, CallbackArgs, Instrument}}).

-spec match_streams(ets:table(), otel_instrument:t()) -> [#stream{}].
match_streams(StreamsTab, Instrument) ->
    InstrumentId = otel_instrument:id(Instrument),
    case ets:lookup(StreamsTab, InstrumentId) of
        [{InstrumentId, Streams}] -> Streams;
        [] -> []
    end.

-spec list_streams(ets:table()) -> [#stream{}].
list_streams(StreamsTab) ->
    lists:append([Streams || {_, Streams} <- ets:tab2list(StreamsTab)]).

-spec fold_instruments(ets:table(), fun((otel_instrument:t(), Acc) -> Acc), Acc) -> Acc.
fold_instruments(InstrumentsTab, Fun, Acc0) ->
    ets:foldl(fun({_, Instrument}, Acc) ->
                      Fun(Instrument, Acc)
              end, Acc0, InstrumentsTab).

-spec lookup_instrument_by_identity(ets:table(), #meter{}, term()) ->
          otel_instrument:t() | undefined.
lookup_instrument_by_identity(InstrumentsTab, Meter, Identity) ->
    try ets:lookup_element(InstrumentsTab, {Meter, Identity}, 2) of
        Instrument -> Instrument
    catch
        error:badarg -> undefined
    end.

-spec lookup_explicit_histogram_bucket_counts(ets:table(), reference(), opentelemetry:attributes_map(), reference(), number()) -> counters:counters_ref().
lookup_explicit_histogram_bucket_counts(Table, StreamId, Attributes, ReaderId, Generation) ->
    ets_lookup_element(Table, {StreamId, Attributes, ReaderId, Generation}, #explicit_histogram_aggregation.bucket_counts, false).

-spec lookup_sum_checkpoint(ets:table(), reference(), opentelemetry:attributes_map(), reference(), number()) -> number().
lookup_sum_checkpoint(Tab, StreamId, Attributes, ReaderId, Generation) ->
    ets_lookup_element(Tab, {StreamId, Attributes, ReaderId, Generation},
                       #sum_aggregation.checkpoint, 0).

-if(?OTP_RELEASE >= 26).
ets_lookup_element(Tab, Key, Pos, Default) ->
    ets:lookup_element(Tab, Key, Pos, Default).
-else.
ets_lookup_element(Tab, Key, Pos, Default) ->
    try
        ets:lookup_element(Tab, Key, Pos)
    catch
        error:badarg ->
            Default
    end.
-endif.
