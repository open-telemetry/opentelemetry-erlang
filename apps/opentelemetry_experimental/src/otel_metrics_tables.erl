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

         insert_instrument/4,
         insert_stream/4,
         insert_callback/5,
         match_streams/3,
         foreach_instrument/2,
         lookup_instrument/3,
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
    ets:new(list_to_atom(lists:concat([streams, "_", Name])), [bag,
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

-spec insert_instrument(ets:table(), #meter{}, otel_instrument:name(), otel_instrument:t())
                       -> boolean().
insert_instrument(InstrumentsTab, Meter, Name, Instrument) ->
    ets:insert_new(InstrumentsTab, {{Meter, Name}, Instrument}).

-spec insert_stream(ets:table(), #meter{}, atom(), #stream{}) -> true.
insert_stream(StreamsTab, Meter, Name, Stream) ->
    ets:insert(StreamsTab, {{Meter, Name}, Stream}).

-spec insert_callback(ets:table(), reference(), otel_instrument:callback(),
                      otel_instrument:callback_args(), otel_instrument:t()) -> true.
insert_callback(CallbacksTab, ReaderId, Callback, CallbackArgs, Instrument)->
    ets:insert(CallbacksTab, {ReaderId, {Callback, CallbackArgs, Instrument}}).

-spec match_streams(ets:table(), #meter{}, atom()) -> [#stream{}].
match_streams(StreamsTab, Meter, Name) ->
    ets:match(StreamsTab, {{Meter, Name}, '$1'}).

foreach_instrument(InstrumentsTab, Fun) ->
    ets:foldl(fun({_, Instrument}, Acc) ->
                      Fun(Instrument),
                      Acc
              end, ok, InstrumentsTab).

-spec lookup_instrument(ets:table(), #meter{}, otel_instrument:name())
                       -> otel_instrument:t() | undefined.
lookup_instrument(InstrumentsTab, Meter, Name) ->
    try ets:lookup_element(InstrumentsTab, {Meter, Name}, 2) of
        Instrument ->
            Instrument
    catch
        _:_ ->
            undefined
    end.

-spec lookup_explicit_histogram_bucket_counts(ets:table(), atom(), opentelemetry:attributes_map(), reference(), number()) -> counters:counters_ref().
lookup_explicit_histogram_bucket_counts(Table, Name, Attributes, ReaderId, Generation) ->
    ets_lookup_element(Table, {Name, Attributes, ReaderId, Generation}, #explicit_histogram_aggregation.bucket_counts, false).

-spec lookup_sum_checkpoint(ets:table(), atom(), opentelemetry:attributes_map(), reference(), number()) -> number().
lookup_sum_checkpoint(Tab, Name, Attributes, ReaderId, Generation) ->
    ets_lookup_element(Tab, {Name, Attributes, ReaderId, Generation},
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
