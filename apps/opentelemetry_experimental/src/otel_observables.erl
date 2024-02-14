%%%------------------------------------------------------------------------
%% Copyright 2023, OpenTelemetry Authors
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
-module(otel_observables).

-export([run_callbacks/5]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").

-type callbacks() :: [{otel_instrument:callback(), otel_instrument:callback_args(), otel_instrument:t()}].

%% call each callback and associate the result with the Instruments it observes
-spec run_callbacks(callbacks(), reference(), ets:table(), ets:table(), ets:table()) -> ok.
run_callbacks(Callbacks, ReaderId, StreamTab, MetricsTab, ExemplarsTab) ->
    lists:foreach(fun({Callback, CallbackArgs, Instruments})
                        when is_list(Instruments) ->
                          Ctx = otel_ctx:new(),
                          Results = Callback(CallbackArgs),
                          handle_instruments_observations(Ctx,
                                                          Results,
                                                          Instruments,
                                                          StreamTab,
                                                          MetricsTab,
                                                          ExemplarsTab,
                                                          ReaderId);
                     ({Callback, CallbackArgs, Instrument}) ->
                          Ctx = otel_ctx:new(),
                          Results = Callback(CallbackArgs),
                          %% when not a list of instruments it isn't expecting named observation
                          %% results so we use handle_instrument instead of handle_instruments
                          %% but we can't type that correctly so have to use a `fixme'
                          %% eqwalizer:fixme can maybe do better typing to not have to ignore this
                          handle_instrument_observations(Ctx,
                                                         Results,
                                                         Instrument,
                                                         StreamTab,
                                                         MetricsTab,
                                                         ExemplarsTab,
                                                         ReaderId)
                  end, Callbacks).

%% lookup Streams for Instrument and aggregate each observation
-spec handle_instrument_observations(otel_ctx:t(), [otel_instrument:observation()], otel_instrument:t(),
                                     ets:table(), ets:table(), ets:table(), reference()) -> ok.
handle_instrument_observations(Ctx, Results, #instrument{meter=Meter,
                                                         name=Name},
                               StreamTab, MetricsTab, ExemplarsTab, ReaderId) ->
    try ets:lookup_element(StreamTab, {Meter, Name}, 2) of
        Streams ->
            [handle_observations(Ctx, MetricsTab, ExemplarsTab, Stream, Results)
             || #stream{reader=Id}=Stream <- Streams,
                Id =:= ReaderId],
            ok
    catch
        error:badarg ->
            %% no Views for this Instrument, so nothing to do
            ok
    end.

%% handle results for a multi-instrument callback
-spec handle_instruments_observations(otel_ctx:t(),
                                      [otel_instrument:named_observations()], [otel_instrument:t()],
                                      ets:table(), ets:table(), ets:table(), reference()) -> ok.
handle_instruments_observations(_Ctx, [], _Instruments, _StreamTab, _MetricsTab, _ExemplarsTab, _ReaderId) ->
    ok;
handle_instruments_observations(Ctx, [{InstrumentName, Results} | Rest], Instruments,
                                StreamTab, MetricsTab, ExemplarsTab, ReaderId) ->
    case lists:keyfind(InstrumentName, #instrument.name, Instruments) of
        false ->
            ?LOG_DEBUG("Unknown Instrument ~p used in metric callback", [InstrumentName]);
        Instrument ->
            handle_instrument_observations(Ctx, Results, Instrument, StreamTab, MetricsTab, ExemplarsTab, ReaderId)
    end,
    handle_instruments_observations(Ctx, Rest, Instruments, StreamTab, MetricsTab, ExemplarsTab, ReaderId);
handle_instruments_observations(Ctx, [Result | Rest], Instruments, StreamTab, MetricsTab, ExemplarsTab, ReaderId) ->
    ?LOG_DEBUG("Each multi-instrument callback result must be a tuple of "
               "type {atom(), [{number(), map()}]} but got ~p", [Result]),
    handle_instruments_observations(Ctx, Rest, Instruments, StreamTab, MetricsTab, ExemplarsTab, ReaderId);
handle_instruments_observations(_Ctx, Results, _Instruments, _StreamTab, _MetricsTab, _ExemplarsTab, _ReaderId) ->
    ?LOG_DEBUG("Multi-instrument callback result must be a list of type "
               "[{atom(), [{number(), map()}]}] but got ~p", [Results]),
    ok.


%% update aggregation for each observation
handle_observations(_Ctx, _MetricsTab, _ExemplarsTab, _Stream, []) ->
    ok;
handle_observations(Ctx, MetricsTab, ExemplarsTab, Stream, [{Number, Attributes} | Rest])
  when is_number(Number),
       is_map(Attributes) ->
    _ = otel_aggregation:maybe_init_aggregate(Ctx, MetricsTab, ExemplarsTab, Stream, Number, Attributes),
    handle_observations(Ctx, MetricsTab, ExemplarsTab, Stream, Rest);
handle_observations(Ctx, MetricsTab, ExemplarsTab, Stream, [Result | Rest]) ->
    ?LOG_DEBUG("Each metric callback result must be of type {number(), map()} but got ~p", [Result]),
    handle_observations(Ctx, MetricsTab, ExemplarsTab, Stream, Rest);
handle_observations(_Ctx, _MetricsTab, _ExemplarsTab, _Stream, Result) ->
    ?LOG_DEBUG("Metric callback return must be a list of type [{number(), map()}] or "
               "[{atom(), [{number(), map()}]}] but got", [Result]),
    ok.
