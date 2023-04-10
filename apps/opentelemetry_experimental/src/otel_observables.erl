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

-export([run_callbacks/4]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_metrics.hrl").
-include("otel_view.hrl").

-type callbacks() :: [{otel_instrument:callback(), otel_instrument:callback_args(), otel_instrument:t()}].

%% call each callback and associate the result with the Instruments it observes
-spec run_callbacks(callbacks(), reference(), ets:table(), ets:table()) -> ok.
run_callbacks(Callbacks, ReaderId, ViewAggregationTab, MetricsTab) ->
    lists:foreach(fun({Callback, CallbackArgs, Instruments})
                        when is_list(Instruments) ->
                          Results = Callback(CallbackArgs),
                          handle_instruments_observations(Results,
                                                          Instruments,
                                                          ViewAggregationTab,
                                                          MetricsTab,
                                                          ReaderId);
                     ({Callback, CallbackArgs, Instrument}) ->
                          Results = Callback(CallbackArgs),
                          %% eqwalizer:ignore we know this is [otel_instrument:observation()] but eqwalizer doesn't
                          handle_instrument_observations(Results,
                                                         Instrument,
                                                         ViewAggregationTab,
                                                         MetricsTab,
                                                         ReaderId)
                  end, Callbacks).

%% lookup ViewAggregations for Instrument and aggregate each observation
-spec handle_instrument_observations([otel_instrument:observation()], otel_instrument:t(),
                                     ets:table(), ets:table(), reference()) -> ok.
handle_instrument_observations(Results, #instrument{meter=Meter,
                                                    name=Name},
                               ViewAggregationTab, MetricsTab, ReaderId) ->
    try ets:lookup_element(ViewAggregationTab, {Meter, Name}, 2) of
        ViewAggregations ->
            [handle_observations(MetricsTab, ViewAggregation, Results)
             || #view_aggregation{reader=Id}=ViewAggregation <- ViewAggregations,
                Id =:= ReaderId],
            ok
    catch
        error:badarg ->
            %% no Views for this Instrument, so nothing to do
            ok
    end.

%% handle results for a multi-instrument callback
-spec handle_instruments_observations([otel_instrument:named_observations()], [otel_instrument:t()],
                                      ets:table(), ets:table(), reference()) -> ok.
handle_instruments_observations([], _Instruments, _ViewAggregationTab, _MetricsTab, _ReaderId) ->
    ok;
handle_instruments_observations([{InstrumentName, Results} | Rest], Instruments,
                                ViewAggregationTab, MetricsTab, ReaderId) ->
    case lists:keyfind(InstrumentName, #instrument.name, Instruments) of
        false ->
            ?LOG_DEBUG("Unknown Instrument ~p used in metric callback", [InstrumentName]);
        Instrument ->
            handle_instrument_observations(Results, Instrument, ViewAggregationTab, MetricsTab, ReaderId)
    end,
    handle_instruments_observations(Rest, Instruments, ViewAggregationTab, MetricsTab, ReaderId);
handle_instruments_observations([Result | Rest], Instruments, ViewAggregationTab, MetricsTab, ReaderId) ->
    ?LOG_DEBUG("Each multi-instrument callback result must be a tuple of "
               "type {atom(), [{number(), map()}]} but got ~p", [Result]),
    handle_instruments_observations(Rest, Instruments, ViewAggregationTab, MetricsTab, ReaderId);
handle_instruments_observations(Results, _Instruments, _ViewAggregationTab, _MetricsTab, _ReaderId) ->
    ?LOG_DEBUG("Multi-instrument callback result must be a list of type "
               "[{atom(), [{number(), map()}]}] but got ~p", [Results]),
    ok.


%% update aggregation for each observation
handle_observations(_MetricsTab, _ViewAggregation, []) ->
    ok;
handle_observations(MetricsTab, ViewAggregation, [{Number, Attributes} | Rest])
  when is_number(Number),
       is_map(Attributes) ->
    _ = otel_aggregation:maybe_init_aggregate(MetricsTab, ViewAggregation, Number, Attributes),
    handle_observations(MetricsTab, ViewAggregation, Rest);
handle_observations(MetricsTab, ViewAggregation, [Result | Rest]) ->
    ?LOG_DEBUG("Each metric callback result must be of type {number(), map()} but got ~p", [Result]),
    handle_observations(MetricsTab, ViewAggregation, Rest);
handle_observations(_MetricsTab, _ViewAggregation, Result) ->
    ?LOG_DEBUG("Metric callback return must be a list of type [{number(), map()}] or "
               "[{atom(), [{number(), map()}]}] but got", [Result]),
    ok.
