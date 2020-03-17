%%%------------------------------------------------------------------------
%% Copyright 2019, OpenTelemetry Authors
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
%%
%% @end
%%%-------------------------------------------------------------------------
-module(ot_metric_integrator).

-behaviour(gen_server).

-export([start_link/1,
         read/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_meter.hrl").

-record(state, {}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

read() ->
    read(ot_metric_accumulator:active_table()).

%% read all active instruments and merge any that end up with the same labelset
%% after the filtering of the label sets has been done.
%% returns a map of all metrics to be exported for this collection interval
read(Tab) ->
    ets:foldl(fun(#active_instrument{key={Name, LabelSet},
                                     instrument=_Instrument=#instrument{label_keys=LabelKeys},
                                     aggregator=Aggregator,
                                     checkpoint=Value}, Acc) ->
                      %% TODO: what to do here with label sets should be configurable
                      FilteredLabelSet = filter_label_set(LabelKeys, LabelSet),
                      NewKey = {Name, FilteredLabelSet},
                      case maps:get(NewKey, Acc, undefined) of
                          undefined ->
                              Acc#{NewKey => Value};
                          Existing ->
                              NewValue = Aggregator:merge(Existing, Value),
                              Acc#{NewKey => NewValue}
                      end
              end, #{}, Tab).

init(_Opts) ->
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%

filter_label_set(Keys, Set) ->
    %% TODO: should label sets be maps?
    maps:to_list(maps:with(Keys, maps:from_list(Set))).
