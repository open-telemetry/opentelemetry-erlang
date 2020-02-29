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
-module(ot_metric_aggregator_measure).

-export([update/4,
         checkpoint/1,
         merge/2]).

-include("ot_meter.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-spec update(ets:tab(), ot_meter:input_type(), active_instrument(), number()) -> boolean().
update(Tab, _, ActiveInstrument, Number) ->
    _ = ets:insert(Tab, ActiveInstrument#active_instrument{value=Number}),
    true.

-spec checkpoint(ets:tab()) -> boolean().
checkpoint(Tab) ->
    MS = ets:fun2ms(fun(A=#active_instrument{aggregator=Aggregator,
                                             value=Value}) when Aggregator =:= ?MODULE ->
                            A#active_instrument{checkpoint=[Value]}
                    end),
    case ets:select_replace(Tab, MS) of
      0 ->
          false;
      _ ->
          true
    end.


-spec merge(term(), term()) -> term().
merge(Measure1, Measure2) when is_list(Measure1)
                               andalso is_number(Measure2) ->
    [Measure2 | Measure1];
merge(Measure1, Measure2) when is_list(Measure2)
                               andalso is_number(Measure1) ->
    Measure2 ++ [Measure1];
merge(Measure1, Measure2) when is_number(Measure1)
                               andalso is_number(Measure2) ->
    [Measure2, Measure1];
merge(Measure1, Measure2) when is_list(Measure1)
                               andalso is_list(Measure2) ->
    Measure2 ++ Measure1.
