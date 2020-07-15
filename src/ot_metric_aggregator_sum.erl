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
%% @doc Sum aggregator for summing recorded counter values.
%%
%% @end
%%%-------------------------------------------------------------------------
-module(ot_metric_aggregator_sum).

-behaviour(ot_metric_aggregator).

-export([update/4,
         checkpoint/2,
         merge/2,
         initial_value/1]).

-include("ot_meter.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-spec update(ets:tab(), ot_meter:key(), ot_meter:number_kind(), number()) -> boolean().
update(Tab, Key, integer, Number) when is_integer(Number) ->
    _ = ets:update_counter(Tab, Key, {#active_instrument.current, Number});
update(Tab, Key, float, Number) ->
    select_replace(Tab, Key, Number);
update(_, _, _, _) ->
    false.

-spec checkpoint(ets:tab(), {ot_meter:name(), ot_meter:label_set()}) -> boolean().
checkpoint(Tab, NameLabelSet) ->
    MS = ets:fun2ms(fun(A=#active_instrument{key=Key,
                                             aggregator=Aggregator,
                                             instrument=#instrument{number_kind=integer},
                                             current=Current}) when Key =:= NameLabelSet ->
                            A#active_instrument{checkpoint=Current,
                                                current=0};
                       (A=#active_instrument{key=Key,
                                             aggregator=Aggregator,
                                             instrument=#instrument{number_kind=float},
                                             current=Current}) when Key =:= NameLabelSet ->
                            A#active_instrument{checkpoint=Current,
                                                current=0.0}
                    end),
    case ets:select_replace(Tab, MS) of
      0 ->
          false;
      _ ->
          true
    end.

-spec merge(number(), number()) -> number().
merge(Number1, Number2) ->
    Number1 + Number2.

-spec initial_value(ot_meter:number_kind()) -> number().
initial_value(integer) ->
    0;
initial_value(float) ->
    0.0;
initial_value(_) ->
    0.

%%

select_replace(Tab, NameLabelSet, Number) ->
    MS = ets:fun2ms(fun(A=#active_instrument{key=Key,
                                             current=Current}) when Key =:= NameLabelSet ->
                            A#active_instrument{current=Current+Number}
                    end),
    case ets:select_replace(Tab, MS) of
        1 ->
            true;
        _ ->
            false
    end.
