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
-module(ot_metric_aggregator_counter).

-export([update/4,
         checkpoint/1,
         merge/2]).

-include("ot_meter.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-spec update(ets:tid(), ot_meter:input_type(), active_instrument(), number()) -> boolean().
update(Tab, integer, ActiveInstrument=#active_instrument{key=Key} , Number) when is_integer(Number) ->
    _ = ets:update_counter(Tab, Key, {#active_instrument.value, Number}, ActiveInstrument);
update(Tab, float, ActiveInstrument, Number) ->
    case select_replace(Tab, ActiveInstrument, Number) of
      false ->
          _ = ets:insert_new(Tab, ActiveInstrument),
          select_replace(Tab, ActiveInstrument, Number);
      true ->
          true
    end;
update(_, _, _, _) ->
    false.

-spec checkpoint(ets:tid()) -> boolean().
checkpoint(Tab) ->
    MS = ets:fun2ms(fun(A=#active_instrument{aggregator=Aggregator,
                                             instrument=#instrument{input_type=integer},
                                             value=Value}) when Aggregator =:= ?MODULE ->
                            A#active_instrument{checkpoint=Value,
                                                value=0};
                       (A=#active_instrument{aggregator=Aggregator,
                                             instrument=#instrument{input_type=float},
                                             value=Value}) when Aggregator =:= ?MODULE ->
                            A#active_instrument{checkpoint=Value,
                                                value=0.0}
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

%%

select_replace(Tab, #active_instrument{key={Name, LabelSet}}, Number) ->
    MS = ets:fun2ms(fun(A=#active_instrument{key=Key,
                                             value=Value}) when Key =:= {Name, LabelSet} ->
                            A#active_instrument{value=Value+Number}
                    end),
    case ets:select_replace(Tab, MS) of
        1 ->
            true;
        _ ->
            false
    end.
