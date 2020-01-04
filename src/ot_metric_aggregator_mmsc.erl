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
-module(ot_metric_aggregator_mmsc).

-export([update/4,
         checkpoint/1,
         merge/2]).

-include("ot_meter.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(MIN, 1).
-define(MAX, 2).
-define(SUM, 3).
-define(COUNT, 4).

-spec update(ets:tid(), ot_meter:input_type(), active_instrument(), number()) -> boolean().
update(_Tab, integer, #active_instrument{value=Counter}, Number) when is_tuple(Counter) ->
    update_counter(Counter, Number);
%% if not a tuple it means the value hasn't been set
update(Tab, integer, ActiveInstrument=#active_instrument{key=Key}, Number) ->
    Counter = counters:new(4, [write_concurrency]),
    %% new counters initialize to all 0's, so we must set min/max first or else
    %% we can't distiguish an unset counter and the acutal value 0
    counters:put(Counter, ?MIN, Number),
    counters:put(Counter, ?MAX, Number),
    case ets:insert_new(Tab, ActiveInstrument#active_instrument{value=Counter}) of
        true ->
            update_counter(Counter, Number);
        false ->
            %% someone added it before us
            Counter = ets:lookup_element(Tab, Key, #active_instrument.key),
            update_counter(Counter, Number)
    end;
update(Tab, float, #active_instrument{key={Name, LabelSet}}, Number) ->
    %% can't use min/max functions in matchspec body so stuck with this
    MS = ets:fun2ms(fun(A=#active_instrument{key=Key,
                                             value={Min, Max, Sum, Count}}) when Key =:= {Name, LabelSet}
                                                                                 andalso Min > Number ->
                            A#active_instrument{value={Number, Max, Sum+Number, Count+1}};
                       (A=#active_instrument{key=Key,
                                             value={Min, Max, Sum, Count}}) when Key =:= {Name, LabelSet}
                                                                                 andalso Max < Number ->
                            A#active_instrument{value={Min, Number, Sum+Number, Count+1}};
                       (A=#active_instrument{key=Key,
                                             value={Min, Max, Sum, Count}}) when Key =:= {Name, LabelSet} ->
                            A#active_instrument{value={Min, Max, Sum+Number, Count+1}};
                       %% this clause is for if the value doesn't exist yet
                       (A=#active_instrument{key=Key,
                                             value=_}) when Key =:= {Name, LabelSet} ->
                            A#active_instrument{value={Number, Number, Number, 1}}
                    end),
    case ets:select_replace(Tab, MS) of
        1 ->
            true;
        _ ->
            false
    end.

-spec checkpoint(ets:tid()) -> boolean().
checkpoint(Tab) ->
    %% first, checkpoint all the float input type mmsc aggregated metrics
    checkpoint_float(Tab),
    checkpoint_integer(Tab),
    true.

-spec merge(term(), term()) -> term().
merge({Min1, Max1, Sum1, Count1}, {Min2, Max2, Sum2, Count2}) ->
    {erlang:min(Min1, Min2), erlang:max(Max1, Max2), Sum1+Sum2, Count1+Count2}.

%%

update_counter(Counter, Number) ->
    counters:put(Counter, ?MIN, erlang:min(counters:get(Counter, ?MIN), Number)),
    counters:put(Counter, ?MAX, erlang:max(counters:get(Counter, ?MAX), Number)),
    counters:add(Counter, ?SUM, Number),
    counters:add(Counter, ?COUNT, 1).

checkpoint_float(Tab) ->
    MS = ets:fun2ms(fun(A=#active_instrument{aggregator=Aggregator,
                                             instrument=#instrument{input_type=float},
                                             value=Value}) when Aggregator =:= ?MODULE ->
                            A#active_instrument{checkpoint=Value}
                    end),
    case ets:select_replace(Tab, MS) of
        0 ->
            false;
        _ ->
            true
    end.

checkpoint_integer(Tab) ->
    MS = ets:fun2ms(fun(A=#active_instrument{key=Key,
                                             aggregator=Aggregator,
                                             instrument=#instrument{input_type=integer},
                                             value=Counter}) when Aggregator =:= ?MODULE
                                                                  andalso is_tuple(Counter) ->
                            {Key, Counter}
                    end),
    List = ets:select(Tab, MS),
    lists:foreach(fun({Key, Counter}) ->
                          Checkpoint = {counters:get(Counter, ?MIN),
                                        counters:get(Counter, ?MAX),
                                        counters:get(Counter, ?SUM),
                                        counters:get(Counter, ?COUNT)},
                          ets:update_element(Tab, Key, {#active_instrument.checkpoint, Checkpoint})
                  end, List).
