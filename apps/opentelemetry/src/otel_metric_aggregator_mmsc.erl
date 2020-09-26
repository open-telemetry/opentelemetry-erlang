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
%% @doc Min, max, sum, count aggregator.
%%
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metric_aggregator_mmsc).

-behaviour(otel_metric_aggregator).

-export([update/4,
         checkpoint/2,
         merge/2,
         initial_value/1]).

-include("otel_meter.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(MIN, 1).
-define(MAX, 2).
-define(SUM, 3).
-define(COUNT, 4).

-spec update(ets:tab(), otel_meter:key(), otel_meter:input_type(), number()) -> boolean().
update(Tab, Key, _, Number) ->
    update_mmsc(Tab, Key, Number).

-spec checkpoint(ets:tab(), otel_meter:key()) -> boolean().
checkpoint(Tab, NameLabelSet) ->
    MS = ets:fun2ms(fun(A=#active_instrument{key=Key,
                                             aggregator=Aggregator,
                                             current=Current}) when Key =:= NameLabelSet ,
                                                                Aggregator =:= ?MODULE ->
                            A#active_instrument{checkpoint=Current,
                                                current={infinity, 0, 0, 0}}
                    end),
    case ets:select_replace(Tab, MS) of
        0 ->
            false;
        _ ->
            true
    end.

-spec merge(term(), term()) -> term().
merge({Min1, Max1, Sum1, Count1}, {Min2, Max2, Sum2, Count2}) ->
    {erlang:min(Min1, Min2), erlang:max(Max1, Max2), Sum1+Sum2, Count1+Count2}.

-spec initial_value(otel_meter:input_type()) -> {infinity, number(), number(), 0}.
initial_value(integer) ->
    {infinity, 0, 0, 0};
initial_value(float) ->
    {infinity, 0.0, 0.0, 0};
initial_value(_) ->
    {infinity, 0, 0, 0}.

%%

update_mmsc(Tab, NameLabelSet, Number) ->
    %% can't use min/max functions in matchspec body so stuck with this
    MS = ets:fun2ms(fun(A=#active_instrument{key=Key,
                                             current={infinity, 0, 0, 0}}) when Key =:= NameLabelSet ->
                            A#active_instrument{current={Number, Number, Number, 1}};
                       (A=#active_instrument{key=Key,
                                             current={Min, Max, Sum, Count}}) when Key =:= NameLabelSet
                                                                                 andalso Min > Number ->
                            A#active_instrument{current={Number, Max, Sum+Number, Count+1}};
                       (A=#active_instrument{key=Key,
                                             current={Min, Max, Sum, Count}}) when Key =:= NameLabelSet
                                                                                 andalso Max < Number ->
                            A#active_instrument{current={Min, Number, Sum+Number, Count+1}};
                       (A=#active_instrument{key=Key,
                                             current={Min, Max, Sum, Count}}) when Key =:= NameLabelSet ->
                            A#active_instrument{current={Min, Max, Sum+Number, Count+1}};
                       %% this clause is for if the current value doesn't exist yet
                       (A=#active_instrument{key=Key,
                                             current=_}) when Key =:= NameLabelSet ->
                            A#active_instrument{current={Number, Number, Number, 1}}
                    end),
    case ets:select_replace(Tab, MS) of
        1 ->
            true;
        _ ->
            false
    end.
