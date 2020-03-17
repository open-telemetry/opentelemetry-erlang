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
-module(ot_metric_aggregator_last_value).

-export([update/4,
         checkpoint/2,
         merge/2,
         initial_value/1]).

-include("ot_meter.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-spec update(ets:tab(), ot_meter:key(), observer | ot_meter:input_type(), number()) -> boolean().
update(Tab, Key, _Type, Number) ->
    Now = erlang:monotonic_time(),
    NewCurrent = {Number, Now},
    select_replace(Tab, Key, NewCurrent).

-spec checkpoint(ets:tab(), {ot_meter:name(), ot_meter:label_set()}) -> boolean().
checkpoint(Tab, NameLabelSet) ->
    MS = ets:fun2ms(fun(A=#active_instrument{key=Key,
                                             aggregator=Aggregator,
                                             current=Current}) when Key =:= NameLabelSet ->
                            A#active_instrument{checkpoint=Current,
                                                current=undefined}
                    end),
    case ets:select_replace(Tab, MS) of
      0 ->
          false;
      _ ->
          true
    end.

-spec merge({number(), integer()}, {number(), integer()}) -> {number(), integer()}.
merge({Number1, Timestamp1}, {_Number2, Timestamp2}) when Timestamp1 > Timestamp2 ->
    {Number1, Timestamp1};
merge({_Number1, Timestamp1}, {Number2, Timestamp2}) when Timestamp2 > Timestamp1 ->
    {Number2, Timestamp2};
%% shouldn't happen since we are using monotonic_time, but better safe than sorry?
merge({_Number1, _Timestamp1}, {Number2, Timestamp2}) ->
    {Number2, Timestamp2};
merge(undefined, {Number2, Timestamp2}) ->
    {Number2, Timestamp2};
merge({Number1, Timestamp1}, undefined) ->
    {Number1, Timestamp1};
merge(undefined, undefined) ->
    undefined.

-spec initial_value(ot_meter:input_type()) -> undefined.
initial_value(_) ->
    undefined.

%%

select_replace(Tab, NameLabelSet, NewCurrent={_, NewTimestamp}) ->
    %% ensure recorded measurement is the latest recording
    MS = ets:fun2ms(fun(A=#active_instrument{key=Key,
                                             current={_, Timestamp}}) when Key =:= NameLabelSet ,
                                                                           NewTimestamp >= Timestamp ->
                            A#active_instrument{current=NewCurrent};
                       (A=#active_instrument{key=Key,
                                             current={Value, Timestamp}}) when Key =:= NameLabelSet ,
                                                                               NewTimestamp < Timestamp ->
                            A;
                       (A=#active_instrument{key=Key,
                                             current=Current}) when Key =:= NameLabelSet ,
                                                                    Current =:= undefined ->
                            %% not a tuple, this must be the first recording
                            A#active_instrument{current=NewCurrent}
                    end),
    case ets:select_replace(Tab, MS) of
        1 ->
            true;
        _ ->
            false
    end.
