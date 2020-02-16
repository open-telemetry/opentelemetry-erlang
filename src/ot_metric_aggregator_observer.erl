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
-module(ot_metric_aggregator_observer).

-export([update/4,
         checkpoint/1,
         merge/2]).

-include("ot_meter.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-spec update(ets:tid(), ot_meter:input_type(), active_instrument(), number())
            -> boolean().
update(Tab, _Type, ActiveInstrument, Number) ->
    Now = erlang:monotonic_time(),
    NewValue = {Number, Now},

    case select_replace(Tab, ActiveInstrument, NewValue) of
        false ->
            _ = ets:insert_new(Tab, ActiveInstrument#active_instrument{value=NewValue}),
            %% a concurrent call with an earlier timestamp could have done the insert_new
            %% before this, so still try another select_replace just in case
            select_replace(Tab, ActiveInstrument, NewValue);
        true ->
            true
    end.

-spec checkpoint(ets:tid()) -> boolean().
checkpoint(Tab) ->
    MS = ets:fun2ms(fun(A=#active_instrument{aggregator=Aggregator,
                                             value=Value}) when Aggregator =:= ?MODULE ->
                            A#active_instrument{checkpoint=Value}
                    end),
    case ets:select_replace(Tab, MS) of
      0 ->
          false;
      _ ->
          true
    end.

-spec merge(number(), number()) -> {number(), integer()}.
merge({Number1, Timestamp1}, {_Number2, Timestamp2}) when Timestamp1 > Timestamp2 ->
    {Number1, Timestamp1};
merge({_Number1, Timestamp1}, {Number2, Timestamp2}) when Timestamp2 > Timestamp1 ->
    {Number2, Timestamp2};
%% shouldn't happen since we are using monotonic_time, but better safe than sorry?
merge({_Number1, _Timestamp1}, {Number2, Timestamp2}) ->
    {Number2, Timestamp2}.

%%

select_replace(Tab, #active_instrument{key={Name, LabelSet}}, NewValue={_, NewTimestamp}) ->
    %% ensure recorded gauge is the latest recording
    MS = ets:fun2ms(fun(A=#active_instrument{key=Key,
                                             value={_, Timestamp}}) when Key =:= {Name, LabelSet}
                                                                         andalso NewTimestamp > Timestamp ->
                            A#active_instrument{value=NewValue}
                    end),
    case ets:select_replace(Tab, MS) of
        1 ->
            true;
        _ ->
            false
    end.
