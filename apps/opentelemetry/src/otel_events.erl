%%%------------------------------------------------------------------------
%% Copyright 2021, OpenTelemetry Authors
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
-module(otel_events).

-export([new/3,
         add/2,
         list/1,
         dropped/1]).

-include("otel_span.hrl").

-record(events, {
                 count_limit :: integer(),
                 attribute_per_event_limit :: integer(),
                 attribute_value_length_limit :: integer() | infinity,
                 dropped :: integer(),
                 list :: [#event{}]
                }).

-type t() :: #events{}.

-export_type([t/0]).

new(EventCountLimit, AttributePerEventLimit, AttributeValueLengthLimit) ->
    #events{count_limit=EventCountLimit,
            attribute_per_event_limit=AttributePerEventLimit,
            attribute_value_length_limit=AttributeValueLengthLimit,
            dropped=0,
            list=[]}.

add(NewEvents, Events) ->
    add_events(NewEvents, Events).

list(#events{list=List}) ->
    List.

dropped(#events{dropped=Dropped}) ->
    Dropped.

%%

add_events(NewEvents, Events=#events{count_limit=CountLimit,
                                     list=List}) ->
    Limit = CountLimit - length(List),
    add_events_(NewEvents, Limit, Events).

add_events_([], _, Events) ->
    Events;
add_events_(NewEvents, Limit, Events=#events{dropped=Dropped}) when Limit =< 0->
    %% we've hit the limit on the number of events so drop the rest
    Events#events{dropped=Dropped + length(NewEvents)};
add_events_([NewEvent | Rest], Limit, Events=#events{attribute_per_event_limit=AttributePerEventLimit,
                                                     attribute_value_length_limit=AttributeValueLengthLimit,
                                                     dropped=Dropped,
                                                     list=List}) ->
    case new_event(NewEvent, AttributePerEventLimit, AttributeValueLengthLimit) of
        drop ->
            add_events_(Rest, Limit, Events#events{dropped=Dropped + 1});
        Event ->
            add_events_(Rest, Limit - 1, Events#events{list=[Event | List]})
    end.

new_event(#{system_time_nano := Timestamp,
            name := Name,
            attributes := Attributes}, AttributePerEventLimit, AttributeValueLengthLimit) ->
    #event{system_time_nano=Timestamp,
           name=Name,
           attributes=otel_attributes:new(Attributes, AttributePerEventLimit, AttributeValueLengthLimit)};
new_event(#{name := Name,
            attributes := Attributes}, AttributePerEventLimit, AttributeValueLengthLimit) ->
    #event{system_time_nano=opentelemetry:timestamp(),
           name=Name,
           attributes=otel_attributes:new(Attributes, AttributePerEventLimit, AttributeValueLengthLimit)};
new_event({Time, Name, Attributes}, AttributePerEventLimit, AttributeValueLengthLimit)
  when is_binary(Name) ; is_atom(Name) ->
    #event{system_time_nano=Time,
           name=Name,
           attributes=otel_attributes:new(Attributes, AttributePerEventLimit, AttributeValueLengthLimit)};
new_event({Name, Attributes}, AttributePerEventLimit, AttributeValueLengthLimit)
  when is_binary(Name) ; is_atom(Name) ->
    #event{system_time_nano=opentelemetry:timestamp(),
           name=Name,
           attributes=otel_attributes:new(Attributes, AttributePerEventLimit, AttributeValueLengthLimit)};
new_event(_, _, _) ->
    drop.

