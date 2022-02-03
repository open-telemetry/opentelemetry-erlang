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
-module(otel_links).

-export([new/4,
         list/1,
         dropped/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_span.hrl").

-record(links, {
                count_limit :: integer(),
                attribute_per_link_limit :: integer(),
                attribute_value_length_limit :: integer() | infinity,
                dropped :: integer(),
                list :: [#link{}]
               }).

-type t() :: #links{}.

-export_type([t/0]).

new(List, LinkCountLimit, AttributePerLinkLimit, AttributeValueLengthLimit) ->
    {Links, Dropped} = create_links(List, LinkCountLimit, AttributePerLinkLimit, AttributeValueLengthLimit),
    #links{count_limit=LinkCountLimit,
           attribute_per_link_limit=AttributePerLinkLimit,
           attribute_value_length_limit=AttributeValueLengthLimit,
           dropped=Dropped,
           list=Links}.

list(#links{list=List}) ->
    List.

dropped(#links{dropped=Dropped}) ->
    Dropped.
%%

create_links(List, LinkCountLimit, AttributePerLinkLimit, AttributeValueLengthLimit) ->
    create_links(List, LinkCountLimit, AttributePerLinkLimit, AttributeValueLengthLimit, 0, []).

create_links([], _, _, _AttributeValueLengthLimit, Dropped, Links) ->
    {Links, Dropped};
create_links(List, 0, _, _, Dropped, Links) ->
    {Links, Dropped + length(List)};
create_links([L | Rest], Limit, AttributePerLinkLimit, AttributeValueLengthLimit, Dropped, List) ->
    case new_link(L, AttributePerLinkLimit, AttributeValueLengthLimit) of
        drop ->
            create_links(Rest, Limit, AttributePerLinkLimit, AttributeValueLengthLimit, Dropped + 1, List);
        Link ->
            create_links(Rest, Limit - 1, AttributePerLinkLimit, AttributeValueLengthLimit, Dropped, [Link | List])
    end.

%%

new_link({TraceId, SpanId, Attributes, TraceState}, AttributePerLinkLimit, AttributeValueLengthLimit)
  when is_integer(TraceId),
       is_integer(SpanId),
       (is_list(Attributes) orelse is_map(Attributes)),
       is_list(TraceState) ->
    #link{trace_id=TraceId,
          span_id=SpanId,
          tracestate=TraceState,
          attributes=otel_attributes:new(Attributes, AttributePerLinkLimit, AttributeValueLengthLimit)};
new_link(#link{trace_id=TraceId,
               span_id=SpanId,
               tracestate=TraceState,
               attributes=Attributes}, AttributePerLinkLimit, AttributeValueLengthLimit) ->
    #link{trace_id=TraceId,
          span_id=SpanId,
          tracestate=TraceState,
          attributes=otel_attributes:new(Attributes, AttributePerLinkLimit, AttributeValueLengthLimit)};
new_link(#{trace_id := TraceId,
           span_id := SpanId,
           tracestate := TraceState,
           attributes := Attributes}, AttributePerLinkLimit, AttributeValueLengthLimit) ->
    #link{trace_id=TraceId,
          span_id=SpanId,
          tracestate=TraceState,
          attributes=otel_attributes:new(Attributes, AttributePerLinkLimit, AttributeValueLengthLimit)};
new_link({#span_ctx{trace_id=TraceId,
                    span_id=SpanId,
                    tracestate=TraceState}, Attributes}, AttributePerLinkLimit, AttributeValueLengthLimit) ->
    #link{trace_id=TraceId,
          span_id=SpanId,
          tracestate=TraceState,
          attributes=otel_attributes:new(Attributes, AttributePerLinkLimit, AttributeValueLengthLimit)};
new_link(#span_ctx{trace_id=TraceId,
                   span_id=SpanId,
                   tracestate=TraceState}, AttributePerLinkLimit, AttributeValueLengthLimit) ->
    #link{trace_id=TraceId,
          span_id=SpanId,
          tracestate=TraceState,
          attributes=otel_attributes:new([], AttributePerLinkLimit, AttributeValueLengthLimit)};
new_link(_, _, _) ->
    drop.
