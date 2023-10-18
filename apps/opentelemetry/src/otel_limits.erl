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
%% @doc Module for setting the global limits for the number of attributes,
%% events and links.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_limits).

-export([get/0,
         set/1,
         attribute_count_limit/0,
         attribute_value_length_limit/0,
         span_attribute_count_limit/0,
         span_attribute_value_length_limit/0,
         event_count_limit/0,
         link_count_limit/0,
         attribute_per_event_limit/0,
         attribute_per_link_limit/0]).

-include("otel_span.hrl").

-define(LIMITS_KEY, {?MODULE, limits}).

-spec get() -> #limits{}.
get() ->
    persistent_term:get(?LIMITS_KEY).

-spec set(otel_configuration:t()) -> ok.
set(#{attribute_count_limit := AttributeCountLimit,
      attribute_value_length_limit := AttributeValueLengthLimit,
      span_attribute_count_limit := SpanAttributeCountLimit,
      span_attribute_value_length_limit := SpanAttributeValueLengthLimit,
      event_count_limit := EventCountLimit,
      link_count_limit := LinkCountLimit,
      attribute_per_event_limit := AttributePerEventLimit,
      attribute_per_link_limit := AttributePerLinkLimit}) ->
    Limits = #limits{attribute_count_limit=AttributeCountLimit,
                              attribute_value_length_limit=AttributeValueLengthLimit,
                              span_attribute_count_limit=SpanAttributeCountLimit,
                              span_attribute_value_length_limit=SpanAttributeValueLengthLimit,
                              event_count_limit=EventCountLimit,
                              link_count_limit=LinkCountLimit,
                              attribute_per_event_limit=AttributePerEventLimit,
                              attribute_per_link_limit=AttributePerLinkLimit},
    persistent_term:put(?LIMITS_KEY, Limits).

attribute_count_limit() ->
    get_limit(attribute_count_limit, ?MODULE:get()).

attribute_value_length_limit() ->
    get_limit(attribute_value_length_limit, ?MODULE:get()).

span_attribute_count_limit() ->
    get_limit(span_attribute_count_limit, ?MODULE:get()).

span_attribute_value_length_limit() ->
    get_limit(span_attribute_value_length_limit, ?MODULE:get()).

event_count_limit() ->
    get_limit(event_count_limit, ?MODULE:get()).

link_count_limit() ->
    get_limit(link_count_limit, ?MODULE:get()).

attribute_per_event_limit() ->
    get_limit(attribute_per_event_limit, ?MODULE:get()).

attribute_per_link_limit() ->
    get_limit(attribute_per_link_limit, ?MODULE:get()).

get_limit(attribute_count_limit, #limits{attribute_count_limit=AttributeCountLimit}) ->
    AttributeCountLimit;
get_limit(attribute_value_length_limit, #limits{attribute_value_length_limit=AttributeValueLengthLimit}) ->
    AttributeValueLengthLimit;
get_limit(span_attribute_count_limit, #limits{span_attribute_count_limit=undefined, attribute_count_limit=AttributeCountLimit}) ->
    AttributeCountLimit;
get_limit(span_attribute_count_limit, #limits{span_attribute_count_limit=SpanAttributeCountLimit}) ->
    SpanAttributeCountLimit;
get_limit(span_attribute_value_length_limit, #limits{span_attribute_value_length_limit=undefined, attribute_value_length_limit=AttributeValueLengthLimit}) ->
    AttributeValueLengthLimit;
get_limit(span_attribute_value_length_limit, #limits{span_attribute_value_length_limit=SpanAttributeValueLengthLimit}) ->
    SpanAttributeValueLengthLimit;
get_limit(event_count_limit, #limits{event_count_limit=EventCountLimit}) ->
    EventCountLimit;
get_limit(link_count_limit, #limits{link_count_limit=LinkCountLimit}) ->
    LinkCountLimit;
get_limit(attribute_per_event_limit, #limits{attribute_per_event_limit=AttributePerEventLimit}) ->
    AttributePerEventLimit;
get_limit(attribute_per_link_limit, #limits{attribute_per_link_limit=AttributePerLinkLimit}) ->
    AttributePerLinkLimit.
