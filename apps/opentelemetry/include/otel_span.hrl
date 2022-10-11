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
%% @end
%%%-------------------------------------------------------------------------

-record(span, {
          %% 128 bit int trace id
          trace_id                :: opentelemetry:trace_id() | undefined | '_',

          %% 64 bit int span id
          span_id                 :: opentelemetry:span_id() | undefined | '$1',

          tracestate = []         :: opentelemetry:tracestate() | '_',

          %% 64 bit int parent span
          parent_span_id          :: opentelemetry:span_id() | undefined | '_',

          %% name of the span
          name                    :: unicode:unicode_binary() | atom() | '_',

          %% Distinguishes between spans generated in a particular context. For example,
          %% two spans with the same name may be distinguished using `client` (caller)
          %% and `server` (callee) to identify queueing latency associated with the span.status
          kind                    :: opentelemetry:span_kind() | undefined | '_',

          start_time              :: opentelemetry:timestamp() | '$2',
          end_time                :: opentelemetry:timestamp() | undefined | '_',

          %% A set of attributes on the span.
          attributes              :: otel_attributes:t() | undefined | '_',

          %% List of time-stamped events in the Span.
          events                  :: otel_events:t() | '_',

          %% links to spans in other traces
          links                   :: otel_links:t() | '_',

          %% An optional final status for this span.
          status                  :: opentelemetry:status() | undefined | '_',

          %% 8-bit integer, lowest bit is if it is sampled
          trace_flags = 1         :: integer() | undefined | '_',

          %% this field is not propagated and is only here as an implementation optimization
          %% If true updates like adding events are done on the span. The same as if the
          %% trace flags lowest bit is 1 but simply not propagated.
          is_recording            :: boolean() | undefined | '_',

          instrumentation_scope :: opentelemetry:instrumentation_scope() | undefined | '_'
         }).

-record(span_limits, {
          attribute_count_limit = 128             :: integer(), %% Maximum allowed attribute count per span;
          attribute_value_length_limit = infinity :: integer() | infinity,  %% Maximum allowed attribute value length
          event_count_limit = 128                 :: integer(), %% Maximum allowed span event count
          link_count_limit = 128                  :: integer(), %% Maximum allowed span link count
          attribute_per_event_limit = 128         :: integer(), %% Maximum allowed attribute per span event count
          attribute_per_link_limit = 128          :: integer() %% Maximum allowed attribute per span link count
       }).

-record(link, {
          trace_id   :: opentelemetry:trace_id(),
          span_id    :: opentelemetry:span_id(),
          attributes :: otel_attributes:t(),
          tracestate :: opentelemetry:tracestate()
         }).

-record(event, {
          system_time_native :: integer(),
          name               :: unicode:unicode_binary() | atom(),
          attributes         :: otel_attributes:t()
         }).
