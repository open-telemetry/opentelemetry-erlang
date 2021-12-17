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

%% The name, version and language of this OpenTelemetry library
-record(telemetry_library, {name     :: unicode:unicode_binary() | undefined,
                            language :: unicode:unicode_binary() | undefined,
                            version  :: unicode:unicode_binary() | undefined}).

-record(span, {
          %% 128 bit int trace id
          trace_id                :: opentelemetry:trace_id() | undefined,

          %% 64 bit int span id
          span_id                 :: opentelemetry:span_id() | undefined,

          tracestate = []         :: opentelemetry:tracestate(),

          %% 64 bit int parent span
          parent_span_id          :: opentelemetry:span_id() | undefined,

          %% name of the span
          name                    :: unicode:unicode_binary() | atom(),

          %% Distinguishes between spans generated in a particular context. For example,
          %% two spans with the same name may be distinguished using `client` (caller)
          %% and `server` (callee) to identify queueing latency associated with the span.status
          kind                    :: opentelemetry:span_kind() | undefined,

          start_time              :: opentelemetry:timestamp(),
          end_time                :: opentelemetry:timestamp() | undefined,

          %% A set of attributes on the span.
          attributes              :: otel_attributes:t() | undefined,

          %% List of time-stamped events in the Span.
          events                  :: otel_events:t(),

          %% links to spans in other traces
          links                   :: otel_links:t(),

          %% An optional final status for this span.
          status                  :: opentelemetry:status() | undefined,

          %% 8-bit integer, lowest bit is if it is sampled
          trace_flags = 1         :: integer() | undefined,

          %% this field is not propagated and is only here as an implementation optimization
          %% If true updates like adding events are done on the span. The same as if the
          %% trace flags lowest bit is 1 but simply not propagated.
          is_recording            :: boolean() | undefined,

          instrumentation_library :: opentelemetry:instrumentation_library() | undefined
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
          system_time_nano :: non_neg_integer(),
          name             :: unicode:unicode_binary() | atom(),
          attributes       :: otel_attributes:t()
         }).
