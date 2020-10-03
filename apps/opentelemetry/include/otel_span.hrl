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

%% Holds information about the instrumentation library specified when
%% getting a Tracer from the TracerProvider.
-record(instrumentation_library, {name    :: unicode:unicode_binary() | undefined,
                                  version :: unicode:unicode_binary() | undefined}).

%% The name, version and language of this OpenTelemetry library
-record(telemetry_library, {name     :: unicode:unicode_binary() | undefined,
                            language :: unicode:unicode_binary() | undefined,
                            version  :: unicode:unicode_binary() | undefined}).

-record(span, {
          %% 128 bit int trace id
          trace_id                :: opentelemetry:trace_id() | undefined,

          %% 64 bit int span id
          span_id                 :: opentelemetry:span_id() | undefined,

          tracestate              :: opentelemetry:tracestate() | undefined,

          %% 64 bit int parent span
          parent_span_id          :: opentelemetry:span_id() | undefined,

          %% name of the span
          name                    :: unicode:unicode_binary() | atom(),

          %% Distinguishes between spans generated in a particular context. For example,
          %% two spans with the same name may be distinguished using `CLIENT` (caller)
          %% and `SERVER` (callee) to identify queueing latency associated with the span.status
          kind                    :: opentelemetry:span_kind() | undefined,

          start_time              :: opentelemetry:timestamp(),
          end_time                :: opentelemetry:timestamp() | undefined,

          %% A set of attributes on the span.
          %% Kept as a list so ets:select_replace/2 can be used to add new elements
          attributes = []         :: opentelemetry:attributes() | undefined,

          %% List of time-stamped events in the Span.
          events = []             :: opentelemetry:events(),

          %% links to spans in other traces
          links = []              :: opentelemetry:links(),

          %% An optional final status for this span.
          status                  :: opentelemetry:status() | undefined,

          %% 8-bit integer, lowest bit is if it is sampled
          trace_options = 1       :: integer() | undefined,

          %% this field is not propagated and is only here as an implementation optimization
          %% If true updates like adding events are done on the span. The same as if the
          %% trace flags lowest bit is 1 but simply not propagated.
          is_recording            :: boolean() | undefined,

          instrumentation_library :: #instrumentation_library{} | undefined
         }).
