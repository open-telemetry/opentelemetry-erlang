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
%%%------------------------------------------------------------------------

%% These records are based on protos found in the opentelemetry-proto repo:
%% src/opentelemetry/proto/trace/v1/trace.proto
%% They are not exact translations because further processing is done after
%% the span has finished and can be vendor specific. For example, there is
%% no count of the number of dropped attributes in the span record. And
%% an attribute's value can be a function to only evaluate the value if it
%% is actually used (at the time of exporting).

%% for use in guards: sampling bit is the first bit in 8-bit trace options
-define(IS_SPAN_ENABLED(X), (X band 1) =/= 0).

-define(MESSAGE_EVENT_TYPE_UNSPECIFIED, 'TYPE_UNSPECIFIED').
-define(MESSAGE_EVENT_TYPE_SENT, 'SENT').
-define(MESSAGE_EVENT_TYPE_RECEIVED, 'RECEIVED').

-define(SPAN_KIND_UNSPECIFIED, 'SPAN_KIND_UNSPECIFIED').
-define(SPAN_KIND_INTERNAL, 'INTERNAL').
-define(SPAN_KIND_SERVER, 'SERVER').
-define(SPAN_KIND_CLIENT, 'CLIENT').
-define(SPAN_KIND_PRODUCER, 'PRODUCER').
-define(SPAN_KIND_CONSUMER, 'CONSUMER').

-record(span_ctx, {
          %% 128 bit int trace id
          trace_id          :: opentelemetry:trace_id() | undefined,
          %% 64 bit int span id
          span_id           :: opentelemetry:span_id() | undefined,
          %% 8-bit integer, lowest bit is if it is sampled
          trace_options = 1 :: integer() | undefined,
          %% Tracestate represents tracing-system specific context in a list of key-value pairs.
          %% Tracestate allows different vendors propagate additional information and
          %% inter-operate with their legacy Id formats.
          tracestate        :: opentelemetry:tracestate() | undefined,
          %% IsValid is a boolean flag which returns true if the SpanContext has a non-zero
          %% TraceID and a non-zero SpanID.
          is_valid          :: boolean() | undefined
         }).

-record(span, {
          %% name of the span
          name                                    :: unicode:unicode_binary(),

          %% 128 bit int trace id
          trace_id                                :: opentelemetry:trace_id() | undefined,

          %% 64 bit int span id
          span_id                                 :: opentelemetry:span_id() | undefined,
          %% 64 bit int parent span
          parent_span_id                          :: opentelemetry:span_id() | undefined,

          tracestate                              :: opentelemetry:tracestate() | undefined,

          %% 8-bit integer, lowest bit is if it is sampled
          trace_options = 1                       :: integer() | undefined,

          kind = ?SPAN_KIND_UNSPECIFIED           :: opentelemetry:span_kind() | undefined,

          start_time                              :: wts:timestamp(),
          end_time                                :: wts:timestamp() | undefined,

          %% A set of attributes on the span.
          %% Kept as a list so ets:select_replace/2 can be used to add new elements
          attributes = []                         :: opentelemetry:attributes() | undefined,

          %% optional stacktrace from where the span was started
          stack_trace                             :: opentelemetry:stack_trace() | undefined,

          %% A time-stamped annotation or message event in the Span.
          time_events = []                        :: opentelemetry:time_events(),

          %% links to spans in other traces
          links = []                              :: opentelemetry:links(),

          %% An optional final status for this span.
          status                                  :: opentelemetry:status() | undefined,

          %% An optional resource that is associated with this span. If not set, this span
          %% should be part of a batch that does include the resource information, unless resource
          %% information is unknown.
          resource                                :: opentelemetry:resource() | undefined,

          %% A highly recommended but not required flag that identifies when a trace
          %% crosses a process boundary. True when the parent_span belongs to the
          %% same process as the current span.
          same_process_as_parent_span = undefined :: boolean() | undefined,

          %% An optional number of child spans that were generated while this span
          %% was active. If set, allows implementation to detect missing child spans.
          child_span_count = undefined            :: integer() | undefined
         }).

-record(link, {
          trace_id                  :: opentelemetry:trace_id(),
          span_id                   :: opentelemetry:span_id(),
          attributes                :: opentelemetry:attributes(),
          tracestate                :: opentelemetry:tracestate()
         }).

-record(message_event, {
          %% type of MessageEvent. Indicates whether the RPC message was sent or received.
          type = 'TYPE_UNSPECIFIED' :: opentelemetry:message_event_type(),

          %% identifier for the message, which must be unique in this span.
          id                        :: integer(),

          %% number of uncompressed bytes sent or received
          uncompressed_size         :: integer(),

          %% number of compressed bytes sent or received
          compressed_size           :: integer()
         }).

-record(annotation, {
          description      :: unicode:unicode_binary() | undefined,
          attributes       :: opentelemetry:attributes() | undefined
         }).

-record(status, {
          code    :: integer(),
          %% developer-facing error message
          message :: unicode:unicode_binary()
         }).
