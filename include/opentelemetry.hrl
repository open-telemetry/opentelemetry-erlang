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
-define(IS_SPAN_ENABLED(TraceOptions), (TraceOptions band 1) =/= 0).

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
          trace_flags = 1   :: integer() | undefined,
          %% Tracestate represents tracing-system specific context in a list of key-value pairs.
          %% Tracestate allows different vendors propagate additional information and
          %% inter-operate with their legacy Id formats.
          tracestate        :: opentelemetry:tracestate() | undefined,
          %% IsValid is a boolean flag which returns true if the SpanContext has a non-zero
          %% TraceID and a non-zero SpanID.
          is_valid          :: boolean() | undefined,
          %% true if the span context came from a remote process
          is_remote         :: boolean() | undefined,
          %% this field is not propagated and is only here as an implementation optimization
          %% If true updates like adding events are done on the span. The same as if the
          %% trace flags lowest bit is 1 but simply not propagated.
          is_recording      :: boolean() | undefined
         }).

-record(link, {
          trace_id   :: opentelemetry:trace_id(),
          span_id    :: opentelemetry:span_id(),
          attributes :: opentelemetry:attributes(),
          tracestate :: opentelemetry:tracestate()
         }).

-record(event, {
          system_time_nano :: non_neg_integer(),
          name             :: unicode:unicode_binary(),
          attributes = []  :: opentelemetry:attributes()
         }).

-record(status, {
          code    :: atom() | integer(),
          %% developer-facing error message
          message :: unicode:unicode_binary()
         }).

-define(OTEL_STATUS_OK, 'Ok').
-define(OTEL_STATUS_CANCELLED, 'Cancelled').
-define(OTEL_STATUS_UNKNOWN, 'UnknownError').
-define(OTEL_STATUS_INVALID_ARGUMENT, 'InvalidArgument').
-define(OTEL_STATUS_DEADLINE_EXCEEDED, 'DeadlineExceeded').
-define(OTEL_STATUS_NOT_FOUND, 'NotFound').
-define(OTEL_STATUS_ALREADY_EXISTS , 'AlreadyExists').
-define(OTEL_STATUS_PERMISSION_DENIED, 'PermissionDenied').
-define(OTEL_STATUS_RESOURCE_EXHAUSTED, 'ResourceExhausted').
-define(OTEL_STATUS_FAILED_PRECONDITION, 'FailedPrecondition').
-define(OTEL_STATUS_ABORTED, 'Aborted').
-define(OTEL_STATUS_OUT_OF_RANGE, 'OutOfRange').
-define(OTEL_STATUS_UNIMPLEMENTED, 'Unimplemented').
-define(OTEL_STATUS_INTERNAL, 'InternalError').
-define(OTEL_STATUS_UNAVAILABLE, 'Unavailable').
-define(OTEL_STATUS_DATA_LOSS, 'DataLoss').
-define(OTEL_STATUS_UNAUTHENTICATED, 'Unauthenticated').
