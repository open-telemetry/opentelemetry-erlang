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
-define(IS_SAMPLED(TraceFlags), (TraceFlags band 1) =/= 0).

-define(SPAN_KIND_INTERNAL, internal).
-define(SPAN_KIND_SERVER, server).
-define(SPAN_KIND_CLIENT, client).
-define(SPAN_KIND_PRODUCER, producer).
-define(SPAN_KIND_CONSUMER, consumer).

-define(OTEL_STATUS_UNSET, unset).
-define(OTEL_STATUS_OK, ok).
-define(OTEL_STATUS_ERROR, error).

%% Holds information about the instrumentation library specified when
%% getting a Tracer from the TracerProvider.
-record(instrumentation_library, {name       :: unicode:unicode_binary() | undefined,
                                  version    :: unicode:unicode_binary() | undefined,
                                  schema_url :: uri_string:uri_string() | undefined}).

-record(span_ctx, {
          %% 128 bit int trace id
          trace_id          :: opentelemetry:trace_id(),
          %% 64 bit int span id
          span_id           :: opentelemetry:span_id(),
          %% 8-bit integer, lowest bit is if it is sampled
          trace_flags = 0   :: integer() | undefined,
          %% Tracestate represents tracing-system specific context in a list of key-value pairs.
          %% Tracestate allows different vendors propagate additional information and
          %% inter-operate with their legacy Id formats.
          tracestate = []   :: opentelemetry:tracestate(),
          %% IsValid is a boolean flag which returns true if the SpanContext has a non-zero
          %% TraceID and a non-zero SpanID.
          is_valid          :: boolean() | undefined,
          %% true if the span context came from a remote process
          %% defaults to false and the propagator must set to true when extracting
          is_remote = false :: boolean(),
          %% this field is not propagated and is only here as an implementation optimization
          %% If true updates like adding events are done on the span. The same as if the
          %% trace flags lowest bit is 1 but simply not propagated.
          is_recording      :: boolean() | undefined,

          %% the sdk must put a module to call in the SDK for span operations
          %% and include an optional term of configuration it needs
          span_sdk          :: {module(), term()} | undefined
         }).

-record(status, {
          code = ?OTEL_STATUS_UNSET :: opentelemetry:status_code(),
          %% developer-facing error message
          message = <<"">>          :: unicode:unicode_binary()
         }).
