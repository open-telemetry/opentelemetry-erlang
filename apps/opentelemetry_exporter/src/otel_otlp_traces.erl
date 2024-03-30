%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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
-module(otel_otlp_traces).

-export([to_proto/2]).

%% for testing
-ifdef(TEST).
-export([to_proto_by_instrumentation_scope/1,
         to_proto/1]).
-endif.

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-spec to_proto(ets:table(), otel_resource:t()) -> opentelemetry_exporter_trace_service_pb:export_trace_service_request() | empty.
to_proto(Tab, Resource) ->
    case to_proto_by_instrumentation_scope(Tab) of
        [] ->
            empty;
        InstrumentationScopeSpans ->
            Attributes = otel_resource:attributes(Resource),
            ResourceSpans = #{resource => #{attributes => otel_otlp_common:to_attributes(Attributes),
                                            dropped_attributes_count => otel_attributes:dropped(Attributes)},
                              scope_spans => InstrumentationScopeSpans},
            case otel_resource:schema_url(Resource) of
                undefined ->
                    #{resource_spans => [ResourceSpans]};
                SchemaUrl ->
                    #{resource_spans => [ResourceSpans#{schema_url => SchemaUrl}]}
            end
    end.

to_proto_by_instrumentation_scope(Tab) ->
    Key = ets:first(Tab),
    to_proto_by_instrumentation_scope(Tab, Key).

to_proto_by_instrumentation_scope(_Tab, '$end_of_table') ->
    [];
to_proto_by_instrumentation_scope(Tab, InstrumentationScope) ->
    InstrumentationScopeSpans = lists:foldl(fun(Span, Acc) ->
                                                      [to_proto(Span) | Acc]
                                              end, [], ets:lookup(Tab, InstrumentationScope)),
    InstrumentationScopeSpansProto = otel_otlp_common:to_instrumentation_scope_proto(InstrumentationScope),
    [InstrumentationScopeSpansProto#{spans => InstrumentationScopeSpans}
    | to_proto_by_instrumentation_scope(Tab, ets:next(Tab, InstrumentationScope))].

%% TODO: figure out why this type spec fails
%% -spec to_proto(#span{}) -> opentelemetry_exporter_trace_service_pb:span().

to_proto(#span{trace_id=TraceId,
               span_id=SpanId,
               tracestate=TraceState,
               parent_span_id=MaybeParentSpanId,
               name=Name,
               kind=Kind,
               start_time=StartTime,
               end_time=EndTime,
               attributes=Attributes,
               events=TimedEvents,
               links=Links,
               status=Status,
               trace_flags=_TraceFlags,
               is_recording=_IsRecording}) when is_integer(TraceId),
                                                is_integer(SpanId) ->
    ParentSpanId = case MaybeParentSpanId of _ when is_integer(MaybeParentSpanId) -> <<MaybeParentSpanId:64>>; _ -> <<>> end,
    #{name                     => otel_otlp_common:to_binary(Name),
      trace_id                 => <<TraceId:128>>,
      span_id                  => <<SpanId:64>>,
      parent_span_id           => ParentSpanId,
      %% eqwalizer:ignore have to have tracestate as type '_' for matchspecs
      trace_state              => otel_tracestate:encode_header(TraceState),
      kind                     => to_otlp_kind(Kind),
      %% eqwalizer:ignore have to allow value '$2' for matchspecs
      start_time_unix_nano     => opentelemetry:timestamp_to_nano(StartTime),
      %% eqwalizer:ignore have to allow value '_' for matchspecs
      end_time_unix_nano       => opentelemetry:timestamp_to_nano(EndTime),
      %% eqwalizer:ignore have to allow value '_' for matchspecs
      attributes               => otel_otlp_common:to_attributes(Attributes),
      dropped_attributes_count => otel_attributes:dropped(Attributes),
      events                   => to_events(otel_events:list(TimedEvents)),
      dropped_events_count     => otel_events:dropped(TimedEvents),
      links                    => to_links(otel_links:list(Links)),
      dropped_links_count      => otel_links:dropped(Links),
      %% eqwalizer:ignore have to allow value '_' for matchspecs
      status                   => to_status(Status)}.

-spec to_status(opentelemetry:status() | undefined) -> opentelemetry_exporter_trace_service_pb:status().
to_status(#status{code=Code,
                  message=Message}) ->
    #{code => to_otlp_status(Code),
      message => Message};
to_status(_) ->
    #{}.

-spec to_events([#event{}]) -> [opentelemetry_exporter_trace_service_pb:event()].
to_events(Events) ->
    [#{time_unix_nano => opentelemetry:timestamp_to_nano(Timestamp),
       name => otel_otlp_common:to_binary(Name),
       attributes => otel_otlp_common:to_attributes(Attributes)}
     || #event{system_time_native=Timestamp,
               name=Name,
               attributes=Attributes} <- Events].

-spec to_links([#link{}]) -> [opentelemetry_exporter_trace_service_pb:link()].
to_links(Links) ->
    [#{trace_id => <<TraceId:128>>,
       span_id => <<SpanId:64>>,
       trace_state => otel_tracestate:encode_header(TraceState),
       attributes => otel_otlp_common:to_attributes(Attributes),
       dropped_attributes_count => 0} || #link{trace_id=TraceId,
                                               span_id=SpanId,
                                               attributes=Attributes,
                                               tracestate=TraceState} <- Links].

-spec to_otlp_kind(atom()) -> opentelemetry_exporter_trace_service_pb:'span.SpanKind'().
to_otlp_kind(?SPAN_KIND_INTERNAL) ->
    'SPAN_KIND_INTERNAL';
to_otlp_kind(?SPAN_KIND_SERVER) ->
    'SPAN_KIND_SERVER';
to_otlp_kind(?SPAN_KIND_CLIENT) ->
    'SPAN_KIND_CLIENT';
to_otlp_kind(?SPAN_KIND_PRODUCER) ->
    'SPAN_KIND_PRODUCER';
to_otlp_kind(?SPAN_KIND_CONSUMER) ->
    'SPAN_KIND_CONSUMER';
to_otlp_kind(_) ->
    'SPAN_KIND_UNSPECIFIED'.

-spec to_otlp_status(atom()) -> opentelemetry_exporter_trace_service_pb:'status.StatusCode'().
to_otlp_status(?OTEL_STATUS_UNSET) ->
    'STATUS_CODE_UNSET';
to_otlp_status(?OTEL_STATUS_OK) ->
    'STATUS_CODE_OK';
to_otlp_status(?OTEL_STATUS_ERROR) ->
    'STATUS_CODE_ERROR'.
