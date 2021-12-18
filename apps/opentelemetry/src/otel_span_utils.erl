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
%% Functional interface for span_ctx and span records.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_span_utils).

-export([start_span/5,
         end_span/1,
         end_span/2]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_sampler.hrl").
-include("otel_span.hrl").

-spec start_span(otel_ctx:t(), opentelemetry:span_name(), otel_sampler:t(), otel_id_generator:t(),
                 otel_span:start_opts()) -> {opentelemetry:span_ctx(), opentelemetry:span() | undefined}.
start_span(Ctx, Name, Sampler, IdGenerator, Opts) ->
    SpanAttributeCountLimit = otel_span_limits:attribute_count_limit(),
    SpanAttributeValueLengthLimit= otel_span_limits:attribute_value_length_limit(),
    EventCountLimit = otel_span_limits:event_count_limit(),
    LinkCountLimit = otel_span_limits:link_count_limit(),
    AttributePerEventLimit = otel_span_limits:attribute_per_event_limit(),
    AttributePerLinkLimit = otel_span_limits:attribute_per_link_limit(),


    Attributes = otel_attributes:new(maps:get(attributes, Opts, #{}),
                                     SpanAttributeCountLimit,
                                     SpanAttributeValueLengthLimit),
    Links = otel_links:new(maps:get(links, Opts, []),
                           LinkCountLimit,
                           AttributePerLinkLimit,
                           SpanAttributeValueLengthLimit),
    Events = otel_events:new(EventCountLimit, AttributePerEventLimit, SpanAttributeValueLengthLimit),

    Kind = maps:get(kind, Opts, ?SPAN_KIND_INTERNAL),
    StartTime = maps:get(start_time, Opts, opentelemetry:timestamp()),
    new_span(Ctx, Name, Sampler, IdGenerator, StartTime, Kind, Attributes, Events, Links).

new_span(Ctx, Name, Sampler, IdGeneratorModule, StartTime, Kind, Attributes, Events, Links) ->
    {NewSpanCtx, ParentSpanId} = new_span_ctx(Ctx, IdGeneratorModule),

    TraceId = NewSpanCtx#span_ctx.trace_id,
    SpanId = NewSpanCtx#span_ctx.span_id,

    {TraceFlags, IsRecording, SamplerAttributes, TraceState} =
        sample(Ctx, Sampler, TraceId, Links, Name, Kind, otel_attributes:map(Attributes)),

    Span = #span{trace_id=TraceId,
                 span_id=SpanId,
                 tracestate=TraceState,
                 start_time=StartTime,
                 parent_span_id=ParentSpanId,
                 kind=Kind,
                 name=Name,
                 attributes=otel_attributes:set(SamplerAttributes, Attributes),
                 events=Events,
                 links=Links,
                 trace_flags=TraceFlags,
                 is_recording=IsRecording},

    {NewSpanCtx#span_ctx{trace_flags=TraceFlags,
                         is_valid=true,
                         is_recording=IsRecording}, Span}.

-spec new_span_ctx(otel_ctx:t(), otel_id_generator:t()) -> {opentelemetry:span_ctx(), opentelemetry:span_id()}.
new_span_ctx(Ctx, IdGeneratorModule) ->
    case otel_tracer:current_span_ctx(Ctx) of
        undefined ->
            {root_span_ctx(IdGeneratorModule), undefined};
        #span_ctx{is_valid=false} ->
            {root_span_ctx(IdGeneratorModule), undefined};
        ParentSpanCtx=#span_ctx{span_id=ParentSpanId} ->
            %% keep the rest of the parent span ctx, simply need to update the span_id
            {ParentSpanCtx#span_ctx{span_id=IdGeneratorModule:generate_span_id()}, ParentSpanId}
    end.

root_span_ctx(IdGeneratorModule) ->
    #span_ctx{trace_id=IdGeneratorModule:generate_trace_id(),
              span_id=IdGeneratorModule:generate_span_id(),
              is_valid=true,
              trace_flags=0}.

%%--------------------------------------------------------------------
%% @doc
%% Set the end time for a span if it hasn't been set before.
%% @end
%%--------------------------------------------------------------------
-spec end_span(opentelemetry:span()) -> opentelemetry:span().
end_span(Span=#span{end_time=undefined,
                    trace_flags=TraceFlags}) when ?IS_SAMPLED(TraceFlags) ->
    end_span(Span, undefined);
end_span(Span) ->
    Span.

-spec end_span(opentelemetry:span(), integer() | undefined) -> opentelemetry:span().
end_span(Span, Timestamp) when is_integer(Timestamp) ->
    Span#span{end_time=Timestamp};
end_span(Span, _) ->
    Span#span{end_time=opentelemetry:timestamp()}.
%%

sample(Ctx, Sampler, TraceId, Links, SpanName, Kind, Attributes) ->
    {Decision, NewAttributes, TraceState} = otel_sampler:should_sample(
        Sampler, Ctx, TraceId, Links, SpanName, Kind, Attributes
    ),
    case Decision of
        ?DROP ->
            {0, false, NewAttributes, TraceState};
        ?RECORD_ONLY ->
            {0, true, NewAttributes, TraceState};
        ?RECORD_AND_SAMPLE ->
            {1, true, NewAttributes, TraceState}
    end.
