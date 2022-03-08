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

-export([start_span/3,
         end_span/1,
         end_span/2]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_sampler.hrl").
-include("otel_span.hrl").

-spec start_span(otel_ctx:t(), opentelemetry:span_name(), otel_span:start_opts())
                -> {opentelemetry:span_ctx(), opentelemetry:span() | undefined}.
start_span(Ctx, Name, Opts) ->
    io:format("otel_span_utils/start_span ~p ~p ~p~n", [Ctx, Name, Opts]),
    Attributes = maps:get(attributes, Opts, []),
    Links = maps:get(links, Opts, []),
    Kind = maps:get(kind, Opts, ?SPAN_KIND_INTERNAL),
    Sampler = maps:get(sampler, Opts),
    StartTime = maps:get(start_time, Opts, opentelemetry:timestamp()),
    io:format("[otel_span_utils/start_span]~nCTX: ~p~nName: ~p~nSampler: ~p~nKind: ~p~nAttributes: ~p~n", [Ctx, Name, Sampler, Kind, Attributes]),
    new_span(Ctx, Name, Sampler, StartTime, Kind, Attributes, Links).

new_span(Ctx, Name, Sampler, StartTime, Kind, Attributes, Links) ->
    {NewSpanCtx, ParentSpanId} = new_span_ctx(Ctx),

    TraceId = NewSpanCtx#span_ctx.trace_id,
    SpanId = NewSpanCtx#span_ctx.span_id,

    {TraceFlags, IsRecording, SamplerAttributes, TraceState} =
        sample(Ctx, Sampler, TraceId, Links, Name, Kind, Attributes),

    Span = #span{trace_id=TraceId,
                 span_id=SpanId,
                 tracestate=TraceState,
                 start_time=StartTime,
                 parent_span_id=ParentSpanId,
                 kind=Kind,
                 name=Name,
                 attributes=Attributes++SamplerAttributes,
                 links=Links,
                 trace_flags=TraceFlags,
                 is_recording=IsRecording},

    {NewSpanCtx#span_ctx{trace_flags=TraceFlags,
                         is_valid=true,
                         is_recording=IsRecording}, Span}.

-spec new_span_ctx(otel_ctx:t()) -> {opentelemetry:span_ctx(), opentelemetry:span_id()}.
new_span_ctx(Ctx) ->
    case otel_tracer:current_span_ctx(Ctx) of
        undefined ->
            {root_span_ctx(), undefined};
        #span_ctx{is_valid=false} ->
            {root_span_ctx(), undefined};
        ParentSpanCtx=#span_ctx{span_id=ParentSpanId} ->
            %% keep the rest of the parent span ctx, simply need to update the span_id
            {ParentSpanCtx#span_ctx{span_id=opentelemetry:generate_span_id()}, ParentSpanId}
    end.

root_span_ctx() ->
    #span_ctx{trace_id=opentelemetry:generate_trace_id(),
              span_id=opentelemetry:generate_span_id(),
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
