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
-module(ot_span_utils).

-export([start_span/2,
         end_span/1]).

-include("opentelemetry.hrl").
-include("ot_sampler.hrl").

-type start_opts() :: #{parent => undefined | opentelemetry:span() | opentelemetry:span_ctx(),
                        sampler := ot_sampler:sampler(),
                        sampling_hint => ot_sampler:sampling_decision(),
                        links => opentelemetry:links(),
                        is_recorded => boolean(),
                        kind => opentelemetry:span_kind()}.

-export_type([start_opts/0]).

%% sampling bit is the first bit in 8-bit trace options
-define(IS_ENABLED(X), (X band 1) =/= 0).

-spec start_span(opentelemetry:span_name(), start_opts())
                -> {opentelemetry:span_ctx(), opentelemetry:span() | undefined}.
start_span(Name, Opts) ->
    Parent = maps:get(parent, Opts, undefined),
    Attributes = maps:get(attributes, Opts, []),
    Links = maps:get(links, Opts, []),
    Kind = maps:get(kind, Opts, ?SPAN_KIND_UNSPECIFIED),
    Sampler = maps:get(sampler, Opts),
    SamplingHint = maps:get(sampler_hint, Opts, undefined),
    new_span(Name, Parent, Sampler, SamplingHint, Kind, Attributes, Links).

%% if parent is undefined create a new trace id
new_span(Name, undefined, Sampler, SamplingHint, Kind, Attributes, Links) ->
    TraceId = opentelemetry:generate_trace_id(),
    Span = #span_ctx{trace_id=TraceId,
                     trace_flags=0},
    new_span(Name, Span, Sampler, SamplingHint, Kind, Attributes, Links);
new_span(Name, Parent=#span_ctx{trace_id=TraceId,
                                tracestate=Tracestate,
                                span_id=ParentSpanId}, Sampler, SamplingHint, Kind, Attributes, Links) ->
    SpanId = opentelemetry:generate_span_id(),
    SpanCtx = Parent#span_ctx{span_id=SpanId},

    {TraceFlags, IsRecorded, SamplerAttributes} = sample(Sampler, TraceId, SpanId, Parent,
                                                         SamplingHint, Links, Name, Kind, Attributes),

    Span = #span{trace_id=TraceId,
                 span_id=SpanId,
                 tracestate=Tracestate,
                 start_time=wts:timestamp(),
                 parent_span_id=ParentSpanId,
                 kind=Kind,
                 name=Name,
                 attributes=Attributes ++ SamplerAttributes,
                 links=Links,
                 is_recorded=IsRecorded},

    {SpanCtx#span_ctx{trace_flags=TraceFlags,
                      is_recorded=IsRecorded}, Span}.

%%--------------------------------------------------------------------
%% @doc
%% Set the end time for a span if it hasn't been set before.
%% @end
%%--------------------------------------------------------------------
-spec end_span(opentelemetry:span()) -> opentelemetry:span().
end_span(Span=#span{end_time=undefined,
                    trace_options=TraceOptions}) when ?IS_ENABLED(TraceOptions) ->
    EndTime = wts:timestamp(),
    Span#span{end_time=EndTime};
end_span(Span) ->
    Span.

%%

sample(Sampler, TraceId, SpanId, Parent, SamplingHint, Links, SpanName, Kind, Attributes) ->
    {Decision, Attributes} = Sampler(TraceId, SpanId, Parent, SamplingHint,
                                     Links, SpanName, Kind, Attributes),
    case Decision of
        ?NOT_RECORD ->
            {0, false, Attributes};
        ?RECORD ->
            {0, true, Attributes};
        ?RECORD_AND_PROPAGATE ->
            {1, true, Attributes}
    end.
