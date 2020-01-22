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

-export([start_span/3,
         end_span/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_sampler.hrl").
-include("ot_span.hrl").

%% sampling bit is the first bit in 8-bit trace options
-define(IS_ENABLED(X), (X band 1) =/= 0).

-spec start_span(opentelemetry:span_name(), ot_span:start_opts(), ot_tracer_server:library_resource())
                -> {opentelemetry:span_ctx(), opentelemetry:span() | undefined}.
start_span(Name, Opts, LibraryResource) ->
    Parent = maps:get(parent, Opts, undefined),
    Attributes = maps:get(attributes, Opts, []),
    Links = maps:get(links, Opts, []),
    Kind = maps:get(kind, Opts, ?SPAN_KIND_UNSPECIFIED),
    Sampler = maps:get(sampler, Opts),
    SamplingHint = maps:get(sampler_hint, Opts, undefined),
    StartTime = maps:get(start_time, Opts, wts:timestamp()),
    new_span(Name, Parent, Sampler, SamplingHint, StartTime, Kind, Attributes, Links, LibraryResource).

%% if parent is undefined create a new trace id
new_span(Name, undefined, Sampler, SamplingHint, StartTime, Kind, Attributes, Links, LibraryResource) ->
    TraceId = opentelemetry:generate_trace_id(),
    Span = #span_ctx{trace_id=TraceId,
                     trace_flags=0},   
    new_span(Name, Span, Sampler, SamplingHint, StartTime, Kind, Attributes, Links, LibraryResource);
new_span(Name, Parent=#span_ctx{trace_id=TraceId,
                                tracestate=Tracestate,
                                span_id=ParentSpanId}, Sampler, SamplingHint, StartTime, Kind, Attributes, Links, LibraryResource) ->
    SpanId = opentelemetry:generate_span_id(),
    SpanCtx = Parent#span_ctx{span_id=SpanId},

    {TraceFlags, IsRecorded, SamplerAttributes} =
        sample(Sampler, TraceId, SpanId, case Parent of
                                             #span_ctx{span_id=undefined} ->
                                                 undefined;
                                             _ ->
                                                 Parent
                                         end,
               SamplingHint, Links, Name, Kind, Attributes),
    
    Span = #span{trace_id=TraceId,
                 span_id=SpanId,
                 tracestate=Tracestate,
                 start_time=StartTime,
                 parent_span_id=ParentSpanId,
                 kind=Kind,
                 name=Name,
                 attributes=Attributes++SamplerAttributes,
                 links=Links,
                 is_recorded=IsRecorded,
                 library_resource=LibraryResource},

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

sample({Sampler, Opts}, TraceId, SpanId, Parent, SamplingHint, Links, SpanName, Kind, Attributes) ->
    {Decision, Attributes} = Sampler(TraceId, SpanId, Parent, SamplingHint, 
                                     Links, SpanName, Kind, Attributes, Opts),
    case Decision of
        ?NOT_RECORD ->
            {0, false, Attributes};
        ?RECORD ->
            {0, true, Attributes};
        ?RECORD_AND_PROPAGATE ->
            {1, true, Attributes}
    end.
