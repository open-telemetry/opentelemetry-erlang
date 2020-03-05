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

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_sampler.hrl").
-include("ot_span.hrl").

%% sampling bit is the first bit in 8-bit trace options
-define(IS_ENABLED(X), (X band 1) =/= 0).

-spec start_span(opentelemetry:span_name(), ot_span:start_opts())
                -> {opentelemetry:span_ctx(), opentelemetry:span() | undefined}.
start_span(Name, Opts) ->
    Parent = maps:get(parent, Opts, undefined),
    Attributes = maps:get(attributes, Opts, []),
    Links = maps:get(links, Opts, []),
    Kind = maps:get(kind, Opts, ?SPAN_KIND_UNSPECIFIED),
    Sampler = maps:get(sampler, Opts),
    StartTime = maps:get(start_time, Opts, wts:timestamp()),
    new_span(Name, Parent, Sampler, StartTime, Kind, Attributes, Links).

%% if parent is undefined create a new trace id
new_span(Name, undefined, Sampler, StartTime, Kind, Attributes, Links) ->
    TraceId = opentelemetry:generate_trace_id(),
    Span = #span_ctx{trace_id=TraceId,
                     trace_flags=0},   
    new_span(Name, Span, Sampler, StartTime, Kind, Attributes, Links);
new_span(Name, Parent=#span_ctx{trace_id=TraceId,
                                tracestate=Tracestate,
                                span_id=ParentSpanId}, Sampler, StartTime, Kind, Attributes, Links) ->
    SpanId = opentelemetry:generate_span_id(),
    SpanCtx = Parent#span_ctx{span_id=SpanId},

    {TraceFlags, IsRecording, SamplerAttributes} =
        sample(Sampler, TraceId, SpanId, case Parent of
                                             #span_ctx{span_id=undefined} ->
                                                 undefined;
                                             _ ->
                                                 Parent
                                         end,
               Links, Name, Kind, Attributes),

    Span = #span{trace_id=TraceId,
                 span_id=SpanId,
                 tracestate=Tracestate,
                 start_time=StartTime,
                 parent_span_id=ParentSpanId,
                 kind=Kind,
                 name=Name,
                 attributes=Attributes++SamplerAttributes,
                 links=Links,
                 is_recording=IsRecording},

    {SpanCtx#span_ctx{trace_flags=TraceFlags,
                      is_recording=IsRecording}, Span}.

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

sample({Sampler, Opts}, TraceId, SpanId, Parent, Links, SpanName, Kind, Attributes) ->
    {Decision, NewAttributes} = Sampler(TraceId, SpanId, Parent, Links,
                                        SpanName, Kind, Attributes, Opts),
    case Decision of
        ?NOT_RECORD ->
            {0, false, NewAttributes};
        ?RECORD ->
            {0, true, NewAttributes};
        ?RECORD_AND_PROPAGATE ->
            {1, true, NewAttributes}
    end.
