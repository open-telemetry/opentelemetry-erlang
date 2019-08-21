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

-export([start_span/1,
         start_span/2,
         end_span/1]).

-include("opentelemetry.hrl").

-type start_opts() :: #{parent => undefined | opentelemetry:span() | opentelemetry:span_ctx(),
                        sampler => module(),
                        links => opentelemetry:links(),
                        is_recorded => boolean(),
                        kind => opentelemetry:span_kind()}.

-export_type([start_opts/0]).

%% sampling bit is the first bit in 8-bit trace options
-define(IS_ENABLED(X), (X band 1) =/= 0).

-spec start_span(opentelemetry:span_name()) -> {opentelemetry:span_ctx(), opentelemetry:span()}.
start_span(Name) ->
    start_span(Name, #{}).

-spec start_span(opentelemetry:span_name(), start_opts())
                -> {opentelemetry:span_ctx(), opentelemetry:span() | undefined}.
start_span(Name, Opts) ->
    Parent = maps:get(parent, Opts, undefined),
    Attributes = maps:get(attributes, Opts, []),
    Links = maps:get(links, Opts, []),
    Kind = maps:get(kind, Opts, ?SPAN_KIND_UNSPECIFIED),

    %% TODO: support overriding the sampler
    _Sampler = maps:get(sampler, Opts, undefined),

    new_span(Name, Parent, Kind, Attributes, Links).

%% if parent is undefined, first run sampler
new_span(Name, undefined, Kind, Attributes, Links) ->
    TraceId = opentelemetry:generate_trace_id(),
    Span = #span_ctx{trace_id=TraceId,
                     trace_options=0},
    TraceOptions = update_trace_options(should_sample, Span),
    new_span(Name, Span#span_ctx{trace_options=TraceOptions}, Kind, Attributes, Links);
%% if parent is remote, first run sampler
%% new_span_(Name, Span=#span_ctx{}, Kind, Attributes) %% when RemoteParent =:= true
%%                                              ->
%%     TraceOptions = update_trace_options(should_sample, Span),
%%     new_span_(Name, Span#span_ctx{trace_options=TraceOptions}, Kind, Attributes);
new_span(Name, Parent=#span_ctx{trace_id=TraceId,
                                 trace_options=TraceOptions,
                                 tracestate=Tracestate,
                                 span_id=ParentSpanId}, Kind, Attributes, Links)
  when ?IS_ENABLED(TraceOptions) ->
    SpanId = opentelemetry:generate_span_id(),
    Span = #span{trace_id=TraceId,
                 span_id=SpanId,
                 tracestate=Tracestate,
                 start_time=wts:timestamp(),
                 parent_span_id=ParentSpanId,
                 kind=Kind,
                 name=Name,
                 attributes=Attributes,
                 links=Links},
    {Parent#span_ctx{span_id=SpanId}, Span};
new_span(_Name, Parent, _Kind, _, _) ->
    SpanId = opentelemetry:generate_span_id(),
    %% since discarded by sampler, create no span
    {Parent#span_ctx{span_id=SpanId}, undefined}.

%%
update_trace_options(should_sample, #span_ctx{trace_id=_TraceId,
                                              span_id=_ParentSpanId,
                                              trace_options=_ParentTraceOptions}) ->
    1.
    %% case oc_sampler:should_sample(TraceId, ParentSpanId, ?IS_ENABLED(ParentTraceOptions)) of
    %%     true ->
    %%         1;
    %%     false ->
    %%         0
    %% end.

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
