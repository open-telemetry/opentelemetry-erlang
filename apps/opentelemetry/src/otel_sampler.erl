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
%% A sampler is a function run on each started span that returns whether to
%% record and propagate, only record or not record the span.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_sampler).

-export([description/1, new/1, should_sample/7]).

-export_type([
    description/0,
    sampler_config/0,
    sampler_opts/0,
    sampling_decision/0,
    sampling_result/0,
    t/0
]).

-callback setup(sampler_opts()) -> sampler_config().

-callback description(sampler_config()) -> description().

-callback should_sample(
    otel_ctx:t(),
    opentelemetry:trace_id(),
    opentelemetry:links(),
    opentelemetry:span_name(),
    opentelemetry:span_kind(),
    opentelemetry:attributes(),
    sampler_config()
) -> sampling_result().

-include("otel_sampler.hrl").

-type description() :: unicode:unicode_binary().
-type sampler_config() :: term().
-type sampler_opts() :: term().
-type sampler_spec() :: {module(), sampler_opts()}.
-type sampling_decision() :: ?DROP | ?RECORD_ONLY | ?RECORD_AND_SAMPLE.
-type sampling_result() :: {
    sampling_decision(), opentelemetry:attributes(), opentelemetry:tracestate()
}.
-opaque t() :: {module(), description(), sampler_opts()}.

-spec new(sampler_spec()) -> t().
new({Sampler, Opts}) ->
    Config = Sampler:setup(Opts),
    {Sampler, Sampler:description(Config), Config}.

-spec should_sample(
    t(),
    otel_ctx:t(),
    opentelemetry:trace_id(),
    opentelemetry:links(),
    opentelemetry:span_name(),
    opentelemetry:span_kind(),
    opentelemetry:attributes()
) -> sampling_result().
should_sample({Sampler, _, Config}, Ctx, TraceId, Links, SpanName, Kind, Attributes) ->
    Sampler:should_sample(Ctx, TraceId, Links, SpanName, Kind, Attributes, Config).

-spec description(t()) -> description().
description({_, Description, _}) -> Description.
