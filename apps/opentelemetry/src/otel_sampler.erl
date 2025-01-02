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
%% @doc Behaviour for defining samplers.
%%
%% A sampler should provide a function run on each started span that returns whether to
%% record and propagate, only record, or not record the span.
%%
%% For more information on the concept of <i>Sampling</i>, see
%% <a href="https://opentelemetry.io/docs/concepts/sampling/">Sampling in the OpenTelemetry
%% documentation</a> or the
%% <a href="https://opentelemetry.io/docs/specs/otel/trace/sdk/#sampling">Sampling spec</a>.
%% For examples of configuring samplers or implementing your own sampler,
%% see <a href="https://opentelemetry.io/docs/languages/erlang/sampling/">the OpenTelemetry
%% Erlang documentation</a>.
%%
%% <h3>Configuration</h3>
%%
%% To configure sampling for the `opentelemetry' application, see
%% <a href="https://hexdocs.pm/opentelemetry/readme.html#samplers">the documentation</a>.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_sampler).

-export([description/1, new/1, should_sample/7]).

-export_type([description/0,
              sampler_spec/0,
              sampler_config/0,
              sampler_opts/0,
              sampling_decision/0,
              sampling_result/0,
              t/0]).

-callback setup(sampler_opts()) -> sampler_config().
%% Called when a sampler is created to set up the sampler.
%% Should return the sampler configuration that is then passed to other callbacks.

-callback description(sampler_config()) -> description().
%% Should return the description of the sampler.

-callback should_sample(
    otel_ctx:t(),
    opentelemetry:trace_id(),
    otel_links:t(),
    opentelemetry:span_name(),
    opentelemetry:span_kind(),
    opentelemetry:attributes_map(),
    sampler_config()
) -> sampling_result().
%% Main callback that determines whether a span should be sampled.\

-include("otel_sampler.hrl").

-type description() :: unicode:unicode_binary().
%% The description of the sampler.

-type sampler_config() :: term().
%% Any term used to configured a given sampler.

-type sampler_opts() :: term().
%% Any options passed to a sampler.

-type builtin_sampler() ::
    always_on
    | always_off
    | {trace_id_ratio_based, float()}
    | {parent_based, #{
        remote_parent_sampled => sampler_spec(),
        remote_parent_not_sampled => sampler_spec(),
        local_parent_sampled => sampler_spec(),
        local_parent_not_sampled => sampler_spec(),
        root => sampler_spec()
    }}.
%% A built-in sampler.

-type sampler_spec() :: builtin_sampler() | {module(), sampler_opts()}.
%% Specification to create a sampler.

-type sampling_decision() :: ?DROP | ?RECORD_ONLY | ?RECORD_AND_SAMPLE.
%% The decision that a sampler can make on a given span.

-type sampling_result() :: {
                            sampling_decision(),
                            opentelemetry:attributes_map(),
                            opentelemetry:tracestate() | otel_tracestate:members()
                           }.
%% The result of a sampling decision.

-opaque t() :: {module(), description(), sampler_opts()}.
%% A sampler.

%% @doc Returns a sampler based on the given specification.
-spec new(SamplerSpec :: sampler_spec()) -> t().
new(always_on) ->
    new({otel_sampler_always_on, #{}});
new(always_off) ->
    new({otel_sampler_always_off, #{}});
new({trace_id_ratio_based, Opts}) ->
    new({otel_sampler_trace_id_ratio_based, Opts});
new({parent_based, Opts}) ->
    new({otel_sampler_parent_based, Opts});
new({Sampler, Opts}) ->
    Config = Sampler:setup(Opts),
    {Sampler, Sampler:description(Config), Config}.

%% @private
-spec should_sample(
    t(),
    otel_ctx:t(),
    opentelemetry:trace_id(),
    otel_links:t(),
    opentelemetry:span_name(),
    opentelemetry:span_kind(),
    opentelemetry:attributes_map()
) -> sampling_result().
should_sample({Sampler, _, Config}, Ctx, TraceId, Links, SpanName, Kind, Attributes) ->
    case Sampler:should_sample(Ctx, TraceId, Links, SpanName, Kind, Attributes, Config) of
        %% to support backwards compatibility with when `tracestate' was just a list, not
        %% a record, we accept a list and create a new `tracestate'
        {Decision, Attributes, Tracestate} when is_list(Tracestate) ->
            {Decision, Attributes, otel_tracestate:new(Tracestate)};
        Result ->
            Result
    end.

%% @doc Returns the description of the given sampler.
-spec description(t()) -> description().
description(_Sampler = {_, Description, _}) -> Description.
