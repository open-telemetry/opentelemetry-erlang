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
%% This sampler makes the decision based on the parent, with the following possibilities:
%% 1) a remote parent that is sampled (by default always_on);
%% 2) a remote parent that is not sampled (by default always_off);
%% 3) a local parent that is sampled (by default always_on);
%% 4) a local parent that is not sampled (by default always_off);
%% 5) no parent (by default always_on).
%%
%% For each of these cases a different sampler can be configured.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_sampler_parent_based).

-behavior(otel_sampler).

-export([description/1, setup/1, should_sample/7]).

-export_type([opts/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-type opts() :: #{
    remote_parent_sampled => otel_sampler:sampler_spec(),
    remote_parent_not_sampled => otel_sampler:sampler_spec(),
    local_parent_sampled => otel_sampler:sampler_spec(),
    local_parent_not_sampled => otel_sampler:sampler_spec(),
    root => otel_sampler:sampler_spec()
}.

setup(Opts = #{root := RootSpec}) ->
    RemoteParentSampledSampler = sampler_for_spec(remote_parent_sampled, Opts, always_on),
    RemoteParentNotSampledSampler = sampler_for_spec(remote_parent_not_sampled, Opts, always_off),
    LocalParentSampledSampler = sampler_for_spec(local_parent_sampled, Opts, always_on),
    LocalParentNotSampledSampler = sampler_for_spec(local_parent_not_sampled, Opts, always_off),
    RootSampler = otel_sampler:new(RootSpec),
    #{
        root => RootSampler,
        remote_parent_sampled => RemoteParentSampledSampler,
        remote_parent_not_sampled => RemoteParentNotSampledSampler,
        local_parent_sampled => LocalParentSampledSampler,
        local_parent_not_sampled => LocalParentNotSampledSampler
    };
setup(Opts) ->
    ?LOG_INFO("No sampler spec found for parent_based 'root' option. The 'always_on' sampler will be used for root spans."),
    setup(Opts#{root => always_on}).

sampler_for_spec(Key, Opts, DefaultModule) ->
    Spec = maps:get(Key, Opts, DefaultModule),
    otel_sampler:new(Spec).

description(#{
    root := RootSampler,
    remote_parent_sampled := RemoteParentSampler,
    remote_parent_not_sampled := RemoteParentNotSampler,
    local_parent_sampled := LocalParentSampler,
    local_parent_not_sampled := LocalParentNotSampler
}) ->
    <<"ParentBased{root:", (otel_sampler:description(RootSampler))/binary, ",remoteParentSampled:",
        (otel_sampler:description(RemoteParentSampler))/binary, ",remoteParentNotSampled:",
        (otel_sampler:description(RemoteParentNotSampler))/binary, ",localParentSampled:",
        (otel_sampler:description(LocalParentSampler))/binary, ",localParentNotSampled:",
        (otel_sampler:description(LocalParentNotSampler))/binary, "}">>.

should_sample(Ctx, TraceId, Links, SpanName, SpanKind, Attributes, Config) ->
    ParentSpanCtx = otel_tracer:current_span_ctx(Ctx),
    SamplerKey = parent_based_sampler(ParentSpanCtx),
    {Sampler, _Description, SamplerOpts} = maps:get(SamplerKey, Config),
    Sampler:should_sample(Ctx, TraceId, Links, SpanName, SpanKind, Attributes, SamplerOpts).

%% remote parent sampled
parent_based_sampler(#span_ctx{trace_flags = TraceFlags, is_remote = true}) when
    ?IS_SAMPLED(TraceFlags)
->
    remote_parent_sampled;
%% remote parent not sampled
parent_based_sampler(#span_ctx{is_remote = true}) ->
    remote_parent_not_sampled;
%% local parent sampled
parent_based_sampler(#span_ctx{trace_flags = TraceFlags}) when
    ?IS_SAMPLED(TraceFlags)
->
    local_parent_sampled;
%% local parent not sampled
parent_based_sampler(#span_ctx{}) ->
    local_parent_not_sampled;
%% root
parent_based_sampler(_SpanCtx) ->
    root.
