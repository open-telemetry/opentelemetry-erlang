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

-export([new/3,
         setup/1,
         get_description/1,
         always_on/7,
         always_off/7,
         parent_based/7,
         trace_id_ratio_based/7]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_sampler.hrl").
-include("otel_span.hrl").

-callback setup(sampler_opts()) -> t().

-type sampling_decision() :: ?DROP | ?RECORD_ONLY | ?RECORD_AND_SAMPLE.
-type sampling_result() :: {sampling_decision(), opentelemetry:attributes(), opentelemetry:tracestate()}.
-type description() :: unicode:unicode_binary().
-type decider() :: fun((otel_ctx:t(),
                        opentelemetry:trace_id(),
                        opentelemetry:links(),
                        opentelemetry:span_name(),
                        opentelemetry:span_kind(),
                        opentelemetry:attributes(),
                        term()) -> sampling_result()).
-type sampler() :: {decider(), description(), sampler_opts()}.
-type sampler_opts() :: term().
-opaque t() :: sampler().
-export_type([decider/0,
              description/0,
              sampling_result/0,
              sampling_decision/0,
              sampler_opts/0,
              t/0]).

-define(MAX_VALUE, 9223372036854775807). %% 2^63 - 1

-spec new(decider(), description(), sampler_opts()) -> t().
new(DecisionFunction, Description, SamplerOpts) ->
    {DecisionFunction, Description, SamplerOpts}.

-spec setup(atom() | {atom() | module(), sampler_opts()}) -> t().
setup({Sampler, Opts}) ->
    setup(Sampler, Opts);
setup(Sampler) when is_atom(Sampler) ->
    setup(Sampler, #{}).

setup(always_on, Opts) ->
    {fun ?MODULE:always_on/7, description(always_on, Opts), []};
setup(always_off, Opts) ->
    {fun ?MODULE:always_off/7, description(always_off, Opts), []};
setup(parent_based, Opts) ->
    {Config, Description} = parent_based_config(Opts),
    {fun ?MODULE:parent_based/7, Description, Config};
setup(trace_id_ratio_based, Probability) ->
    IdUpperBound = case Probability of
                       P when P =:= 0.0 ->
                           0;
                       P when P =:= 1.0 ->
                           ?MAX_VALUE;
                       P when P >= 0.0 andalso P =< 1.0 ->
                           P * ?MAX_VALUE
                   end,
    {fun ?MODULE:trace_id_ratio_based/7, description(trace_id_ratio_based, Probability), IdUpperBound};
setup(Sampler, Opts) ->
    Sampler:setup(Opts).

always_on(Ctx, _TraceId, _Links, _SpanName, _Kind, _Attributes, _Opts) ->
    {?RECORD_AND_SAMPLE, [], tracestate(Ctx)}.

always_off(Ctx, _TraceId, _Links, _SpanName, _Kind, _Attributes, _Opts) ->
    {?DROP, [], tracestate(Ctx)}.

-spec get_description(sampler()) -> description().
get_description({_Fun, Description, _Opts}) ->
    Description.

parent_based_config(Opts=#{root := {RootSampler, RootOpts}})
  when is_atom(RootSampler)->
    {RemoteParentSampled, RemoteParentSampledOpts}
        = maps:get(remote_parent_sampled, Opts, {always_on, #{}}),
    {RemoteParentNotSampled, RemoteParentNotSampledOpts}
        = maps:get(remote_parent_not_sampled, Opts, {always_off, #{}}),
    {LocalParentSampled, LocalParentSampledOpts}
        = maps:get(local_parent_sampled, Opts, {always_on, #{}}),
    {LocalParentNotSampled, LocalParentNotSampledOpts}
        = maps:get(local_parent_not_sampled, Opts, {always_off, #{}}),

    ParentBasedConfig = #{root => setup(RootSampler, RootOpts),
                          remote_parent_sampled =>
                              setup(RemoteParentSampled, RemoteParentSampledOpts),
                          remote_parent_not_sampled =>
                              setup(RemoteParentNotSampled, RemoteParentNotSampledOpts),
                          local_parent_sampled =>
                              setup(LocalParentSampled, LocalParentSampledOpts),
                          local_parent_not_sampled =>
                              setup(LocalParentNotSampled, LocalParentNotSampledOpts)},
    {ParentBasedConfig, description(parent_based, ParentBasedConfig)};
parent_based_config(Opts) ->
    ?LOG_INFO("no root opt found for sampler parent_based. always_on will be used for root spans"),
    parent_based_config(Opts#{root => {always_on, #{}}}).

parent_based(Ctx, TraceId, Links, SpanName, Kind, Attributes, Opts) ->
    ParentSpanCtx = otel_tracer:current_span_ctx(Ctx),
    {Sampler, _Description, SamplerOpts} = parent_based_sampler(ParentSpanCtx, Opts),
    Sampler(Ctx, TraceId, Links, SpanName, Kind, Attributes, SamplerOpts).

%% remote parent sampled
parent_based_sampler(#span_ctx{trace_flags=TraceFlags,
                               is_remote=true}, #{remote_parent_sampled := SamplerAndOpts})
  when ?IS_SAMPLED(TraceFlags) ->
    SamplerAndOpts;
%% remote parent not sampled
parent_based_sampler(#span_ctx{is_remote=true}, #{remote_parent_not_sampled := SamplerAndOpts}) ->
    SamplerAndOpts;
%% local parent sampled
parent_based_sampler(#span_ctx{trace_flags=TraceFlags,
                               is_remote=false}, #{local_parent_sampled := SamplerAndOpts})
  when ?IS_SAMPLED(TraceFlags) ->
    SamplerAndOpts;
%% local parent not sampled
parent_based_sampler(#span_ctx{is_remote=false}, #{local_parent_not_sampled := SamplerAndOpts}) ->
    SamplerAndOpts;
%% root
parent_based_sampler(_SpanCtx, #{root := SamplerAndOpts}) ->
    SamplerAndOpts.


trace_id_ratio_based(Ctx, undefined, _, _, _, _, _IdUpperBound) ->
    {?DROP, [], tracestate(Ctx)};
trace_id_ratio_based(Ctx, 0, _, _, _, _, _IdUpperBound) ->
    {?DROP, [], tracestate(Ctx)};
trace_id_ratio_based(Ctx, TraceId, _, _, _, _, IdUpperBound) ->
    Lower64Bits = TraceId band ?MAX_VALUE,
    case erlang:abs(Lower64Bits) < IdUpperBound of
        true ->
            {?RECORD_AND_SAMPLE, [], tracestate(Ctx)};
        false ->
            {?DROP, [], tracestate(Ctx)}
    end.

tracestate(Ctx) ->
    tracestate_(otel_tracer:current_span_ctx(Ctx)).

tracestate_(#span_ctx{tracestate=undefined}) ->
    [];
tracestate_(#span_ctx{tracestate=TraceState}) ->
    TraceState;
tracestate_(undefined) ->
    [].

description(always_on, _) ->
    <<"AlwaysOnSampler">>;
description(always_off, _) ->
    <<"AlwaysOffSampler">>;
description(trace_id_ratio_based, Probability) ->
    unicode:characters_to_binary(io_lib:format("TraceIdRatioBased{~.6f}", [Probability]));
description(parent_based, #{root := RootSampler,
                            remote_parent_sampled := RemoteParentSampler,
                            remote_parent_not_sampled := RemoteParentNotSampler,
                            local_parent_sampled := LocalParentSampler,
                            local_parent_not_sampled := LocalParentNotSampler}) ->
    <<"ParentBased{root:", (get_description(RootSampler))/binary,
      ",remoteParentSampled:", (get_description(RemoteParentSampler))/binary,
      ",remoteParentNotSampled:", (get_description(RemoteParentNotSampler))/binary,
      ",localParentSampled:", (get_description(LocalParentSampler))/binary,
      ",localParentNotSampled:", (get_description(LocalParentNotSampler))/binary,
      "}">>.
