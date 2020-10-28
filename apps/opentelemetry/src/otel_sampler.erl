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

-export([setup/2,
         always_on/7,
         always_off/7,
         parent_based/7,
         trace_id_ratio_based/7]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_sampler.hrl").
-include("otel_span.hrl").

-type sampling_decision() :: ?NOT_RECORD | ?RECORD | ?RECORD_AND_SAMPLED.
-type sampling_result() :: {sampling_decision(), opentelemetry:attributes()}.
-type sampler() :: {fun((opentelemetry:trace_id(),
                         opentelemetry:span_ctx() | undefined,
                         opentelemetry:links(),
                         opentelemetry:span_name(),
                         opentelemetry:kind(),
                         opentelemetry:attributes(),
                         term()) -> sampling_result()), term()}.
-opaque t() :: sampler().
-export_type([sampling_result/0,
              sampling_decision/0,
              t/0]).

-define(MAX_VALUE, 9223372036854775807). %% 2^63 - 1

-spec setup(atom() | module(), map()) -> t().
setup(always_on, _Opts) ->
    {fun ?MODULE:always_on/7, []};
setup(always_off, _Opts) ->
    {fun ?MODULE:always_off/7, []};
setup(parent_based, Opts) ->
    Config = parent_based_config(Opts),
    {fun ?MODULE:parent_based/7, Config};
setup(trace_id_ratio_based, Probability) ->
    IdUpperBound = case Probability of
                       P when P =:= 0.0 ->
                           0;
                       P when P =:= 1.0 ->
                           ?MAX_VALUE;
                       P when P >= 0.0 andalso P =< 1.0 ->
                           P * ?MAX_VALUE
                   end,

    {fun ?MODULE:trace_id_ratio_based/7, IdUpperBound};
setup(Sampler, Opts) ->
    Sampler:setup(Opts).

always_on(_TraceId, _SpanCtx, _Links, _SpanName, _Kind, _Attributes, _Opts) ->
    {?RECORD_AND_SAMPLED, []}.

always_off(_TraceId, _SpanCtx, _Links, _SpanName, _Kind, _Attributes, _Opts) ->
    {?NOT_RECORD, []}.

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

    #{root => setup(RootSampler, RootOpts),
      remote_parent_sampled =>
          setup(RemoteParentSampled, RemoteParentSampledOpts),
      remote_parent_not_sampled =>
          setup(RemoteParentNotSampled, RemoteParentNotSampledOpts),
      local_parent_sampled =>
          setup(LocalParentSampled, LocalParentSampledOpts),
      local_parent_not_sampled =>
          setup(LocalParentNotSampled, LocalParentNotSampledOpts)};
parent_based_config(Opts) ->
    ?LOG_INFO("no root opt found for sampler parent_based. always_on will be used for root spans"),
    parent_based_config(Opts#{root => {always_on, #{}}}).

parent_based(TraceId, SpanCtx, Links, SpanName, Kind, Attributes, Opts) ->
    {Sampler, SamplerOpts} = parent_based_sampler(SpanCtx, Opts),
    Sampler(TraceId, SpanCtx, Links, SpanName, Kind, Attributes, SamplerOpts).

%% remote parent sampled
parent_based_sampler(#span_ctx{trace_flags=TraceFlags,
                               is_remote=true}, #{remote_parent_sampled := SamplerAndOpts})
  when ?IS_SPAN_ENABLED(TraceFlags) ->
    SamplerAndOpts;
%% remote parent not sampled
parent_based_sampler(#span_ctx{is_remote=true}, #{remote_parent_not_sampled := SamplerAndOpts}) ->
    SamplerAndOpts;
%% local parent sampled
parent_based_sampler(#span_ctx{trace_flags=TraceFlags,
                               is_remote=false}, #{local_parent_sampled := SamplerAndOpts})
  when ?IS_SPAN_ENABLED(TraceFlags) ->
    SamplerAndOpts;
%% local parent not sampled
parent_based_sampler(#span_ctx{is_remote=false}, #{local_parent_not_sampled := SamplerAndOpts}) ->
    SamplerAndOpts;
%% root
parent_based_sampler(_SpanCtx, #{root := SamplerAndOpts}) ->
    SamplerAndOpts.

trace_id_ratio_based(TraceId, _, _, _, _, _, IdUpperBound) ->
    Lower64Bits = TraceId band ?MAX_VALUE,
    case erlang:abs(Lower64Bits) < IdUpperBound of
        true ->
            {?RECORD_AND_SAMPLED, []};
        false ->
            {?NOT_RECORD, []}
    end.

