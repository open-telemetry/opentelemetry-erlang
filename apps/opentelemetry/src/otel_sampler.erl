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
         parent_or_else/7,
         probability/7]).

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
-define(DEFAULT_PROBABILITY, 0.5).

-spec setup(atom() | module(), map()) -> t().
setup(always_on, _Opts) ->
    {fun ?MODULE:always_on/7, []};
setup(always_off, _Opts) ->
    {fun ?MODULE:always_off/7, []};
setup(parent_or_else, #{delegate_sampler := {DelegateSampler, DelegateOpts}})
  when is_atom(DelegateSampler) ->
    {fun ?MODULE:parent_or_else/7, setup(DelegateSampler, DelegateOpts)};
setup(parent_or_else, _Opts) ->
    ?LOG_INFO("no delegate_sampler opt found for sampler parent_or_else. always_on will be used for root spans"),
    {fun ?MODULE:parent_or_else/7, setup(always_on, #{})};
setup(probability, Opts) ->
    IdUpperBound =
        case maps:get(probability, Opts, ?DEFAULT_PROBABILITY) of
            P when P =:= 0.0 ->
                0;
            P when P =:= 1.0 ->
                ?MAX_VALUE;
            P when P >= 0.0 andalso P =< 1.0 ->
                P * ?MAX_VALUE
        end,

    IgnoreParentFlag = maps:get(ignore_parent_flag, Opts, false),
    OnlyRoot = maps:get(only_rootel_spans, Opts, true),

    {fun ?MODULE:probability/7, {IdUpperBound, IgnoreParentFlag, OnlyRoot}};
setup(Sampler, Opts) ->
    Sampler:setup(Opts).

always_on(_TraceId, _SpanCtx, _Links, _SpanName, _Kind, _Attributes, _Opts) ->
    {?RECORD_AND_SAMPLED, []}.

always_off(_TraceId, _SpanCtx, _Links, _SpanName, _Kind, _Attributes, _Opts) ->
    {?NOT_RECORD, []}.

parent_or_else(_, #span_ctx{trace_flags=TraceFlags}, _, _, _, _, _) when ?IS_SPAN_ENABLED(TraceFlags) ->
    {?RECORD_AND_SAMPLED, []};
parent_or_else(_, #span_ctx{trace_flags=_TraceFlags}, _, _, _, _, _) ->
    {?NOT_RECORD, []};
parent_or_else(TraceId, SpanCtx, Links, SpanName, Kind, Attributes, {DelegateSampler, DelegateOpts}) ->
    DelegateSampler(TraceId, SpanCtx, Links, SpanName, Kind, Attributes, DelegateOpts).

probability(TraceId, Parent, _, _, _, _, {IdUpperBound,
                                          IgnoreParentFlag,
                                          _OnlyRoot})
  when IgnoreParentFlag =:= true orelse Parent =:= undefined ->
    {do_probability_sample(TraceId, IdUpperBound), []};
probability(_, #span_ctx{trace_flags=TraceFlags}, _, _, _, _, {_IdUpperBound,
                                                               _IgnoreParentFlag,
                                                               _OnlyRoot})
  when ?IS_SPAN_ENABLED(TraceFlags) ->
    {?RECORD_AND_SAMPLED, []};
probability(TraceId, #span_ctx{is_remote=IsRemote}, _, _, _, _, {IdUpperBound,
                                                                 _IgnoreParentFlag,
                                                                 OnlyRoot}) ->
    case OnlyRoot =:= false andalso IsRemote =:= true
        andalso do_probability_sample(TraceId, IdUpperBound) of
        ?RECORD_AND_SAMPLED ->
            {?RECORD_AND_SAMPLED, []};
        _ ->
            {?NOT_RECORD, []}
    end.

do_probability_sample(TraceId, IdUpperBound) ->
    Lower64Bits = TraceId band ?MAX_VALUE,
    case erlang:abs(Lower64Bits) < IdUpperBound of
        true ->
            ?RECORD_AND_SAMPLED;
        false ->
            ?NOT_RECORD
    end.

