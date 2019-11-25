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
-module(ot_sampler).

-export([setup/2,
         always_on/9,
         always_off/9,
         always_parent/9,
         probability/9]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("ot_sampler.hrl").

-type sampling_decision() :: ?NOT_RECORD | ?RECORD | ?RECORD_AND_PROPAGATE.
-type sampling_hint() :: sampling_decision() | undefined.
-type sampling_result() :: {sampling_decision(), opentelemetry:attributes()}.
-type sampler() :: {fun((opentelemetry:trace_id(),
                         opentelemetry:span_id(),
                         opentelemetry:span_ctx() | undefined,
                         sampling_hint(),
                         opentelemetry:links(),
                         opentelemetry:span_name(),
                         opentelemetry:kind(),
                         opentelemetry:attributes(),
                         term()) -> sampling_result()), term()}.
-export_type([sampling_result/0,
              sampling_decision/0,
              sampler/0]).

-define(MAX_VALUE, 9223372036854775807). %% 2^63 - 1
-define(DEFAULT_PROBABILITY, 0.5).

-define(IGNORE_HINT(SamplingHint, IgnoreHints), SamplingHint =:= undefined
        orelse lists:member(SamplingHint, IgnoreHints)).

-spec setup(atom() | module(), map()) -> sampler().
setup(always_on, _Opts) ->
    {fun ?MODULE:always_on/9, []};
setup(always_off, _Opts) ->
    {fun ?MODULE:always_off/9, []};
setup(always_parent, _Opts) ->
    {fun ?MODULE:always_parent/9, []};
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

    %% by default only ignore the RECORD hint
    IgnoreHints = maps:get(ignore_hints, Opts, [?RECORD]),
    IgnoreParentFlag = maps:get(ignore_parent_flag, Opts, false),
    OnlyRoot = maps:get(only_root_spans, Opts, true),

    {fun ?MODULE:probability/9, {IdUpperBound, IgnoreHints, IgnoreParentFlag, OnlyRoot}};
setup(Sampler, Opts) ->
    Sampler:setup(Opts).

always_on(_TraceId, _SpanId, _SpanCtx, _SamplingHint, _Links, _SpanName, _Kind, _Attributes, _Opts) ->
    {?RECORD_AND_PROPAGATE, []}.

always_off(_TraceId, _SpanId, _SpanCtx, _SamplingHint, _Links, _SpanName, _Kind, _Attributes, _Opts) ->
    {?NOT_RECORD, []}.

always_parent(_, _, #span_ctx{trace_flags=TraceFlags}, _, _, _, _, _, _) when ?IS_SPAN_ENABLED(TraceFlags) ->
    {?RECORD_AND_PROPAGATE, []};
always_parent(_, _, _, _, _, _, _, _, _) ->
    {?NOT_RECORD, []}.

probability(TraceId, _, Parent, SamplingHint, _, _, _, _, {IdUpperBound,
                                                           IgnoreHints,
                                                           IgnoreParentFlag,
                                                           _OnlyRoot})
  when IgnoreParentFlag =:= true orelse Parent =:= undefined ->
    case ?IGNORE_HINT(SamplingHint, IgnoreHints) of
        true ->
            {do_probability_sample(TraceId, IdUpperBound), []};
        false ->
            {SamplingHint, []}
    end;
probability(_, _, #span_ctx{trace_flags=TraceFlags}, _, _, _, _, _, {_IdUpperBound,
                                                                     _IgnoreHints,
                                                                     _IgnoreParentFlag,
                                                                     _OnlyRoot})
  when ?IS_SPAN_ENABLED(TraceFlags) ->
    {?RECORD_AND_PROPAGATE, []};
probability(TraceId, _, #span_ctx{is_remote=IsRemote}, SamplingHint, _, _, _, _, {IdUpperBound,
                                                                                  IgnoreHints,
                                                                                  _IgnoreParentFlag,
                                                                                  OnlyRoot}) ->
    case SamplingHint =:= ?RECORD_AND_PROPAGATE
        andalso not(lists:member(SamplingHint, IgnoreHints)) of
        true ->
            {?RECORD_AND_PROPAGATE, []};
        false ->
            case OnlyRoot =:= false andalso IsRemote =:= true
                andalso do_probability_sample(TraceId, IdUpperBound) of
                ?RECORD_AND_PROPAGATE ->
                    {?RECORD_AND_PROPAGATE, []};
                _ ->
                    %% sampling hint could still be ?RECORD
                    maybe_sampling_hint(SamplingHint, IgnoreHints)
            end
    end.

maybe_sampling_hint(SamplingHint, IgnoreHints) ->
    case ?IGNORE_HINT(SamplingHint, IgnoreHints) of
        true ->
            {?NOT_RECORD, []};
        false ->
            {SamplingHint, []}
    end.

do_probability_sample(TraceId, IdUpperBound) ->
    Lower64Bits = TraceId band ?MAX_VALUE,
    case erlang:abs(Lower64Bits) < IdUpperBound of
        true ->
            ?RECORD_AND_PROPAGATE;
        false ->
            ?NOT_RECORD
    end.

