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
%% This sampler always decides to neither record nor sample each span.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_sampler_always_off).

-behavior(otel_sampler).

-export([description/1, setup/1, should_sample/7]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include("otel_sampler.hrl").

setup(_Opts) -> [].

description(_) -> <<"AlwaysOffSampler">>.

should_sample(Ctx, _TraceId, _Links, _SpanName, _SpanKind, _Attributes, _Opts) ->
    SpanCtx = otel_tracer:current_span_ctx(Ctx),
    {?DROP, [], otel_span:tracestate(SpanCtx)}.
