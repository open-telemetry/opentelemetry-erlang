%%%------------------------------------------------------------------------
%% Copyright 2024, OpenTelemetry Authors
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
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metric_exemplar_filter).

-export([always_off/6,
         always_on/6,
         sampled/6]).

always_off(_, _, _, _, _, _) ->
    false.

always_on(_, _, _, _, _, _) ->
    true.

sampled(Ctx, _ExemplarReservoir, _ExemplarTab, _Key, _Value, _DroppedAttributes) ->
    SpanCtx = otel_tracer:current_span_ctx(Ctx),
    otel_span:is_recording(SpanCtx).
