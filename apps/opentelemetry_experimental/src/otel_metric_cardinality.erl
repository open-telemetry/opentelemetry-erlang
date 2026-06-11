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
%% @doc Pure decision helper for the metrics cardinality limit + overflow.
%%
%% Mirrors the semantics of opentelemetry-go's
%% sdk/metric/internal/aggregate/limit.go and opentelemetry-java's
%% MetricStorage cardinality overflow handling.
%%
%% With a limit of N, up to N-1 distinct "real" series are admitted and the
%% Nth distinct attribute set (and every distinct set after it) is folded into
%% a single overflow series tagged `#{<<"otel.metric.overflow">> => true}'.
%% The `N-1' reservation matches Go's `len(measurements) >= aggLimit-1' and
%% keeps one slot for the overflow series itself.
%%
%% A limit `=< 0' disables the cap (Go semantics; note Go treats `<= 0' as
%% unlimited, unlike Java which rejects it).
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metric_cardinality).

-export([default_limit/0,
         overflow_attributes/0,
         limit_attributes/4]).

-define(OVERFLOW_KEY, <<"otel.metric.overflow">>).

%% @doc The default per-stream cardinality limit when none is configured.
-spec default_limit() -> 2000.
default_limit() ->
    2000.

%% @doc The attribute set used for the single overflow series.
-spec overflow_attributes() -> #{binary() => true}.
overflow_attributes() ->
    #{?OVERFLOW_KEY => true}.

%% @doc Decide which attribute set a measurement should be recorded under.
%%
%%   * `Limit =< 0'           -> unlimited; always return `Attributes'
%%   * `Exists =:= true'      -> series already counted; return `Attributes'
%%   * not Exists and `CurrentCount >= Limit - 1' -> return overflow attributes
%%   * otherwise              -> return `Attributes'
-spec limit_attributes(Attributes, CurrentCount, Exists, Limit) -> Attributes | OverflowAttributes when
      Attributes :: opentelemetry:attributes_map(),
      CurrentCount :: non_neg_integer(),
      Exists :: boolean(),
      Limit :: integer(),
      OverflowAttributes :: #{binary() => true}.
limit_attributes(Attributes, _CurrentCount, _Exists, Limit) when Limit =< 0 ->
    %% unlimited
    Attributes;
limit_attributes(Attributes, _CurrentCount, true, _Limit) ->
    %% already-counted series are always allowed through; this is what lets the
    %% single overflow series keep accumulating spillover measurements
    Attributes;
limit_attributes(_Attributes, CurrentCount, _Exists, Limit) when CurrentCount >= Limit - 1 ->
    overflow_attributes();
limit_attributes(Attributes, _CurrentCount, _Exists, _Limit) ->
    Attributes.
