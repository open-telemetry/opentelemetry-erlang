%%%------------------------------------------------------------------------
%% Copyright 2021, OpenTelemetry Authors
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
%% @doc This noop TextMap Propagator makes no changes to the context or the
%% carrier when doing an extract or inject call.
%% '''
%% @end
%%%-------------------------------------------------------------------------
-module(otel_propagator_text_map_noop).

-behaviour(otel_propagator_text_map).

-export([fields/1,
         inject/4,
         extract/5]).

fields(_Propagators) ->
    [].

-spec inject(otel_ctx:t(), otel_propagator:carrier(), fun(),
             [otel_propagator_text_map:t()]) -> otel_propagator:carrier().
inject(_Context, Carrier, _CarrierSetFun, _Injectors) ->
    Carrier.

-spec extract(otel_ctx:t(), otel_propagator:carrier(), fun(), fun(),
              [otel_propagator_text_map:t()]) -> otel_ctx:t().
extract(Context, _Carrier, _CarrierKeysFun, _CarrierGetFun, _Extractors) ->
    Context.
