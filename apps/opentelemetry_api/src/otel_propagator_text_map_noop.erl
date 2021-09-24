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

-spec inject(Context, Carrier, CarrierSetFun, Options) -> Carrier
              when Context :: otel_ctx:t(),
                   Carrier :: otel_propagator:carrier(),
                   CarrierSetFun :: otel_propagator_text_map:carrier_set(),
                   Options :: otel_propagator_text_map:propagator_options().
inject(_Context, Carrier, _CarrierSetFun, _Options) ->
    Carrier.

-spec extract(Context, Carrier, CarrierKeysFun, CarrierGetFun, Options) -> Context
              when Context :: otel_ctx:t(),
                   Carrier :: otel_propagator:carrier(),
                   CarrierKeysFun :: otel_propagator_text_map:carrier_keys(),
                   CarrierGetFun :: otel_propagator_text_map:carrier_get(),
                   Options :: otel_propagator_text_map:propagator_options().
extract(Context, _Carrier, _CarrierKeysFun, _CarrierGetFun, _Options) ->
    Context.
