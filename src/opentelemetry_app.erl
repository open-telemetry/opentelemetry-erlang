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
%% @end
%%%-------------------------------------------------------------------------
-module(opentelemetry_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Opts = application:get_all_env(opentelemetry),
    opentelemetry:set_default_tracer(ot_tracer_default),
    opentelemetry:set_default_context_manager(ot_ctx_pdict),

    {BaggageHttpExtractor, BaggageHttpInjector} = ot_baggage:get_http_propagators(),
    {CorrelationsHttpExtractor, CorrelationsHttpInjector} = ot_correlations:get_http_propagators(),
    {B3HttpExtractor, B3HttpInjector} = ot_tracer_default:b3_propagators(),
    opentelemetry:set_http_extractor([BaggageHttpExtractor, CorrelationsHttpExtractor, B3HttpExtractor]),
    opentelemetry:set_http_injector([BaggageHttpInjector, CorrelationsHttpInjector, B3HttpInjector]),

    opentelemetry_sup:start_link(Opts).

stop(_State) ->
    ok.

%% internal functions
