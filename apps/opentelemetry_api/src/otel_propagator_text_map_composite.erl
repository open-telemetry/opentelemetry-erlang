%%%------------------------------------------------------------------------
%% Copyright 2020, OpenTelemetry Authors
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
%% @doc A Composite TextMap Propagator is a Propagator that performs run
%% multiple TextMap Propagators in a specified order.
%%
%% An example of creating a Composite TextMap Propagator to inject and
%% extract Baggage and TraceContext:
%%
%% ```
%% Propagator = otel_propagator_text_map_composite:create([trace_context, baggage]),
%% otel_propagator_text_map:extract(Propagator, Carrier)
%% '''
%% @end
%%%-------------------------------------------------------------------------
-module(otel_propagator_text_map_composite).

-behaviour(otel_propagator_text_map).

-export([create/1,
         fields/1,
         inject/4,
         extract/5]).

-include_lib("kernel/include/logger.hrl").

create(Propagators) ->
    {?MODULE, otel_propagator:builtins_to_modules(Propagators)}.

fields(Propagators) ->
    lists:flatmap(fun(Propagator) ->
                          otel_propagator_text_map:fields(Propagator)
                  end, Propagators).

-spec inject(otel_ctx:t(), otel_propagator:carrier(), fun(),
             [otel_propagator_text_map:t()]) -> otel_propagator:carrier().
inject(Context, Carrier, CarrierSetFun, Injectors) ->
    run_injectors(Context, Injectors, Carrier, CarrierSetFun).

-spec extract(otel_ctx:t(), otel_propagator:carrier(), fun(), fun(),
              [otel_propagator_text_map:t()]) -> otel_ctx:t().
extract(Context, Carrier, CarrierKeysFun, CarrierGetFun, Extractors) ->
    run_extractors(Context, Extractors, Carrier, CarrierKeysFun, CarrierGetFun).

run_extractors(Context, Extractors, Carrier, CarrierKeysFun, CarrierGetFun) when is_list(Extractors) ->
    lists:foldl(fun(Propagator, ContextAcc) ->
                        try otel_propagator_text_map:extract_to(ContextAcc, Propagator, Carrier, CarrierKeysFun, CarrierGetFun)
                        catch
                            C:E:S ->
                                ?LOG_INFO("text map propagator failed to extract from carrier",
                                          #{extractor => Propagator, carrier => Carrier,
                                            class => C, exception => E, stacktrace => S}),
                                ContextAcc
                        end
                end, Context, otel_propagator:builtins_to_modules(Extractors)).

run_injectors(Context, Injectors, Carrier, Setter) when is_list(Injectors) ->
    lists:foldl(fun(Propagator, CarrierAcc) ->
                        try otel_propagator_text_map:inject_from(Context, Propagator, CarrierAcc, Setter)
                        catch
                            C:E:S ->
                                ?LOG_INFO("text map propagator failed to inject to carrier",
                                          #{injector => Propagator, carrier => CarrierAcc,
                                            class => C, exception => E, stacktrace => S}),
                                CarrierAcc
                        end
                end, Carrier, otel_propagator:builtins_to_modules(Injectors)).
