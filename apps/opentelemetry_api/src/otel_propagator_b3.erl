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
%% @doc An implementation of {@link otel_propagator_text_map} that injects and
%% extracts trace context using the B3 specification from Zipkin.
%%
%% Since `trace_context' and `baggage' are the two default propagators the
%% global TextMap Propagators must be configured if B3 is to be used for
%% propagation:
%%
%% ```
%% {text_map_propagators, [b3, baggage]},
%% '''
%%
%% To use B3 multi-header format use:
%%
%% ```
%% {text_map_propagators, [b3multi, baggage]},
%% '''
%%
%% ```
%% CompositePropagator = otel_propagator_text_map_composite:create([b3, baggage]),
%% opentelemetry:set_text_map_propagator(CompositePropagator).
%% '''
%%
%% It is also possible to set a separate list of injectors or extractors.
%% For example, if the service should extract B3 encoded context but you
%% only want to inject context encoded with the W3C TraceContext format
%% (maybe you have some services only supporting B3 that are making requests
%% to your server but you have no reason to continue propagating in both
%% formats when communicating to other services further down the stack).
%% In that case you would instead set configuration like:
%%
%%
%% ```
%% {text_map_extractors, [b3, trace_context, baggage]},
%% {text_map_injectors, [trace_context, baggage]},
%% '''
%%
%% Or using calls to {@link opentelemetry} at runtime:
%%
%% ```
%% B3CompositePropagator = otel_propagator_text_map_composite:create([b3, trace_context, baggage]),
%% CompositePropagator = otel_propagator_text_map_composite:create([trace_context, baggage]),
%% opentelemetry:set_text_map_extractor(B3CompositePropagator),
%% opentelemetry:set_text_map_injector(CompositePropagator).
%% '''
%% @end
%%%-----------------------------------------------------------------------
-module(otel_propagator_b3).

-behaviour(otel_propagator_text_map).

-export([fields/1,
         inject/4,
         extract/5]).

-include("opentelemetry.hrl").

-define(B3_CONTEXT_KEY, <<"b3">>).

%% Returns all the keys the propagator sets with `inject'
fields(b3single) ->
    otel_propagator_b3single:fields(b3single);
fields(b3multi) ->
    otel_propagator_b3multi:fields(b3multi);
fields(_) ->
    [].

-spec inject(Context, Carrier, CarrierSetFun, Options) -> Carrier
              when Context :: otel_ctx:t(),
                   Carrier :: otel_propagator:carrier(),
                   CarrierSetFun :: otel_propagator_text_map:carrier_set(),
                   Options :: b3multi | b3single.
inject(Ctx, Carrier, CarrierSet, Options=b3single) ->
    otel_propagator_b3single:inject(Ctx, Carrier, CarrierSet, Options);
inject(Ctx, Carrier, CarrierSet, Options=b3multi) ->
    otel_propagator_b3multi:inject(Ctx, Carrier, CarrierSet, Options);
inject(_Ctx, Carrier, _CarrierSet, _Options) ->
    Carrier.

% Extract trace context from the supplied carrier. The b3 single header takes
% precedence over the multi-header format.
%
% If extraction fails, the original context will be returned.
-spec extract(Context, Carrier, CarrierKeysFun, CarrierGetFun, Options) -> Context
              when Context :: otel_ctx:t(),
                   Carrier :: otel_propagator:carrier(),
                   CarrierKeysFun :: otel_propagator_text_map:carrier_keys(),
                   CarrierGetFun :: otel_propagator_text_map:carrier_get(),
                   Options :: otel_propagator_text_map:propagator_options().
extract(Ctx, Carrier, CarrierKeysFun, CarrierGet, Options) ->
    case otel_propagator_b3single:extract(Ctx, Carrier, CarrierKeysFun, CarrierGet, Options) of
        Result when Result =/= undefined -> Result;
        _ ->
            case otel_propagator_b3multi:extract(Ctx, Carrier, CarrierKeysFun, CarrierGet, Options) of
                Result when Result =/= undefined -> Result;
                _ -> Ctx
            end
    end.
