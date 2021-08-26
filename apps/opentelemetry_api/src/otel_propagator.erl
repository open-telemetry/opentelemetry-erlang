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
%% @doc A Propagator injects or extracts data from a Context so information
%% like baggage and trace context can be transported along with cross service
%% requests, like an HTTP request.
%%
%% Propagators are defined based on the type of encoding they inject and
%% extract. At this time there is only a TextMapPropagator,
%% {@link otel_propagator_text_map}, which works on ASCII keys and values.
%%
%% This behaviour is only for defining the callbacks used by each propagator
%% per type and is only used by developers adding a new type of propagator
%% (like for binary protocols), not implementations of propagators themselves
%% (like B3 or W3C TraceContext).
%%
%% Users configure and call propagators based on their type. See the docs
%% for {@link otel_propagator_text_map} for more details.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_propagator).

-export([builtin_to_module/1]).

%% Sets a value into a carrier
-callback inject(otel_ctx:t(), carrier()) -> carrier().
%% extracts values from a carrier and sets them in the context
-callback extract(otel_ctx:t(), carrier()) -> otel_ctx:t().

%% a carrier can be any type
-type carrier() :: term().

-export_type([carrier/0]).

%% convert the short name of a propagator to its module name if it is a builtin
%% if the name doens't match a builtin it is assumed to be a module
%% @hidden
builtin_to_module(tracecontext) ->
    otel_propagator_trace_context;
builtin_to_module(trace_context) ->
    otel_propagator_trace_context;
builtin_to_module(b3) ->
    otel_propagator_b3;
%% TODO: add multib3 and jaeger as builtin propagators
%% builtin_to_module(multib3) ->
%%     otel_propagator_multib3;
%% builtin_to_module(jaeger) ->
%%     otel_propagator_jaeger;
builtin_to_module(Module) ->
    Module.
