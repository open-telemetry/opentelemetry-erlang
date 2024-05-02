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

-export([builtins_to_modules/1,
         builtin_to_module/1]).

-callback inject(t(), carrier()) -> carrier().
%% Sets a value into a carrier.

-callback inject_from(otel_ctx:t(), t(), carrier()) -> carrier().
%% Sets a value from a context into a carrier.

-callback extract(t(), carrier()) -> otel_ctx:t() | otel_ctx:token().
%% Extracts a value from a carrier.

-callback extract_to(otel_ctx:t(), t(), carrier()) -> otel_ctx:t().
%% Extracts a value from a carrier into a context.

-type t() :: builtin() | module() | {module(), term()}.
%% A propagator, which can be a built-in propagator, a module, or a module
%% and associated term.

%% trace_context and tracecontext are the same. tracecontext is the term
%% in Otel specs and trace_context is the more idiomatic Erlang spelling
%% In the future, also: jaeger
-type builtin() :: trace_context | tracecontext | b3 | b3multi | baggage.
%% A built-in propagator.

-type carrier() :: term().
%% A carrier, which can be any type.

-export_type([t/0,
              builtin/0,
              carrier/0]).

%% convert the short name of a propagator to its module name if it is a builtin
%% if the name doesn't match a builtin it is assumed to be a module
%% @private
-spec builtins_to_modules([t()]) -> [module() | {module(), term()}].
builtins_to_modules(Propagators) ->
    [builtin_to_module(P) || P <- Propagators].

%% @private
-spec builtin_to_module(builtin() | module() | {module(), term()}) -> module() | {module(), term()}.
builtin_to_module(tracecontext) ->
    otel_propagator_trace_context;
builtin_to_module(trace_context) ->
    otel_propagator_trace_context;
builtin_to_module(b3) ->
    {otel_propagator_b3, b3single};
builtin_to_module(b3multi) ->
    {otel_propagator_b3, b3multi};
builtin_to_module(baggage) ->
    otel_propagator_baggage;
%% TODO: add jaeger as builtin propagator
%% builtin_to_module(jaeger) ->
%%     otel_propagator_jaeger;
builtin_to_module(Module) when is_atom(Module) ->
    Module;
builtin_to_module(Propagator) ->
    Propagator.
