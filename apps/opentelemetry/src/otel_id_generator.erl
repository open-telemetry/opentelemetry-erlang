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
%% @doc This module provides the behaviour to implement for custom trace
%% and span id generation and the default implementation of the
%% generators which produces random 128 bit and 64 bit integers for the
%% trace id and span id.
%% @end
%%%-----------------------------------------------------------------------
-module(otel_id_generator).

-export([generate_trace_id/0,
         generate_trace_id/1,
         generate_span_id/0,
         generate_span_id/1]).

-callback generate_trace_id() -> opentelemetry:trace_id().

-callback generate_span_id() -> opentelemetry:span_id().

-type t() :: module().

-export_type([t/0]).

%% @doc Calls a module implementing the `otel_id_generator' behaviour to generate a trace id
-spec generate_trace_id(t()) -> opentelemetry:trace_id().
generate_trace_id(Module) ->
    Module:generate_trace_id().

%% @doc Calls a module implementing the `otel_id_generator' behaviour to generate a span id
-spec generate_span_id(t()) -> opentelemetry:span_id().
generate_span_id(Module) ->
    Module:generate_span_id().

%% @doc Generates a 128 bit random integer to use as a trace id.
-spec generate_trace_id() -> opentelemetry:trace_id().
generate_trace_id() ->
    rand:uniform(2 bsl 127 - 1). %% 2 shifted left by 127 == 2 ^ 128

%% @doc Generates a 64 bit random integer to use as a span id.
-spec generate_span_id() -> opentelemetry:span_id().
generate_span_id() ->
    rand:uniform(2 bsl 63 - 1). %% 2 shifted left by 63 == 2 ^ 64
