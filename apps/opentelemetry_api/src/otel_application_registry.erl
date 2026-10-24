%%%------------------------------------------------------------------------
%% Copyright 2026, OpenTelemetry Authors
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
%% @private
%%%------------------------------------------------------------------------
-module(otel_application_registry).

-export([replace/1,
         lookup/1,
         clear/0]).

-type instrumentation_scope() ::
        {atom(),
         unicode:unicode_binary() | undefined,
         uri_string:uri_string() | undefined}.
-type registry() :: #{module() => instrumentation_scope()}.

-define(REGISTRY_KEY, {?MODULE, registry}).

-spec replace(registry()) -> ok.
replace(Registry) when is_map(Registry) ->
    persistent_term:put(?REGISTRY_KEY, Registry).

-spec lookup(module()) -> instrumentation_scope() | '$__default_tracer'.
lookup(Module) ->
    maps:get(Module, persistent_term:get(?REGISTRY_KEY, #{}), '$__default_tracer').

-spec clear() -> ok.
clear() ->
    _ = persistent_term:erase(?REGISTRY_KEY),
    ok.
