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
%%%-------------------------------------------------------------------------
-module(opentelemetry_sdk_app).

-behaviour(application).

-export([start/2,
         stop/1,
         configuration/0]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-define(CONFIGURATION_KEY, {?MODULE, configuration}).

start(_StartType, _StartArgs) ->
    case otel_configuration_source:resolve(
           application:get_all_env(opentelemetry_sdk)) of
        {ok, Configuration} ->
            start_with_configuration(Configuration);
        {error, Reason} ->
            {error, {configuration_error, Reason}}
    end.

stop(_State) ->
    persistent_term:erase(?CONFIGURATION_KEY),
    ok.

%% @private
-spec configuration() -> otel_configuration_sdk:configuration().
configuration() ->
    persistent_term:get(?CONFIGURATION_KEY).

start_with_configuration(Configuration) ->
    setup_text_map_propagators(Configuration),
    case opentelemetry_sdk_sup:start_link(Configuration) of
        {ok, _}=SupervisorResult ->
            configure_tracing(Configuration),
            persistent_term:put(?CONFIGURATION_KEY, Configuration),
            SupervisorResult;
        Other ->
            Other
    end.

configure_tracing(Configuration) ->
    case otel_configuration_sdk:disabled(Configuration) of
        true ->
            ok;
        false ->
            otel_span_limits:set(Configuration),
            start_global_tracer_provider(Configuration)
    end.

start_global_tracer_provider(Configuration) ->
    case otel_configuration_sdk:tracer_provider(Configuration) of
        undefined ->
            ok;
        TracerProvider when is_map(TracerProvider) ->
            Resource = otel_resource_detector:get_resource(),
            _ = otel_tracer_provider_sup:start_resolved(?GLOBAL_TRACER_PROVIDER_NAME,
                                                         Resource, TracerProvider),
            ok
    end.

setup_text_map_propagators(Configuration) ->
    Propagators = otel_configuration_sdk:text_map_propagators(Configuration),
    CompositePropagator = otel_propagator_text_map_composite:create(Propagators),
    opentelemetry:set_text_map_propagator(CompositePropagator).
