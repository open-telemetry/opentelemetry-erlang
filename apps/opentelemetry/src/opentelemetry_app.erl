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
%% @private
%%%-------------------------------------------------------------------------
-module(opentelemetry_app).

-behaviour(application).

-export([start/2,
         stop/1]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

start(_StartType, _StartArgs) ->
    case otel_configuration_source:resolve(
           application:get_all_env(opentelemetry)) of
        {ok, Config} ->
            start_with_configuration(Config);
        {error, Reason} ->
            {error, {configuration_error, Reason}}
    end.

start_with_configuration(Config) ->

    %% set the global propagators for HTTP based on the resolved configuration
    %% these get set even if the SDK is disabled
    setup_text_map_propagators(Config),

    case opentelemetry_sup:start_link(Config) of
        {ok, _}=SupResult ->
            configure_tracing(Config),
            otel_configuration:store_runtime(Config),
            SupResult;
        Other ->
            Other
    end.

configure_tracing(Config) ->
    case Config of
        #{sdk_disabled := true} ->
            ok;
        #{traces_enabled := false} ->
            ok;
        _ ->
            %% set global span limits record based on configuration
            otel_span_limits:set(Config),

            Resource = otel_resource_detector:get_resource(),
            _ = otel_tracer_provider_sup:start(?GLOBAL_TRACER_PROVIDER_NAME, Resource, Config),

            %% must be done after the supervisor starts so that otel_tracer_server is running
            %% TODO: make this work with release upgrades. Currently if an application's version
            %% changes the version in the tracer will not be updated.
            create_loaded_application_tracers(Config),

            ok
    end.

stop(_State) ->
    otel_configuration:clear_runtime().

%% internal functions

setup_text_map_propagators(#{text_map_propagators := List}) ->
    CompositePropagator = otel_propagator_text_map_composite:create(List),
    opentelemetry:set_text_map_propagator(CompositePropagator).

create_loaded_application_tracers(#{create_application_tracers := true}) ->
    %% TODO: filter out OTP apps that will not have any instrumentation
    LoadedApplications = application:loaded_applications(),
    opentelemetry:create_application_tracers(LoadedApplications),
    ok;
create_loaded_application_tracers(_) ->
    ok.
