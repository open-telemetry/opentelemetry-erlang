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
    Config = otel_configuration:merge_with_os(
               application:get_all_env(opentelemetry)),

    %% set the global propagators for HTTP based on the application env
    %% these get set even if the SDK is disabled
    setup_text_map_propagators(Config),

    SupResult = opentelemetry_sup:start_link(Config),

    case Config of
        #{disabled := true} ->
            %% skip the rest if the SDK is disabled
            SupResult;
        _ ->
            %% set global span limits record based on configuration
            otel_span_limits:set(Config),

            Resource = otel_resource_detector:get_resource(),
            _ = otel_tracer_provider_sup:start(?GLOBAL_TRACER_PROVIDER_NAME, Resource, Config),

            %% must be done after the supervisor starts so that otel_tracer_server is running
            %% TODO: make this work with release upgrades. Currently if an application's version
            %% changes the version in the tracer will not be updated.
            create_loaded_application_tracers(Config),

            SupResult
    end.

stop(_State) ->
    ok.

%% internal functions

setup_text_map_propagators(Config) ->
    Propagator = maps:get(propagator, Config, #{}),
    PropagatorList = maps:get(composite_list, Propagator, []),
    Propagators = maps:get(composite, Propagator, []),

    Combined = append_if_not_found(otel_configuration:transform(propagators, PropagatorList), Propagators),

    CompositePropagator = otel_propagator_text_map_composite:create(Combined),
    opentelemetry:set_text_map_propagator(CompositePropagator).

create_loaded_application_tracers(#{create_application_tracers := true}) ->
    %% TODO: filter out OTP apps that will not have any instrumentation
    LoadedApplications = application:loaded_applications(),
    opentelemetry:create_application_tracers(LoadedApplications),
    ok;
create_loaded_application_tracers(_) ->
    ok.

append_if_not_found([], Existing) ->
    Existing;
append_if_not_found([C | Rest], Existing) ->
    case lists:member(C, Existing) of
        true ->
            append_if_not_found(Rest, Existing);
        false ->
            append_if_not_found(Rest, Existing ++ [C])
    end.
