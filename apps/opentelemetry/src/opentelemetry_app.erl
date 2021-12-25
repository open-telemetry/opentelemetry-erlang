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

-export([start/2,
         prep_stop/1,
         stop/1]).

start(_StartType, _StartArgs) ->
    Config = otel_configuration:merge_with_os(
             application:get_all_env(opentelemetry)),

    %% set global span limits record based on configuration
    otel_span_limits:set(Config),

    %% set the global propagators for HTTP based on the application env
    setup_text_map_propagators(Config),

    SupResult = opentelemetry_sup:start_link(Config),

    %% must be done after the supervisor starts so that otel_tracer_server is running
    %% TODO: make this work with release upgrades. Currently if an application's version
    %% changes the version in the tracer will not be updated.
    create_loaded_application_tracers(Config),

    SupResult.

%% called before the supervision tree is shutdown.
prep_stop(_State) ->
    %% on application stop set tracer to the noop implementation.
    %% This is to ensure no crashes if the sdk isn't the last
    %% thing to shutdown or if the opentelemetry application crashed.
    opentelemetry:set_default_tracer({otel_tracer_noop, []}),
    ok.

stop(_State) ->
    ok.

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
