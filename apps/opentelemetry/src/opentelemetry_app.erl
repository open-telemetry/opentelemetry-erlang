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
    Configuration = opentelemetry_sdk_app:configuration(),
    case opentelemetry_sup:start_link() of
        {ok, _}=SupervisorResult ->
            create_application_tracers(Configuration),
            SupervisorResult;
        Other ->
            Other
    end.

-spec create_application_tracers(otel_configuration_sdk:configuration()) -> ok.
create_application_tracers(Configuration) ->
    case {otel_configuration_sdk:disabled(Configuration),
          otel_configuration_sdk:tracer_provider(Configuration)} of
        {true, _} ->
            ok;
        {_, undefined} ->
            ok;
        {_, TracerProvider} when is_map(TracerProvider) ->
            %% TODO: make this work with release upgrades. Currently if an application's version
            %% changes the version in the tracer will not be updated.
            create_loaded_application_tracers(Configuration)
    end.

stop(_State) ->
    ok.

-spec create_loaded_application_tracers(otel_configuration_sdk:configuration()) -> ok.
create_loaded_application_tracers(Config) ->
    Erlang = otel_configuration_sdk:erlang_distribution(Config),
    case otel_configuration_sdk:value(create_application_tracers, Erlang, true) of
        true ->
            %% TODO: filter out OTP apps that will not have any instrumentation
            LoadedApplications = application:loaded_applications(),
            opentelemetry:create_application_tracers(LoadedApplications),
            ok;
        false ->
            ok
    end.
