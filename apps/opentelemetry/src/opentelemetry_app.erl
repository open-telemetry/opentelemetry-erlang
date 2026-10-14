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

start(_StartType, _StartArgs) ->
    case configuration() of
        {ok, IntegrationConfiguration} ->
            start_with_configuration(IntegrationConfiguration,
                                     opentelemetry_sdk_app:configuration());
        {error, Reason} ->
            {error, {configuration_error, Reason}}
    end.

-type configuration() ::
        #{create_application_tracers := boolean(),
          excluded_applications := [atom()]}.

-spec configuration() -> {ok, configuration()} | {error, term()}.
configuration() ->
    CreateApplicationTracers =
        application:get_env(opentelemetry, create_application_tracers, true),
    ExcludedApplications =
        application:get_env(opentelemetry, excluded_applications, []),
    case CreateApplicationTracers of
        Value when not is_boolean(Value) ->
            {error, {invalid_configuration, [create_application_tracers], Value}};
        _ ->
            case is_atom_list(ExcludedApplications) of
                true ->
                    {ok, #{create_application_tracers => CreateApplicationTracers,
                           excluded_applications => ExcludedApplications}};
                false ->
                    {error, {invalid_configuration,
                             [excluded_applications], ExcludedApplications}}
            end
    end.

is_atom_list(Value) when is_list(Value) ->
    lists:all(fun is_atom/1, Value);
is_atom_list(_) ->
    false.

start_with_configuration(IntegrationConfiguration, SdkConfiguration) ->
    case opentelemetry_sup:start_link() of
        {ok, _}=SupervisorResult ->
            create_application_tracers(IntegrationConfiguration, SdkConfiguration),
            SupervisorResult;
        Other ->
            Other
    end.

-spec create_application_tracers(configuration(),
                                 otel_configuration_sdk:configuration()) -> ok.
create_application_tracers(IntegrationConfiguration, SdkConfiguration) ->
    case {maps:get(create_application_tracers, IntegrationConfiguration),
          otel_configuration_sdk:disabled(SdkConfiguration),
          otel_configuration_sdk:tracer_provider(SdkConfiguration)} of
        {false, _, _} ->
            clear_application_tracers();
        {_, true, _} ->
            clear_application_tracers();
        {_, _, undefined} ->
            clear_application_tracers();
        {true, false, TracerProvider} when is_map(TracerProvider) ->
            %% TODO: make this work with release upgrades. Currently if an application's version
            %% changes the version in the tracer will not be updated.
            create_loaded_application_tracers(
              maps:get(excluded_applications, IntegrationConfiguration))
    end.

stop(_State) ->
    clear_application_tracers(),
    ok.

clear_application_tracers() ->
    opentelemetry:create_application_tracers([]).

-spec create_loaded_application_tracers([atom()]) -> ok.
create_loaded_application_tracers(ExcludedApplications) ->
    LoadedApplications =
        [Application
         || {Name, _Description, _Vsn}=Application <- application:loaded_applications(),
            not lists:member(Name, ExcludedApplications)],
    opentelemetry:create_application_tracers(LoadedApplications),
    ok.
