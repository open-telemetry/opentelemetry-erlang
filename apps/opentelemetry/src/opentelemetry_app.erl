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
    case integration_configuration() of
        {ok, IntegrationConfiguration} ->
            AppEnv = [{Key, Value} || {Key, Value} <- application:get_all_env(opentelemetry),
                                     Key =/= create_application_tracers,
                                     Key =/= excluded_applications],
            case otel_configuration_source:resolve(AppEnv) of
                {ok, Config} ->
                    start_with_configuration(Config, IntegrationConfiguration);
                {error, Reason} ->
                    {error, {configuration_error, Reason}}
            end;
        {error, Reason} ->
            {error, {configuration_error, Reason}}
    end.

-spec start_with_configuration(otel_configuration_sdk:configuration(), map()) ->
          {ok, pid()} | ignore | {error, term()}.
start_with_configuration(Config, IntegrationConfiguration) ->

    %% set the global propagators for HTTP based on the application env
    %% these get set even if the SDK is disabled
    setup_text_map_propagators(Config),

    case opentelemetry_sup:start_link(Config) of
        {ok, _}=SupResult ->
            configure_tracing(Config),
            {ok, _} = start_application_tracers(IntegrationConfiguration, Config),
            SupResult;
        Other ->
            Other
    end.

-spec configure_tracing(otel_configuration_sdk:configuration()) -> ok.
configure_tracing(Config) ->
    case {otel_configuration_sdk:disabled(Config),
          otel_configuration_sdk:tracer_provider(Config)} of
        {true, _} ->
            ok;
        {_, undefined} ->
            ok;
        {_, TracerProvider} when is_map(TracerProvider) ->
            %% set global span limits record based on configuration
            otel_span_limits:set(Config),

            Resource = otel_resource_detector:get_resource(),
            _ = otel_tracer_provider_sup:start_resolved(?GLOBAL_TRACER_PROVIDER_NAME,
                                                         Resource, TracerProvider),

            ok
    end.

stop(_State) ->
    ok.

%% internal functions

-spec setup_text_map_propagators(otel_configuration_sdk:configuration()) -> ok.
setup_text_map_propagators(Config) ->
    List = otel_configuration_sdk:text_map_propagators(Config),
    CompositePropagator = otel_propagator_text_map_composite:create(List),
    opentelemetry:set_text_map_propagator(CompositePropagator).

-type configuration() ::
        #{create_application_tracers := boolean(),
          excluded_applications := [atom()]}.

-spec integration_configuration() -> {ok, configuration()} | {error, term()}.
integration_configuration() ->
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

%% Start after configuring the global provider so warmup uses the real tracer.
%% The supervisor owns registration, restart and shutdown cleanup.
start_application_tracers(IntegrationConfiguration, SdkConfiguration) ->
    supervisor:start_child(opentelemetry_sup,
      #{id => otel_application_tracers,
        start => {otel_application_tracers, start_link,
                  [IntegrationConfiguration, SdkConfiguration]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [otel_application_tracers]}).
