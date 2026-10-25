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
-module(otel_application_tracers).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

-spec start_link(map(), otel_configuration_sdk:configuration()) -> gen_server:start_ret().
start_link(IntegrationConfiguration, SdkConfiguration) ->
    gen_server:start_link(
      ?MODULE, {IntegrationConfiguration, SdkConfiguration}, []).

init({IntegrationConfiguration, SdkConfiguration}) ->
    process_flag(trap_exit, true),
    create_application_tracers(IntegrationConfiguration, SdkConfiguration),
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_call}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    otel_application_registry:clear().

create_application_tracers(IntegrationConfiguration, SdkConfiguration) ->
    case {maps:get(create_application_tracers, IntegrationConfiguration),
          otel_configuration_sdk:disabled(SdkConfiguration),
          otel_configuration_sdk:tracer_provider(SdkConfiguration)} of
        {false, _, _} ->
            otel_application_registry:clear();
        {_, true, _} ->
            otel_application_registry:clear();
        {_, _, undefined} ->
            otel_application_registry:clear();
        {true, false, TracerProvider} when is_map(TracerProvider) ->
            %% TODO: make this work with release upgrades. Currently if an application's version
            %% changes the version in the tracer will not be updated.
            create_loaded_application_tracers(
              maps:get(excluded_applications, IntegrationConfiguration))
    end.

create_loaded_application_tracers(ExcludedApplications) ->
    LoadedApplications =
        [Application
         || {Name, _Description, _Vsn}=Application <- application:loaded_applications(),
            not lists:member(Name, ExcludedApplications)],
    Registry = lists:foldl(fun register_application/2, #{}, LoadedApplications),
    otel_application_registry:replace(Registry).

register_application({Name, _Description, Version}, Registry) ->
    Vsn = opentelemetry:vsn_to_binary(Version),
    SchemaUrl = application:get_env(Name, otel_schema_url, undefined),
    _ = opentelemetry:get_tracer(Name, Vsn, SchemaUrl),
    {ok, Modules} = application:get_key(Name, modules),
    lists:foldl(
      fun(Module, Acc) ->
              Acc#{Module => {Name, Vsn, SchemaUrl}}
      end,
      Registry,
      Modules).
