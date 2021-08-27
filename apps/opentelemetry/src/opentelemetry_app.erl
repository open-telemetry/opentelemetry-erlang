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
    Opts = otel_configuration:merge_with_os(
             application:get_all_env(opentelemetry)),

    %% set the global propagators for HTTP based on the application env
    setup_text_map_propagators(Opts),

    SupResult = opentelemetry_sup:start_link(Opts),

    %% must be done after the supervisor starts so that otel_tracer_server is running
    %% TODO: make this work with release upgrades. Currently if an application's version
    %% changes the version in the tracer will not be updated.
    register_loaded_application_tracers(Opts),

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

setup_text_map_propagators(Opts) ->
    Propagators = proplists:get_value(text_map_propagators, Opts, []),
    opentelemetry:set_text_map_propagators(Propagators).

register_loaded_application_tracers(Opts) ->
    RegisterLoadedApplications = proplists:get_value(register_loaded_applications, Opts, true),
    register_loaded_applications_(RegisterLoadedApplications).

register_loaded_applications_(true) ->
    LoadedApplications = application:loaded_applications(),
    [opentelemetry:register_application_tracer(Name) || {Name, _, _} <- LoadedApplications],
    ok;
register_loaded_applications_(_) ->
    ok.
