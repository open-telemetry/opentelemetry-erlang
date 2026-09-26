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
%% @doc Programmatic entry point for starting named SDK TracerProviders.
%%
%% The configuration map uses the same native Erlang form as the
%% `tracer_provider' application environment entry. It is resolved and
%% validated before any provider processes are started.
%% @end
%%%------------------------------------------------------------------------
-module(otel_tracer_provider_sdk).

-export([start/2,
         start/3]).

-type processor() :: {batch | simple | module(), map()}.
-type sampler() :: always_on
                 | always_off
                 | {trace_id_ratio_based, number()}
                 | {parent_based, map()}
                 | {module(), map()}.
-type configuration() ::
        #{processors := [processor()],
          sampler => sampler(),
          id_generator => module(),
          limits => map()}.

-export_type([processor/0,
              sampler/0,
              configuration/0]).

%% @doc Starts a named TracerProvider with an empty resource.
-spec start(atom(), configuration()) ->
          supervisor:startchild_ret().
start(Name, Configuration) ->
    start(Name, otel_resource:create([]), Configuration).

%% @doc Starts a named TracerProvider with the supplied resource.
-spec start(atom(), otel_resource:t(), configuration()) ->
          supervisor:startchild_ret().
start(Name, Resource, Configuration) when is_atom(Name), is_map(Configuration) ->
    case otel_configuration_model:from_application_env(
           [{tracer_provider, Configuration}]) of
        {ok, Model} ->
            case otel_configuration_sdk:create(Model) of
                {ok, RuntimeConfiguration} ->
                    otel_tracer_provider_sup:start(Name, Resource,
                                                   RuntimeConfiguration);
                {error, _}=Error ->
                    Error
            end;
        {error, _}=Error ->
            Error
    end.
