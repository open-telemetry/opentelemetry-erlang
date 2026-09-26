%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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
%% @doc Starts and supervises SDK TracerProviders.
%%
%% `start/2' and `start/3' accept the same native Erlang configuration as the
%% `tracer_provider' application environment entry. Configuration is resolved
%% and validated before any provider processes are started.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_tracer_provider_sup).

-behaviour(supervisor).

-export([start_link/0,
         start/2,
         start/3,
         start_resolved/3]).

-export([init/1]).

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

-define(SERVER, ?MODULE).

%% @private
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Starts a named TracerProvider with an empty resource.
-spec start(atom(), configuration()) -> supervisor:startchild_ret().
start(Name, Configuration) ->
    start(Name, otel_resource:create([]), Configuration).

%% @doc Starts a named TracerProvider with the supplied resource.
-spec start(atom(), otel_resource:t(), configuration()) -> supervisor:startchild_ret().
start(Name, Resource, Configuration) when is_atom(Name), is_map(Configuration) ->
    case otel_configuration_sdk:create_tracer_provider(Configuration) of
        {ok, Resolved} ->
            start_resolved(Name, Resource, Resolved);
        {error, _}=Error ->
            Error
    end.

%% @private
%% Application startup has already resolved and validated the configuration.
-spec start_resolved(atom(), otel_resource:t(),
                     otel_configuration_sdk:tracer_provider_configuration()) ->
          supervisor:startchild_ret().
start_resolved(Name, Resource, Config) ->
    try
        supervisor:start_child(?MODULE, [Name, Resource, Config])
    catch
        exit:{noproc, _} ->
            %% no tracer provider sup is started, the sdk is probably disabled
            {error, no_tracer_provider_supervisor}
    end.

%% @private
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},

    TracerServerSup = #{id => otel_tracer_server_sup,
                        start => {otel_tracer_server_sup, start_link, []},
                        restart => permanent,
                        type => supervisor,
                        modules => [otel_tracer_server_sup]},

    {ok, {SupFlags, [TracerServerSup]}}.
