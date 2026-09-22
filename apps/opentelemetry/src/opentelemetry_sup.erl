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
-module(opentelemetry_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-spec start_link(otel_configuration_sdk:configuration()) ->
          {ok, pid()} | ignore | {error, term()}.
start_link(Configuration) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Configuration).

-spec init(otel_configuration_sdk:configuration()) ->
          {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Configuration) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    SdkSup = #{id => opentelemetry_sdk_sup,
               start => {opentelemetry_sdk_sup, start_link, [Configuration]},
               restart => permanent,
               shutdown => infinity,
               type => supervisor,
               modules => [opentelemetry_sdk_sup]},

    {ok, {SupFlags, [SdkSup]}}.
